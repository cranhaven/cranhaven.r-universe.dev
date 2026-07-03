// SPDX-License-Identifier: MIT
//! The fetch orchestrator: download every canonical file of a [`Resolved`] dataset
//! into the cache, streaming with an incremental SHA-256 and an atomic rename, and
//! return a per-file status. Mirrors the Python `fetch_by_doi` / `fetch_private` it
//! replaces — including the token gate and the redirect-safe Dataverse download.

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::cache::{default_cache_dir, part_path, safe_join};
use crate::error::{Error, Result};
use crate::hash::{sha256_hex_file, HashingWriter};
use crate::http::HttpClient;
use crate::model::Resolved;
use crate::origins::{locate, FileLocation};

/// Options for [`fetch`] (the C ABI's `opts_json`).
#[derive(Debug, Clone, Default, Deserialize)]
pub struct FetchOptions {
    /// Cache root (defaults to the OS cache dir).
    #[serde(default)]
    pub cache_dir: Option<String>,
    /// Dataverse token for private / anonymized datasets.
    #[serde(default)]
    pub token: Option<String>,
    /// Override the dataset's Dataverse instance.
    #[serde(default)]
    pub instance: Option<String>,
    /// Per-request read timeout in seconds (used by the real client, not the seam).
    #[serde(default)]
    pub timeout_secs: Option<u64>,
}

impl FetchOptions {
    /// Parse options from JSON (an empty/`null` string yields the defaults).
    pub fn from_json(opts_json: &str) -> Result<FetchOptions> {
        let t = opts_json.trim();
        if t.is_empty() || t == "null" {
            return Ok(FetchOptions::default());
        }
        Ok(serde_json::from_str(t)?)
    }
}

/// The outcome for one file.
#[derive(Debug, Clone, Serialize)]
pub struct FileStatus {
    /// Basename.
    pub name: String,
    /// Path relative to the dataset directory.
    pub relpath: String,
    /// Absolute path in the cache.
    pub path: String,
    /// `cached` (already present + verified) or `downloaded`.
    pub status: String,
}

/// The result of a fetch: the dataset directory + per-file statuses.
#[derive(Debug, Clone, Serialize)]
pub struct FetchResult {
    /// The dataset directory (`<cache>/<id>`); its `canonical/dataset.json` is loadable.
    pub dir: String,
    /// One entry per canonical file.
    pub files: Vec<FileStatus>,
}

/// Download (or reuse from cache) every canonical file of `resolved`.
pub fn fetch(
    resolved: &Resolved,
    opts: &FetchOptions,
    http: &dyn HttpClient,
) -> Result<FetchResult> {
    if resolved.files.is_empty() {
        return Err(Error::NotFetchable(
            "the index records no canonical files for this dataset".into(),
        ));
    }
    let cache_root = match &opts.cache_dir {
        Some(p) => std::path::PathBuf::from(p),
        None => default_cache_dir()?,
    };
    let dataset_dir = cache_root.join(&resolved.id);
    let instance = opts
        .instance
        .clone()
        .unwrap_or_else(|| resolved.instance.clone());

    // Validate every relpath (it must not escape the cache dir) and classify each file
    // as already-cached (present + verified) or needing a download — in one pass.
    let mut plans: Vec<(&crate::model::FileContract, std::path::PathBuf, bool)> =
        Vec::with_capacity(resolved.files.len());
    for f in &resolved.files {
        let dest = safe_join(&dataset_dir, &f.relpath)?;
        let cached = dest.exists()
            && sha256_hex_file(&dest)
                .map(|s| s == f.sha256.to_lowercase())
                .unwrap_or(false);
        plans.push((f, dest, cached));
    }
    let all_cached = plans.iter().all(|(_, _, cached)| *cached);

    // Token gate (mirrors Python): a private/anonymized dataset needs a token unless
    // every file is already cached locally.
    if resolved.tier().needs_token() && opts.token.is_none() && !all_cached {
        return Err(Error::TokenRequired(format!(
            "{:?} is tier {:?}: a Dataverse token is required to fetch it (pass a token, set NIRS4ALL_DATAVERSE_TOKEN, or configure ~/.config/nirs4all-datasets/config.toml).",
            resolved.id, resolved.tier
        )));
    }

    // Only resolve download locations if at least one file must be fetched.
    let locations: HashMap<String, FileLocation> = if all_cached {
        HashMap::new()
    } else {
        locate(resolved, &instance, opts.token.as_deref(), http)?
    };

    let mut statuses = Vec::with_capacity(resolved.files.len());
    for (f, dest, cached) in &plans {
        if *cached {
            statuses.push(FileStatus {
                name: f.name.clone(),
                relpath: f.relpath.clone(),
                path: dest.to_string_lossy().into_owned(),
                status: "cached".into(),
            });
            continue;
        }
        let loc = locations.get(&f.relpath).ok_or_else(|| {
            Error::NotFetchable(format!("no download location for {}", f.relpath))
        })?;
        download_verified(http, loc, dest, &f.name, &f.sha256)?;
        statuses.push(FileStatus {
            name: f.name.clone(),
            relpath: f.relpath.clone(),
            path: dest.to_string_lossy().into_owned(),
            status: "downloaded".into(),
        });
    }

    Ok(FetchResult {
        dir: dataset_dir.to_string_lossy().into_owned(),
        files: statuses,
    })
}

/// GET one file (redirect-safe for the token path), stream it to a temp file while
/// hashing, verify the SHA-256, then atomically rename into place.
fn download_verified(
    http: &dyn HttpClient,
    loc: &FileLocation,
    dest: &Path,
    name: &str,
    expected_sha: &str,
) -> Result<()> {
    if let Some(parent) = dest.parent() {
        fs::create_dir_all(parent)?;
    }

    // The token-carrying request must NOT follow the redirect to signed storage
    // (the key would leak); follow the Location manually, without the token.
    let resp = match &loc.token {
        Some(tok) => {
            let r = http.get(
                &loc.url,
                &[("X-Dataverse-key".to_string(), tok.clone())],
                false,
                None,
            )?;
            if r.is_redirect() {
                let location = r.location.clone().ok_or_else(|| {
                    Error::Http(format!(
                        "download {name} redirected without a Location header"
                    ))
                })?;
                http.get(&location, &[], true, None)?
            } else {
                r
            }
        }
        None => http.get(&loc.url, &[], true, None)?,
    };
    if !resp.is_success() {
        return Err(Error::Http(format!(
            "download {name} failed: HTTP {}",
            resp.status
        )));
    }

    // Stream to a sibling `.part`, hashing as we go; remove it on any failure so a
    // partial/corrupt temp is never left behind.
    let tmp = part_path(dest);
    let mut body = resp.body;
    let mut hw = HashingWriter::new(fs::File::create(&tmp)?);
    if let Err(e) = std::io::copy(&mut body, &mut hw) {
        drop(hw);
        let _ = fs::remove_file(&tmp);
        return Err(e.into());
    }
    let (file, sha, _n) = hw.finish();
    file.sync_all().ok();

    if sha != expected_sha.to_lowercase() {
        let _ = fs::remove_file(&tmp);
        return Err(Error::ChecksumMismatch {
            name: name.to_string(),
            expected: expected_sha.to_string(),
            actual: sha,
        });
    }
    // Replace-portable: drop any existing (e.g. corrupt) destination first so the
    // rename succeeds on every platform (Windows rename is not replace-by-default).
    if dest.exists() {
        let _ = fs::remove_file(dest);
    }
    if let Err(e) = fs::rename(&tmp, dest) {
        let _ = fs::remove_file(&tmp);
        return Err(e.into());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hash::sha256_hex_bytes;
    use crate::http::testing::{MockHttp, MockResponse};
    use crate::model::{FileContract, Origin, Resolved};

    fn resolved_with(
        files: Vec<FileContract>,
        doi: Option<&str>,
        tier: &str,
        origins: Vec<Origin>,
    ) -> Resolved {
        Resolved {
            id: "demo".into(),
            tier: tier.into(),
            instance: "https://dv.example".into(),
            doi: doi.map(str::to_string),
            dataset_version: Some("1.0".into()),
            files,
            origins,
        }
    }

    fn fc(
        name: &str,
        relpath: &str,
        dir: &str,
        body: &[u8],
        file_id: Option<i64>,
    ) -> (FileContract, Vec<u8>) {
        (
            FileContract {
                name: name.into(),
                relpath: relpath.into(),
                directory_label: dir.into(),
                sha256: sha256_hex_bytes(body),
                size: body.len() as u64,
                file_id,
            },
            body.to_vec(),
        )
    }

    #[test]
    fn fetches_public_dataverse_by_file_id_and_verifies() {
        let (f, body) = fc(
            "X.parquet",
            "canonical/sources/X.parquet",
            "canonical/sources",
            b"PARQUET-BYTES",
            Some(11),
        );
        let resolved = resolved_with(vec![f], Some("10.70112/ABC"), "public", vec![]);
        let body2 = body.clone();
        let http = MockHttp::new(move |call| {
            assert!(call.url.contains("/api/access/datafile/11"));
            MockResponse::ok(body2.clone())
        });
        let dir = tempfile::tempdir().unwrap();
        let opts = FetchOptions {
            cache_dir: Some(dir.path().to_string_lossy().into()),
            ..Default::default()
        };
        let res = fetch(&resolved, &opts, &http).unwrap();
        assert_eq!(res.files[0].status, "downloaded");
        let got = std::fs::read(dir.path().join("demo/canonical/sources/X.parquet")).unwrap();
        assert_eq!(got, body);
        // a second fetch is a cache hit (no new download)
        let res2 = fetch(&resolved, &opts, &http).unwrap();
        assert_eq!(res2.files[0].status, "cached");
    }

    #[test]
    fn private_download_does_not_replay_token_on_redirect() {
        let (f, body) = fc(
            "X.parquet",
            "canonical/sources/X.parquet",
            "canonical/sources",
            b"secret-bytes",
            Some(7),
        );
        let resolved = resolved_with(vec![f], Some("10.70112/PRIV"), "private", vec![]);
        let body2 = body.clone();
        let http = MockHttp::new(move |call| {
            if call.url.contains("/api/access/datafile/7") {
                assert_eq!(call.header("X-Dataverse-key"), Some("TKN"));
                assert!(
                    !call.follow_redirects,
                    "the token request must not auto-follow redirects"
                );
                MockResponse::redirect("https://s3.example/signed?token=storage")
            } else {
                // the storage redirect must NOT carry the Dataverse key
                assert!(call.url.starts_with("https://s3.example/"));
                assert_eq!(call.header("X-Dataverse-key"), None);
                MockResponse::ok(body2.clone())
            }
        });
        let dir = tempfile::tempdir().unwrap();
        let opts = FetchOptions {
            cache_dir: Some(dir.path().to_string_lossy().into()),
            token: Some("TKN".into()),
            ..Default::default()
        };
        let res = fetch(&resolved, &opts, &http).unwrap();
        assert_eq!(res.files[0].status, "downloaded");
        assert_eq!(
            std::fs::read(dir.path().join("demo/canonical/sources/X.parquet")).unwrap(),
            body
        );
    }

    #[test]
    fn private_without_token_is_rejected_before_any_network() {
        let (f, _body) = fc(
            "X.parquet",
            "canonical/sources/X.parquet",
            "canonical/sources",
            b"x",
            Some(7),
        );
        let resolved = resolved_with(vec![f], Some("10.70112/PRIV"), "private", vec![]);
        let http = MockHttp::new(|_| panic!("no network must happen"));
        let dir = tempfile::tempdir().unwrap();
        let opts = FetchOptions {
            cache_dir: Some(dir.path().to_string_lossy().into()),
            ..Default::default()
        };
        let err = fetch(&resolved, &opts, &http).unwrap_err();
        assert!(matches!(err, Error::TokenRequired(_)));
        assert_eq!(http.call_count(), 0);
    }

    #[test]
    fn checksum_mismatch_is_rejected_and_leaves_no_file() {
        let f = FileContract {
            name: "X.parquet".into(),
            relpath: "canonical/sources/X.parquet".into(),
            directory_label: "canonical/sources".into(),
            sha256: "0".repeat(64),
            size: 3,
            file_id: Some(1),
        };
        let resolved = resolved_with(vec![f], Some("10.70112/ABC"), "public", vec![]);
        let http = MockHttp::new(|_| MockResponse::ok(b"abc".to_vec()));
        let dir = tempfile::tempdir().unwrap();
        let opts = FetchOptions {
            cache_dir: Some(dir.path().to_string_lossy().into()),
            ..Default::default()
        };
        let err = fetch(&resolved, &opts, &http).unwrap_err();
        assert!(matches!(err, Error::ChecksumMismatch { .. }));
        assert!(!dir.path().join("demo/canonical/sources/X.parquet").exists());
        assert!(!part_path(&dir.path().join("demo/canonical/sources/X.parquet")).exists());
    }

    #[test]
    fn fetches_from_open_zenodo_origin() {
        let (f, body) = fc(
            "X.parquet",
            "canonical/sources/X.parquet",
            "canonical/sources",
            b"zenodo-bytes",
            None,
        );
        let origins = vec![Origin {
            kind: "zenodo".into(),
            mode: "canonical".into(),
            locator: "10.5281/zenodo.42".into(),
            access: "open".into(),
        }];
        let resolved = resolved_with(vec![f], None, "public", origins);
        let body2 = body.clone();
        let http = MockHttp::new(move |call| {
            if call.url == "https://zenodo.org/api/records/42" {
                MockResponse::ok(br#"{"files":[{"key":"X.parquet","links":{"self":"https://zenodo.org/dl/X.parquet"}}]}"#.to_vec())
            } else {
                assert_eq!(call.url, "https://zenodo.org/dl/X.parquet");
                MockResponse::ok(body2.clone())
            }
        });
        let dir = tempfile::tempdir().unwrap();
        let opts = FetchOptions {
            cache_dir: Some(dir.path().to_string_lossy().into()),
            ..Default::default()
        };
        let res = fetch(&resolved, &opts, &http).unwrap();
        assert_eq!(res.files[0].status, "downloaded");
        assert_eq!(
            std::fs::read(dir.path().join("demo/canonical/sources/X.parquet")).unwrap(),
            body
        );
    }

    #[test]
    fn no_doi_and_no_open_origin_is_not_fetchable() {
        let (f, _) = fc(
            "X.parquet",
            "canonical/sources/X.parquet",
            "canonical/sources",
            b"x",
            None,
        );
        let origins = vec![Origin {
            kind: "url".into(),
            mode: "raw".into(),
            locator: "https://vendor/".into(),
            access: "manual".into(),
        }];
        let resolved = resolved_with(vec![f], None, "public", origins);
        let http = MockHttp::new(|_| MockResponse::status(404));
        let dir = tempfile::tempdir().unwrap();
        let opts = FetchOptions {
            cache_dir: Some(dir.path().to_string_lossy().into()),
            ..Default::default()
        };
        assert!(matches!(
            fetch(&resolved, &opts, &http).unwrap_err(),
            Error::NotFetchable(_)
        ));
    }
}
