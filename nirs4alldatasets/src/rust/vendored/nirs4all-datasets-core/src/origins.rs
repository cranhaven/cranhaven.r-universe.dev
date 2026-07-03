// SPDX-License-Identifier: MIT
//! Per-repository file resolution: turn a [`Resolved`] dataset into a map of
//! `relpath -> FileLocation` (the download URL + any token to attach).
//!
//! Two acquisition strategies mirror the Python `get()`:
//!
//! * **personal Dataverse** — when the dataset has a minted DOI, every canonical
//!   file is fetched from the Dataverse Access API by its `file_id`
//!   (`/api/access/datafile/<id>`); the `file_id` itself pins the exact bytes of the
//!   published version. A missing `file_id` is resolved by listing the pinned
//!   version's files (Native API `…/versions/{version}/files`).
//! * **open origin** — when there is no personal DOI, the first OPEN *canonical*
//!   origin (Zenodo / figshare / Dataverse) that exposes every canonical file wins.
//!   `url` / `script` / `manual` origins are never auto-fetched (they need a human),
//!   matching the Python `_FETCHABLE_KINDS`.

use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::http::HttpClient;
use crate::model::Resolved;

/// Where and how to download one canonical file.
#[derive(Debug, Clone)]
pub struct FileLocation {
    /// The URL to GET.
    pub url: String,
    /// A Dataverse token to attach in `X-Dataverse-key` — and to **not** replay onto
    /// a signed-storage redirect. `None` for open origins (no auth).
    pub token: Option<String>,
}

/// Build the `relpath -> FileLocation` map for a resolved dataset.
pub fn locate(
    resolved: &Resolved,
    instance: &str,
    token: Option<&str>,
    http: &dyn HttpClient,
) -> Result<HashMap<String, FileLocation>> {
    if resolved.doi.is_some() {
        dataverse_personal(resolved, instance, token, http)
    } else {
        open_origin(resolved, http)
    }
}

/// Personal-Dataverse strategy: download each file by its `file_id`.
fn dataverse_personal(
    resolved: &Resolved,
    instance: &str,
    token: Option<&str>,
    http: &dyn HttpClient,
) -> Result<HashMap<String, FileLocation>> {
    let doi = resolved
        .doi
        .as_deref()
        .expect("caller checked doi is present");
    let attach = if resolved.tier().needs_token() {
        token.map(str::to_string)
    } else {
        None
    };

    // Resolve any missing file ids from the pinned version's file listing.
    let id_by_key: HashMap<(String, String), i64> =
        if resolved.files.iter().any(|f| f.file_id.is_none()) {
            let version = resolved
                .dataset_version
                .as_deref()
                .unwrap_or(":latest-published");
            dataverse::list_version_files(http, instance, doi, version, attach.as_deref())?
                .into_iter()
                .map(|f| ((f.directory_label, f.filename), f.file_id))
                .collect()
        } else {
            HashMap::new()
        };

    let mut out = HashMap::with_capacity(resolved.files.len());
    for f in &resolved.files {
        let file_id = match f.file_id {
            Some(id) => id,
            None => *id_by_key
                .get(&(f.directory_label.clone(), f.name.clone()))
                .ok_or_else(|| {
                    Error::NotFetchable(format!(
                        "Dataverse has no file {:?} under {:?} in the pinned version",
                        f.name, f.directory_label
                    ))
                })?,
        };
        out.insert(
            f.relpath.clone(),
            FileLocation {
                url: dataverse::access_url(instance, file_id),
                token: attach.clone(),
            },
        );
    }
    Ok(out)
}

/// Open-origin strategy: the first OPEN canonical origin that provides every file.
fn open_origin(
    resolved: &Resolved,
    http: &dyn HttpClient,
) -> Result<HashMap<String, FileLocation>> {
    for origin in &resolved.origins {
        if origin.access != "open" || origin.mode != "canonical" {
            continue;
        }
        let by_name = match origin.kind.as_str() {
            "zenodo" => zenodo::list_files(http, &origin.locator),
            "figshare" => figshare::list_files(http, &origin.locator),
            "dataverse" => dataverse::list_files_by_doi(http, &origin.locator),
            _ => continue, // url / script / manual are never auto-fetched
        };
        let Ok(by_name) = by_name else { continue }; // link rot / API change: try the next origin
        let mut out = HashMap::with_capacity(resolved.files.len());
        let mut complete = true;
        for f in &resolved.files {
            match by_name.get(&f.name) {
                Some(url) => {
                    out.insert(
                        f.relpath.clone(),
                        FileLocation {
                            url: url.clone(),
                            token: None,
                        },
                    );
                }
                None => {
                    complete = false;
                    break;
                }
            }
        }
        if complete && !out.is_empty() {
            return Ok(out);
        }
    }
    Err(Error::NotFetchable(
        "no OPEN canonical origin (Zenodo / figshare / Dataverse) exposes all of this dataset's canonical files".into(),
    ))
}

/// `scheme://host` of a URL (drops the path), e.g. for deriving a Dataverse instance.
fn scheme_host(url: &str) -> Option<String> {
    let (scheme, rest) = url.split_once("://")?;
    let host = rest.split('/').next()?;
    if host.is_empty() {
        None
    } else {
        Some(format!("{scheme}://{host}"))
    }
}

// ---------------------------------------------------------------------------
// Dataverse
// ---------------------------------------------------------------------------
pub mod dataverse {
    use std::collections::HashMap;

    use super::scheme_host;
    use crate::error::{Error, Result};
    use crate::http::HttpClient;

    /// One file in a Dataverse version listing.
    pub struct DvFile {
        pub file_id: i64,
        pub filename: String,
        pub directory_label: String,
    }

    /// The Access-API download URL for a datafile id.
    pub fn access_url(instance: &str, file_id: i64) -> String {
        format!(
            "{}/api/access/datafile/{}",
            instance.trim_end_matches('/'),
            file_id
        )
    }

    /// The persistent-id form Dataverse expects (`doi:<suffix>`).
    pub fn persistent_id(doi: &str) -> String {
        if doi.starts_with("doi:") {
            doi.to_string()
        } else {
            format!("doi:{doi}")
        }
    }

    /// Percent-encode a query-parameter value (RFC 3986 unreserved kept verbatim) — the
    /// persistentId carries `:` and `/`, which we encode so every instance accepts it.
    fn encode_query(value: &str) -> String {
        let mut out = String::with_capacity(value.len() + value.len() / 2);
        for b in value.bytes() {
            match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                    out.push(b as char)
                }
                _ => out.push_str(&format!("%{b:02X}")),
            }
        }
        out
    }

    /// List a pinned version's files via the Native API list-files endpoint
    /// (`/versions/{version}/files`, which returns the file metadata array directly).
    pub fn list_version_files(
        http: &dyn HttpClient,
        instance: &str,
        doi: &str,
        version: &str,
        token: Option<&str>,
    ) -> Result<Vec<DvFile>> {
        let url = format!(
            "{}/api/datasets/:persistentId/versions/{}/files?persistentId={}",
            instance.trim_end_matches('/'),
            version,
            encode_query(&persistent_id(doi)),
        );
        let headers: Vec<(String, String)> = token
            .map(|t| vec![("X-Dataverse-key".to_string(), t.to_string())])
            .unwrap_or_default();
        let resp = http.get(&url, &headers, true, None)?;
        if !resp.is_success() {
            return Err(Error::Http(format!(
                "Dataverse version files: HTTP {}",
                resp.status
            )));
        }
        parse_version_files(&resp.text()?)
    }

    fn parse_version_files(text: &str) -> Result<Vec<DvFile>> {
        let v: serde_json::Value = serde_json::from_str(text)?;
        let data = v.get("data").unwrap_or(&v);
        let arr = data
            .as_array()
            .or_else(|| data.get("files").and_then(|f| f.as_array()))
            .ok_or_else(|| Error::Http("Dataverse: unexpected version-files payload".into()))?;
        let mut out = Vec::new();
        for entry in arr {
            let df = entry.get("dataFile").unwrap_or(entry);
            let file_id = df.get("id").and_then(serde_json::Value::as_i64);
            let filename = df
                .get("filename")
                .and_then(serde_json::Value::as_str)
                .or_else(|| entry.get("label").and_then(serde_json::Value::as_str));
            let dir = entry
                .get("directoryLabel")
                .or_else(|| df.get("directoryLabel"))
                .and_then(serde_json::Value::as_str)
                .unwrap_or("");
            if let (Some(id), Some(name)) = (file_id, filename) {
                out.push(DvFile {
                    file_id: id,
                    filename: name.to_string(),
                    directory_label: dir.to_string(),
                });
            }
        }
        Ok(out)
    }

    /// Resolve an open Dataverse DOI to `{filename -> access url}` by deriving the
    /// instance from doi.org and listing its latest published version.
    pub fn list_files_by_doi(http: &dyn HttpClient, doi: &str) -> Result<HashMap<String, String>> {
        let instance = instance_from_doi(http, doi)?;
        let files = list_version_files(http, &instance, doi, ":latest-published", None)?;
        Ok(files
            .into_iter()
            .map(|f| (f.filename, access_url(&instance, f.file_id)))
            .collect())
    }

    /// Follow `https://doi.org/<doi>` without following the redirect and take the
    /// scheme+host of the `Location` as the hosting Dataverse instance.
    pub fn instance_from_doi(http: &dyn HttpClient, doi: &str) -> Result<String> {
        let bare = doi.strip_prefix("doi:").unwrap_or(doi);
        let resp = http.get(&format!("https://doi.org/{bare}"), &[], false, None)?;
        let loc = resp
            .location
            .ok_or_else(|| Error::Http("doi.org did not redirect to a host".into()))?;
        scheme_host(&loc).ok_or_else(|| {
            Error::Http("could not parse a Dataverse instance from the DOI redirect".into())
        })
    }
}

// ---------------------------------------------------------------------------
// Zenodo
// ---------------------------------------------------------------------------
pub mod zenodo {
    use std::collections::HashMap;

    use crate::error::{Error, Result};
    use crate::http::HttpClient;

    /// The Zenodo record id from a `10.5281/zenodo.<id>` DOI (or a bare locator).
    pub fn record_id(locator: &str) -> Option<String> {
        let l = locator.trim();
        l.rfind("zenodo.")
            .map(|i| l[i + "zenodo.".len()..].trim_matches('/').to_string())
            .filter(|s| !s.is_empty())
    }

    /// Resolve a Zenodo DOI to `{filename -> download url}`.
    pub fn list_files(http: &dyn HttpClient, locator: &str) -> Result<HashMap<String, String>> {
        let rec = record_id(locator)
            .ok_or_else(|| Error::NotFetchable(format!("not a Zenodo DOI: {locator:?}")))?;
        let resp = http.get(
            &format!("https://zenodo.org/api/records/{rec}"),
            &[],
            true,
            None,
        )?;
        if !resp.is_success() {
            return Err(Error::Http(format!("Zenodo record: HTTP {}", resp.status)));
        }
        parse_files(&resp.text()?)
    }

    fn parse_files(text: &str) -> Result<HashMap<String, String>> {
        let v: serde_json::Value = serde_json::from_str(text)?;
        let files = v
            .get("files")
            .and_then(serde_json::Value::as_array)
            .ok_or_else(|| Error::Http("Zenodo: no files[]".into()))?;
        let mut out = HashMap::new();
        for f in files {
            let key = f.get("key").and_then(serde_json::Value::as_str);
            let url = f
                .get("links")
                .and_then(|l| l.get("self"))
                .and_then(serde_json::Value::as_str);
            if let (Some(k), Some(u)) = (key, url) {
                out.insert(k.to_string(), u.to_string());
            }
        }
        Ok(out)
    }
}

// ---------------------------------------------------------------------------
// figshare
// ---------------------------------------------------------------------------
pub mod figshare {
    use std::collections::HashMap;

    use crate::error::{Error, Result};
    use crate::http::HttpClient;

    /// The figshare article id from a `10.6084/m9.figshare.<id>[.v<n>]` DOI/locator.
    pub fn article_id(locator: &str) -> Option<String> {
        let l = locator.trim();
        let i = l.rfind("figshare.")?;
        let tail = &l[i + "figshare.".len()..];
        let id = tail.split(['.', '/']).next()?; // drop a ".v2" version suffix
        if !id.is_empty() && id.chars().all(|c| c.is_ascii_digit()) {
            Some(id.to_string())
        } else {
            None
        }
    }

    /// Resolve a figshare DOI to `{filename -> download url}`.
    pub fn list_files(http: &dyn HttpClient, locator: &str) -> Result<HashMap<String, String>> {
        let id = article_id(locator)
            .ok_or_else(|| Error::NotFetchable(format!("not a figshare DOI: {locator:?}")))?;
        let resp = http.get(
            &format!("https://api.figshare.com/v2/articles/{id}"),
            &[],
            true,
            None,
        )?;
        if !resp.is_success() {
            return Err(Error::Http(format!(
                "figshare article: HTTP {}",
                resp.status
            )));
        }
        parse_files(&resp.text()?)
    }

    fn parse_files(text: &str) -> Result<HashMap<String, String>> {
        let v: serde_json::Value = serde_json::from_str(text)?;
        let files = v
            .get("files")
            .and_then(serde_json::Value::as_array)
            .ok_or_else(|| Error::Http("figshare: no files[]".into()))?;
        let mut out = HashMap::new();
        for f in files {
            let name = f.get("name").and_then(serde_json::Value::as_str);
            let url = f.get("download_url").and_then(serde_json::Value::as_str);
            if let (Some(n), Some(u)) = (name, url) {
                out.insert(n.to_string(), u.to_string());
            }
        }
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zenodo_and_figshare_id_parsing() {
        assert_eq!(
            zenodo::record_id("10.5281/zenodo.123456").as_deref(),
            Some("123456")
        );
        assert_eq!(
            figshare::article_id("10.6084/m9.figshare.987654.v3").as_deref(),
            Some("987654")
        );
        assert_eq!(figshare::article_id("not-a-doi"), None);
    }

    #[test]
    fn dataverse_access_url_and_pid() {
        assert_eq!(
            dataverse::access_url("https://dv.example/", 42),
            "https://dv.example/api/access/datafile/42"
        );
        assert_eq!(dataverse::persistent_id("10.70112/ABC"), "doi:10.70112/ABC");
        assert_eq!(
            dataverse::persistent_id("doi:10.70112/ABC"),
            "doi:10.70112/ABC"
        );
    }

    #[test]
    fn scheme_host_extracts_instance() {
        assert_eq!(
            scheme_host("https://entrepot.recherche.data.gouv.fr/dataset.xhtml?x=1").as_deref(),
            Some("https://entrepot.recherche.data.gouv.fr")
        );
    }
}
