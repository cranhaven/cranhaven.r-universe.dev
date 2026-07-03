// SPDX-License-Identifier: MIT
//! Offline integrity re-check of an already-cached dataset directory (no network) —
//! the C ABI's `n4ds_verify_cached`.

use std::path::Path;

use serde::Serialize;

use crate::cache::safe_join;
use crate::error::Result;
use crate::hash::sha256_hex_file;
use crate::model::Resolved;

/// The verification outcome for one file.
#[derive(Debug, Clone, Serialize)]
pub struct VerifyEntry {
    /// Basename.
    pub name: String,
    /// Path relative to the dataset directory.
    pub relpath: String,
    /// `ok` | `missing` | `corrupt`.
    pub status: String,
}

/// The verification outcome for a dataset directory.
#[derive(Debug, Clone, Serialize)]
pub struct VerifyResult {
    /// The directory that was checked.
    pub dir: String,
    /// `true` iff every file is present and matches its pinned SHA-256.
    pub ok: bool,
    /// One entry per canonical file.
    pub files: Vec<VerifyEntry>,
}

/// Re-check every canonical file under `dir` against the resolved contract's hashes.
pub fn verify_cached(resolved: &Resolved, dir: &Path) -> Result<VerifyResult> {
    let mut files = Vec::with_capacity(resolved.files.len());
    let mut all_ok = true;
    for f in &resolved.files {
        let path = safe_join(dir, &f.relpath)?;
        let status = if !path.exists() {
            all_ok = false;
            "missing"
        } else if sha256_hex_file(&path)? == f.sha256.to_lowercase() {
            "ok"
        } else {
            all_ok = false;
            "corrupt"
        };
        files.push(VerifyEntry {
            name: f.name.clone(),
            relpath: f.relpath.clone(),
            status: status.to_string(),
        });
    }
    Ok(VerifyResult {
        dir: dir.to_string_lossy().into_owned(),
        ok: all_ok,
        files,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hash::sha256_hex_bytes;
    use crate::model::{FileContract, Resolved};

    fn resolved(body: &[u8]) -> Resolved {
        Resolved {
            id: "demo".into(),
            tier: "public".into(),
            instance: "https://dv.example".into(),
            doi: None,
            dataset_version: None,
            files: vec![FileContract {
                name: "X.parquet".into(),
                relpath: "canonical/sources/X.parquet".into(),
                directory_label: "canonical/sources".into(),
                sha256: sha256_hex_bytes(body),
                size: body.len() as u64,
                file_id: None,
            }],
            origins: vec![],
        }
    }

    #[test]
    fn reports_ok_missing_and_corrupt() {
        let dir = tempfile::tempdir().unwrap();
        let r = resolved(b"good-bytes");

        // missing
        assert_eq!(
            verify_cached(&r, dir.path()).unwrap().files[0].status,
            "missing"
        );
        assert!(!verify_cached(&r, dir.path()).unwrap().ok);

        // ok
        let p = dir.path().join("canonical/sources/X.parquet");
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(&p, b"good-bytes").unwrap();
        let res = verify_cached(&r, dir.path()).unwrap();
        assert!(res.ok);
        assert_eq!(res.files[0].status, "ok");

        // corrupt
        std::fs::write(&p, b"tampered!!").unwrap();
        let res = verify_cached(&r, dir.path()).unwrap();
        assert!(!res.ok);
        assert_eq!(res.files[0].status, "corrupt");
    }
}
