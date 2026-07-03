// SPDX-License-Identifier: MIT
//! The pooch-style local cache layout + atomic writes.
//!
//! The default cache root matches `pooch.os_cache("nirs4all-datasets")`
//! (`~/.cache/nirs4all-datasets` on Linux, `~/Library/Caches/nirs4all-datasets` on
//! macOS, `%LOCALAPPDATA%\nirs4all-datasets\cache` on Windows). A dataset is cached
//! under `<root>/<id>/`, with files written at their full `relpath`
//! (`<root>/<id>/canonical/sources/X.parquet`) so `canonical/dataset.json`'s relative
//! references resolve without rewriting.

use std::fs;
use std::io::Write;
use std::path::{Component, Path, PathBuf};

use directories::ProjectDirs;

use crate::error::{Error, Result};

/// Join a contract's `relpath` under `base`, rejecting anything that could escape the
/// cache directory: an absolute path, a Windows prefix, a root, or any `..` component.
/// The index is normally trusted, but a stale/hostile contract must never be able to
/// read or write outside `<cache>/<id>` — defense in depth for both fetch and verify.
pub fn safe_join(base: &Path, relpath: &str) -> Result<PathBuf> {
    let mut out = base.to_path_buf();
    let mut pushed = false;
    for component in Path::new(relpath).components() {
        match component {
            Component::Normal(c) => {
                out.push(c);
                pushed = true;
            }
            Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                return Err(Error::InvalidArgument(format!(
                    "unsafe path in contract (escapes the cache dir): {relpath:?}"
                )));
            }
        }
    }
    if !pushed {
        return Err(Error::InvalidArgument(format!(
            "empty or invalid relpath in contract: {relpath:?}"
        )));
    }
    Ok(out)
}

/// The OS cache directory for downloaded datasets (the pooch `os_cache` equivalent).
pub fn default_cache_dir() -> Result<PathBuf> {
    ProjectDirs::from("", "", "nirs4all-datasets")
        .map(|d| d.cache_dir().to_path_buf())
        .ok_or_else(|| {
            Error::Io(std::io::Error::other(
                "could not determine an OS cache directory",
            ))
        })
}

/// Write `bytes` to `path` atomically: a sibling `*.part` temp file then a rename
/// (so a reader never sees a half-written file). Parent directories are created.
pub fn atomic_write(path: &Path, bytes: &[u8]) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let tmp = part_path(path);
    {
        let mut f = fs::File::create(&tmp)?;
        f.write_all(bytes)?;
        f.flush()?;
    }
    fs::rename(&tmp, path)?;
    Ok(())
}

/// The temp path used during an atomic write/download (`<path>.part`).
pub(crate) fn part_path(path: &Path) -> PathBuf {
    let mut name = path
        .file_name()
        .map(|n| n.to_os_string())
        .unwrap_or_default();
    name.push(".part");
    path.with_file_name(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atomic_write_creates_parents_and_no_leftover_tmp() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("a/b/c/file.bin");
        atomic_write(&target, b"hello").unwrap();
        assert_eq!(fs::read(&target).unwrap(), b"hello");
        assert!(!part_path(&target).exists());
    }

    #[test]
    fn default_cache_dir_is_namespaced() {
        let d = default_cache_dir().unwrap();
        assert!(d.to_string_lossy().contains("nirs4all-datasets"));
    }

    #[test]
    fn safe_join_allows_normal_relpaths_and_rejects_escapes() {
        let base = Path::new("/cache/demo");
        assert_eq!(
            safe_join(base, "canonical/sources/X.parquet").unwrap(),
            base.join("canonical/sources/X.parquet")
        );
        assert_eq!(
            safe_join(base, "./canonical/X.parquet").unwrap(),
            base.join("canonical/X.parquet")
        );
        for bad in ["../escape", "canonical/../../etc/passwd", "/etc/passwd", ""] {
            assert!(safe_join(base, bad).is_err(), "{bad:?} must be rejected");
        }
    }
}
