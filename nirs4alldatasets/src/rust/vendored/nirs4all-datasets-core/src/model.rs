// SPDX-License-Identifier: MIT
//! Serde models for the distributable download contract (`catalog/index.json`,
//! index schema 1.0) and the [`Resolved`] view a binding gets back.
//!
//! These mirror, field-for-field, the Python writer in
//! `src/nirs4all_datasets/index.py` — keep the two in sync.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

/// The whole distributable index (`catalog/index.json`).
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Index {
    /// Index-format version (`"1.0"`).
    pub schema: String,
    /// Number of datasets (informational; equals `datasets.len()`).
    #[serde(default)]
    pub n_datasets: usize,
    /// One entry per dataset id.
    pub datasets: BTreeMap<String, IndexEntry>,
}

/// One dataset's complete, version-pinned download contract.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct IndexEntry {
    /// `public` | `private` | `anonymized`.
    pub tier: String,
    /// The personal-Dataverse pin (instance + DOI + dataset_version).
    pub dataverse: DataverseRef,
    /// The canonical files to download (integrity contract).
    #[serde(default)]
    pub files: Vec<FileContract>,
    /// Where open bytes can be fetched from.
    #[serde(default)]
    pub origins: Vec<Origin>,
    /// The tier-sanitized public descriptor (opaque pass-through; never inspected
    /// here — bindings hand it back to the host's analysis layer).
    #[serde(default)]
    pub descriptor: serde_json::Value,
}

/// A pointer to a dataset's personal Dataverse location + pinned version.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DataverseRef {
    /// Base URL, no trailing slash (e.g. `https://entrepot.recherche.data.gouv.fr`).
    pub instance: String,
    /// The minted dataset DOI (`10.x/y`), or `None` until published.
    #[serde(default)]
    pub doi: Option<String>,
    /// The pinned Dataverse version (`"1.0"`, `:latest-published`, …), or `None`.
    #[serde(default)]
    pub dataset_version: Option<String>,
}

/// One canonical file's frozen download + integrity contract.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FileContract {
    /// Basename (e.g. `X.parquet`).
    pub name: String,
    /// Path relative to the dataset directory (e.g. `canonical/sources/X.parquet`);
    /// the cache mirrors this layout so `canonical/dataset.json`'s relative refs
    /// resolve.
    pub relpath: String,
    /// The POSIX parent of `relpath` — the Dataverse `directoryLabel` used at upload;
    /// matched together with `name` (never bare basename, which would collide across
    /// `raw/` and `canonical/sources/`).
    #[serde(default)]
    pub directory_label: String,
    /// Lowercase 64-hex SHA-256 — the authoritative byte-identity pin.
    pub sha256: String,
    /// File size in bytes.
    pub size: u64,
    /// Dataverse datafile id (set once published); used by the Access API.
    #[serde(default)]
    pub file_id: Option<i64>,
}

/// An authoritative home for a dataset's bytes.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Origin {
    /// `dataverse` | `zenodo` | `figshare` | `url` | `script` | `manual`.
    pub kind: String,
    /// `canonical` (byte-verifiable) | `raw` (re-ingested locally).
    pub mode: String,
    /// A version-pinned DOI (`10.x/y`), a URL, or a script path.
    pub locator: String,
    /// `open` | `token` | `manual`.
    pub access: String,
}

/// Visibility / access tier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tier {
    /// Shown + openly fetchable from the origin.
    Public,
    /// Shown; export needs a Dataverse token.
    Private,
    /// Names masked + numeric targets z-scored; export needs a token.
    Anonymized,
}

impl Tier {
    /// Parse a tier string; anything unrecognized is treated as the safe
    /// (token-gated) `Private`.
    pub fn parse(s: &str) -> Tier {
        match s {
            "public" => Tier::Public,
            "anonymized" => Tier::Anonymized,
            _ => Tier::Private,
        }
    }

    /// Whether fetching this tier's bytes needs a Dataverse token.
    pub fn needs_token(self) -> bool {
        !matches!(self, Tier::Public)
    }
}

/// The resolver's answer for one dataset — the JSON a binding receives from
/// `n4ds_resolve` and feeds back into `n4ds_fetch`.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Resolved {
    /// The dataset id.
    pub id: String,
    /// Tier string (`public` | `private` | `anonymized`).
    pub tier: String,
    /// Dataverse instance base URL.
    pub instance: String,
    /// Minted DOI, if any.
    #[serde(default)]
    pub doi: Option<String>,
    /// Pinned dataset version, if any.
    #[serde(default)]
    pub dataset_version: Option<String>,
    /// The canonical files to download.
    pub files: Vec<FileContract>,
    /// Open origins to try when there is no personal DOI.
    #[serde(default)]
    pub origins: Vec<Origin>,
}

impl Resolved {
    /// Build a [`Resolved`] from an index entry + its id.
    pub fn from_entry(id: &str, entry: &IndexEntry) -> Resolved {
        Resolved {
            id: id.to_string(),
            tier: entry.tier.clone(),
            instance: entry.dataverse.instance.clone(),
            doi: entry.dataverse.doi.clone(),
            dataset_version: entry.dataverse.dataset_version.clone(),
            files: entry.files.clone(),
            origins: entry.origins.clone(),
        }
    }

    /// The dataset's tier.
    pub fn tier(&self) -> Tier {
        Tier::parse(&self.tier)
    }
}
