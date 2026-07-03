// SPDX-License-Identifier: MIT
//! Resolve a dataset id, via the distributable index, to its complete download
//! contract. Pure and offline — it only reads JSON.

use crate::error::{Error, Result};
use crate::model::{Index, Resolved};

/// Look up `dataset_id` in a parsed [`Index`] and return its [`Resolved`] contract.
pub fn resolve(index: &Index, dataset_id: &str) -> Result<Resolved> {
    index
        .datasets
        .get(dataset_id)
        .map(|entry| Resolved::from_entry(dataset_id, entry))
        .ok_or_else(|| Error::UnknownDataset(dataset_id.to_string()))
}

/// Parse an index JSON string and resolve `dataset_id`, returning the [`Resolved`]
/// contract serialized as JSON (the C ABI's `n4ds_resolve` payload).
pub fn resolve_json(index_json: &str, dataset_id: &str) -> Result<String> {
    let index: Index = serde_json::from_str(index_json)?;
    let resolved = resolve(&index, dataset_id)?;
    Ok(serde_json::to_string(&resolved)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    const INDEX: &str = r#"{
      "schema": "1.0", "n_datasets": 1,
      "datasets": {
        "demo": {
          "tier": "public",
          "dataverse": {"instance": "https://dv.example", "doi": "10.70112/ABC", "dataset_version": "1.0"},
          "files": [{"name":"X.parquet","relpath":"canonical/sources/X.parquet","directory_label":"canonical/sources","sha256":"aa","size":9,"file_id":42}],
          "origins": [{"kind":"zenodo","mode":"canonical","locator":"10.5281/zenodo.5","access":"open"}],
          "descriptor": {"id":"demo"}
        }
      }
    }"#;

    #[test]
    fn resolves_known_dataset() {
        let r = resolve_json(INDEX, "demo").unwrap();
        let v: serde_json::Value = serde_json::from_str(&r).unwrap();
        assert_eq!(v["id"], "demo");
        assert_eq!(v["tier"], "public");
        assert_eq!(v["instance"], "https://dv.example");
        assert_eq!(v["doi"], "10.70112/ABC");
        assert_eq!(v["files"][0]["file_id"], 42);
    }

    #[test]
    fn unknown_dataset_errors() {
        let err = resolve_json(INDEX, "nope").unwrap_err();
        assert!(matches!(err, Error::UnknownDataset(_)));
    }
}
