// SPDX-License-Identifier: MIT
//! The crate's error type. Messages are token-free (a Dataverse key is never
//! interpolated into an error) so they are safe to surface across the C ABI.

use thiserror::Error;

/// Anything that can go wrong while resolving, downloading, or verifying a dataset.
#[derive(Debug, Error)]
pub enum Error {
    /// A null / non-UTF-8 / malformed argument from the caller (maps to the ABI's
    /// `N4DS_ERR_INVALID_ARGUMENT`).
    #[error("invalid argument: {0}")]
    InvalidArgument(String),

    /// The requested dataset id is not present in the index.
    #[error("dataset {0:?} is not in the index")]
    UnknownDataset(String),

    /// A private / anonymized dataset must be fetched but no token is available.
    #[error("{0}")]
    TokenRequired(String),

    /// Nothing in the contract is auto-fetchable (no DOI, no open canonical origin).
    #[error("{0}")]
    NotFetchable(String),

    /// A download completed but its bytes did not match the pinned SHA-256.
    #[error("checksum mismatch for {name}: expected {expected}, got {actual}")]
    ChecksumMismatch {
        /// The file that failed verification.
        name: String,
        /// The SHA-256 recorded in the index (the pin).
        expected: String,
        /// The SHA-256 actually downloaded.
        actual: String,
    },

    /// A transport-level HTTP failure or a non-success status. The message never
    /// contains a URL query string (a token could ride there).
    #[error("http: {0}")]
    Http(String),

    /// JSON (de)serialization failed.
    #[error("json: {0}")]
    Json(#[from] serde_json::Error),

    /// A filesystem / cache error.
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
}

/// Convenience alias.
pub type Result<T> = std::result::Result<T, Error>;
