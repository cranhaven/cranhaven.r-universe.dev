// SPDX-License-Identifier: MIT
//! Dataset-acquisition core for the **nirs4all-datasets** reference bank.
//!
//! This crate owns exactly the surface the feasibility study (`migration_ABI_C.md`)
//! moves out of Python: turning a dataset id into verified, cached canonical bytes.
//! It is pure I/O + integrity — **no scientific stack, no `nirs4all`**. The card /
//! qualify / site / health analysis stays in Python on the maintainer side.
//!
//! The flow mirrors the Python `get()` it replaces:
//!
//! 1. [`resolve`] reads the distributable `catalog/index.json` (the cross-language
//!    download contract) and returns a [`Resolved`] dataset — tier, the Dataverse
//!    instance / DOI / pinned `dataset_version`, the per-file list
//!    (`name`, `relpath`, `sha256`, `size`, `file_id`, `directory_label`), and the
//!    origin sources.
//! 2. [`fetch`] downloads each canonical file from its pinned origin (the personal
//!    Dataverse by `file_id`, else an OPEN canonical origin on Zenodo / figshare /
//!    Dataverse), streaming with an **incremental SHA-256** and an atomic write into
//!    a pooch-style OS cache. A Dataverse token travels only in the
//!    `X-Dataverse-key` header and is **never replayed onto a signed-storage
//!    redirect** (the redirect is followed without it).
//! 3. [`verify_cached`] re-checks an already-cached directory offline.
//!
//! All network I/O goes through the [`HttpClient`] trait, so the resolvers and the
//! fetch orchestration are unit-tested against an injected fake client exactly as the
//! Python tests inject a fake `requests.Session` — no network in the test suite.
//!
//! The C ABI (`crates/nirs4all-datasets-capi`) and every language binding are thin
//! JSON-on-the-wire wrappers over [`resolve`], [`fetch`], and [`verify_cached`].

// Always available (pure, portable — these compile on wasm32-unknown-unknown too).
pub mod error;
pub mod hash;
pub mod http;
pub mod model;
pub mod resolve;

// The native surface: filesystem cache + downloading + offline verify. Gated behind
// `net` (default) so `--no-default-features` yields a pure resolve core for WASM.
#[cfg(feature = "net")]
pub mod cache;
#[cfg(feature = "net")]
pub mod fetch;
#[cfg(feature = "net")]
pub mod origins;
#[cfg(feature = "net")]
pub mod verify;

pub use error::{Error, Result};
pub use http::{HttpClient, HttpResponse};
pub use model::{DataverseRef, FileContract, Index, IndexEntry, Origin, Resolved, Tier};
pub use resolve::{resolve, resolve_json};

/// The catalog index-schema version this core reads (`catalog/index.json` `"schema"`).
pub const INDEX_SCHEMA: &str = "1.0";

#[cfg(feature = "net")]
pub use fetch::{fetch, FetchOptions, FetchResult, FileStatus};
#[cfg(feature = "net")]
pub use http::UreqClient;
#[cfg(feature = "net")]
pub use verify::{verify_cached, VerifyEntry, VerifyResult};
