// SPDX-License-Identifier: MIT
//! Stable C ABI for `nirs4all-datasets` (symbol prefix `n4ds_`).
//!
//! JSON-string in / JSON-string out — `resolve` / `fetch` / `verify_cached`. No
//! arrays or scientific handles cross the boundary (the host reads the verified
//! Parquet natively). Memory: the caller frees strings returned by the library with
//! [`n4ds_string_free`] and contexts with [`n4ds_context_destroy`]; never free a
//! library pointer with the host allocator (`bindings/SPEC.md` §4). Every fallible
//! call returns an [`n4ds_status_t`]; on a non-OK status the message is in the
//! context's error buffer (copy it before the next call on that context).
#![allow(non_camel_case_types)] // C-ABI snake_case type names + N4DS_* variants are intentional.

use std::ffi::{c_char, CStr, CString};
use std::path::Path;

use nirs4all_datasets_core::fetch::FetchOptions;
use nirs4all_datasets_core::model::Resolved;
use nirs4all_datasets_core::{fetch, resolve_json, verify_cached, Error, UreqClient};

/// ABI version string. Independent of crate semver; bump on an ABI change.
pub const N4DS_ABI_VERSION: &str = "0.1.0";
const ABI_MAJOR: u32 = 0;
const ABI_MINOR: u32 = 1;

/// Status code returned by every fallible call. `N4DS_OK == 0`.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum n4ds_status_t {
    /// Success.
    N4DS_OK = 0,
    /// A null / non-UTF-8 / malformed-JSON argument.
    N4DS_ERR_INVALID_ARGUMENT = 2,
    /// The dataset id is not in the index.
    N4DS_ERR_NOT_FOUND = 3,
    /// A private/anonymized dataset needs a Dataverse token that was not provided.
    N4DS_ERR_TOKEN_REQUIRED = 4,
    /// Nothing in the contract is auto-fetchable (no DOI, no open canonical origin).
    N4DS_ERR_NOT_FETCHABLE = 5,
    /// A download did not match its pinned SHA-256.
    N4DS_ERR_CHECKSUM = 6,
    /// A transport-level HTTP failure or a non-success status.
    N4DS_ERR_HTTP = 7,
    /// A filesystem / cache error.
    N4DS_ERR_IO = 8,
    /// ABI major mismatch.
    N4DS_ERR_ABI_MISMATCH = 12,
    /// Library minor older than the header requested.
    N4DS_ERR_VERSION_INCOMPATIBLE = 15,
    /// Unexpected internal error.
    N4DS_ERR_INTERNAL = 255,
}

fn status_for(err: &Error) -> n4ds_status_t {
    match err {
        Error::InvalidArgument(_) | Error::Json(_) => n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT,
        Error::UnknownDataset(_) => n4ds_status_t::N4DS_ERR_NOT_FOUND,
        Error::TokenRequired(_) => n4ds_status_t::N4DS_ERR_TOKEN_REQUIRED,
        Error::NotFetchable(_) => n4ds_status_t::N4DS_ERR_NOT_FETCHABLE,
        Error::ChecksumMismatch { .. } => n4ds_status_t::N4DS_ERR_CHECKSUM,
        Error::Http(_) => n4ds_status_t::N4DS_ERR_HTTP,
        Error::Io(_) => n4ds_status_t::N4DS_ERR_IO,
    }
}

/// Opaque per-context handle carrying the last error message.
pub struct n4ds_context_t {
    last_error: CString,
}

/// Return the ABI version as a freshly allocated C string (free with [`n4ds_string_free`]).
///
/// # Safety
/// The returned pointer is non-null and owned by the caller.
#[no_mangle]
pub extern "C" fn n4ds_abi_version() -> *mut c_char {
    CString::new(N4DS_ABI_VERSION)
        .expect("static version contains no nul")
        .into_raw()
}

/// Check ABI compatibility: header MAJOR must equal the library MAJOR and the library
/// MINOR must be `>=` the header MINOR (forward-compatible additions).
#[no_mangle]
pub extern "C" fn n4ds_check_abi_compatibility(
    header_major: u32,
    header_minor: u32,
) -> n4ds_status_t {
    if header_major != ABI_MAJOR {
        n4ds_status_t::N4DS_ERR_ABI_MISMATCH
    } else if header_minor > ABI_MINOR {
        n4ds_status_t::N4DS_ERR_VERSION_INCOMPATIBLE
    } else {
        n4ds_status_t::N4DS_OK
    }
}

/// Free a string previously returned by this library.
///
/// # Safety
/// `ptr` must be a pointer returned by an `n4ds_*` function that transfers ownership,
/// or null. Each such pointer must be freed exactly once.
#[no_mangle]
pub unsafe extern "C" fn n4ds_string_free(ptr: *mut c_char) {
    if !ptr.is_null() {
        drop(CString::from_raw(ptr));
    }
}

/// Create a context (carries the last error buffer). Returns `N4DS_OK` and sets
/// `*out`; on failure sets `*out` to null.
///
/// # Safety
/// `out` must be a valid, writable `*mut n4ds_context_t` location.
#[no_mangle]
pub unsafe extern "C" fn n4ds_context_create(out: *mut *mut n4ds_context_t) -> n4ds_status_t {
    if out.is_null() {
        return n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT;
    }
    *out = Box::into_raw(Box::new(n4ds_context_t {
        last_error: CString::default(),
    }));
    n4ds_status_t::N4DS_OK
}

/// Destroy a context. No-op on null.
///
/// # Safety
/// `ctx` must have come from [`n4ds_context_create`] and not been destroyed.
#[no_mangle]
pub unsafe extern "C" fn n4ds_context_destroy(ctx: *mut n4ds_context_t) {
    if !ctx.is_null() {
        drop(Box::from_raw(ctx));
    }
}

/// Return the context's last error message (context-owned; copy before the next call
/// on this context). Never null: empty string when no error / null ctx.
///
/// # Safety
/// `ctx` must be a valid context pointer or null.
#[no_mangle]
pub unsafe extern "C" fn n4ds_context_last_error(ctx: *const n4ds_context_t) -> *const c_char {
    if ctx.is_null() {
        return c"".as_ptr();
    }
    (*ctx).last_error.as_ptr()
}

unsafe fn set_error(ctx: *mut n4ds_context_t, msg: &str) {
    if !ctx.is_null() {
        (*ctx).last_error = CString::new(msg.replace('\0', " ")).unwrap_or_default();
    }
}

unsafe fn clear_error(ctx: *mut n4ds_context_t) {
    if !ctx.is_null() {
        (*ctx).last_error = CString::default();
    }
}

unsafe fn cstr<'a>(ptr: *const c_char) -> Option<&'a str> {
    if ptr.is_null() {
        None
    } else {
        CStr::from_ptr(ptr).to_str().ok()
    }
}

unsafe fn write_out(ctx: *mut n4ds_context_t, out: *mut *mut c_char, s: String) -> n4ds_status_t {
    match CString::new(s) {
        Ok(c) => {
            *out = c.into_raw();
            n4ds_status_t::N4DS_OK
        }
        Err(_) => {
            set_error(ctx, "result contained an interior NUL byte");
            n4ds_status_t::N4DS_ERR_INTERNAL
        }
    }
}

/// Finish a JSON-returning call: write the Ok value or set the error + map the status.
unsafe fn finish(
    ctx: *mut n4ds_context_t,
    out: *mut *mut c_char,
    result: Result<String, Error>,
) -> n4ds_status_t {
    match result {
        Ok(s) => write_out(ctx, out, s),
        Err(e) => {
            set_error(ctx, &e.to_string());
            status_for(&e)
        }
    }
}

/// Resolve a dataset id against an index JSON. Writes the resolved download contract
/// (JSON, owned) to `*out`.
///
/// # Safety
/// All pointers must be valid for the call; `out` writable.
#[no_mangle]
pub unsafe extern "C" fn n4ds_resolve(
    ctx: *mut n4ds_context_t,
    index_json: *const c_char,
    dataset_id: *const c_char,
    out: *mut *mut c_char,
) -> n4ds_status_t {
    clear_error(ctx);
    if out.is_null() {
        set_error(ctx, "out pointer is null");
        return n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT;
    }
    *out = std::ptr::null_mut();
    let (Some(index_json), Some(dataset_id)) = (cstr(index_json), cstr(dataset_id)) else {
        set_error(ctx, "index_json / dataset_id is null or not UTF-8");
        return n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT;
    };
    finish(ctx, out, resolve_json(index_json, dataset_id))
}

/// Download + verify a resolved dataset into the cache. `opts_json` =
/// `{cache_dir?, token?, instance?, timeout_secs?}` (or empty/null for defaults).
/// Writes the fetch status (JSON, owned) — `{dir, files:[{name,relpath,path,status}]}`.
///
/// # Safety
/// All pointers must be valid for the call; `out` writable.
#[no_mangle]
pub unsafe extern "C" fn n4ds_fetch(
    ctx: *mut n4ds_context_t,
    resolved_json: *const c_char,
    opts_json: *const c_char,
    out: *mut *mut c_char,
) -> n4ds_status_t {
    clear_error(ctx);
    if out.is_null() {
        set_error(ctx, "out pointer is null");
        return n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT;
    }
    *out = std::ptr::null_mut();
    let Some(resolved_json) = cstr(resolved_json) else {
        set_error(ctx, "resolved_json is null or not UTF-8");
        return n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT;
    };
    let opts_json = cstr(opts_json).unwrap_or("");
    let result = (|| -> Result<String, Error> {
        let resolved: Resolved = serde_json::from_str(resolved_json)?;
        let opts = FetchOptions::from_json(opts_json)?;
        let client = UreqClient::new(opts.timeout_secs.unwrap_or(300));
        let res = fetch(&resolved, &opts, &client)?;
        Ok(serde_json::to_string(&res)?)
    })();
    finish(ctx, out, result)
}

/// Offline re-verify of an already-cached dataset directory. Writes the verification
/// report (JSON, owned) — `{dir, ok, files:[{name,relpath,status}]}`.
///
/// # Safety
/// All pointers must be valid for the call; `out` writable.
#[no_mangle]
pub unsafe extern "C" fn n4ds_verify_cached(
    ctx: *mut n4ds_context_t,
    resolved_json: *const c_char,
    dir: *const c_char,
    out: *mut *mut c_char,
) -> n4ds_status_t {
    clear_error(ctx);
    if out.is_null() {
        set_error(ctx, "out pointer is null");
        return n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT;
    }
    *out = std::ptr::null_mut();
    let (Some(resolved_json), Some(dir)) = (cstr(resolved_json), cstr(dir)) else {
        set_error(ctx, "resolved_json / dir is null or not UTF-8");
        return n4ds_status_t::N4DS_ERR_INVALID_ARGUMENT;
    };
    let result = (|| -> Result<String, Error> {
        let resolved: Resolved = serde_json::from_str(resolved_json)?;
        let report = verify_cached(&resolved, Path::new(dir))?;
        Ok(serde_json::to_string(&report)?)
    })();
    finish(ctx, out, result)
}
