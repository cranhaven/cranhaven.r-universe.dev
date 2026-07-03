# SPDX-License-Identifier: MIT

#' Resolve a dataset id against an index JSON into its download contract (JSON string).
#'
#' @param index_json The distributable catalog index as a JSON string.
#' @param dataset_id The dataset id to resolve.
#' @return The resolved download contract as a JSON string
#'   (\code{id, tier, instance, doi, dataset_version, files, origins}).
#' @export
n4ds_resolve <- function(index_json, dataset_id) {
  .Call("r_n4ds_resolve", index_json, dataset_id)
}

#' Download + SHA-256-verify a resolved dataset into the cache (JSON status string).
#'
#' @param resolved_json A resolved contract (from \code{n4ds_resolve}) as JSON.
#' @param opts_json Options as JSON: \code{{cache_dir?, token?, instance?, timeout_secs?}};
#'   \code{""} for the defaults.
#' @return The fetch status as a JSON string
#'   (\code{dir, files:[{name, relpath, path, status}]}).
#' @export
n4ds_fetch <- function(resolved_json, opts_json = "") {
  .Call("r_n4ds_fetch", resolved_json, opts_json)
}

#' Re-verify a cached dataset directory against the contract's SHA-256s (JSON report).
#'
#' @param resolved_json A resolved contract as JSON.
#' @param dir The cached dataset directory.
#' @return The verification report as a JSON string
#'   (\code{dir, ok, files:[{name, relpath, status}]}).
#' @export
n4ds_verify_cached <- function(resolved_json, dir) {
  .Call("r_n4ds_verify_cached", resolved_json, dir)
}

#' The C ABI version string.
#' @return The C ABI version as a string.
#' @export
n4ds_abi_version <- function() {
  .Call("r_n4ds_abi_version")
}
