#' Abbreviations for RCM and GCM names
#'
#' character vectors with short names of RCM and GCMs, with the long RCM/GCM
#' names as vector-names, so it can be used for renaming:
#'
#' @examples
#' # for example from inventory
#' fn_zip <- system.file("extdata", "inv-test-files.zip", package = "eurocordexr")
#' tmpdir <- tempdir()
#' unzip(fn_zip, exdir = tmpdir)
#' dat_inv <- get_inventory(fs::path(tmpdir, "testdata"))
#' # compare
#' cbind(dat_inv$gcm, shortnames_gcm[dat_inv$gcm])
#' cbind(dat_inv$institute_rcm, shortnames_rcm[dat_inv$institute_rcm])
#'
#'
#'
"shortnames_gcm"

#' @rdname shortnames_gcm
"shortnames_rcm"
