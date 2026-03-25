auth_sucess <- tryCatch(
  khisr:::khis_cred_testing(),
  khis_cred_internal_error = function(e) NULL
)

if(!isTRUE(auth_sucess)) {
  khisr:::khis_info(c(
    "!" = "Internal auth failed; calling {.fun khis_cred_clear}."
  ))
  khis_cred_clear()
}

skip_if_no_cred <- function() {
  testthat::skip_if_not(khis_has_cred(), "No DHIS2 credentials")
}

skip_if_server_error <- function() {
    auth = tryCatch(
        khis_cred(config_path = system.file("extdata", "valid_cred_conf.json", package = "khisr")),
        error = function(e) NULL
    )
    testthat::skip_if_not(!is.null(auth), "Server Error")
}
