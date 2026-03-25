auth_sucess <- tryCatch(
  khisr:::khis_cred_testing(),
  khis_cred_internal_error = function(e) NULL
)
if(!isTRUE(auth_sucess)) {
  cancerscreening:::cancerscreening_bullets(c(
    "!" = "Internal auth failed; calling {.fun khis_cred_clear}."
  ))
  khis_cred_clear()
}

skip_if_no_cred <- function() {
  testthat::skip_if_not(khisr::khis_has_cred(), "No KHIS credentials")
}
