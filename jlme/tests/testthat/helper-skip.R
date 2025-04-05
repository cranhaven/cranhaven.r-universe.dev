skip_conditionally <- function() {
  if (!JuliaConnectoR::juliaSetupOk()) {
    testthat::skip("No Julia installation detected via {JuliaConnectoR}.")
  }
  if (!julia_version_compatible()) {
    testthat::skip("Julia version >=1.8 required.")
  }
  if (!check_julia_ok()) {
    testthat::skip("`check_julia_ok()` is FALSE.")
  }
  invisible()
}
