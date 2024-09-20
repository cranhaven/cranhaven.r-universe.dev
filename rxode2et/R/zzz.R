
.onLoad <- function(libname, pkgname) {
  requireNamespace("rxode2random", quietly=TRUE)
  requireNamespace("data.table", quietly=TRUE)
  if (requireNamespace("pillar", quietly = TRUE)) {
    .s3register("pillar::type_sum", "rxEvid")
    .s3register("pillar::type_sum", "rxRateDur")
    .s3register("pillar::pillar_shaft", "rxEvid")
    .s3register("pillar::pillar_shaft", "rxRateDur")
  }
  if (requireNamespace("tibble", quietly = TRUE)) {
    .s3register("tibble::as_tibble", "rxEt")
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    .s3register("data.table::as.data.table", "rxEt")
  }
  if (requireNamespace("units", quietly = TRUE)) {
    .s3register("units::set_units", "rxEt")
    .s3register("units::set_units", "rxRateDur")
    .s3register("units::drop_units", "rxEt")
    .s3register("units::units<-", "rxEvid")
  }


}
