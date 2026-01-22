# nocov start
.onLoad <- function(...){
  vctrs::s3_register("base::print", "idx_tbl")
}
# nocov end
