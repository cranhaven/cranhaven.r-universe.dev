.onLoad <- function(...) {


  vctrs::s3_register("dplyr::dplyr_reconstruct", "incidence2_fit", method = dplyr_reconstruct_incidence2_fit)
}


