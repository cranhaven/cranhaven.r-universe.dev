#' Turns GMYC Results Into a Tibble
#'
#' @description
#' `gmyc_tbl()` processes output from [gmyc][splits::gmyc] into an
#' object of class \code{\link[tibble]{tbl_df}}.
#'
#' @param gmyc_res Output from [gmyc][splits::gmyc].
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'gmyc'.
#'
#' @details
#' `splits` package uses [gmyc][splits::gmyc] to optimize
#' genetic clusters and [spec.list][splits::spec.list] to cluster samples into
#' species partitions. `gmyc_tbl()` turns these results into a tibble which matches
#' the output from [bgmyc_tbl] and [locmin_tbl].
#'
#' @return
#' An object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Thomas Ezard, Tomochika Fujisawa, Tim Barraclough.
#' 
#' @source
#' Pons J., Barraclough T. G., Gomez-Zurita J., Cardoso A., Duran D. P., Hazell S., 
#' Kamoun S., Sumlin W. D., Vogler A. P. 2006. Sequence-based species delimitation for 
#' the DNA taxonomy of undescribed insects. Systematic Biology. 55:595-609.
#' 
#' Monaghan M. T., Wild R., Elliot M., Fujisawa T., Balke M., Inward D. J. G., Lees D. C., Ranaivosolo R.,
#' Eggleton P., Barraclough T. G., Vogler A. P. 2009. Accelerated species inventory 
#' on Madagascar using coalescent-based models of species delineation. 
#' Systematic Biology. 58:298-311.
#' 
#' Fujisawa T., Barraclough T. G. 2013. Delimiting Species Using Single-Locus Data 
#' and the Generalized Mixed Yule Coalescent Approach: A Revised Method and Evaluation 
#' on Simulated Data Sets. Systematic Biology. 62(5):707–724.
#'
#' @examples
#' 
#' \donttest{
#' # run GMYC
#' gmyc_res <- splits::gmyc(ape::as.phylo(geophagus_beast))
#'
#' # create a tibble
#' gmyc_df <- gmyc_tbl(gmyc_res)
#' 
#' # check
#' gmyc_df
#'}
#'
#' @export
gmyc_tbl <- function(gmyc_res, delimname = "gmyc"){
  
  # check if `splits` is installed
  rlang::check_installed("splits", reason = "to run `gmyc_tbl` properly.")

  dname <- rlang::sym(delimname)

  if(methods::is(gmyc_res, "gmyc")){

    gmyc_spec <- splits::spec.list(gmyc_res)

    gmyc_tbl <- tibble::tibble(labels= as.character(gmyc_spec$sample_name),
                               !!dname:= as.integer(gmyc_spec$GMYC_spec))
      

    return(gmyc_tbl)

  } else {

    cli::cli_abort(c("Input data must have class {.cls gmyc}.",
                     "i" = "You've supplied an input of class {.cls {class(gmyc_res)}}."))
  }
}
