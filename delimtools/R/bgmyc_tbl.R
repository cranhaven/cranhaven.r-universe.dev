#' Turns bGMYC Results Into a Tibble
#'
#' @description
#' `bgmyc_tbl()` processes output from [bgmyc.singlephy][bGMYC::bgmyc.singlephy] into an
#' object of class [tbl_df][tibble::tbl_df].
#'
#' @param bgmyc_res Output from [bgmyc.singlephy][bGMYC::bgmyc.singlephy].
#' @param ppcutoff  Posterior probability threshold for clustering samples into
#' species partitions. See [bgmyc.point][bGMYC::bgmyc.point] for details. Default to 0.05.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'bgmyc'.
#'
#' @details
#' `bGMYC` package uses [spec.probmat][bGMYC::spec.probmat] to create a
#' matrix of probability of conspecificity and [bgmyc.point][bGMYC::bgmyc.point]
#' to split samples into a list which individuals
#' meets the threshold specified by `ppcutoff`. `bgmyc_tbl()` wraps up these
#' two functions into a single one and turns these inputs into a tibble which matches
#' the output from [gmyc_tbl] and [locmin_tbl].
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Noah M. Reid.
#'
#' @source
#' Reid N.M., Carstens B.C. 2012. Phylogenetic estimation error can decrease 
#' the accuracy of species delimitation: a Bayesian implementation of the general 
#' mixed Yule-coalescent model. BMC Evolutionary Biology 12 (196).
#' 
#' @examples
#' 
#'\donttest{
#' # run bGMYC
#' bgmyc_res <- bGMYC::bgmyc.singlephy(ape::as.phylo(geophagus_beast),
#'   mcmc = 11000,
#'   burnin = 1000,
#'   thinning = 100,
#'   t1 = 2,
#'   t2 = ape::Ntip(geophagus_beast),
#'   start = c(1, 0.5, 50)
#' )
#'
#' # create a tibble
#' bgmyc_df <- bgmyc_tbl(bgmyc_res, ppcutoff = 0.05)
#'
#' # check
#' bgmyc_df
#'}
#'
#' @export
bgmyc_tbl <- function(bgmyc_res, ppcutoff = 0.05, delimname = "bgmyc") {
  
  # check if `bGMYC` is installed
  rlang::check_installed(pkg= "bGMYC", reason= "to run `bgmyc_tbl` properly.")
  
  dname <- rlang::sym(delimname)

  if (methods::is(bgmyc_res, "singlebgmyc")) {
    bgmyc_probmat <- bGMYC::spec.probmat(bgmyc_res)

    splist <- bGMYC::bgmyc.point(bgmyc_probmat, ppcutoff)

    bgmyc_tbl <- tibble::tibble(
      labels = unlist(splist),
      !!dname := rep(
        seq_along(splist),
        sapply(splist, length)
      )
    )
    return(bgmyc_tbl)
  } else {
    cli::cli_abort(c("Input data must have class {.cls singlebgmyc}.",
      "i" = "You've supplied an input of class {.cls {class(bgmyc_res)}}."
    ))
  }
}
