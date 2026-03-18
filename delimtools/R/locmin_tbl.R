#' Turns Local Minima Results into a Tibble
#'
#' @description
#' `locmin_tbl()` processes output from [tclust][spider::tclust] into an object of 
#' class [tbl_df][tibble::tbl_df].
#'
#' @param distobj A distance object (usually from [dist.dna][ape::dist.dna]).
#' @param threshold Distance cutoff for clustering. Default of 0.01. See
#' [localMinima][spider::localMinima] for details.
#' @param haps Optional. A vector of haplotypes to keep into the [tbl_df][tibble::tbl_df].
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'locmin'.
#'
#' @details
#' `spider` package uses [localMinima][spider::localMinima] to
#' determine possible thresholds for any distance matrix and [tclust][spider::tclust]
#' to cluster samples within a given `threshold` into species partitions.
#' `locmin_tbl()` turns these inputs into a tibble which matches
#' the output from [gmyc_tbl] and [bgmyc_tbl].
#'
#' @return
#' An object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Samuel Brown.
#' 
#' @source
#' Brown S.D.J., Collins R.A., Boyer S., Lefort M.-C., Malumbres-Olarte J., Vink C.J., 
#' Cruickshank, R.H. 2012. Spider: An R package for the analysis of species identity 
#' and evolution, with particular reference to DNA barcoding. 
#' Molecular Ecology Resources, 12: 562-565.
#'
#' @examples
#' # create a distance matrix
#' mat <- ape::dist.dna(geophagus, model = "raw", pairwise.deletion = TRUE)
#' 
#' # run Local Minima
#' locmin_res <- spider::localMinima(mat)
#' 
#' # create a tibble
#' locmin_df <- locmin_tbl(mat, 
#'                         threshold = locmin_res$localMinima[1], 
#'                         haps = ape::as.phylo(geophagus_beast)$tip.label)
#'
#' # check
#' locmin_df 
#'
#' @export
locmin_tbl <- function(distobj, threshold = 0.01, haps = NULL, delimname = "locmin"){
  
  dname <- rlang::sym(delimname)
  
  if(methods::is(distobj, "dist")){
    
    labs <- labels(distobj)
    clu <- spider::tclust(distobj, threshold) |>
      purrr::map(~labs[.x])
    
    locmin_df <- tibble::tibble(labels= unlist(clu),
                                !!dname:= rep(seq_along(clu),
                                              sapply(clu, length)))
  } else {
    
    cli::cli_abort(c("Input data must have class {.cls dist}.",
                     "i" = "You've supplied an input of class {.cls {class(distobj)}}."))
  }
  
  if(!is.null(haps)){
    
    locmin_df <- locmin_df |>
      dplyr::filter(.data$labels %in% haps)
    
  }
  
  return(locmin_df)
}
