#' Aggregation of individual configurations over partition-specific models
#'
#' @description 
#' Models that have been derived for individual partitions are first 
#' decomposed into sufficient terms, that is single sufficient conditions or 
#' configurations. The individual terms are aggregated using UpSet plots to 
#' determine how frequent they are individually and in combination.
#'
#' @importFrom magrittr %>%
#' @importFrom stringi stri_trim stri_unique stri_split_fixed
#' @importFrom purrr map
#' @importFrom rlist list.flatten
#' @import UpSetR
#'
#' @param df Dataframe created with \code{\link{partition_min}} or
#' \code{\link{partition_min_inter}}.
#' @param nsets Number of sets to include in plot (default is 5).
#' @md
#'
#' @return An UpSet plot produced with \code{\link[UpSetR]{upset}}.
#' @md
#' 
#' @examples
#' data(Grauvogel2014)
#' GS_pars <- partition_min(
#'  dataset = Grauvogel2014,
#'  units = "Sender",
#'  cond = c("Comprehensiveness", "Linkage", "Vulnerability",
#'           "Repression", "Claims"),
#'  out = "Persistence",
#'  n_cut = 1, incl_cut = 0.75,
#'  solution = "P",
#'  BE_cons = rep(0.75, 3),
#'  BE_ncut = rep(1, 3))
#' upset_configurations(GS_pars, nsets = 4)
#'
#' @export
upset_configurations <- function(df, nsets) {
  if (!is.data.frame(df)) {
    stop('Models should be part of a dataframe or tibble. 
    Object used in function is not a dataframe or tibble.') 
  }
  if ("solution" %in% colnames(df) == FALSE) {
    stop('There is no column named "solution" in the object.
         The models to be aggregated over should be in a column "solution".')
  }
  else {
  #prior to internal function
  temp1 <- unlist(df$solution) 
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  temp1 <- purrr::map(temp1, function(x) stringi::stri_split_fixed(x, "+"))
  temp1 <- rlist::list.flatten(temp1)
  all_values <- stringi::stri_unique(unlist(temp1))
  
  # using the new internal function
  finl <- lapply(temp1, mdetection_upset, all_values) 
  
  # post internal function
  finl <- lapply(finl, as.numeric)
  finl <- data.frame(Reduce(rbind, finl))
  colnames(finl) <- all_values
  rownames(finl) <- NULL
  UpSetR::upset(finl, order.by = "freq", nsets = nsets)
  }
}
