#' Get ID data from `Biomart` database use `attributes`.
#'
#' @param dataset Datasets of the selected BioMart database.
#' @param mirror Specify an Ensembl mirror to connect to.
#' @param biomart BioMart database name you want to connect to.Use `biomaRt::listEnsembl` to retrieve the possible database names.
#' @param attributes A dataframe.The information we want to retrieve.Use `mi_get_ID_attr` to hava try.
#'
#' @importFrom biomaRt useEnsembl getBM
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across rename select
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @return A `tibble` dataframe.
#' @export
mi_get_ID <- function(attributes, biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
    Ensembl <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  out <- vector("list", length = nrow(attributes))
  for (i in 1:nrow(attributes)) {
    try_result <- try({
      out[[i]] <- getBM(attributes = unique(attributes[["name"]])[i], mart = Ensembl)
      sprintf("The %dth getFunc successed!", i)
    })
    if ("try-error" %in% class(try_result)) {
      next
    }
    Sys.sleep(0.5)
  }
  to_2col <- function(df) {
    df %>%
      as_tibble() %>%
      mutate(class = colnames(.)[1]) %>%
      rename(ID = 1) %>%
      mutate(across(.cols = 1, .fns = as.character))
  }
  map_dfr(out, .f = to_2col)
}
