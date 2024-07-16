#' Get ID attributes from `Biomart` database.
#'
#' @param dataset Datasets of the selected BioMart database.
#' @param mirror Specify an Ensembl mirror to connect to.
#' @param biomart BioMart database name you want to connect to.Use `biomaRt::listEnsembl` to retrieve the possible database names.
#' @importFrom dplyr filter
#' @importFrom stringr str_length
#' @importFrom biomaRt useEnsembl listAttributes
#' @importFrom magrittr %>%
#' @return A dataframe.
#' @export
mi_get_ID_attr <- function(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
  ensemb_hm_dset <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  attributes <- listAttributes(ensemb_hm_dset) %>%
    filter(grepl(.[["description"]], pattern = "(id)|(name)", ignore.case = TRUE)) %>%
    filter(!grepl(.[["description"]], pattern = "(end)|(start)|(description)|(probe)|(version)|(content)|(Aberrant)|(Source)|(Strain)|(Chromosome)", ignore.case = TRUE)) %>%
    filter(str_length(.[["name"]]) < 18)
}
