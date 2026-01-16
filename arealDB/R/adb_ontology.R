#' Load the currently active ontology
#'
#' @param ... combination of column name in the ontology and value to filter
#'   that column by to build a tree of the concepts nested into it; see
#'   \code{\link[ontologics]{make_tree}}.
#' @param type [`character(1)`][character]\cr the type of ontology to load,
#'   either \code{"ontology"} to get the thematic concepts, or
#'   \code{"gazetteer"} to get the territories.
#' @return returns a tidy table of an ontology or gazetteer that is used in an
#'   areal database.
#' @importFrom ontologics load_ontology make_tree
#' @importFrom rlang list2
#' @importFrom tidyr pivot_longer separate_rows separate_wider_delim pivot_wider
#' @importFrom dplyr rowwise mutate select left_join group_by across ungroup
#'   na_if summarise
#' @importFrom tidyselect all_of
#' @export

adb_ontology <- function(..., type = "ontology"){

  assertChoice(x = type, choices = c("ontology", "gazetteer"))

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  if(type == "ontology"){
    thePath <- paste0(unique(getOption(x = "ontology_path")))
  } else {
    thePath <- paste0(getOption(x = "gazetteer_path"))
  }

  sbst <- list2(...)

  if(length(sbst) == 0){
    target <- load_ontology(thePath)@concepts$harmonised
  } else {
    target <- make_tree(!!names(sbst) := sbst[[1]], ontology = thePath)
  }

  extConcepts <- load_ontology(thePath)@concepts$external %>%
    separate_wider_delim(cols = id, names = c("dataseries", "nr"), delim = "_", cols_remove = FALSE) %>%
    rowwise() %>%
    mutate(label = paste0(label, " [", dataseries, "]")) %>%
    select(external = label, temp = id)

  out <- target %>%
    pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                 names_to = "match", values_to = "external") %>%
    separate_rows(external, sep = " \\| ") %>%
    separate_wider_delim(cols = external, names = c("temp"), delim = ".", too_many = "drop") %>%
    left_join(extConcepts, by = "temp") %>%
    group_by(across(all_of(c("id", "label", "class", "has_broader", "description", "match")))) %>%
    summarise(external = paste0(na.omit(external), collapse = " | ")) %>%
    ungroup() %>%
    mutate(external = na_if(external, "")) %>%
    pivot_wider(id_cols = c("id", "label", "class", "has_broader", "description"), names_from = match, values_from = external)

  return(out)

}