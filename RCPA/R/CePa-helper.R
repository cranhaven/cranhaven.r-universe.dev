#' @title Get KEGG pathway catalogue (network) for CePa.ORA and CePa.GSA methods
#' @description Get KEGG pathway catalogue for CePa.ORA and CePa.GSA methods
#' @param org The organism abbreviation. E.g, hsa, mmu, dme, etc.
#' To see the full list of supported organisms, visit https://www.genome.jp/kegg/catalog/org_list.html.
#' @param updateCache A parameter to enable/disable cache update.
#' @return A named list with three elements: network, names and sizes for CePa.ORA and CePa.GSA methods.
#' @examples
#' \donttest{
#' cepaNetwork <- getCePaPathwayCatalogue("hsa")
#' }
#' @export
#' @importFrom graph nodes edges
#' @importFrom dplyr %>% filter select group_by group_split
#' @importFrom stringr str_split
getCePaPathwayCatalogue <- function(org = "hsa", updateCache = FALSE){

  if (!.requirePackage("CePa")){
    return(NULL)
  }
  if (!.requirePackage("ROntoTools")){
    return(NULL)
  }

  # keggPathway <- ROntoTools::keggPathwayGraphs(org, updateCache = updateCache)
  keggPathway <- ROntoTools::keggPathwayGraphs(org, relPercThresh = 0, updateCache = updateCache)

  interactionList <- lapply(keggPathway, function(pathway){
    pathway@edgeData %>% names()
  }) %>% do.call(what=c) %>% unique()

  pathList <- lapply(keggPathway, function(pathway){
    (interactionList %in% (pathway@edgeData %>% names())) %>% which() %>% as.character()
  })

  interactionList <- seq(length(interactionList)) %>% as.character() %>% cbind(
    interactionList %>% strsplit('\\|') %>% do.call(what=rbind)
  ) %>% data.frame(stringsAsFactors = FALSE)

  colnames(interactionList) <- c("interaction.id", "input", "output")
  rownames(interactionList) <- interactionList$interaction.id

  interactionList_modified <- interactionList
  interactionList_modified$input <- interactionList$input %>% strsplit(":") %>% sapply(function(x) x[2]) %>% as.character()
  interactionList_modified$output <- interactionList$output %>% strsplit(":") %>% sapply(function(x) x[2]) %>% as.character()

  mapping <- lapply(keggPathway, function(pathway) pathway@nodes) %>% unlist() %>% unique()
  mapping_modified <- mapping %>% strsplit(":") %>% sapply(function(x) x[2]) %>% as.character()
  mapping_modified <- data.frame(node.id = mapping_modified, symbol = mapping_modified, stringsAsFactors = FALSE)

  cat <- CePa::set.pathway.catalogue(pathList, interactionList_modified, mapping_modified, min.node = 2, max.node = 1e+6)

  pathways_size <- cat[["pathList"]] %>% lapply(function (path) length(path)) %>% unlist() %>% as.vector()
  names(pathways_size) <- names(cat[["pathList"]])

  list(
      network = cat,
      names = .getKEGGPathwayNames(org)[names(cat$pathList)],
      sizes = pathways_size
  )
}