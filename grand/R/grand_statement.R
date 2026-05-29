#' Generates a Guidelines for Reporting About Network Data (GRAND) narrative summary of an igraph object
#'
#' @param G An \link[igraph]{igraph} object with GRAND attributes
#' @param verbose boolean: provide an extended summary
#'
#' @return string: GRAND summary of G
#' @noRd
.grand_statement <- function(G, verbose = FALSE) {

  #### Basic Description ####
  #Determine characteristics
  bipartite <- igraph::is_bipartite(G)
  if (igraph::is_directed(G)) {directed <- "a directed"} else {directed <- "an undirected"}
  if ("weight" %in% names(igraph::edge_attr(G))) {if (all(igraph::E(G)$weight %in% c(-1,1))) {weighted <- "signed"} else {weighted <- "weighted"}} else {weighted <- "unweighted"}

  #Name, weight, and direction
  if (is.na(G$grand$name) & !bipartite) {basic <- paste0("This is ", directed, " and ", weighted, " network that contains ")}
  if (is.na(G$grand$name) & bipartite) {basic <- paste0("This is ", directed, " and ", weighted, " bipartite network that contains ")}
  if (!is.na(G$grand$name) & !bipartite) {basic <- paste0("The ", G$grand$name, " network is ", directed, " and ", weighted, " network that contains ")}
  if (!is.na(G$grand$name) & bipartite) {basic <- paste0("The ", G$grand$name, " network is ", directed, " and ", weighted, " bipartite network that contains ")}

  #Contents
  if (!bipartite & weighted != "signed") {
    if (!is.na(G$grand$vertex1) & !is.na(G$grand$positive)) {basic <- paste0(basic, igraph::gorder(G), " nodes representing ", tolower(G$grand$vertex1), ", which are connected by ", igraph::gsize(G)," edges representing ", tolower(G$grand$positive),".")}
    if (is.na(G$grand$vertex1) & !is.na(G$grand$positive)) {basic <- paste0(basic, igraph::gorder(G), " nodes, which are connected by ", igraph::gsize(G)," edges representing ", tolower(G$grand$positive),".")}
    if (!is.na(G$grand$vertex1) & is.na(G$grand$positive)) {basic <- paste0(basic, igraph::gorder(G), " nodes representing ", tolower(G$grand$vertex1), ", which are connected by ", igraph::gsize(G)," edges.")}
    if (is.na(G$grand$vertex1) & is.na(G$grand$positive)) {basic <- paste0(basic, igraph::gorder(G), " nodes, which are connected by edges.")}
    }

  if (!bipartite & weighted == "signed") {
    if (!is.na(G$grand$vertex1) & !is.na(G$grand$positive) & !is.na(G$grand$negative)) {basic <- paste0(basic, igraph::gorder(G), " nodes representing ", tolower(G$grand$vertex1), ", which are connected by ", sum(igraph::E(G)$weight==1), " positive edges representing ", tolower(G$grand$positive)," and ", sum(igraph::E(G)$weight==-1), " negative edges representing " , tolower(G$grand$negative),". ")}
    if (is.na(G$grand$vertex1) & !is.na(G$grand$positive) & !is.na(G$grand$negative)) {basic <- paste0(basic, igraph::gorder(G), " nodes, which are connected by ", sum(igraph::E(G)$weight==1), " positive edges representing ", tolower(G$grand$positive)," and ", sum(igraph::E(G)$weight==-1), " negative edges representing " , tolower(G$grand$negative),". ")}
    if (!is.na(G$grand$vertex1) & (is.na(G$grand$positive) | is.na(G$grand$negative))) {basic <- paste0(basic, igraph::gorder(G), " nodes representing ", tolower(G$grand$vertex1), ", which are connected by ", sum(igraph::E(G)$weight==1), " positive edges and ", sum(igraph::E(G)$weight==-1), " negative edges. ")}
    if (is.na(G$grand$vertex1) & (is.na(G$grand$positive) | is.na(G$grand$negative))) {basic <- paste0(basic, igraph::gorder(G), " nodes, which are connected by ", sum(igraph::E(G)$weight==1), " positive edges and ", sum(igraph::E(G)$weight==-1), " negative edges. ")}
    }

  if (bipartite & weighted != "signed") {
    if (!is.na(G$grand$vertex1) & !is.na(G$grand$vertex2) & !is.na(G$grand$positive)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " nodes representing ", tolower(G$grand$vertex1), " and ", sum(igraph::V(G)$type==TRUE), " nodes representing ", tolower(G$grand$vertex2), ", which are connected by ", igraph::gsize(G), " edges representing ", tolower(G$grand$positive),". ")}
    if ((is.na(G$grand$vertex1) | is.na(G$grand$vertex2)) & !is.na(G$grand$positive)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " top-nodes and ", sum(igraph::V(G)$type==TRUE), " bottom-nodes, which are connected by ", igraph::gsize(G), " edges representing ", tolower(G$grand$positive),". ")}
    if (!is.na(G$grand$vertex1) & !is.na(G$grand$vertex2) & is.na(G$grand$positive)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " nodes representing ", tolower(G$grand$vertex1), " and ", sum(igraph::V(G)$type==TRUE), " nodes representing ", tolower(G$grand$vertex2), ", which are connected by ", igraph::gsize(G), " edges. ")}
    if ((is.na(G$grand$vertex1) | is.na(G$grand$vertex2)) & is.na(G$grand$positive)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " top-nodes and ", sum(igraph::V(G)$type==TRUE), " bottom-nodes, which are connected by ", igraph::gsize(G), " edges. ")}
    }

  if (bipartite & weighted == "signed") {
    if (!is.na(G$grand$vertex1) & !is.na(G$grand$vertex2) & !is.na(G$grand$positive) & !is.na(G$grand$negative)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " nodes representing ", tolower(G$grand$vertex1), " and ", sum(igraph::V(G)$type==TRUE), " nodes representing ", tolower(G$grand$vertex2), ", which are connected by ", sum(igraph::E(G)$weight==1)," positive edges representing ", tolower(G$grand$positive)," and ", sum(igraph::E(G)$weight==-1)," negative edges representing ", tolower(G$grand$negative),". ")}
    if ((is.na(G$grand$vertex1) | is.na(G$grand$vertex2)) & !is.na(G$grand$positive) & !is.na(G$grand$negative)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " top-nodes and ", sum(igraph::V(G)$type==TRUE), " bottom-nodes, which are connected by ", sum(igraph::E(G)$weight==1)," positive edges representing ", tolower(G$grand$positive)," and ", sum(igraph::E(G)$weight==-1)," negative edges representing ", tolower(G$grand$negative),". ")}
    if (!is.na(G$grand$vertex1) & !is.na(G$grand$vertex2) & is.na(G$grand$positive) & is.na(G$grand$negative)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " nodes representing ", tolower(G$grand$vertex1), " and ", sum(igraph::V(G)$type==TRUE), " nodes representing ", tolower(G$grand$vertex2), ", which are connected by ", sum(igraph::E(G)$weight==1)," positive edges and ", sum(igraph::E(G)$weight==-1)," negative edges. ")}
    if ((is.na(G$grand$vertex1) | is.na(G$grand$vertex2)) & is.na(G$grand$positive) & is.na(G$grand$negative)) {basic <- paste0(basic, sum(igraph::V(G)$type==FALSE), " top-nodes and ", sum(igraph::V(G)$type==TRUE), " bottom-nodes, which are connected by ", sum(igraph::E(G)$weight==1)," positive edges and ", sum(igraph::E(G)$weight==-1)," negative edges. ")}
    }

  #### EXTENDED DESCRIPTION ####
  extended <- ""

  #Edge measurement
  if (!is.na(G$grand$weight) & !is.na(G$grand$level)) {extended <- paste0(extended, "The edges are weighted by ", tolower(G$grand$weight), ", which was measured on a ", tolower(G$grand$level), " scale. ")}
  if (is.na(G$grand$weight) & !is.na(G$grand$level)) {extended <- paste0(extended, "The edges are weighted using weights that were measured on a ", tolower(G$grand$level), " scale. ")}
  if (!is.na(G$grand$weight) & is.na(G$grand$level)) {extended <- paste0(extended, "The edges are weighted by ", tolower(G$grand$weight), ". ")}

  #Data collection
  if (!is.na(G$grand$year) & !is.na(G$grand$mode)) {extended <- paste0(extended, "These data were collected in ", G$grand$year, " using ", tolower(G$grand$mode), " methods. ")}
  if (is.na(G$grand$year) & !is.na(G$grand$mode)) {extended <- paste0(extended, "These data were collected using ", tolower(G$grand$mode), " methods. ")}
  if (!is.na(G$grand$year) & is.na(G$grand$mode)) {extended <- paste0(extended, "These data were collected in ", G$grand$year, ". ")}

  #Source
  if (!is.na(G$grand$doi) & !is.na(G$grand$url)) {extended <- paste0(extended, paste0("This network is described in ", G$grand$doi, " and is available from ", G$grand$url, "."))}
  if (is.na(G$grand$doi) & !is.na(G$grand$url)) {extended <- paste0(extended, paste0("This network is available from ", G$grand$url, "."))}
  if (!is.na(G$grand$doi) & is.na(G$grand$url)) {extended <- paste0(extended, paste0("This network is described in ", G$grand$doi, "."))}

  #### Return description ####
  if (!verbose) {return(basic)}
  if (verbose) {return(paste0(basic, " ", extended))}
}
