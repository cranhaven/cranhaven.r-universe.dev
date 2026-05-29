#' Applies Guidelines for Reporting About Network Data
#'
#' The `grand` function interactively adds information about a network and generates a narrative summary of the
#'    network following the Guidelines for Reporting About Network Data.
#'
#' @param G An \link[igraph]{igraph} object, with weights/signs (if present) stored in `E(G)$weight`
#' @param mode string: "interview" to interactively add information, or "statement" to generate a summary statement
#' @param help boolean: Should interview prompts be accompanied by explanations and examples?
#' @param verbose boolean: Should an extended summary be generated?
#'
#' @return An \link[igraph]{igraph} object if `mode == "interview"`, or a string if `mode == "statement"`
#'
#' @examples
#' G <- igraph::make_graph("Zachary")  #An example network
#'
#' #G <- grand(G, mode = "interview")  #Interactively enter information...
#'
#' G <- grand_manual(G,  #...or manually enter information
#'      name = "Zachary Karate Club",
#'      doi = "10.1086/jar.33.4.3629752",
#'      url = "https://networks.skewed.de/net/karate",
#'      vertex1 = "People",
#'      positive = "Friendship",
#'      mode = "Observation",
#'      year = "1977")
#'
#' grand(G, mode = "statement")  #Display summary statement
#'
#' @export
grand <- function(G, mode, help = FALSE, verbose = FALSE) {

  #### Check inputs ####
  if (!methods::is(G, "igraph")) {stop("The input must be an igraph object")}
  if (!igraph::is_simple(G)) {stop("The graph must be simple (i.e., no loops or multi-edges)")}
  if (!(mode %in% c("interview", "statement"))) {stop("mode must be one of c(\"interview\", \"statement\")")}

  #### Interview ####
  if (mode == "interview") {
    if (!interactive()) {stop("When `mode = \"interview\"`, grand() requires that R is running interactively")}
    G <- .grand_interview(G, help = help)
    return(G)
  }

  #### Statement ####
  if (mode == "statement") {
    if (!("grand" %in% names(igraph::graph_attr(G)))) {stop("G does not contain GRAND attributes. Run grand(G, mode = \"interview\") first.")}
    statement <- .grand_statement(G, verbose = verbose)
    return(statement)
  }

}
