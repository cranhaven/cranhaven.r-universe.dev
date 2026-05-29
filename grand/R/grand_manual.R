#' Manually add Guidelines for Reporting About Network Data (GRAND) information to an igraph object
#'
#' The `grand_manual` function allows a user to manually and non-interactively add GRAND information to
#'    an \link[igraph]{igraph} object. Normally this information should be added interactively using
#'    `grand(mode = "interview")`.
#'
#' @param G An \link[igraph]{igraph} object, with weights/signs (if present) stored in `E(G)$weight`
#' @param name string: Name of the network (e.g., Zachary Karate Club)
#' @param doi string: DOI associated with the data (e.g., 10.1086/jar.33.4.3629752)
#' @param url string: Link to data (e.g., https://networks.skewed.de/net/karate)
#' @param mode string: Mode of data collection (e.g., survey, interview, sensor, observation, archival, simulation)
#' @param year numeric: Year in which data was collected (e.g., 1977)
#' @param vertex1 string: Entity represented by nodes in a unipartite network, or by `FALSE` nodes in a bipartite network (e.g., people)
#' @param vertex2 string: Entity represented by `TRUE` nodes in a bipartite network
#' @param positive string: Relationship represented by (positive) edges (e.g., friendship)
#' @param negative string: Relationship represented by negative edges, if present
#' @param weight string: What the edge weights represent (e.g., frequency, intensity, multiplexity)
#' @param level string: Scale on which edge weights are measured (e.g., continuous, count, ordinal, categorical)
#'
#' @return An \link[igraph]{igraph} object
#'
#' @examples
#' G <- igraph::make_graph("Zachary")  #An example network
#'
#' G <- grand_manual(G,  #Manually enter information
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
grand_manual <- function(G,
                  name = NA,
                  doi = NA,
                  url = NA,
                  vertex1 = NA,
                  vertex2 = NA,
                  positive = NA,
                  negative = NA,
                  weight = NA,
                  level = NA,
                  mode = NA,
                  year = NA) {

  if (!methods::is(G, "igraph")) {stop("The input must be an igraph object")}

  G$grand$name <- name
  G$grand$doi <- doi
  G$grand$url <- url
  G$grand$vertex1 <- vertex1
  G$grand$vertex2 <- vertex2
  G$grand$positive <- positive
  G$grand$negative <- negative
  G$grand$weight <- weight
  G$grand$level <- level
  G$grand$mode <- mode
  G$grand$year <- year

  return(G)
}
