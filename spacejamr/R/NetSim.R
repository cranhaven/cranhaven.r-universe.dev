# Author: Darren Colby
# Date: 1/27/2022
# Purpose: To simulate spatial Bernoulli networks

# Constructor methods to simulate a standard power law network ------------


#' Validate input to a NetSim constructor
#'
#' @description validates a spatial Bernoulli network
#'
#' @usage validate_NetSim(point_sm, sif, base_prob, scale, threshold, power)
#'
#' @details This function should not be called by the user
#'
#' @param point_sim a PointSim object
#' @param sif the spatial interaction function to use
#' @param base_prob the theoretical probability that two nodes (points) with
#' distance 0 share a tie.
#' @param scale a coefficient to multiply the distance by.
#' @param threshold if two node exceed this probability, they will be coded as
#' having a tie.
#' @param power the exponent at which tie probability decays.
#'
#' @return An igraph object
#'
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
validate_NetSim <- function(point_sim, sif, base_prob, scale, threshold, power) {

    # Ensures proper input
    stopifnot(inherits(point_sim, "PointSim"))

    # Calculate the distance between all pairs of nodes
    distances <- suppressWarnings(dplyr::as_tibble(spatstat.geom::pairdist(point_sim),
                                  column_name = c("V1", "V2"))) %>%

    # Apply the power law function
    dplyr::mutate(dplyr::across(.fns = sif, base_prob = base_prob,
                                scale = scale, power = power),

    # Code as a tie if the probability exceeds the given threshold
    dplyr::across(.fns = function(prob){ifelse(prob > threshold, 1, 0)})) %>%

    # This is just a good practice
    dplyr::ungroup() %>%
    as.matrix()

    simulated_network <- igraph::graph_from_adjacency_matrix(distances,
                                                             mode = "undirected") %>%
    igraph::simplify() # Removes self loops

    return(simulated_network)

}


#' Validate and set class attribute of input to the NetSim constructor
#'
#' @description creates a spatial Bernoulli network using a standard power law
#'
#' @usage new_NetSim(point_sim, sif, base_prob, scale, threshold, power)
#'
#' @details This function should not be called by the user
#'
#' @param point_sim a PointSim object
#' @param sif a spatial interaction function
#' @param base_prob the theoretical probability that two nodes (points) with
#' distance 0 share a tie.
#' @param scale a coefficient to multiply the distance by.
#' @param threshold if two node exceed this probability, they will be coded as
#' having a tie.
#' @param power the exponent at which tie probability decays.
#'
#' @return An object of classes 'NetSim' and 'igraph' that can be further
#' manipulated with 'igraph' functions and methods
#'
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
new_NetSim <- function(point_sim, sif, base_prob, scale, threshold, power) {

    validated_net <- validate_NetSim(point_sim, sif, base_prob, scale,
                                     threshold, power)

    validated_net <- structure(validated_net, class = c("NetSim", "igraph"))

    return(validated_net)

}


#' Simulate a network from a point process or sequence
#'
#' @description Simulates a spatial Bernoulli network from a NetSim object
#' using one of six probability law distributions.
#'
#' @usage NetSim(point_sim, sif, base_prob, scale, threshold, power)
#'
#' @details The algorithm proceeds in three steps. First, it calculates the
#' distance between simulated points from a PointSim object. Then it calculates
#' the distance between all pairs of points. Finally, it uses a spatial
#' interaction function to calculate that any two point share a tie. If the
#' threshold is exceeded, a tie is created.
#'
#' @param point_sim a PointSim object
#' @param sif the spatial interaction function to use. Use attenuated to use an
#' attenuated power law; arctan to use an arctangent probability law; decay to
#' use an exponential decay law; or logistic to use a logistic probability law.
#' Default is a standard power law function.
#'
#' @param base_prob the theoretical probability that two nodes (points) with
#' distance 0 share a tie. Default is 0.9.
#' @param scale a coefficient to multiply the distance by. Default is 1.
#' @param threshold if two node exceed this probability, they will be coded as
#' having a tie. Default is 0.5.
#' @param power the exponent at which tie probability decays. Default is -2.8.
#'
#' @return A network object of class 'NetSim' and 'igraph' that can be
#' manipulated using the igraph' package.
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#' power_law <- NetSim(ri_points, base_prob = 0.92, scale = 1, threshold = 0.5,
#'                     power = -2.4)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#'
#' @references Butts, Carter T. Spatial models of large-scale interpersonal
#' networks. Dissertation. (2002).
#' @export
NetSim <- function(point_sim, sif = standard, base_prob = 0.9, scale = 1,
                   threshold = 0.5, power = -2.8) {

    validated_net <- new_NetSim(point_sim, sif, base_prob, scale, threshold,
                                power)

    return(validated_net)

}


# Plot, print, and summary methods ----------------------------------------


#' Plot a simulated network from a NetSim object
#'
#' @description This can take either a PowerLawNetwork or APLNetwork object as
#' input, both of which are chidren of the NetSim class.
#'
#' @details This method returns a ggraph object, which can be further refined
#' using standard ggraph and ggplot facilities.
#'
#' @param x a NetSim graph
#' @param y ignored.
#' @param ... ignored.
#' @param layout a layout to display the graph. Layout must be a valid string.
#' from the ggraph package. Default is "stress".
#' @param title an optional title.
#' @param node_color a color for the nodes. Default is blue.
#' @param edge_color a color for the edges. Default is red.
#'
#' @return A plot of classes 'ggraph' 'gg' and 'ggplot'
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#' spl_points <- NetSim(ri_points, base_prob = 0.92, scale = 1, threshold = 0.5,
#'                      power = -2.4)
#' plot(spl_points)
#'
#' @author Darren Colby \cr
#' Email:dscolby17@@gmail.com
#'
#' @export
plot.NetSim <- function(x, y, ..., layout = "stress",
                        title = "Network Simulation", node_color = "red",
                        edge_color = "blue") {

    stopifnot(class(x) == c("NetSim", "igraph"))

    plot <- ggraph::ggraph(x,
                           layout = layout) +
        ggraph::geom_edge_link(color = "blue") +
        ggraph::geom_node_point(color = "red") +
        ggraph::theme_graph()

    return(plot)

}


#' Print information from a NetSim object
#'
#' @description Plots a NetSim object and returns a ggraph object
#'
#' @param x a NetSim object
#' @param ... ignored.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Create spacejamr object
#' data("RI")
#'
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#' spl_points <- NetSim(ri_points, base_prob = 0.92, scale = 1, threshold = 0.5,
#'                      power = -2.4)
#' print(spl_points)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
print.NetSim <- function(x, ...) {

    # If it ain't broke, don't fix it
    igraph::print.igraph(x)

}


#' Summary of NetSim graphs
#'
#' @description Prints a summary of a NetSim object
#'
#' @param object a NetSim object
#' @param ... ignored.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#' spl_points <- NetSim(ri_points, base_prob = 0.92, scale = 1, threshold = 0.5,
#'                      power = -2.4)
#' summary(spl_points)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
summary.NetSim <- function(object, ...) {

    print(object)

}
