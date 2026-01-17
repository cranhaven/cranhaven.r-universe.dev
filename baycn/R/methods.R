#' show
#'
#' @param object An object of class baycn.
#'
#' @importFrom methods show
#'
#' @export
#'
setMethod('show',
          signature = 'baycn',
          definition = function (object) {

            # Print the class name.
            cat('Object of class: ',
                class(object),
                '\n',
                sep = '')

            # Print the number of iterations.
            cat('Number of iterations: ',
                object@iterations,
                '\n',
                sep = '')

            # Print the stepsize.
            cat('Step size: ',
                object@stepSize,
                '\n',
                sep = '')

            # Print the number of nodes.
            cat('Number of nodes in the network: ',
                dim(object@posteriorPM)[1],
                '\n',
                sep = '')

            # Print the number of edges.
            cat('Number of edges considered: ',
                dim(object@posteriorES)[1],
                '\n',
                sep = '')

          })

#' summary
#'
#' @param object An object of class baycn.
#'
#' @param ... Other Arguments passed to methods.
#'
#' @export
#'
setMethod('summary',
          signature(object = 'baycn'),
          definition = function (object, ...) {

            # Get the number of edges to loop through when calculating the
            # probability of each edge state.
            nEdges <- ncol(object@chain)

            # Get the number of samples kept.
            nRow <- nrow(object@chain)

            # Create a matrix for the summary of the likelihood.
            logLikSummary <- matrix(nrow = 1,
                                    ncol = 5)

            # Name the rows and columns of the log likelihood summary matrix.
            colnames(logLikSummary) <- c('Min', '1Q', 'Median', '3Q', 'Max')
            rownames(logLikSummary) <- ''

            # Calculate the summary of the log likelihood.
            logLikSummary[1, ] <- round(summary(object@likelihood)[c(1:3,
                                                                     5:6)],
                                        2)

            # Display the posterior probability for each edge.
            cat('Posterior probability: \n',
                sep = '')
            print(object@posteriorES)

            # Display the min, 1q, median, 3q, and max.
            cat('\n',
                'Log likelihood: \n',
                sep = '')
            print(logLikSummary)

            # Display the number of unique graphs
            cat('\n',
                'Number of unique graphs: ',
                length(unique(object@decimal)),
                sep = '')

            # Display the amount of time it took to complete.
            cat('\n',
                'Run time in seconds: ',
                object@time,
                sep = '')

            # Display the number of iterations, burn in, and step size
            cat('\n',
                'Iterations: ',
                object@iterations,
                '\n',
                'Burn in: ',
                object@burnIn,
                '%',
                '\n',
                'Step size: ',
                object@stepSize,
                sep = '')

          })

#' plot
#'
#' @param x An object of class baycn.
#'
#' @param presence A scalar between 0 and 1. This is the cutoff for considering
#' an edge to be present. For example, if presence = 0.4 then an edge is
#' considered to be present if the sum of the posterior proability for the two
#' edge directions is greater than 0.4. The edge will be considered to be absent
#' if this sum is less than 0.4.
#'
#' @param direction A scalar between 0 and 1. This is the cutoff for determining
#' the direction of an edge. For example, if direction = 0.2 then an edge is
#' considered to be directed if the difference between the posterior proability
#' for the two edge directions is greater than 0.2. An edge will be considered
#' undirected if the difference is less than 0.2.
#'
#' @param edgeLabel Logical - indicates whether the posterior probabilities
#' should be included as edge labels in the plot. If edgeLabel is TRUE then
#' weighted must also be set to TRUE.
#'
#' @param mode See \code{\link[igraph]{graph_from_adjacency_matrix}} for
#' details.
#'
#' @param weighted See \code{\link[igraph]{graph_from_adjacency_matrix}} for
#' details.
#'
#' @param ... Other Arguments passed to plot.igraph.
#'
#' @importFrom igraph graph_from_adjacency_matrix plot.igraph E
#'
#' @aliases plot,baycn-method
#'
#' @export
#'
setMethod('plot',
          signature(x = 'baycn'),
          definition = function (x,
                                 presence = 0.4,
                                 direction = 0.2,
                                 edgeLabel = TRUE,
                                 mode = 'directed',
                                 weighted = TRUE,
                                 ...) {

            # Create a directed adjacency matrix to pass to igraph functions.
            dam <- directedAM(ppm = x@posteriorPM,
                              presence = presence,
                              direction = direction)

            # Create an igraph object from the directed adjacency matrix.
            igraph_dam <- graph_from_adjacency_matrix(dam,
                                                      mode = mode,
                                                      weighted = weighted)

            # Create edge labels from the graph_from_adjacency_matrix function.
            weightLabels <- round(E(igraph_dam)$weight,
                                  3)

            # Determine whether or not to include edge labels in the plot.
            if (edgeLabel == TRUE) {

              # Plot the network.
              plot.igraph(igraph_dam,
                          edge.label = weightLabels,
                          ...)

            } else {

              # Plot the network.
              plot.igraph(igraph_dam,
                          ...)

            }

          })
