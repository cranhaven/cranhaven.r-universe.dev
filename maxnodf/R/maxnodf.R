#' Calculate the maximum nestedness of a bipartite network
#'
#' Calculates the maximum NODF that be achieved in a network with a given number of rows, columns and links.
#' @param web Either a numeric matrix describing a bipartite network (a bipartite incidence matrix where elements are positive numbers if nodes interact, and 0 otherwise) 
#' or a numeric vector of length 3 of the form web = c(#Rows, #Columns, #Links).
#' @param quality An optional quality parameter to control the tradeoff between computation time and result quality. Can be 0, 1 or 2.
#' @details For a given network, \code{maxnodf} calculates the maximum nestedness that can be achieved in a network with a given number of rows, columns and links, subject to the constraint that all rows and columns must have at least one link (i.e. marginal totals must always be >= 1). 
#' This allows nestedness values to be normalised as \eqn{NODF/max(NODF)} following Song et al (2017). To control for connectance and network size, Song et al. (2017) suggest an additional normalisation that
#' can be used: \eqn{(NODF/max(NODF))/(C * log(S))} where C is the network connectance and S is the geometric mean of the number of plants and pollinators in the network.
#' 
#' \code{maxnodf} has three algorithms for finding the maximum nestedness of a bipartite network. These can be set using the \code{quality} argument. Lower quality settings are faster, but find worse optima. Higher quality settings
#' are slower, but find better optima.
#' \itemize{
#' \item{\code{quality} = 0, uses a greedy algorithm.}
#' \item{\code{quality} = 1, uses a greedy algorithm plus hillclimbing.}
#' \item{\code{quality} = 2, uses a simulated annealing algorithm, with the greedy algorithm output as the start point. Best results, but requires the most computation time.}
#' }
#' @return Returns a list of length 2, where the first element ('max_nodf') is the maximum nestedness of the network and the second element ('max_nodf_mtx') is the incidence matrix corresponding to this maximum nestedness.
#' @references 
#' Song, C., Rohr, R.P. and Saavedra, S., 2017. Why are some plant–pollinator networks more nested than others? Journal of Animal Ecology, 86(6), pp.1417-1424
#' @examples
#' maxnodf(matrix(1.0, 12, 10))
#' maxnodf(c(14, 13, 52), 2)
#' @useDynLib maxnodf
#' @import Rcpp
#' @export
maxnodf <- function(web, quality = 0){
    NodesA <- -1
    NodesB <- -1
    Edges <- -1
    if(is.matrix(web)){
        if(all(is.numeric(web))){
            web[web>0] <- 1
            if(any(web < 0)){
                stop("Invalid network. Ensure all elements of web >= 0.")
            }
            mt_0  <- computeMT0(web)
            mt_t  <- computeMTt(web)
            if(any(mt_0 < 1)){
                stop("Invalid network. Ensure all row and column totals are >= 1.")
            }
            if(any(mt_t < 1)){
                stop("Invalid network. Ensure all row and column totals are >= 1.")
            }
            NodesA <- nrow(web)
            NodesB <- ncol(web)
            Edges <- sum(web)
        }
        else{
            stop("Parameter 'web' is expected to be a numeric matrix or a numeric vector.")
        }
    }else if(is.vector(web)){
        if(length(web) == 3){
            if(all(round(web) == web)){
                NodesA <- web[[1]]
                NodesB <- web[[2]]
                Edges <- web[[3]]
            }else{
                stop("The vector 'web' is expected to have three integer members")
            }
        }else{
            stop("The vector 'web' is expected to have three integer members")
        }
    }else{
        stop("Parameter 'web' is expected to either be a matrix or a vector containing the matrix dimensions and number of links.")
    }
    if(Edges <= NodesA + NodesB){
        # stop("Number of links needs to satisfy 'Links > nrow(web) + ncol(web).")
        out_str <- 'Number of links does not satisfy Number of links > nrow(web) + ncol(web).\n'
        out_str <- paste(out_str, 'This indicates that web contains more than one compartment')
        out_str <- paste(out_str, '(subsets of the web which are not connected to the main network).\n')
        out_str <- paste(out_str, 'The input data had', NodesA + NodesB, "nodes and", Edges, "links.")
        out_str <- paste(out_str, 'We have added', (NodesA + NodesB + 1) - Edges, "links")
        out_str <- paste(out_str, 'to satisfy Number of links > nrow(web) + ncol(web)')
        out_str <- paste(out_str, 'and ensure the web contains only one compartment.\n')
        out_str <- paste(out_str, 'This allows us to compute a maximum NODF estimate.\n')
        out_str <- paste(out_str, 'NODF maximisation will not be accurate in this case, though the estimate')
        out_str <- paste(out_str, 'is conservative.\nPlease consider running maxnodf on individual')
        out_str <- paste(out_str, 'compartments, such as by removing nodes which are not connected to the main network, for better results.')
        warning(out_str)
        # We increase the number of edges to get an estimate.
        Edges <- NodesA + NodesB + 1
    }
    if(Edges > NodesA * NodesB){
        stop("Number of links needs to satisfy 'Links <= nrow(web) * ncol(web).")
    }
    if( !quality %in% 0:2){
        stop("Please chose a valid quality parameter. Options: \n\tquality = 0 -> Use a very fast greedy algorithm.\n\tquality = 1 -> Improved result using hillclimbing in combination with greedy.\n\tquality = 2 -> Use a simulated annealing algorithm. Best results but requires the most computation time.")
    }

    if(quality == 0){
        # Use a basic greedy algorithm to find the optimum
        mtx <- greedy_solve2(NodesA, NodesB, Edges)
        cat("\n")
    }else if(quality == 1){
        # Use a greedy algorithm with one round of hill climbing to
        # improve the results
        mtx <- greedy_solve2(NodesA, NodesB, Edges)
        cat("\n")
        mtx <- full_hill_climb_cpp(mtx)
    }else if(quality == 2){
        # Use a full round of simulated annealing to find a highly
        # improved solution.
        mtx <- greedy_solve2(NodesA, NodesB, Edges)
        cat("\n")
        mtx <- sim_anneal_opt_cpp(mtx)
    }
    return(list(max_nodf = nodf_cpp(mtx), max_nodf_mtx = mtx))
}
