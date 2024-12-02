# Roxygen docs:
# https://roxygen2.r-lib.org/articles/rd.html#packages

#' Optimal Multi-Stratum Experimental Design with a Multi-Criteria approach
#' using the Multi-Stratum Two-Phase Local Search (MS-TPLS) Algorithm.
#'
#' @description Many industrial and laboratory experiments involve some factors
#' whose levels are more difficult to set than others, most times due to
#' the high cost and/or time required to reset them in consecutive tests. The
#' best way to handle such situations is to design the experiment according to a
#' multi-stratum structure, where restrictions on the complete randomization of
#' the experiment limit the total number of hard-to-set factor level changes. \cr
#' In the construction of optimal experimental designs, multi-criteria optimization
#' allows for the trade-off between competing research objectives, e.g. between
#' estimation-oriented and prediction-oriented optimization objectives. \cr
#' The \code{MultiDoE} package can be used to construct multi-stratum experimental
#' designs (for any number of strata) that optimize up to six statistical criteria
#' simultaneously. To solve such optimization problems, the innovative MS-Opt and
#' MS-TPLS algorithms proposed in Sambo, Borrotti, Mylona, and Gilmour (2016) are
#' implemented. The former relies on a local search procedure to find a locally
#' optimal experimental design. More precisely, it is an extension of the
#' Coordinate-Exchange (CE) algorithm that allows both the search of experimental
#' designs for any type of nested multi-stratum experiment and the optimization
#' of multiple criteria simultaneously. The latter, by embedding MS-Opt in a
#' Two-Phase Local Search framework, is able to generate a good Pareto front
#' approximation for the optimization problem under study. The package provides
#' different ways to choose the final optimal experimental design among those
#' belonging to the Pareto front.
#'
#' @details
#' Package: multiDoE \cr
#' Version: ?\cr
#' Date: ?\cr
#' License: ?\cr
#'
#' Although the implemented algorithms are designed to handle experimental designs
#' with experimental factors in at least two different strata, their flexibility
#' allows their application to even the simplest cases of completely randomized
#' and randomized block designs. Whatever the experimental structure of interest,
#' the designs manageable by the package are balanced. The user can choose the
#' structure of the experiment, through the \code{\link{MSOpt}} and
#' \code{\link{runTPLS}} functions, by defining:
#' \itemize{
#' \item the number of strata of the experiment;
#' \item the number of experimental factors in each stratum;
#' \item the number of experimental units in each stratum and thus the total
#' number of runs;
#' \item the number of levels of each experimental factor;
#' \item the presence or not of blocking factors.
#' }
#' It is possible to choose the a priori model among: the main effects model,
#' the model with main and interaction effects and the full quadratic model.
#' Finally, estimation of the ratios of error variances in consecutive strata is
#' required. The package \code{MultiDoE} allows to obtain experimental designs
#' that optimize any combination of the following optimality criteria: "I", "Id",
#' "D", "Ds", "A" and "As". \cr
#' Depending on the function used, it is possible to obtain either a single global
#' optimal solution for the optimization problem of interest (\code{\link{MSSearch}})
#' or the set of solutions forming the approximate Pareto front (\code{\link{runTPLS}}).
#' Once the Pareto front is available \code{\link{plotPareto}} serves as a method
#' for its graphical visualization (up to a three-dimensional criteria space).
#' \code{\link{optMultiCrit}} is an objective method for selecting a final
#' experimental design and it is based on minimization of the Euclidean distance
#' between the set of optimal designs forming the Pareto front and an unfeasible
#' ideal point called utopian point. While \code{\link{optMultiCrit}} chooses the
#' best design with respect to the entire set of criteria considered,
#' \code{\link{optSingleCrit}} select from the Pareto front the designs that
#' optimize the criteria when individually considered. Alternatively the
#' \code{\link{topsisOpt}} function implements the TOPSIS method, a
#' multi-criteria decision analysis approach that in the final design selection
#' process allows the differentiation of criteria on the basis of their importance.
#'
#' @author
#' Francesca Cucchi, Andrea Melloncelli, Francesco Sambo, Kalliopi Mylona
#' and Matteo Borrotti
#'
#' Maintainer: Andrea Melloncelli <andrea.melloncelli@vanlog.it>
#'
#' @references
#' M. Borrotti, F. Sambo and K. Mylona. A coordinate-exchange two-phase local
#' search algorithm for the D- and I-optimal designs of split-plot experiments.
#' Computational Statistics and Data Analysis, 2013.
#' \url{https://www.sciencedirect.com/science/article/abs/pii/S0167947313001175}
#'
#' M. Borrotti, F. Sambo, K. Mylona and S. Gilmour. A multi-objective
#' coordinate-exchange two-phase local search algorithm for multi-stratum
#' experiments. Statistics & Computing, 2016.
#' \url{https://link.springer.com/article/10.1007/s11222-016-9633-6}
#'
#' M. Méndez, M. Frutos, F. Miguel and R. Aguasca-Colomo. TOPSIS Decision on
#' Approximate Pareto Fronts by Using Evolutionary Algorithms: Application to an
#' Engineering Design Problem. Mathematics, 2020.
#' \url{https://www.mdpi.com/2227-7390/8/11/2072}
#'
#'

