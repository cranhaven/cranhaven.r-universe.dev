# Filename: mosalloc.R
#
# Date: 01.07.2024, modified: 13.05.2025, 28.12.2025
# Author: Felix Willems
# Contact: mail.willemsf+MOSAlloc@gmail.com 
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
# Licensing: GPL-3.0-or-later
#
# Please report any bugs or unexpected behavior to
# mail.willemsf+MOSAlloc@gmail.com 
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
#
#---------------------------------------------------------------------------
#
#' @title Multiobjective sample allocation for constraint multivariate and
#' multidomain optimal allocation in survey sampling
#'
#' @description Computes solutions to standard sample allocation problems
#' under various precision and cost restrictions. The input data is
#' transformed and parsed to the Embedded COnic Solver (ECOS) from the
#' 'ECOSolveR' package. Multiple survey purposes can be optimized
#' simultaneously through a weighted Chebyshev minimization. Note that in
#' the case of multiple objectives, \code{mosalloc()} does not necessarily
#' lead to Pareto optimality. This highly depends on the problem structure.
#' A strong indicator for Pareto optimality is when the weighted objective
#' values given by \code{Dbounds} are constant over all objective
#' components or when all components of \code{Qbounds} equal \code{1}.
#' In addition, \code{mosalloc()} can handle twice-differential convex
#' decision functionals (in which case Pareto optimality is ensured).
#' \code{mosalloc()} returns dual variables, enabling a detailed sensitivity
#' analysis.
#'
#' @param D (type: \code{matrix})
#' The objective matrix. A matrix of either precision or cost units.
#' @param d (type: \code{vector})
#' The objective vector. A vector of either fixed precision components
#' (e.g. finite population corrections) or fixed costs.
#' @param A (type: \code{matrix})
#' A matrix of precision units for precision constraints.
#' @param a (type: \code{vector})
#' The right-hand side vector of the precision constraints.
#' @param C (type: \code{matrix})
#' A matrix of cost coefficients for cost constraints
#' @param c (type: \code{vector})
#' The right-hand side vector of the cost constraints.
#' @param l (type: \code{vector})
#' A vector of lower box constraints.
#' @param u (type: \code{vector})
#' A vector of upper box constraints.
#' @param opts (type: \code{list})
#' The options used by the algorithms:
#' \cr \code{$sense} (type: \code{character}) Sense of optimization
#' (default = \code{"max_precision"}, alternative \code{"min_cost"}).
#' \cr \code{$f} (type: \code{function}) Decision functional over the objective
#' vector (default = \code{NULL}).
#' \cr \code{$df} (type: \code{function})
#' The gradient of f (default = NULL).
#' \cr \code{$Hf} (type: \code{function})
#' The Hesse matrix of f (default = NULL).
#' \cr \code{$init_w} (type: \code{numeric} or \code{matrix})
#' Preference weightings (default = \code{1}; The weight for first objective
#' component must be 1).
#' \cr \code{$mc_cores} (type: \code{integer})
#' The number of cores for parallelizing multiple input weightings stacked
#' rowwise (default = \code{1L}).
#' \cr \code{$pm_tol} (type: \code{numeric})
#' The tolerance for the projection method (default = \code{1e-5}).
#' \cr \code{max_iters} (type: \code{integer})
#' The maximum number of iterations (default = \code{100L}).
#' \cr \code{$print_pm} (type: \code{logical}) A \code{TRUE} or \code{FALSE}
#' statement whether iterations of the projection method should be printed
#' (default = \code{FALSE}).
#'
#' @references
#' See:
#'
#' Folks, J.L., Antle, C.E. (1965). *Optimum Allocation of Sampling Units to
#' Strata when there are R Responses of Interest*. Journal of the American
#' Statistical Association, 60(309), 225-233.
#' \doi{10.1080/01621459.1965.10480786}.
#'
#' Münnich, R., Sachs, E., Wagner, M. (2012). *Numerical solution of optimal
#' allocation problems in stratified sampling under box constraints*.
#' AStA Advances in Statistical Analysis, 96, 435-450.
#' \doi{10.1007/s10182-011-0176-z}.
#'
#' Neyman, J. (1934). *On the Two Different Aspects of the Representative
#' Method: The Method of Stratified Sampling and the Method of Purposive
#' Selection*. Journal of the Royal Statistical Society, 97(4), 558--625.
#'
#' Tschuprow, A.A. (1923). *On the Mathematical Expectation of the Moments of
#' Frequency Distribution in the Case of Correlated Observations*. Metron,
#' 2(3,4), 461-493, 646-683.
#'
#' Rupp, M. (2018). *Optimization for Multivariate and Multi-domain Methods
#' in Survey Statistics* (Doctoral dissertation). Trier University.
#' \doi{10.25353/UBTR-8351-5432-14XX}.
#'
#' Srikantan, K.S. (1963). *A Problem in Optimum Allocation*.
#' Operations Research, 11(2), 265-274.
#'
#' Willems, F. (2025). *A Framework for Multiobjective and Uncertain Resource
#' Allocation Problems in Survey Sampling based on Conic Optimization*
#' (Doctoral dissertation). Trier University.
#' \doi{10.25353/ubtr-9200-484c-5c89}.
#'
#' @note
#'
#' **Precision optimization** *(\code{opts$sense == "max_precision"},
#' \code{opts$f == NULL})*
#'
#' The mathematical problem solved is
#' \deqn{\min_{n, z, t}\{t: Dz-d\leq w^{-1}t, Az\leq a, Cn\leq c,
#' 1\leq n_iz_i\, \forall i, l\leq n \leq u\}}
#' with \eqn{\textbf{1}\leq l\leq u \leq N}, where
#' * \eqn{n,z\in\mathbb{R}^m} and \eqn{t\in\mathbb{R}} are the
#' optimization variables,
#' * \eqn{D\in\{M\in\mathbb{R}^{k_D\times m}: \sum_i M_{ij}>0\, \forall j,
#' \sum_j M_{ij}>0\, \forall i\}} a matrix of nonnegative objective precision
#' components with \eqn{k_D} the number of precision objectives
#' (the number of variables of interest),
#' * \eqn{d\in\mathbb{R}^{k_D}} the objective right hand-side (RHS), e.g.
#' the finite population correction (fpc),
#' * \eqn{A\in\mathbb{R}^{k_A\times m}} a nonnegative precision matrix with
#' \eqn{k_A} the number of precision constraints,
#' \eqn{a\in \mathbb{R}^{k_A}} the corresponding RHS, e.g. fpc + (upper bound to
#' the coefficient of variation)^2,
#' * \eqn{C\in\mathbb{R}^{k_C\times m}} a cost matrix with \eqn{k_C} the number
#' of cost constraint,
#' * \eqn{c\in\mathbb{R}^{k_C}} the corresponding RHS,
#' * \eqn{l,u\in\mathbb{R}^m} the bounds to the sample size vector \eqn{n},
#' * \eqn{N\in\mathbb{N}^m} the vector of population sizes, and
#' * \eqn{w} a given strictly positive preference weighting.
#'
#' Special cases of this formulation are
#' * Neyman-Tschuprow allocation (Neyman, 1934 and Tschuprow, 1923):
#' \deqn{\min_n \Big\{\sum_{h=1}^H
#' \Big(\frac{N_h^2S_h^2}{n_h}-N_hS_h^2\Big):\sum_{h=1}^H n_h \leq c\Big\}
#' \quad \Leftrightarrow \quad \min_{n, z, t}\{t: Dz-d\leq t, Cn\leq c,
#' 1\leq n_iz_i\, \forall i\}} with \eqn{D = (N_1^2S_1^2,\dots,N_H^2S_H^2)},
#' \eqn{d = \sum_{h=1}^H N_hS_h^2}, \eqn{C = (1,\dots,1)} and \eqn{c} a
#' maximum sample size. Here, \eqn{H} is the number of strata, \eqn{N_h}
#' the size of stratum \eqn{h} and \eqn{S_h^2} the variance of the variable
#' of interest in stratum \eqn{h}.
#' * box-constrained optimal allocation:
#' \deqn{\min_{n, z, t}\{t: Dz-d\leq t, Cn\leq c,
#' 1\leq n_iz_i\, \forall i, l\leq n \leq u\}} with
#' \eqn{D = (N_1^2S_1^2,\dots,N_H^2S_H^2)},
#' \eqn{d = \sum_{h=1}^H N_hS_h^2}, \eqn{C = (1,\dots,1)} and \eqn{c} a
#' maximum sample size (cf. Srikantan, 1963 and Münnich et al., 2012).
#' Here, \eqn{l} and \eqn{u} are bounds to the optimal sample size vector.
#' * cost and precision constrained univariate optimal allocation:
#' \deqn{\min_{n, z, t}\{t: Dz-d\leq t, Az\leq a, Cn\leq c,
#' 1\leq n_iz_i\, \forall i, l\leq n \leq u\}} with \eqn{k_D = 1}
#' (cf. Willems, 2025, Chapter 3).
#' * multivariate optimal allocation with weighted sum scalarization:
#' \deqn{\min_{n, z, t}\{t: w^\top Dz-w^\top d\leq t, Az\leq a, Cn\leq c,
#' 1\leq n_iz_i\, \forall i, l\leq n \leq u\}} where \eqn{w\in\mathbb{R}^{k_D}}
#' is a strictly positive preference weighting (cf. Folks and Antle, 1965,
#' and Rupp, 2018). Note that for this case the problem reduces to cost and
#' precision constrained univariate optimal allocation. Solutions are ensured
#' to be optimal in the Pareto sense.
#' * box-constraint two-stage cluster sampling:
#' \deqn{\quad \quad \quad\min_{n_\textbf{I},n_\textbf{II}}
#' \Big\{\Big(\frac{N_\textbf{I}^2
#' S_\textbf{I}^2}{n_\textbf{I}}-N_\textbf{I}S_\textbf{I}^2\Big) +
#' \frac{N_\textbf{I}}{n_\textbf{I}}\sum_{j=1}^{N_\textbf{I}}
#' \Big(\frac{N_{\textbf{II}j}^2S_{\textbf{II}j}^2}{n_{\textbf{II}j}}-
#' N_{\textbf{II}j}S_{\textbf{II}j}^2\Big):
#' c_{\textbf{I}}n_\textbf{I} + \frac{n_\textbf{I}}{N_\textbf{I}}
#' \sum_{j=1}^{N_\textbf{I}}c_{\textbf{II}j}n_{\textbf{II}j} \leq
#' c_\textrm{max},\hspace{3.5cm}}
#' \deqn{\hspace{9.1cm} l_\textbf{I}\leq n_\textbf{I}\leq u_\textbf{I},
#' l_\textbf{II}\leq n_\textbf{II}\leq u_\textbf{II} \Big\}}
#' \deqn{\Leftrightarrow \quad \min_{n, z, t}\{t: Dz-d\leq t, Cn\leq c,
#' 1\leq n_iz_i\, \forall i, l\leq n \leq u\}\hspace{4cm}} with
#' \eqn{D = (N_\textbf{I}^2S_\textbf{I}^2 -
#' N_\textbf{I}\sum_{j=1}^{N_\textbf{I}} N_{\textbf{II}j}S_{\textbf{II}j}^2,
#' N_\textbf{I}N_{\textbf{II}1}^2 S_{\textbf{II}1}^2,\dots,
#' N_\textbf{I}N_{\textbf{II}N_\textbf{I}}^2S_{\textbf{II}N_\textbf{I}}^2)},
#' \eqn{d = N_\textbf{I}S_\textbf{I}^2},
#' \eqn{C = [C_1, C_2]}, where
#' \eqn{C_1=(c_\textbf{I},l_\textbf{II}^\top,-u_\textbf{II}^\top)^\top} and
#' \eqn{C_2 = [N_\textbf{I}^{-1}c_\textbf{II}^\top;-\textbf{I};\textbf{I}]}
#' (\eqn{\textbf{I}} is the identity matrix),
#' \eqn{c = (c_\textrm{max},0,\dots,0)^\top},
#' where \eqn{l = (l_\textbf{I},l_\textbf{I}l_\textbf{II}^\top)^\top} and
#' \eqn{u = (u_\textbf{I},u_\textbf{I}u_\textbf{II}^\top)^\top}
#' (cf. Willems, 2025, Chapter 3).
#' Here, \eqn{N_\textbf{I}} is the number of
#' clusters, \eqn{N_{\textbf{II}j}} the size of cluster \eqn{j},
#' \eqn{S_\textbf{I}^2} the between cluster variance and
#' \eqn{S_{\textbf{II}j}^2} the within cluster variances
#' of the variable of interest. Furthermore, \eqn{c_\textrm{max}} is a
#' maximum expected cost, \eqn{c_\textbf{I}} a variable cost for sampling
#' one cluster, and \eqn{c_{\textbf{II}j}} a variable cost for sampling
#' one unit in cluster \eqn{j}. The optimal number of clusters to be drawn
#' and the optimal sample sizes are given through \eqn{n = (n_\textbf{I},
#' n_\textbf{I}n_{\textbf{II}1},\dots, n_\textbf{I}n_{\textbf{II}
#' N_\textbf{I}})^\top}.
#'
#' For the special cases above, solutions are unique and, thus, Pareto optimal.
#' For the general multiobjective problem formulation this is not the case.
#' However, a strong indicator for uniqueness of solutions is
#' \eqn{n_iz_i = 1\, \forall i} (\code{Qbounds}) or
#' \eqn{Dz-d = w^{-1}t} (\code{Dbounds}). Uniqueness can be ensured via a
#' stepwise procedure implemented in \code{mosallocStepwiseFirst()}.\cr
#'
#' **Precision optimization** *(\code{opts$sense == "max_precision"},
#' \code{opts$f ==}\eqn{f}, \code{opts$f ==} \eqn{\nabla f},
#' \code{opts$f ==} \eqn{Hf})*
#'
#' The mathematical problem solved is
#' \deqn{\min_{n, z}\{f(Dz-d): Az\leq a, Cn\leq c,
#' 1\leq n_iz_i\, \forall i, l\leq n \leq u\}}
#' with components as specified above and where
#' \eqn{f:\mathbb{R}^{k_D}\rightarrow \mathbb{R}, x \mapsto f(x)}
#' is a twice-differentiable convex decision functional.
#' E.g. a \eqn{p}-norm \eqn{f(x) = \lVert x \rVert_p} with
#' \eqn{p\in\mathbb{N}}.\cr
#'
#' **Cost optimization** *(\code{opts$sense == "min_cost"})*
#'
#' The mathematical problem solved is
#' \deqn{\min_{n, z, t}\{t: Dn-d\leq\textbf{1}t, Az\leq a, Cn\leq c,
#' 1\leq n_iz_i\, \forall i, l\leq n \leq u\}}
#' with \eqn{1\leq l\leq u \leq N}. Hence, the only difference to
#' precision optimization is the type of objective constraint
#' \eqn{Dn-d\leq\textbf{1}t}.
#'
#' Special cases of this formulation are
#' * the cost optimal allocation (possibly multivariate, i.e. \eqn{k_A\geq 2}):
#' \deqn{\min_{n, z, t}\{t: Dn-d\leq\textbf{1}t, Az\leq a, 1\leq n_iz_i\,
#' \forall i\}} where \eqn{D^\top} is a vector of stratum-specific sampling
#' cost and \eqn{d} some fixed cost.
#'
#' @return \code{mosalloc()} returns a list containing the following
#' components:
#' @returns \code{$w} The initial preference weighting \code{opts$init_w}.
#' @returns \code{$n} The vector of optimal sample sizes.
#' @returns \code{$J} The optimal objective vector.
#' @returns \code{$Objective} The objective value with respect to decision
#' functional f. \code{NULL} if \code{opts$f = NULL}.
#' @returns \code{$Utopian} The component-wise univariate optimal
#' objective vector. \code{NULL} if \code{opts$f = NULL}.
#' @returns \code{$Normal} The vector normal to the Pareto frontier at
#' \code{$J}.
#' @returns \code{$dfJ} The gradient of \code{opts$f} evaluated at \code{$J}.
#' @returns \code{$Sensitivity} The dual variables of the objectives and
#' constraints.
#' @returns \code{$Qbounds} The Quality bounds of the Lorentz cones.
#' @returns \code{$Dbounds} The weighted objective constraints ($w * $J).
#' @returns \code{$Scalepar} An internal scaling parameter.
#' @returns \code{$Ecosolver} A list of ECOSolveR returns including:
#' \cr \code{...$Ecoinfostring} The info string of
#' \code{ECOSolveR::ECOS_csolve()}.
#' \cr \code{...$Ecoredcodes} The redcodes of \code{ECOSolveR::ECOS_csolve()}.
#' \cr \code{...$Ecosummary} Problem summary of \code{ECOSolveR::ECOS_csolve()}.
#' @returns \code{$Timing} Run time info.
#' @returns \code{$Iteration} A list of internal iterates. \code{NULL} if
#' \code{opts$f = NULL}.
#'
#' @examples
#' # Artificial population of 50 568 business establishments and 5 business
#' # sectors (data from Valliant, R., Dever, J. A., & Kreuter, F. (2013).
#' # Practical tools for designing and weighting survey samples. Springer.
#' # https://doi.org/10.1007/978-1-4614-6449-5, Example 5.2 pages 133-9)
#'
#' # See also <https://umd.app.box.com/s/9yvvibu4nz4q6rlw98ac/file/297813512360>
#' # file: Code 5.3 constrOptim.example.R
#'
#' Nh <- c(6221, 11738, 4333, 22809, 5467) # stratum sizes
#' ch <- c(120, 80, 80, 90, 150) # stratum-specific cost of surveying
#'
#' # Revenues
#' mh.rev <- c(85, 11, 23, 17, 126) # mean revenue
#' Sh.rev <- c(170.0, 8.8, 23.0, 25.5, 315.0) # standard deviation revenue
#'
#' # Employees
#' mh.emp <- c(511, 21, 70, 32, 157) # mean number of employees
#' Sh.emp <- c(255.50, 5.25, 35.00, 32.00, 471.00) # std. dev. employees
#'
#' # Proportion of estabs claiming research credit
#' ph.rsch <- c(0.8, 0.2, 0.5, 0.3, 0.9)
#'
#' # Proportion of estabs with offshore affiliates
#' ph.offsh <- c(0.06, 0.03, 0.03, 0.21, 0.77)
#'
#' budget <- 300000 # overall available budget
#' n.min  <- 100 # minimum stratum-specific sample size
#'
#' # Examples
#' #----------------------------------------------------------------------------
#' # Example 1: Minimization of the variation of estimates for revenue subject
#' # to cost restrictions and precision restrictions to the coefficient of
#' # variation of estimates for the proportion of businesses with offshore
#' # affiliates.
#'
#' l <- rep(n.min, 5) # minimum sample size per stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- rbind(ch,
#'            ch * c(-1, -1, -1, 0, 0))
#' c <- c(budget,                        # Maximum overall survey budget
#'        - 0.5 * budget)                # Minimum overall budget for strata 1-3
#'
#' # We require at maximum 5 % relative standard error for estimates of
#' # proportion of businesses with offshore affiliates
#' A <- matrix(ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2,
#'  nrow = 1)
#' a <- sum(ph.offsh * (1 - ph.offsh) * Nh**2/(Nh - 1)
#' )/sum(Nh * ph.offsh)**2 + 0.05**2
#'
#' D <- matrix(Sh.rev**2 * Nh**2, nrow = 1) # objective variance components
#' d <- sum(Sh.rev**2 * Nh) # finite population correction
#'
#' opts <- list(sense = "max_precision",
#'              f = NULL, df = NULL, Hf = NULL,
#'              init_w = 1,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 100L, print_pm = FALSE)
#'
#' sol <- mosalloc(D = D, d = d, A = A, a = a, C = C, c = c, l = l, u = u,
#'                 opts = opts)
#'
#' # Check solution statement of the internal solver to verify feasibility
#' sol$Ecosolver$Ecoinfostring # [1] "Optimal solution found"
#'
#' # Check constraints
#' c(C[1, ] %*% sol$n) # [1] 3e+05
#' c(C[2, ] %*% sol$n) # [1] -150000
#' c(sqrt(A %*% (1 / sol$n) - A %*% (1 / Nh))) # 5 % rel. std. err.
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Minimization of the maximum relative variation of estimates for
#' # the total revenue, the number of employee, the number of businesses claimed
#' # research credit, and the number of businesses with offshore affiliates
#' # subject to cost restrictions
#'
#' l <- rep(n.min, 5) # minimum sample size ber stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- rbind(ch, ch * c(-1, -1, -1, 0, 0))
#' c <- c(budget, - 0.5 * budget)
#' A <- NULL # no precision constraint
#' a <- NULL # no precision constraint
#'
#' # Precision components (Variance / Totals^2) for multidimensional objective
#' D <- rbind(Sh.rev**2 * Nh**2/sum(Nh * mh.rev)**2,
#'            Sh.emp**2 * Nh**2/sum(Nh * mh.emp)**2,
#'            ph.rsch * (1 - ph.rsch) * Nh**3/(Nh - 1)/sum(Nh * ph.rsch)**2,
#'            ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2)
#'
#' d <- as.vector(D %*% (1 / Nh)) # finite population correction
#'
#' opts <- list(sense = "max_precision",
#'              f = NULL, df = NULL, Hf = NULL,
#'              init_w = 1,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 100L, print_pm = FALSE)
#'
#' sol <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#'
#' # Obtain optimal objective value
#' sol$J # [1] 0.0017058896 0.0004396972 0.0006428475 0.0017058896
#'
#' # Obtain corresponding normal vector
#' sol$Normal # [1] 6.983113e-01 1.337310e-11 1.596167e-11 3.016887e-01
#'
#' # => Revenue and offshore affiliates are dominating the solution with a
#' #    ratio of approximately 2:1 (sol$Normal[1] / sol$Normal[4])
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Example 2 with preference weighting
#'
#' w <- c(1, 3.85, 3.8, 1.3) # preference weighting
#' l <- rep(n.min, 5) # minimum sample size ber stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- rbind(ch, ch * c(-1, -1, -1, 0, 0))
#' c <- c(budget, - 0.5 * budget)
#' A <- NULL # no precision constraint
#' a <- NULL # no precision constraint
#'
#' D <- rbind(Sh.rev**2 * Nh**2/sum(Nh * mh.rev)**2,
#'            Sh.emp**2 * Nh**2/sum(Nh * mh.emp)**2,
#'            ph.rsch * (1 - ph.rsch) * Nh**3/(Nh - 1)/sum(Nh * ph.rsch)**2,
#'            ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2)
#'
#' d <- as.vector(D %*% (1 / Nh))
#'
#' opts <- list(sense = "max_precision",
#'              f = NULL, df = NULL, Hf = NULL,
#'              init_w = w,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 100L, print_pm = FALSE)
#'
#' mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#'
#' #----------------------------------------------------------------------------
#' # Example 4: Example 2 with multiple preference weightings for simultaneous
#' # evaluation
#'
#' w <- matrix(c(1.0, 1.0, 1.0, 1.0,       # matrix of preference weightings
#'               1.0, 3.9, 3.9, 1.3,
#'               0.8, 4.2, 4.8, 1.5,
#'               1.2, 3.5, 4.8, 2.0,
#'               2.0, 1.0, 1.0, 2.0), 5, 4, byrow = TRUE)
#' w <- w / w[,1]     # rescale w (ensure the first weighting to be one)
#' l <- rep(n.min, 5) # minimum sample size ber stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- rbind(ch, ch * c(-1, -1, -1, 0, 0))
#' c <- c(budget, - 0.5 * budget)
#' A <- NULL # no precision constraint
#' a <- NULL # no precision constraint
#'
#' D <- rbind(Sh.rev**2 * Nh**2/sum(Nh * mh.rev)**2,
#'            Sh.emp**2 * Nh**2/sum(Nh * mh.emp)**2,
#'            ph.rsch * (1 - ph.rsch) * Nh**3/(Nh - 1)/sum(Nh * ph.rsch)**2,
#'            ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2)
#'
#' d <- as.vector(D %*% (1 / Nh))
#'
#' opts <- list(sense = "max_precision",
#'              f = NULL, df = NULL, Hf = NULL,
#'              init_w = w,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 100L, print_pm = FALSE)
#'
#' sols <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#' lapply(sols, function(sol){sol$Qbounds})
#'
#' #----------------------------------------------------------------------------
#' # Example 5: Example 2 where a weighted sum scalarization of the objective
#' # components is minimized
#'
#' l <- rep(n.min, 5) # minimum sample size ber stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- matrix(ch, nrow = 1)
#' c <- budget
#' A <- NULL # no precision constraint
#' a <- NULL # no precision constraint
#'
#' # Objective variance components
#' D <- rbind(Sh.rev**2 * Nh**2/sum(Nh * mh.rev)**2,
#'            Sh.emp**2 * Nh**2/sum(Nh * mh.emp)**2,
#'            ph.rsch * (1 - ph.rsch) * Nh**3/(Nh - 1)/sum(Nh * ph.rsch)**2,
#'            ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2)
#'
#' d <- as.vector(D %*% (1 / Nh)) # finite population correction
#'
#' # Simple weighted sum as decision functional
#' wss <- c(1, 1, 0.5, 0.5) # preference weighting (weighted sum scalarization)
#'
#' Dw <- wss %*% D
#' dw <- as.vector(Dw %*% (1 / Nh))
#'
#' opts <- list(sense = "max_precision",
#'              f = NULL, df = NULL, Hf = NULL,
#'              init_w = 1,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 1000L, print_pm = FALSE)
#'
#' # Solve weighted sum scalarization (WSS) via mosalloc
#' sol_wss <- mosalloc(D = Dw, d = dw, C = C, c = c, l = l, u = u, opts = opts)
#'
#' # Obtain optimal objective values
#' J <- D %*% (1 / sol_wss$n) - d
#'
#' # Reconstruct solution via a weighted Chebyshev minimization
#' wcm <- J[1] / J
#' opts = list(sense = "max_precision",
#'             f = NULL, df = NULL, Hf = NULL,
#'             init_w = matrix(wcm, 1),
#'             mc_cores = 1L, pm_tol = 1e-05,
#'             max_iters = 1000L, print_pm = FALSE)
#'
#' sol_wcm <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#'
#' # Compare solutions
#' rbind(t(J), sol_wcm$J)
#' #            [,1]         [,2]         [,3]        [,4]
#' # [1,] 0.00155645 0.0004037429 0.0005934474 0.001327165
#' # [2,] 0.00155645 0.0004037429 0.0005934474 0.001327165
#'
#' rbind(sol_wss$n, sol_wcm$n)
#' #          [,1]     [,2]     [,3]     [,4]     [,5]
#' # [1,] 582.8247 236.6479 116.7866 839.5988 841.4825
#' # [2,] 582.8226 236.6475 116.7871 839.5989 841.4841
#'
#' rbind(wss, sol_wcm$Normal / sol_wcm$Normal[1])
#' #    [,1]      [,2]      [,3]      [,4]
#' #wss    1 1.0000000 0.5000000 0.5000000
#' #       1 0.9976722 0.4997552 0.4997462
#'
#' #----------------------------------------------------------------------------
#' # Example 6: Example 1 with two subpopulations and a p-norm as decision
#' # functional
#'
#' l <- rep(n.min, 5) # minimum sample size per stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- rbind(ch, ch * c(-1, -1, -1, 0, 0))
#' c <- c(budget, - 0.5 * budget)
#'
#' # At maximum 5 % relative standard error for estimates of proportion of
#' # businesses with offshore affiliates
#' A <- matrix(ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2,
#'  nrow = 1)
#' a <- sum(ph.offsh * (1 - ph.offsh) * Nh**2/(Nh - 1)
#' )/sum(Nh * ph.offsh)**2 + 0.05**2
#'
#' D <- rbind((Sh.rev**2 * Nh**2)*c(0,0,1,1,0),
#'            (Sh.rev**2 * Nh**2)*c(1,1,0,0,1))# objective variance components
#' d <- as.vector(D %*% (1 / Nh)) # finite population correction
#'
#' # p-norm solution
#' p <- 5 # p-norm
#' opts <- list(sense = "max_precision",
#'              f = function(x) sum(x**p),
#'              df = function(x) p * x**(p - 1),
#'              Hf = function(x) diag(p * (p - 1) * x**(p - 2)),
#'              init_w = 1,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 1000L, print_pm = TRUE)
#'
#' sol <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#'
#' c(sol$Normal/sol$dfJ)/mean(c(sol$Normal/sol$dfJ))
#' # [1] 0.9999972 1.0000028
#'
#' #----------------------------------------------------------------------------
#' # Example 7: Example 2 with p-norm as decision functional and only one
#' # overall cost constraint
#'
#' l <- rep(n.min, 5) # minimum sample size ber stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- matrix(ch, nrow = 1)
#' c <- budget
#' A <- NULL # no precision constraint
#' a <- NULL # no precision constraint
#'
#' # Objective precision components
#' D <- rbind(Sh.rev**2 * Nh**2/sum(Nh * mh.rev)**2,
#'            Sh.emp**2 * Nh**2/sum(Nh * mh.emp)**2,
#'            ph.rsch * (1 - ph.rsch) * Nh**3/(Nh - 1)/sum(Nh * ph.rsch)**2,
#'            ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2)
#'
#' d <- as.vector(D %*% (1 / Nh)) # finite population correction
#'
#' # p-norm solution
#' p <- 5 # p-norm
#' opts <- list(sense = "max_precision",
#'              f = function(x) sum(x**p),
#'              df = function(x) p * x**(p - 1),
#'              Hf = function(x) diag(p * (p - 1) * x**(p - 2)),
#'              init_w = 1,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 1000L, print_pm = TRUE)
#'
#' sol <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#'
#' c(sol$Normal/sol$dfJ)/mean(c(sol$Normal/sol$dfJ))
#' # [1] 1.0014362 0.9780042 1.0197807 1.0007789
#'
#' #----------------------------------------------------------------------------
#' # Example 8: Minimization of sample sizes subject to precision constraints
#'
#' l <- rep(n.min, 5) # minimum sample size ber stratum
#' u <- Nh            # maximum sample size per stratum
#'
#' # We require at maximum 4.66 % relative standard error for the estimate of 
#' # total revenuee, 5 % for the number of employees, 3 % for the proportion of
#' # businesses claiming research credit, and 3 % for the proportion of
#' # businesses with offshore affiliates
#' A <- rbind(Sh.rev**2 * Nh**2/sum(Nh * mh.rev)**2,
#'            Sh.emp**2 * Nh**2/sum(Nh * mh.emp)**2,
#'            ph.rsch * (1 - ph.rsch) * Nh**3/(Nh - 1)/sum(Nh * ph.rsch)**2,
#'            ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2)
#' a <- as.vector(A%*%(1 / Nh) + c(0.0466, 0.05, 0.03, 0.03)**2)
#'
#' # We do not consider any additional sample size or cost constraints
#' C <- NULL # no cost constraint
#' c <- NULL # no cost constraint
#'
#' # Since we minimize the sample size, we define D and d as follows:
#' D <- matrix(1, nrow = 1, ncol = length(Nh)) # objective cost components
#' d <- as.vector(0)                           # vector of possible fixed cost
#'
#' opts <- list(sense = "min_cost", # Sense of optimization is survey cost
#'              f = NULL,
#'              df = NULL,
#'              Hf = NULL,
#'              init_w = 1,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 100L, print_pm = TRUE)
#'
#' sol <- mosalloc(D = D, d = d, A = A, a = a, l = l, u = u, opts = opts)
#'
#' sum(sol$n) # [1] 2843.219
#' sol$J # [1] 2843.219
#'
#' #----------------------------------------------------------------------------
#' #----------------------------------------------------------------------------
#' # Note: Sample size optimization for two-stage cluster sampling can be
#' #       reduced to the structure of optimal stratified random samplin when
#' #       considering expected costs. Therefore, mosalloc() can handle such
#' #       designs. A benefit is that mosalloc() allows relatively complex
#' #       sample size restrictions such as box constraints for subsampling.
#' #       Optimal sample sizes at secondary stages have to be reconstructed
#' #       from sol$n.
#' #
#' # Example 9: Optimal number of primary sampling units (PSU) and secondary
#' # sampling units (SSU) in 2-stage cluster sampling.
#'
#' set.seed(1234)
#' pop <- data.frame(value = rnorm(100, 100, 35),
#'                   cluster = sample(1:4, 100, replace = TRUE))
#'
#' CI <- 36  # Sampling cost per PSU
#' CII <- 10 # Average sampling cost per SSU
#'
#' NI <- 4                   # Number of PSUs
#' NII <- table(pop$cluster) # PSU/cluster sizes
#'
#' S2I <- var(by(pop$value, pop$cluster, sum)) # between PSU variance
#' S2II <- by(pop$value, pop$cluster, var)     # within PSU variances
#'
#' D <- matrix(c(NI**2 * S2I - NI * sum(NII * S2II), NI * NII**2 * S2II), 1)
#' d <- as.vector(D %*% (1 / c(NI, NI * NII))) # = NI * S2I
#'
#' C <- cbind(c(CI, rep(2, NI), -NII),
#'            rbind(rep(CII / NI, 4), -diag(4), diag(4)))
#' c <- as.vector(c(500, rep(0, 8)))
#'
#' l <- c(2, rep(4, 4))
#' u <- c(NI, NI * NII)
#'
#' opts <- list(sense = "max_precision",
#'              f = NULL,
#'              df = NULL,
#'              Hf = NULL,
#'              init_w = 1,
#'              mc_cores = 1L, pm_tol = 1e-05,
#'              max_iters = 100L, print_pm = TRUE)
#'
#' sol <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#'
#' # Optimum number of clusters to be drawn
#' sol$n[1] # [1] 2.991551
#'
#' # Optimum number of elements to be drawn within clusters
#' sol$n[-1] / sol$n[1] # [1] 12.16454 11.60828 15.87949 12.80266
#'
#' @export
mosalloc <- function(D, d, A = NULL, a = NULL, C = NULL, c = NULL,
                     l = 2, u = NULL, opts = list(sense = "max_precision",
                                                  f = NULL,
                                                  df = NULL,
                                                  Hf = NULL,
                                                  init_w = 1,
                                                  mc_cores = 1L,
                                                  pm_tol = 1e-5,
                                                  max_iters = 100L,
                                                  print_pm = FALSE)) {
  time_t0 <- proc.time()

  # Check input
  #-----------------------------------------------------------------------------
  # check if package parallel is available; if not, never try parallelization
  if (nchar(system.file(package = "parallel")) == 0) {
    opts$mc_cores <- 1L
  }

  # get sense of optimization (precision maximization or cost minimization)
  if (opts$sense == "max_precision") {
    ksense <- ncol(D)
    if (is.null(C) | is.null(c)) {
      stop("No cost constraint specified!")
    }
  } else if (opts$sense == "min_cost") {
    ksense <- 0
    if (is.null(A) | is.null(a)) {
      stop("No precision constraint specified!")
    }
  } else {
    stop("No or invalid 'sense'!")
  }

  # check objective matrix and vector
  if (!is.matrix(D)) {
    stop("D is not a matrix!")
  } else {
    # get number of strata H and number of variates Q
    H <- ncol(D)
    Q <- nrow(D)
    if (!(all(rowSums(D) > 0) & all(colSums(D) > 0))) {
      stop("D is not a valid input matrix!")
    }
  }

  if (!is.vector(d)) {
    stop("d is not a vector!")
  }else {
    if (length(d) != Q) {
      stop("Dimension of D and d do not match!")
    }
    if (any(d < 0)) {
      stop("d is negative!")
    }
  }

  # precision constraints
  if (is.null(A)) {
    A <- matrix(NA, nrow = 0, ncol = H)
    a <- c()
  } else {
    if (!is.matrix(A)) {
      stop("A is not a matrix!")
    } else {
      if (ncol(A) != H) {
        stop("Worng dimensin of A!")
      }
      if (any(A < 0)) {
        stop("Negative accuracies. This is not valid!")
      }
      if (!is.vector(a)) {
        stop("a is not a vector!")
      } else {
        if(nrow(A) != length(a)){
          stop("Dimension of A and a do not match!")
        }
        if (any(a <= 0)) {
          stop("a is not problem compatible (nonpositivity)!")
        }
      }
    }
  }

  # cost constraints
  if (is.null(C)) {
    C <- matrix(NA, nrow = 0, ncol = H)
    c <- c()
  } else {
    if (!is.matrix(C)) {
      stop("C is not a matrix!")
    } else {
      if (ncol(C) != H) {
        stop("Worng dimensin of C!")
      }
      if (!is.vector(c)) {
        stop("c is not a vector!")
      } else {
        if(nrow(C) != length(c)){
          stop("Dimension of C and c do not match!")
        }
      }
    }
  }

  # box constraints must be specified to ensure existence of solutions
  if (is.null(u)) {
    stop("No upper box-constraints defined!")
  } else {
    if (!is.vector(u)) {
      stop("u is not a vector!")
    }
    if (length(u) != H) {
      stop("Wrong dimension of u!")
    }
  }

  if (!is.vector(l)) {
    stop("l is not a vector!")
  } else {
    if(length(l) == 1) {
      l <- as.vector(rep(l, H))
    } else {
      if (length(l) != H) {
        stop("Wrong dimension of l!")
      }
    }
    if (any(l < 1)) {
      stop("Invalid problem structure! l has to be greater or equal to 1!")
    }
    if (any(u < l)) {
      stop("Invalid problem structure! u has to be greater or equal to l!")
    }
  }

  # check decision functional with its gradient and Hesse matrix
  if (is.function(opts$f)) {
    if (!is.function(opts$df)) {
      stop("df not specified!")
    }
    if (!is.function(opts$Hf)) {
      stop("Hf not specified!")
    }
  }

  # check weights
  if (is.vector(opts$init_w)) {
    if (length(opts$init_w) == 1) {
      w <- matrix(rep(1, Q), 1, Q)
    } else {
      if (length(opts$init_w) != Q) {
        stop("Wrong number of weigths!")
      }
      if (opts$init_w[1] != 1) {
        stop("First weight component is not 1!")
      }
      w <- matrix(opts$init_w, 1, Q)
    }
  } else if (is.matrix(opts$init_w)) {
    if (ncol(opts$init_w) != Q) {
      stop("Wrong number of weigths!")
    }
    if (any(opts$init_w[, 1] != 1)) {
      stop("First weight component is not 1!")
    }
    w <- opts$init_w
  } else {
    stop("Weights are not correctly specified!")
  }
  if (any(w <= 0)) {
    stop("Nonpositive weights detected!")
  }

  # scale input data and built problem matrix
  #-----------------------------------------------------------------------------
  if (opts$sense == "max_precision") {
    if (Q > 1 & any(as.vector(D %*% (1 / u)) / d - 1 < -1e8
        ) & any(rowSums(D / rep(u, each = dim(D)[1])) / d - 1 < -1e8))  {
      stop("d is not an utopian vector! d is too large. Try: d <- D %*% (1 / u)")
    }

    ecos_control <- ECOSolveR::ecos.control(
      maxit = as.integer(max(floor(log(ncol(D))**1.5) + 1, 35,
                             opts$max_iters / 10)),
      FEASTOL <- 1e-10,
      ABSTOL <- 1e-8,
      RELTOL <- 1e-8
    )

    kD <- nrow(D) # No. of objectives
    kA <- nrow(A) # No. of precision constraints
    kC <- nrow(C) # No. of cost constraints excluding box constraints

    # standardize input data
    Dsc <- D / min(D %*% (1 / u))
    dsc <- d / min(D %*% (1 / u))

    if (all(d == 0)) {
      d0 <- d
      dsc <- rep(1, kD)
    } else {
      Dsc <- Dsc / dsc
      d0 <- rep(1, kD)
    }

    Asc <- A / a
    # Csc <- C / c * sign(c) #;modification 20.01.2026
    cbool <- c == 0
    Csc <- C / abs(c)
    if (any(cbool)) {
      Csc[cbool, ] <- C[cbool, ]
    }


    # presolve via Neyman-Tschuprow allocation to estimate range of sample sizes
    sDsc    <- sqrt(Dsc)
    sCsc    <- sqrt(abs(Csc[!cbool, , drop = FALSE]))
    neyman  <- colSums(sDsc) / colSums(sCsc) / sum(colSums(sDsc) * colSums(sCsc))
    scale_n <- pmax(l + 0.1, pmin(neyman, u - 0.5))
    ny      <- sum(neyman) / sum(scale_n)

    scale_nexp <- scale_n * ny / exp(mean(log((scale_n))))

    # scale problem data
    scaleV   <- rbind(Dsc, Asc)
    zscale   <- exp(mean(log(scaleV[scaleV != 0])))
    scaleC   <- rbind(Csc[!cbool, , drop = FALSE])
    nscale   <- exp(mean(log(abs(scaleC[scaleC != 0]))))
    scale_nz <- exp((log(nscale) + log(zscale)) / 2) / zscale
    scalepar <- scale_nz / scale_nexp

    Dsc <- Dsc * rep(scalepar, each = dim(Dsc)[1])
    Asc <- Asc * rep(scalepar, each = dim(Asc)[1])
    Csc <- Csc / rep(scalepar, each = dim(Csc)[1])

    tsc <-  exp(mean(abs(log(1 / dsc))))
    scale_t <- exp(mean(log(Dsc %*% (1 / scale_n / scalepar) - d0))) * tsc

  } else {
    # respecify precision control for ECOSolveR
    ecos_control <- ECOSolveR::ecos.control(
      maxit = as.integer(max(floor(log(ncol(D))**1.5) + 1, 35,
                           opts$max_iters / 10)),
      FEASTOL <- 1e-10,
      ABSTOL <- 1e-8,
      RELTOL <- 1e-8)

    kD <- nrow(D) # No. of objectives
    kA <- nrow(A) # No. of precision constraints
    kC <- nrow(C) # No. of cost constraints excluding box constraints

    # standardize input data
    Dsc <- D / min(D %*% l)
    dsc <- d / min(D %*% l)

    if (all(d == 0)) {
      d0 <- -d
      dsc <- rep(1, kD)
    } else {
      Dsc <- Dsc / dsc
      d0 <- rep(-1, kD)
    }

    Asc <- A / a
    # Csc <- C / c * sign(c) #;modification 20.01.2026
    if (is.null(c)) {
      cbool <- c == 0
      Csc <- C / c
    } else {
      cbool <- c == 0
      Csc <- C / c * abs(c)
      if (any(cbool)) {
        Csc[cbool, ] <- C[cbool, ]
      }
    }

    # presolve via Neyman-Tschuprow allocation to estimate range of sample sizes
    sDsc    <- sqrt(abs(Dsc))
    sAsc    <- sqrt(Asc)
    neyman  <- colSums(sAsc) / colSums(sDsc) / sum(colSums(sAsc) * colSums(sDsc))
    scale_n <- pmax(l + 0.1, pmin(neyman, u - 0.5))
    ny      <- sum(neyman) / sum(scale_n)

    scale_nexp <- scale_n * ny / exp(mean(log((scale_n))))

    # scale problem data
    scaleV   <- Asc
    zscale   <- exp(mean(log(scaleV[scaleV != 0])))
    scaleC   <- rbind(Dsc, Csc[!cbool, , drop = FALSE])
    nscale   <- exp(mean(log(abs(scaleC[scaleC != 0]))))
    scale_nz <- exp((log(nscale) + log(zscale)) / 2) / zscale
    scalepar <- scale_nz / scale_nexp

    Dsc <- Dsc / rep(scalepar, each = dim(Dsc)[1])
    Asc <- Asc * rep(scalepar, each = dim(Asc)[1])
    Csc <- Csc / rep(scalepar, each = dim(Csc)[1])

    tsc <-  exp(mean(abs(log(1 / dsc))))
    scale_t <- exp(mean(log(Dsc %*% (scale_n / scalepar) - d0))) * tsc
  }

  # construct sparse data matrix for large-scale allocation problems
  A_idx <- list(getColRowVal(Dsc, ksense, 0),
                list(rep(H * 2 + 1, kD), 1:kD,
                     -scale_t / as.vector(dsc)),
                getColRowVal(Asc, H, kD),
                getColRowVal(Csc, 0, kD + kA),
                list(1:H, 1:H + kC + kD + kA, -1 / l / scalepar),
                list(1:H, 1:H + H + kC + kD + kA, 1 / u / scalepar),
                list(rep(1:H, rep(2, H)),
                     (1:(3*H))[-(1:H*3)] + H*2 + kC + kD + kA, rep(-1, 2 * H)),
                list(rep(1:H + H, rep(2, H)),
                     (1:(3*H))[-(1:H*3)] + H*2 + kC + kD + kA,
                     rep(c(-1, 1), H)))

  A_init <- Matrix::sparseMatrix(i = unlist(lapply(A_idx, function(X) X[[2]])),
                                 j = unlist(lapply(A_idx, function(X) X[[1]])),
                                 x = unlist(lapply(A_idx, function(X) X[[3]])),
                                 dims = c(H * 5 + kC + kD + kA, 2 * H + 1))

  # construct right hand-side
  b_init <- matrix(0, 1, kA + kC + kD + 5 * H)
  b_init[1:(kD + kA + kC + 2 * H)] <- sign(c(d0, a, c, -l, u))
  b_init[seq(1, 3 * H, 3) + kA + kC + 2 * H + kD + 2] <- 2

  # presolve, scale test, and feasibility check
  sol <- ECOSolveR::ECOS_csolve(
    c = c(matrix(0, 1, 2 * H), 1),
    G = A_init,
    h = b_init,
    dims = list(l = nrow(A_init) - 3 * H, q = matrix(3, H, 1), e = 0),
    A = NULL,
    b = numeric(0),
    bool_vars = integer(0),
    int_vars = integer(0),
    control = ecos_control
  )
  # check primal feasibility
  if (sol$retcodes[1] == 1) {
    warning("Allocation problem is infeasible!\n",
            "Specify results to verify infeasibility.")
  }
  # check quality bounds of Lorentz cones
  if (any(sol$x[1:H] * sol$x[1:H + H] - 1 > 1e-6)) {
    # rescale problem according to the solution of the presolve
    ecos_control$MAXIT <- as.integer(opts$max_iters)
    ecos_control$FEASTOL <- 1e-10
    ecos_control$ABSTOL <- 1e-9
    ecos_control$RELTOL <- 1e-9
    scaleadd <- 10**mean(log(sol$x[1:H], 10))

    Dsc <- Dsc / rep(scaleadd, each = dim(Dsc)[1])
    Asc <- Asc / rep(scaleadd, each = dim(Asc)[1])
    Csc <- Csc * rep(scaleadd, each = dim(Csc)[1])
    scalepar <- scalepar / scaleadd
    tsc     <-  exp(mean(abs(log(1 / dsc))))
    scale_t <- exp(mean(log(Dsc %*% (scale_n / scalepar) - d0))) * tsc / 10

    # construct sparse data matrix
    A_idx <- list(getColRowVal(Dsc, ksense, 0),
                  list(rep(H * 2 + 1, kD), 1:kD,
                       -scale_t / as.vector(dsc)),
                  getColRowVal(Asc, H, kD),
                  getColRowVal(Csc, 0, kD + kA),
                  list(1:H, 1:H + kC + kD + kA, -1 / l / scalepar),
                  list(1:H, 1:H + H + kC + kD + kA, 1 / u / scalepar),
                  list(rep(1:H, rep(2, H)),
                       (1:(3*H))[-(1:H*3)] + H*2 + kC + kD + kA,
                       rep(-1, 2*H)),
                  list(rep(1:H + H, rep(2, H)),
                       (1:(3*H))[-(1:H*3)] + H*2 + kC + kD + kA,
                       rep(c(-1, 1), H)))

    A_init <- Matrix::sparseMatrix(i = unlist(lapply(A_idx,
                                                     function(X) X[[2]])),
                                   j = unlist(lapply(A_idx,
                                                     function(X) X[[1]])),
                                   x = unlist(lapply(A_idx,
                                                     function(X) X[[3]])),
                                   dims = c(H * 5 + kC + kD + kA, 2 * H + 1))
  }

  # get decision functional, its gradient and Hesse matrix
  f <- opts$f
  df <- opts$df
  Hf <- opts$Hf

  # if(is.null(f)) => calculate solution for given weight matrix w 
  #-----------------------------------------------------------------------------
  if (is.null(f)) {
    time_t1 <- proc.time()
    sol <- wECOSCsolve(w = w[1, ], dsc = dsc, D = D, d = d, a = a, c = c,
                       l = l, u = u, scalepar = scalepar, scale_t = scale_t,
                       A_init = A_init, b_init = b_init,
                       ecos_control = ecos_control,
                       t0 = time_t0, t1 = time_t1)
    # if weighting matrix w has multiple rows
    if (nrow(w) > 1) {
      if (opts$mc_cores == 1) {
        SOL <- lapply(2:nrow(w), function(i) {
          w0 <- w[i, ]
          wECOSCsolve(w = w0, dsc = dsc, D = D, d = d,
                      a = a, c = c, l = l, u = u,
                      scalepar = scalepar, scale_t = scale_t,
                      A_init = A_init, b_init = b_init,
                      ecos_control = ecos_control,
                      t0 = time_t0, t1 = time_t1)
        })
      } else {
        SOL <- parallel::mclapply(2:nrow(w), function(i) {
          w0 <- w[i, ]
          wECOSCsolve(w = w0, dsc = dsc, D = D, d = d, a = a, c = c,
                      l = l, u = u, scalepar = scalepar, scale_t = scale_t,
                      A_init = A_init, b_init = b_init,
                      ecos_control = ecos_control,
                      t0 = time_t0, t1 = time_t1)
        }, mc.cores = opts$mc_cores)
      }
      # output
      SOL <- append(SOL, list(sol), after = 0)
      return(SOL)
    } else {
      return(sol)
    }
  }

  # Projection method for given decision functional f
  # Willems (2025, Algorithm 3)
  #---------------------------------------------------------------------------
  if (is.function(f)) {

    if (Q == 1) {
      stop("No multiobjective problem!")
    }

    if (opts$sense == "min_cost") {
      stop("Minimization via decision functional not implemented for ",
           "'min_cost'!")
    }

    time_f1 <- proc.time() # get time for outer solution

    # Readjust accuracy of ECOS
    ecos_control$MAXIT <- 1000L
    ecos_control$FEASTOL <- 1e-10
    ecos_control$ABSTOL <- 1e-10
    ecos_control$RELTOL <- 1e-10

    # Initialize multiobjective solver
    #---------------------------------------------------------------------------
    # get initial weight
    sumtst <- sumSol(1, A_init, b_init, kD, H, ecos_control)
    if (sumtst$feasibility[1] == 1) {stop("Problem is infeasible!")}
    if (sumtst$feasibility[1] == 2) {stop("Slaters condition not fulfilled!")}

    # calculate initial weights according to weighted sum approach
    # with equal weights
    nstart <- sumtst$x[1:H] / scalepar
    Jstart <- D %*% matrix(1 / nstart, ncol = 1) - d
    wstd <- exp(mean(log(Jstart)))
    w0     <- wstd / Jstart

    # calculate utopian vector
    uniobj <- getUnix(A_init, b_init, kD, H, ecos_control, opts$mc_cores)
    utopia <- abs((D * rep(scalepar, each = dim(uniobj)[1]) / uniobj
                  ) %*% matrix(1, nrow = H, ncol = 1) - d)

    # solve problem for initial weights w0 via ECOS
    # get solution n0(w0), objective vector J0(w0), normal vector N0(w0),
    # gradient of f in J0(w0) and its projection p0(w0) onto N0(w0)
    sol <- wSolve(w0, dsc, A_init, b_init, D, d, df,
                  scalepar, scale_t, ecos_control)

    #n0      <- sol$n
    J0      <- sol$J
    #lambda0 <- sol$lamda
    N0      <- sol$normalJ
    gradfJ0 <- sol$gradfJ
    p0      <- sol$projection
    acc0    <- max(abs(Jstart - J0)) # Check numerical accuracy

    # initialize iteration parameter
    iter      <- 0  # number of iterations
    tb        <- 0   # distance to control point between cutting hyper planes
    angle     <- c() # angle between normal on Pareto frontier and gradient
    # of decision functional

    # calculate cutting hyperplanes
    NORMALS   <- diag(1, nrow = kD, ncol = kD)
    NJ        <- utopia
    JJ        <- matrix(0, nrow = kD, ncol = 0)
    PP        <- matrix(0, nrow = 0, ncol = kD)

    # define stopping criteria
    stop_crit <- 1 - min(N0 / gradfJ0) / max(N0 / gradfJ0)
    angle     <- rbind(angle, optDeg(gradfJ0, N0))
    rloop <- stop_crit > opts$pm_tol & iter <= opts$max_iters
    #Jold <- Inf

    # store iterations in a list
    ITERATIONS <- list()
    ITERATIONS[[iter + 1]] <- list(iter = iter,
                                   w = as.vector(w0),
                                   sol = sol,
                                   obj_z = f(J0),
                                   Qbounds =  sol$n * sol$z,
                                   Dbounds = 0,
                                   stop_crit = stop_crit)

    # return results of iteration via text message
    if (opts$print_pm) {
      cat("\n Projection method:\n")
      cat("k = ", iter,
          "; stop_crit = ", round(stop_crit, 4),
          "; angle =", round(angle[iter + 1], 4),
          #"; |y_tilde-a| = ", -99,
          #"; |b* -y^{k+1}| = ", -99,
          #"; norm_p = ", round(sqrt(sum((p0)**2)),8),
          "; ||J^k-J^{k-1}|| = ", NA,
          "\n")
    }

    # loop until pm_tol reached or max_iters is reached
    # break if numerical instability occurs, tb < 0 or step length alpha < 1e-25
    # stop()
    while (rloop) {

      # iteration counter
      iter <- iter + 1
      PP   <- rbind(PP, p0)

      # calculate next tangent hyperplane in search direction and 
      # a feasible step length
      alphas <- (NJ - NORMALS %*% J0) / (NORMALS %*% p0)
      n_a <- which.min(alphas[alphas > 0])
      alpha <- min((-J0 / p0)[-J0 / p0 > 0])

      # if next tangent hyperplane is orthogonal to axes
      # better projections
      if (any(NORMALS[n_a, ] == 1)) {
        # shrink alpha until search direction gets valid
        while (any(PP %*% df(J0 + alpha * p0) >= 0) || any(J0 + alpha * p0 <= 0)) {
          alpha <- alpha * 0.89
          if (alpha < 1e-25) break
        }
      } else {
        # shrink alpha until search direction gets valid
        while (any(PP %*% df(J0 + alpha * p0) > 0) || any(J0 + alpha * p0 <= utopia)) {
          alpha <- alpha * 0.89
          if (alpha < 1e-25) break
          #print(alpha)
        }
      }

      # project calculated point to the Pareto frontier
      # shrink alpha until test point lies in relative search direction
      BOOL <- TRUE
      Jup <- rep(Inf, kD)
      while (BOOL) {
        tJ <- J0 + alpha * p0
        ry <- as.vector(NJ / NORMALS %*% tJ)
        pJ0alphap0 <- max(c(ry, 1)) * tJ

        BOOL <- f(pJ0alphap0) <= f(Jup)
        if (BOOL) alpha <- alpha * 0.89
        Jup <- pJ0alphap0
        if (alpha < 1e-25) break
      }
      if (alpha < 1e-25) {
        message("Maximum accuracy reached! Possibly increase accuracy for ECOS.")
        break
      }

      # Add tangential hyperplane to PF approximation
      NORMALS   <- rbind(NORMALS, matrix(N0, nrow = 1))
      NJ        <- c(NJ, sum(N0 * J0))
      JJ        <- cbind(JJ, J0)

      # solve worst-case problem for test point pJ0alphap0
      # solution gives Pareto optimal point on PF with max upper bound according
      # to w2a
      wstd <- exp(mean(log(Jup)))
      w2a <- wstd / Jup
      scalet <- as.vector(w2a) * as.vector(dsc)
      A_init[1:kD, 2 * H + 1] <- -1 * scale_t / as.vector(scalet)
      w2wcsol <- wcSolquick(A_init, b_init, Q, H, ecos_control)
      Jw2 <- D %*% matrix(scalepar / w2wcsol) - d

      # update weight, scale problem data and solve
      # get solution n2(w2), objective vector J2(w2),
      # Lagrange multiplier lambda2(w2)normal vector N2(w2),
      # gradient of f in J2(w2), projection p2
      wstd <- exp(mean(log(Jw2)))
      w2   <- wstd / Jw2
      sol2 <-  wSolve(w2, dsc, A_init, b_init, D, d, df,
                      scalepar, scale_t, ecos_control)

      #n2      <- sol2$n
      J2      <- sol2$J
      #lambda2 <- sol2$lamda
      N2      <- sol2$normalJ
      gradfJ2 <- sol2$gradfJ
      p2      <- sol2$projection
      acc2    <- max(abs(Jw2 - J2))
      # Test: Evaluate solution: sol2$n*sol2$z

      # calculate control point for Bezier approximation of Pareto frontier
      # and find minimum f(auadraticBezier(t, J0, b1, J2))
      tb <- sum(N2 * (J2 - J0)) / sum(N2 * p0)
      b1 <- J0 + tb * p0

      # Newton method to optimize f over the Bezier curve
      if (tb > 0) {
        if (f(quadraticBezier(1-0.0001, J0, b1, J2)) > f(quadraticBezier(1, J0, b1, J2))) {
          tnew <- 1
        } else {

          tnew <- 0.66; t0 <- 0; inner_iter <- 0
          c2 <- 0.9
          c1 <- 1e-4
          while(abs(tnew - t0) > 1e-16 & inner_iter < 25){
            inner_iter <- inner_iter + 1
            t0         <- tnew
            tstep      <- matrix(dquadraticBezier(t0, J0, b1, J2), nrow = 1)%*%df(
              quadraticBezier(t0, J0, b1, J2))/(matrix(
                d2quadraticBezier(t0, J0, b1, J2), nrow = 1)%*%df(
                  quadraticBezier(t0, J0, b1, J2)) +  matrix(
                    dquadraticBezier(t0, J0, b1, J2), nrow = 1)%*%Hf(
                      quadraticBezier(t0, J0, b1, J2))%*% matrix(
                        dquadraticBezier(t0, J0, b1, J2), ncol = 1))
            p_nr <- as.vector(-tstep)
            if (TRUE) {
              alpha_nr <- 1
              # Armijo rule
              while (
                f(quadraticBezier(t0+alpha_nr*p_nr, J0, b1, J2)) > f(
                  quadraticBezier(t0, J0, b1, J2)) + c1*alpha_nr*p_nr*sum(
                    dquadraticBezier(t0, J0, b1, J2)*df(quadraticBezier(
                      t0, J0, b1, J2))) || -p_nr*sum(dquadraticBezier(
                        t0+alpha_nr*p_nr, J0, b1, J2)*df(quadraticBezier(
                          t0+alpha_nr*p_nr, J0, b1, J2))) > -c2*p_nr*sum(
                            dquadraticBezier(t0, J0, b1, J2)*df(quadraticBezier(
                              t0, J0, b1, J2))) ){
                
                alpha_nr <- alpha_nr / 3 * 2
                if (alpha_nr < 1e-16) break
              }
              #print(alpha_nr)
            }
            if (tnew >= 1 & p_nr > 0) break
            tnew <- as.vector(t0 + p_nr * alpha_nr)
          }
        }
      }

      # if J2 is mimimum do not calculate extra step and jump to next iteration
      #Jold <- J0
      if (tnew >= 1 || tnew <= 0 || tb < 0) {
        if (f(J2) >= f(J0)) {
          message("Procedur terminated due to numerical accuracy! Verify result!")
          break
        } else {
          # prepare data for next loop
          w0 <- w2; J0 <- J2; N0 <- N2; gradfJ0 <- gradfJ2
          p0 <- p2; sol <- sol2; acc0 <- acc2
          angle <- rbind(angle, optDeg(gradfJ0, N0))
          Jtestinner <- NA
        }
      } else { # project Bezier optimum to Pareto frontier via problem solve
        # Add tangential hyperplane to PF approximation
        NORMALS   <- rbind(NORMALS, matrix(N2, nrow = 1))
        NJ        <- c(NJ, sum(N2 * J2))
        JJ        <- cbind(JJ, J2)
        PP        <- rbind(PP, p2)

        # solve worst-case problem for test point Jtestinner
        # solution gives Pareto optimal point on PF with max bound for w1a
        Jtestinner <- quadraticBezier(tnew, J0, b1, J2)
        wstd <- exp(mean(log(Jtestinner)))
        w1a <- wstd / Jtestinner
        scalet <- as.vector(w1a) * as.vector(dsc)
        A_init[1:kD, 2 * H + 1] <- -1 * scale_t / as.vector(scalet)
        w1wcsol <- wcSolquick(A_init, b_init, Q, H, ecos_control)
        Jw1 <- D %*% matrix(scalepar / w1wcsol) - d

        # update weight, scale problem data and solve
        # get solution n1(w1), objective vector J1(w1),
        # Lagrange multiplier lambda2(w2)normal vector N1(w1),
        # gradient of f in J1(w1), projection p1
        wstd <- exp(mean(log(Jw1)))
        w1 <- wstd / Jw1
        sol1 <-  wSolve(w1, dsc, A_init, b_init, D, d, df,
                        scalepar, scale_t, ecos_control)
        #n1      <- sol1$n
        J1      <- sol1$J
        #lambda1 <- sol1$lamda
        N1      <- sol1$normalJ
        gradfJ1 <- sol1$gradfJ
        p1      <- sol1$projection
        acc1    <- max(abs(Jw1 - J1))

        if (f(J1) < f(J0) & f(J1) < f(J2)) {
          # prepare data for next loop
          w0 <- w1; J0 <- J1; N0 <- N1; gradfJ0 <- gradfJ1
          p0 <- p1; sol <- sol1; acc0 <- acc1
          angle <- rbind(angle,optDeg(gradfJ0, N0))

        } else if (f(J2) <= f(J1)) {
          # prepare data for next loop
          w0 <- w2; J0 <- J2; N0 <- N2; gradfJ0 <- gradfJ2
          p0 <- p2; sol <- sol2; acc0 <- acc2
          angle <- rbind(angle, optDeg(gradfJ0, N0))
          Jtestinner <- NA

        } else {
          # add tangential hyperplane to PF approximation
          NORMALS   <- rbind(NORMALS, matrix(N1, nrow = 1))
          NJ        <- c(NJ, sum(N1 * J1))
          JJ        <- cbind(JJ, J1)
          PP        <- rbind(PP, p1)
          angle <- rbind(angle, optDeg(gradfJ1, N1))
        }
      }

      # conditions for convergence
      stop_crit <- 1 - min(N0 / gradfJ0) / max(N0 / gradfJ0)
      rloop <- stop_crit > opts$pm_tol & iter < opts$max_iters
      #step <- sqrt(sum((ITERATIONS[[iter]]$obj_z - f(J0))**2))

      # return results of iteration via text message
      if (opts$print_pm) {
        cat("k = ", iter,
            "; stop_crit = ", round(stop_crit, 4),
            "; angle =", round(angle[iter + 1], 4),
            #"; |y_tilde-a| = ", round(sqrt(sum((Jtestinner - J0)**2)), 8),
            #"; |b*-y^{k+1}| = ", round(sqrt(sum((pJ0alphap0 - J2)**2)), 8),
            #"; norm_p = ", round(sqrt(sum((p0)**2)),8),
            #"; step = ",step,"\n")
            "; ||J^k-J^{k-1}|| = ", sqrt(sum((ITERATIONS[[iter]]$sol$J-J0)**2)),
            "\n")
      }

      # store iteration
      ITERATIONS[[iter + 1]] <- list(iter = iter,
                                     w = as.vector(w0),
                                     sol = sol,
                                     obj_z = f(J0),
                                     Qbounds = sol$Qbound,
                                     Dbounds = sol$Dbound,
                                     ang = gradfJ0 / N0,
                                     stop_crit = stop_crit)
    }
    stop_nr <- which.min(unlist(sapply(ITERATIONS, function(X)X$stop_crit)))
    sol <- ITERATIONS[[stop_nr]]$sol

    if (kA == 0) {KA <- NULL} else {KA <- 1:kA}
    n <- do.call("rbind", lapply(ITERATIONS, function(X)X$sol$n))
    colnames(n) <- paste0("n", 1:H)
    rownames(n) <- paste0("iter", 1:length(ITERATIONS) - 1)
    rownames(angle) <- c(0, 1:iter)[1:length(angle)]
    colnames(angle) <- paste0("N ", intToUtf8(8738)," ",intToUtf8(8711),"f(J)")
    solz <- sol$solver$z * scale_t
    time_f2 <- proc.time()

    return(list(w = sol$w,
                n = sol$n,
                J = sol$J,
                Objective = f(sol$J),
                Utiopian = utopia,
                Normal = sol$lamda * sol$w,
                dfJ = sol$gradfJ,
                Sensitivity = list(D = solz[1:kD] / as.vector(
                                   sol$w) / as.vector(dsc),
                  A = solz[KA + kD] / a, # sensi. of pre. con.
                  C = solz[1:kC + kD + kA] / c, # / c
                  lbox = solz[1:H + kD + kA + kC] / l, # / l
                  ubox = solz[1:H + kD + kA + kC + H] / u), # / u
                Qbounds = sol$x[1:H] * sol$x[1:H + H],
                Dbounds = sol$w * sol$J,
                Scalingfactor = scalepar,
                Ecosolver = list(Ecoinfostring = sol$infostring,
                                 Ecoredcodes = sol$retcodes,
                                 Ecosummary = sol$summary),
                Timing = list(TotalTime = time_f2[3] - time_t0[3],
                              InnerTime = time_f2[3] - time_f1[3],
                              avgsolverTime = mean(unlist(lapply(ITERATIONS,
                                                                 function(X)X$sol$solver$timing[3])))),
                Iteration = list(angle = angle,
                                 ITERATIONS = ITERATIONS)))
  }
}