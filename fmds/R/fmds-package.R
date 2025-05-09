#' fmds-package {fmds} R Documentation
#'
#' Multidimensional scaling finds a spacial representation of proximities between a set of objects.
#' The distances between the points correspond as closely as possible with the original proximities.
#'
#' It is possible to fix certain points to a pre-defined coordinate.
#' This restriction is called \emph{fixed coordinates}.
#'
#' The coordinates can also be defined as a linear combination of independent variables,
#' as $\mathbf{Z} = \mathbf{Q}\mathbf{B}$.
#' The independent variables $\mathbf{Q}$ are given and the regression coefficients $\mathbf{B}$ are estimated
#' after which the coordinates can be determined following the above equation.
#'
#' There can be only one restriction, either fixed coordinates or a model restriction.
#'
#' The function follows algorithms given by de Leeuw and Heiser (1980)
#' using iterative majorization and alternating least squares.
#' The final function value, normalized Stress, is the square of Stress-I,
#' the loss function that started it all (Kruskal, 1964).
#'
#' @references Kruskal, J.B. (1964a). Multidimensional scaling by optimizing goodness-of-fit to a nonmetric hypothesis. 
#'             Psychometrika, 29, 1–27.
#'
#'             Kruskal, J. B. (1964b). Nonmetric multidimensional scaling: A numerical method.
#'             Psychometrika, 29, 115–129.
#'
#'             de Leeuw, J., and Heiser, W. J. (1980). Multidimensional scaling with restrictions on the configuration. 
#'             In P.R. Krishnaiah (Ed.), Multivariate analysis (Vol. 5, pp. 501–522).
#'             Amsterdam, The Netherlands: North-Holland Publishing Company.
#'
#' @author Frank M.T.A. Busing
