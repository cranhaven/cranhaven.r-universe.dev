#' fmdu-package {fmdu} R Documentation
#'
#' Multidimensional unfolding finds a spacial representation of proximities between two sets of objects.
#' The two sets, row objects and column objects, are represented as points in a lower-dimensional configuration.
#' The distances between the point of different sets correspond as closely as possible with the original proximities.
#'
#' It is possible to fix certain points to a pre-defined coordinate.
#' This restriction is called \emph{fixed coordinates}.
#' Fixed coordinates can be defined on row objects or on the column objects or on both sets of objects.
#'
#' The coordinates of either set can also be defined as a linear combination of independent variables,
#' for the row coordinates as $\mathbf{X} = \mathbf{Q}_x\mathbf{B}_x$
#' and for the column coordinates as $\mathbf{Y} = \mathbf{Q}_y\mathbf{B}_y$.
#' The independent variables $\mathbf{Q}$ are given and the regression coefficients $\mathbf{B}$ are estimated
#' after which the coordinates, either $\mathbf{X}$ or $\mathbf{Y}$, can be determined following the above equations.
#'
#' Each set can have only one restriction, either fixed coordinates or a model restriction.
#'
#' The functions follow algorithms given by de Leeuw and Heiser (1980), Heiser (1987), and Busing (2010),
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
#'             Heiser,W. J. (1987a). Joint ordination of species and sites: The unfolding technique.
#'             In P. Legendre and L. Legendre (Eds.), Developments in numerical ecology (pp. 189–221). 
#'             Berlin, Heidelberg: Springer-Verlag.
#'
#'             Busing, F.M.T.A. (2010). Advances in multidimensional unfolding.
#'             Unpublished doctoral dissertation, Leiden University, Leiden, the Netherlands.
#'
