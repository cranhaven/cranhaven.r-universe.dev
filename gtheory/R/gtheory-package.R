#' \code{gtheory} provides functions for estimating variance components, generalizability coefficients, universe scores, and standard errors when observed scores contain variation from one or more measurement facets (e.g., items and raters).
#'
#' \code{gtheory} was designed to apply univariate and multivariate generalizability (G).  Use it to conduct G and decision (D) studies with balanced or unbalanced data.  Future releases will provide the means for calculating universe scores under univariate G theory or within strata, as well as universe-score profiles (a.k.a. augmented true scores) and composite scores under multivariate G theory.  Functions to find an optimal study design and to calculate conditional error variance are also in the works.
#'
#' @name gtheory-package
#' @aliases gtheory
#' @docType package
#' @title Apply generalizability theory with R.
#' @author Christopher T. Moore \email{moor0554@@umn.edu}
#' @seealso \url{http://EvaluationDashboard.com}
#' @keywords package
#' @importFrom stats as.formula median model.matrix update var
#' @importFrom lme4 lmer VarCorr
#' 
NULL
#' Brennan's (2001) Table 3.2
#'
#' Brennan's (2001) table 3.2 (synthetic data set number 4) contains item scores from a person * (rater : task) generalizability (G) study.  Note that the data are in long format (one item score per row) and the facets are factors.
#'
#' @param Brennan.3.2 data frame in long format with a column for item scores and columns for sources of variance
#' @name Brennan.3.2
#' @docType data
#' @keywords data
#' @source Brennan, R. L. (2001). \emph{Generalizability theory}. New York: Springer.
#' @examples
#' data(Brennan.3.2)
#' head(Brennan.3.2)
#' sapply(Brennan.3.2, class)
#' 
NULL
#' Rajaratnam, Cronbach and Gleser's (1965) Table 2
#'
#' Rajaratnam, Cronbach and Gleser's (1965) table 2 contains item scores from a person * (item : subscale) multivariate generalizability (G) study.  The same data are found in Brennan (2001) as table 9.1.  Note that the data are in long format (one item score per row) and the facets are factors.
#'
#' @param Rajaratnam.2 data frame in long format with a column for item scores and columns for sources of variance
#' @name Rajaratnam.2
#' @docType data
#' @keywords data
#' @source Rajaratnam, N., Cronbach, L. J., & Gleser, G. C. (1965). Generalizability of stratified-parallel tests. \emph{Psychometrika}, \emph{30}(1), 39-56.
#' @references Brennan, R. L. (2001). \emph{Generalizability theory}. New York: Springer.
#' @examples
#' data(Rajaratnam.2)
#' head(Rajaratnam.2)
#' sapply(Rajaratnam.2, class)
#' 
NULL