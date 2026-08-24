

#' Correlation matrix for 61 landmarks from Japanese macaque 
#'   (\emph{Macaca fuscata}) craniums.
#'
#' 3D coordinates for 61 landmarks on the crania of 42 juvenile Japanese macaque 
#'   (\emph{Macaca fuscata}) from the Primate Research Institute at Inuyama, 
#'   Japan, was aligned and scaled with Generalized Procrustes superimposition. 
#'   The vector congruence coefficient correlation was then calculated for each pair
#'   of landmarks 
#'
#'#'@seealso \code{\link{macacaModels}}
#'
#' @format A 61 x 61 matrix
#' @source Goswami, A. and P. D. Polly. 2010. Methods for studying morphological integration and
#'    modularity. Pp. 213-243 in J. Alroy, and E. G. Hunt, eds. Quantitative Methods in
#'    Paleobiology. Paleontological Society Special Publications.

"macacaCorrel"



#' Models of (\emph{Macaca fuscata}) cranial modularity.
#'
#' Seven models of cranial landmark modularity. These models group the 61
#'   landmarks in \code{\link{macacaCorrel}} into modules. See  Goswami and 
#'   Finarelli (2016) for more details.
#'
#'@seealso \code{\link{macacaCorrel}}
#'
#' @format A data frame with 61 rows (one for each landmark) and 8 coluumns. 
#' \itemize{
#'  \item{X.1}{Landmark names (character or factor)}
#'  \item{Other columns}{Model specification. Integers determine which module each landmark is 
#'    in. NAs indicate that a landmark is not in any module.}
#' }
#' @source  A. Goswami and J. Finarelli (2016) EMMLi: A maximum likelihood approach 
#'    to the analysis of modularity.
#'    Evolution \url{http://onlinelibrary.wiley.com/doi/10.1111/evo.12956/abstract}.
"macacaModels"
