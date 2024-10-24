#' @docType package
#' @name spqdep-package
#' @rdname spqdep-package
#'
#' @title Testing for Spatial Dependence of Qualitative Data in Cross Section.
#'
#' @description
#'  \pkg{spqdep} offers the user a collection of functions to testing for spatial independence of categorical cross-section datasets.
#'
#' @details
#' Some functionalities that have been included in \pkg{spqdep} package are:
#'
#' @section 1. Test Q:
#' Testing for spatial dependence with the Q test based on the symbolic entropy. See references
#' @section 2. Test QMap:
#' Two statistics for the spatial analysis of qualitative data are obtained
#'  that are based on the symbolic entropy of the maps. See references
#' @section 3. Spatial runs test:
#' Testing for spatial dependence with the global test based on runs. See references
#' @section 4. Local spatial runs test:
#' Obtain the local test based on runs. See references
#' @section 5. Scan test:
#' Testing for spatial dependence with the Scan test. See references
#' @section 6. Join-count tests:
#' Testing for spatial dependence with the classical Join-count test. See references
#' @section 7. Similarity test:
#' Testing for spatial dependence with the Similarity test. See references
#'
#' @section Datasets: (example provinces_spain, FastFood.sf)
#'   \pkg{spqdep} includes two different datasets: provinces_spain and FastFood.sf. These
#'   sets are used to illustrate the capabilities of different functions.
#'   Briefly, their main characteristics are the following \cr
#'   \itemize{
#'     \item The \emph{FastFood.sf} A sf object with points of the localization of Fastfood restaurants in Toronto.
#'      \item The \emph{provinces_spain} An object of the class sf with the geometry of spanish provinces and several data sets.
#'    }
#'
#' @references
#'   \itemize{
#'     \item Ruiz M, López FA and A Páez (2011).
#'     Comparison of Thematic Maps Using Symbolic Entropy.
#'       \emph{International Journal of Geographical Information Science},  26, 413-439.
#'     \item Ruiz, M., López, F., and Páez, A. (2010).
#'     Testing for spatial association of qualitative data using symbolic dynamics.
#'       \emph{Journal of Geographical Systems}, 12(3), 281-309.0.
#'     \item Kulldorff M, Nagarwalla N. (1995).
#'     Spatial disease clusters: Detection and Inference.
#'       \emph{Statistics in Medicine}. 14:799-810
#'     \item Jung I, Kulldorff M, Richard OJ (2010).
#'     A spatial scan statistic for multinomial data.
#'       \emph{Statistics in Medicine}. 29(18), 1910-1918
#'      \item Páez, A, López-Hernández, FA, Ortega-García, JA, & Ruiz, M. (2016).
#'      Clustering and co-occurrence of cancer types: A comparison of techniques with an application to pediatric cancer in Murcia, Spain.
#'      \emph{Spatial Analysis in Health Geography}, 69-90.
#'   }
#'
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#'
#' @importFrom dplyr group_by rename select
#' @importFrom gtools combinations permutations
#' @importFrom ggplot2 ggplot geom_bar geom_errorbar
#' @importFrom ggplot2 geom_point geom_sf
#' @importFrom ggplot2 scale_fill_manual labs xlab
#' @importFrom ggplot2 aes element_text element_blank
#' @importFrom ggplot2 theme theme_bw
#' @importFrom graphics pairs par symbols title
#' @importFrom gridExtra grid.arrange
#' @importFrom igraph graph.adjacency layout.norm
#' @importFrom magrittr %>%
#' @importFrom Matrix solve
#' @importFrom methods as
#' @importFrom rsample bootstraps analysis
#' @importFrom sf st_coordinates st_distance
#' @importFrom sf st_geometry st_as_sf st_centroid
#' @importFrom sf st_read
#' @importFrom sp SpatialPoints
#' @importFrom spatialreg get.ZeroPolicyOption
#' @importFrom spdep knearneigh knn2nb poly2nb
#' @importFrom spdep mat2listw nb2listw nb2mat
#' @importFrom spdep listw2mat
#' @importFrom spdep joincount.multi joincount.test
#' @importFrom spdep joincount.mc
#' @importFrom stats quantile pnorm rnorm qnorm pchisq
#' @importFrom stats get_all_vars na.action model.frame
#' @importFrom stats addmargins sd var
NULL
