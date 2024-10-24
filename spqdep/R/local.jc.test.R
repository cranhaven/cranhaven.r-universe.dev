#'
#' @title A function to calculate the local Join Count tests.
#'
#' @description This function calculates the local Join Count tests.This function
#'   is a wrapper of \code{\link[rgeoda]{local_joincount}} in \pkg{rgeoda} package.
#' @param formula An (optional) formula with the factor included in \code{data}
#' @param data An (optional) data frame or a sf object containing the variable to testing for.
#' @param fx An (optional) factor of observations with the same length as the neighbours list in \code{listw}
#' @param listw A neighbourhood list (an object type knn, listw or nb).
#' @param coor (optional) a 2xN vector with spatial coordinates.
#'             Used when *data* is not a spatial object.
#' @param case level of the factor used to obtain the local Join Count test. See details.
#' @param nsim The number of permutation to obtain the local Join Count tests. Default value 999.
#' @param control Optional argument. See Control argument section.
#' @usage local.jc.test(formula = NULL, data = NULL, fx = NULL, case = NULL, coor = NULL,
#'  listw = listw, nsim = 999, control = list())
#' @details This test develop by Anselin and Li (2019) can be apply only for binary variables
#' (usually named B and W), and count the number of joins of type B-B where the proportion of
#'  observations with B is lower than half. The interest lies in identifying co-occurrences of
#' uncommon events, i.e., situations where observations that take on the value of B constitute
#' much less than half of the sample. \cr
#' The statistic is defined as:\cr
#' \deqn{BB_i = x_i  \sum_j{w_{ij}x_j}}\cr
#' only meaningful for those observations where \eqn{x_i=1}, since for \eqn{x_i=0} the result will always equal zero.\cr
#' \cr
#' The object \code{listw} can be the class:
#' \itemize{
#'     \item \code{knn}: Objects of the class knn that consider the neighbours in
#'     order of proximity.
#'     \item \code{nb}: If the neighbours are obtained from an sf object, the code internally
#'     will call the function \code{\link{nb2nb_order}} it will order them in order
#'     of proximity of the centroids.
#'     \item \code{matrix}: If a object of matrix class based in the inverse of
#'     the distance in introduced as argument, the function \code{\link{nb2nb_order}} will
#'     also be called internally to transform the object the class matrix to a matrix of the
#'      class nb with ordered neighbours.
#'     }
#' @return The output is an object of the class localjc \cr
#' \cr
#'   \code{local.SRQ} A matrix with \cr
#'   \tabular{ll}{
#'     \code{nn} \tab number of neighbourhood in the localization 'i'. \cr
#'     \code{ljc} \tab local Join Count statistics. \cr
#'     \code{pseudo.value} \tab p-value of local jc tests. \cr
#'     }
#'
#' @section Control arguments:
#'   \tabular{ll}{
#'     \code{seedinit} \tab Numerical value for the seed in boot version. Default value seedinit = 123 \cr
#'       }
#'
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Anselin, L., and Li, X. (2019).
#'     Operational local join Count statistics for cluster detection.
#'       \emph{Journal of Geographical Systems, 21(2), 189-210}.
#'   }
#'
#' @seealso
#' \code{\link{local.sp.runs.test}}, \code{\link{dgp.spq}}
#'
#' @export
#'
#' @examples
#'
#' # Case 1: Local spatial runs test based on knn
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' coor <- cbind(cx,cy)
#' listw <- spdep::knearneigh(coor, k = 4)
#' p <- c(1/2,1/2)
#' rho <- 0.5
#' fx <- dgp.spq(p = p, listw = listw, rho = rho)
#' ljc <- local.jc.test(fx = fx, coor = coor, case= "A", listw = listw)
#' print(ljc)
#' plot(ljc, coor = coor, sig = 0.1)
#'
#' \donttest{
#' # Case 2:Fastfood example. sf (points)
#' data("FastFood.sf")
#' # sf::sf_use_s2(FALSE)
#' x <- sf::st_coordinates(sf::st_centroid(FastFood.sf))
#' listw <- spdep::knearneigh(x, k = 6)
#' formula <- ~ Type
#' ljc <- local.jc.test(formula = formula, data = FastFood.sf, case = "H",listw = listw)
#' print(ljc)
#' plot(ljc, sf = FastFood.sf, sig = 0.05)
#'
#' # Case 3: With a sf object (poligons)
#' fname <- system.file("shape/nc.shp", package="sf")
#' nc <- sf::st_read(fname)
#' listw <- spdep::poly2nb(as(nc,"Spatial"), queen = FALSE)
#' p <- c(1/3,2/3)
#' rho = 0.5
#' nc$fx <- dgp.spq(p = p, listw = listw, rho = rho)
#' plot(nc["fx"])
#' formula <- ~ fx
#' ljc <- local.jc.test(formula = formula, data = nc, case = "A", listw = listw)
#' ljc
#' plot(ljc, sf = nc)
#'
#' # Case 4: With isolated areas
#' data(provinces_spain)
#' # sf::sf_use_s2(FALSE)
#' listw <- spdep::poly2nb(as(provinces_spain,"Spatial"), queen = FALSE)
#' provinces_spain$Mal2Fml<- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' formula <- ~ Mal2Fml
#' ljc <- local.jc.test(formula = formula, data = provinces_spain, listw = listw)
#' print(ljc)
#' plot(ljc, sf = provinces_spain, sig = 0.1)
#'
#' # Case 5: Regular lattice
#' data(Boots.sf)
#' listw <- spdep::poly2nb(as(Boots.sf,"Spatial"), queen = TRUE)
#' formula <- ~ BW
#' ljc <- local.jc.test(formula = formula, data = Boots.sf, case="B", listw = listw)
#' print(ljc)
#' plot(ljc, sf = Boots.sf, sig = 0.05)
#' lsr <- local.sp.runs.test(formula = formula, data = Boots.sf,
#' distr = "bootstrap", nsim= 99, listw = listw)
#' print(lsr)
#' plot(lsr, sf = Boots.sf, sig = 0.1)
#' scan <- scan.test(formula = formula, data = Boots.sf, nsim = 299,
#' dist = "bernoulli", nv = 60 , case = "B", windows = "elliptic")
#' plot(scan, sf = Boots.sf)
#' }

local.jc.test <- function(formula = NULL, data = NULL, fx = NULL,case = NULL,
                          coor = NULL, listw = listw, nsim = 999,
                          control = list()){

  con <- list(nsim = 999, seedinit = 1111)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control

  # Only knn, nb, or matrix objects
  if (!inherits(listw, "knn")){
    if (!inherits(listw, "nb")){
      if (!inherits(listw, "matrix")){
        stop ("listw must be is an object of the class: knn, nb or matrix")
      }
    }
  }
  ## Define the W matrix like a rgeoda object
  if (inherits(listw, "knn")){
    listw <- nb2listw(knn2nb(listw))
  } else if (inherits(listw, "matrix")){
    listw <- (listw > 0)*1
    listw <- mat2listw(listw)
  } else if (inherits(listw, "nb")){
    listw <- nb2listw(listw,zero.policy = TRUE)
  }
  ### Building the W matrix
  N <- as.numeric(length(listw$neighbours))
  W <- rgeoda::create_weights(N)
  for (i in 1:N){
    rgeoda::set_neighbors(W, i, as.numeric(listw$neighbours[[i]]))
  }
  rgeoda::update_weights(W)
  ## The local jc must identify the case (=1) vesus no-case  (=0)
  if (is.null(fx) == FALSE){
  if (inherits(fx, "factor")){
    fx <- (fx == case)*1
  }
  }
  ##
  if (!is.null(formula) && !is.null(data)){
    if (inherits(data, "Spatial")) data <- as(data, "sf")
    mfx <- model.frame(formula, data)
  } else if (!is.null(fx) && !is.null(coor)){
    mfx <- fx
    if (!is.matrix(mfx) && !is.data.frame(mfx)) mfx <- as.matrix(mfx, ncol = 1)
    mfx <- as.data.frame(mfx)
    if (is.matrix(coor)) coor <- sp::SpatialPoints(coor)
    if (inherits(coor, "Spatial")) coor <- as(coor, "sf")
    data <- coor #sf object
  } else stop("input data wrong")

## Wrapper the local_joincount function from rgeoda
  ljc <- rgeoda::local_joincount(W, mfx, permutations = nsim)
  local <- list(ljc = data.frame(nn = ljc$nn_vals,
                ljc = ljc$lisa_vals,
                pseudo.value = ljc$p_vals),
                nsim         = nsim)
  class(local) <- "localjc"
  return <- local
}
