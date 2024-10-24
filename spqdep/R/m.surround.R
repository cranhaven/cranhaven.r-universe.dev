#'
#' @title A function to generate m-surroundings
#' @usage m.surround(x, m, r = 1, distance = "Euclidean", control = list())
#' @param x input sf object with points/multipolygons geometry or matrix
#'          of spatial coordinates
#' @param m dimension of m-surrounding
#' @param r maximum overlapping between any two m-surroundings.
#' @param distance character. For Cartesian coordinates only: one of Euclidean,
#' Hausdorff or Frechet; for geodetic coordinates, Great Circle distances are
#' computed. (see \code{sf::st_distance()}).
#' Default = "Euclidean".
#' @param control List of additional control arguments.
#'
#' @description This function obtains the m-surroundings
#'   by selecting the *m-1* nearest neighbors
#'   of each observation, allowing for a degree of
#'   overlap of *r*.
#'
#' @return A list of class \code{list} and \code{m_surr} containing the following components:\cr
#'   \tabular{ll}{
#'     \code{ms} \tab a matrix. Each row is a m-surrounding. \cr
#'     \code{R} \tab total number of observations. \cr
#'     \code{rowexcl} \tab index of rows excluded. \cr
#'     \code{mdtms} \tab distances between the observations of
#'       each m-surrounding.\cr
#'     \code{N} \tab total number of symbolized observations.\cr
#'     \code{m} \tab length of the m-surroundings.\cr
#'     \code{r} \tab overlapping degree.\cr
#'     \code{initobs} \tab element to start the generation of m-surroundings.\cr
#'     \code{distance} \tab type of distance.\cr
#'     \code{m} \tab length of the m-surroundings.\cr
#'     \code{x} \tab the input "x" as sf-object.\cr
#'     }
#'
#' @section Control arguments:
#'   \itemize{
#'     \item \code{initobs}: Initial observation to begin the
#'       m-surrounding process. Default = 1.
#'     \item \code{dtmaxabs}: Threshold of distance (in absolute
#'       value) to prune the m-surroundings. Any m-surrounding
#'       exceeding the threshold is excluded.
#'       If \code{dtmaxabs} = 0 there is no exclusion of m-surroundings.
#'       Default = 0.
#'     \item  \code{dtmaxpc}: Threshold of distance (as a percentage
#'       of the maximum distance between observations) to prune the m-surroundings. Any m-surrounding
#'       exceeding the threshold is excluded.
#'       Example if \code{dtmaxpc} = 0.1 the m-surrounding exceeding
#'       the 10% of the maximum distance between observations are excluded.
#'       If \code{dtmaxpc} = 0 there is no exclusion of m-surroundings.
#'       Default = 0.
#'     \item  \code{dtmaxknn}: Eliminate m-surroundings where some
#'       of the elements are not among the closest
#'       knn (k-nearest-neighbors).
#'       Example, if \code{dtmaxknn} = 4 exclude m-surroundings
#'       where some of the elements are not between the 4 closest.
#'       Default \code{dtmaxknn} = 0 (no exclusion)
#'    }
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
#'     \item Ruiz M, López FA, A Páez. (2010). Testing for spatial association of qualitative
#'     data using symbolic dynamics. \emph{Journal of Geographical Systems}. 12 (3) 281-309
#'   }
#' @export
#' @examples
#'
#' # Example 1: Obtain m-surroundings with degree of overlapping r
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' x <- cbind(cx,cy)
#' m <- 3
#' r <- 1
#' msurr_points <- m.surround(x = x, m = m, r = r)
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#' summary(msurr_points)
#' msurr_points <- m.surround(x = x, m = m, r = r,
#'                 control = list(dtmaxpc = 0.1))
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#' summary(msurr_points)
#' msurr_points <- m.surround(x = x, m = m, r = r,
#'                    control = list(dtmaxknn = 20))
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#' summary(msurr_points)
#'
#' # Example 2:
#' \donttest{
#' data("FastFood.sf")
#' m <- 3
#' r <- 1
#' msurr_points <- m.surround(x = FastFood.sf, m = m, r = r,
#'                            distance = "Euclidean",
#'                            control = list(dtmaxpc = .001))
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#' print(msurr_points)
#' summary(msurr_points)
#' msurr_points <- m.surround(x = FastFood.sf, m = m, r = r,
#'                            distance = "Euclidean",
#'                            control = list(dtmaxknn = 20))
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#' summary(msurr_points)
#' }
#'
#' # Example 3: With isolated areas
#' data(provinces_spain)
#' # sf::sf_use_s2(FALSE)
#' plot(sf::st_geometry(provinces_spain))
#' m <- 3
#' r <- 1
#' msurr_points <- m.surround(x = provinces_spain, m = m, r = r,
#'                            distance = "Euclidean",
#'                            control = list(dtmaxknn = 8))
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#'
#' # Example 4: Examples with multipolygons
#' fname <- system.file("shape/nc.shp", package="sf")
#' nc <- sf::st_read(fname)
#' plot(sf::st_geometry(nc))
#' m <- 3
#' r <- 1
#' msurr_polygonsf <- m.surround(x = nc, m = m, r = r,
#'                    distance = "Great Circle",
#'                    control=list(dtmaxpc = 0.20))
#' plot(msurr_polygonsf, type = 1)
#' plot(msurr_polygonsf, type = 2)
#'
#' # Example 5: With regular lattice
#' sfc = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))
#' hexs <- sf::st_make_grid(sfc, cellsize = 0.1, square = FALSE)
#' hexs.sf <- sf::st_sf(hexs)
#' listw  <- spdep::poly2nb(as(hexs.sf, "Spatial"), queen = FALSE)
#' m <- 3
#' r <- 1
#' msurr_polygonsf <- m.surround(x = hexs.sf, m = m, r = r)
#' plot(msurr_polygonsf, type = 1)
#' plot(msurr_polygonsf, type = 2)
#' summary(msurr_polygonsf)

m.surround <- function(x , m, r = 1, distance = "Euclidean", control = list()) {
  con <- list(initobs = 1, dtmaxabs = 0, dtmaxpc = 0, dtmaxknn = 0)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  if (sum((con$dtmaxabs>0) + (con$dtmaxpc>0) + (con$dtmaxknn>0))>1){
    stop("Include only a restriction to create m-surrounding")
  }

  if (!is.null(r)) {
    if (r < 1 || r > (m-1))
      warning("overlapping degree must be between 1 and m-1")
  }

  # Transform matrix coordinates into SpatialPoints class
  if (is.matrix(x)) x <- sp::SpatialPoints(x)
  # Transform Spatial classes into sf class
  if (inherits(x, "Spatial")) x <- as(x, "sf")
  # Compute centroids/coordinates/distances from sf objects
  if (inherits(x, "sf")) {
  xct <- suppressWarnings(st_centroid(st_geometry(x)))
  } else stop("object must be either sf, sp or matrix class")
  N <- length(xct)
  if (!is.null(r)) R <- trunc((N - m)/(m - r)) + 1 else R <- N

  if (inherits(st_geometry(x)[1],
               "sfc_MULTIPOLYGON"))
    mcoor <- st_coordinates(xct)
  if (inherits(st_geometry(x)[1],
               "sfc_POINT"))
    mcoor <- st_coordinates(x)
  mcoor <- st_coordinates(xct)
  rownames(mcoor) <- as.character(1:N)
  mdtfull <- st_distance(xct, which = distance)
  # full distance matrix
  rownames(mdtfull) <- colnames(mdtfull) <- as.character(1:N)

  ms <- m_surr_no(mdtfull = mdtfull, m = m, r = r,
                  initobs = con$initobs, control_knn = con$dtmaxknn, coor = mcoor)
  mdtms <- NULL
  for (i in seq_along(1:nrow(ms))) {
    rowds <- mdtfull[ms[i,],ms[i,]]
    rowds <- rowds[1,]
    mdtms <- rbind(mdtms, rowds)
  }
  colnames(mdtms) <- NULL
  rownames(mdtms) <- ms[, 1]
  # ms <- matrix(NA, R, m)
  # mdtms <- matrix(NA, R, m) # m-surrounding distance matrix
  # rownames(ms) <- rownames(mdtms) <- 1:R
  # set.seed(con$seedinit)
  # Si <- as.character(1:N)
  # s0i <- as.character(sample(1:N, 1)) # initial s0
  # mdti <- mdtfull
  # if (!is.null(r)) {
  #   for (i in 1:R) {
  #     vdsts0i <- mdti[s0i,] # Vector of distances to s0i
  #     vidxnbs0i <- sort(vdsts0i, decreasing = FALSE, index.return = TRUE)
  #     # Distances to s0i ordered
  #     if (is.list(vidxnbs0i)) vnbs0i <- names(vidxnbs0i$x[2:m])
  #     if (inherits(vidxnbs0i, "units")) vnbs0i <- names(vidxnbs0i[2:m])
  #     # Set of (m-1) neighbors to s0i (excluding itself)
  #     ms[i,] <- c(s0i, vnbs0i) # m-surrounding ith
  #     mdtms[i,] <- vdsts0i[c(s0i,vnbs0i)] # distance vector of ms_i
  #     if ((m-r-1) > 0) Ai <- c(s0i,vnbs0i[1:(m-r-1)]) else Ai <- c(s0i)
  #     #    if ((m-r-2) > 0) Ai <- c(s0i,vnbs0i[1:(m-r-2)]) else Ai <- c(s0i)
  #     Si <- Si[!(Si %in% Ai)]
  #     mdti <- mdti[Si, Si]
  #     s0i <- vnbs0i[m-r]
  #   }
  # } else { # Bootstrap resampling
  #   ms <- cbind(1:N, knearneigh(mcoor, k=(m-1))$nn)
  #
  #
  # }
  if (is.null(con$dtmaxabs)) dtmaxabs <- 0 else dtmaxabs <- con$dtmaxabs
  if (is.null(con$dtmaxpc)) dtmaxpc <- 0 else dtmaxpc <- con$dtmaxpc
  if (is.null(con$dtmaxknn)) dtmaxknn <- 0 else dtmaxknn <- con$dtmaxknn

  lms <- prunemsdthr(dtmaxabs = dtmaxabs, dtmaxpc = dtmaxpc,
                     mdtfull = mdtfull, ms = ms,
                     mdtms = mdtms)
  lms$N <- N
  lms$m <- m
  lms$r <- r
  lms$initobs <- con$initobs
  lms$distance <- distance
  lms$x <- x
  class(lms) <- c("m_surr","list")
  return(lms)
}
  # nnlist <- matrix(0, N, m - 1)  # Matrix with list of nearest neighbors
  # nn1 <- cbind(1:N, mdtfull[1,], mcoor)
  # #nn1 <- cbind(1:N, sqrt((mcoor[1,1] - mcoor[,1])^2 +
  # #                          (mcoor[1,2] - mcoor[,2])^2), mcoor)
  # nn1 <- nn1[order(nn1[,2]), ]
  # nnlist[1, ] <- nn1[2:m, 1]
  # ns <- trunc((N - m)/(m - r)) + 1
  # list <- rep(0, ns)  #zeros(1,ns)
  # list[1] = 1
  # blacklist <- c(1, nnlist[1, 1:(m - (r + 1))])
  # t <- 1
  # for (v in 2:ns) {
  #   list[v] = nnlist[t, m - r]
  #   h <- list[v]
  #   nn1 <- nn1[!nn1[,1] %in% blacklist,]
  #   #**VIP**: SALE DISTINTO PERO DEBERÍA DAR IGUAL... CONSULTAR CON
  #   # FERNANDO
  #   #nn1[,2] <- mdtfull[nn1[1,1],nn1[,1]]
  #   nn1[,2] <-  sqrt((nn1[1,3] - nn1[,3])^2 + (nn1[1,4] - nn1[,4])^2)
  #   nn1 <- nn1[order(nn1[,2]),]
  #   nnlist[h,] <- nn1[2:(m),1]
  #   t = h
  #   blacklist <- c(blacklist, h, nnlist[h, 1:(m - (r + 1))])
  # }
  # nnlist <- cbind(1:N, nnlist)
  # nnlist1 <- cbind(nnlist[which(!nnlist[,2] == 0),])
  # # control debería ser una lista
  # # Se eliminan aquellas m-historias que contengan vecinos que no estén
  # # dentro de los k vecinos más próximos
  # if (!is.null(control)){
  #   knn <- cbind(1:N, knearneigh(mcoor, control)$nn)
  #   int <- numeric()
  #   for (i in 1:dim(nnlist1)[1]){
  #     int[i] <- length(intersect(nnlist1[i,],knn[nnlist1[i,1],]))
  #   }
  #   nnlist1 <- nnlist1[int==m,]
  # }
  #return(nnlist1)
#}
