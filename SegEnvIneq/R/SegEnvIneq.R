library(OasisR)
library(spdep)
library(outliers)
library(sf)


################################


#' A function to compute environmental dissimilarity index
#'
#' @usage EDfunc (x, a, vers = "standard", w = NULL, ar = NULL, per = NULL,
#' b = NULL, folder = NULL, shape = NULL, spatobj = NULL, queen = TRUE,
#' ptype = "int", K = 1, f = "exp", beta = 1)
#' @param x - a vector of the population/group distribution across spatial units
#' @param a - a vector of the environmental variable spatial distribution
#' @param vers - the index version:
#' "standard" (by default) for aspatial environmental dissimilarity index (Duncan);
#' "contig" for adjusted index with a contiguity spatial interactions matrix (Morrill);
#' "bound" for adjusted index with a boundaries spatial interactions matrix (Wong);
#' "shape" for adjusted index with a boundaries and shape spatial interactions matrix (Wong);
#' "user" for adjusted index with any user spatial interactions matrix
#' @param w - an optional spatial weights matrix.
#' If necessary and not provided, it will be computed in the function
#' @param ar - an optional vector of spatial units area.
#' If necessary and not provided, it will be computed in the function
#' @param b - an optional shared border matrix.
#' If necessary and not provided, it will be computed in the function.
#' @param per - an optional vector of spatial units perimeter.
#' If necessary and not provided, it will be computed in the function.
#' @param folder - a character vector with the folder (directory) name
#' indicating where the shapefile with geographical info is located on the drive
#' if the interactions matrix is computed in the function
#' @param shape - a character vector with the name of the shapefile
#' (without the .shp extension)
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with
#' geographic information as alternative for the shapefile,
#' if the interactions matrix is computed in the function
#' @param queen - logical parameter defining criteria used for contiguity matrix computation,
#' TRUE for queen (by default), FALSE  for rook
#' @param ptype - a string variable giving two options for perimeter calculation for  Wong's indices:
#' "int" to use only interior borders of spatial units and
#' "all" to use entire borders, including to the exterior of the area
#' @param K - the order of contiguity matrix if "contig" version is chosen (K = 1 by default)
#' @param f - spatial decay function of contiguity matrix when K > 1, with
#' f = "exp" (by default) for exponential function of contiguity "distance" exp(beta*(1-k))
#  and f = "rec" for 1/(k^beta)
#' @param beta - spatial decay intensity parameter (equal to 1 by default),
#' used only when the version with contiguity is chosen and K > 1
#' @return The value of the environmental dissimilarity index
#' @references Schaeffer Y. and Tivadar M. (2019)
#' Measuring Environmental Inequalities: Insights from the Residential
#' Segregation Literature. \emph{Ecological Economics}, 164, 106329
#' @references Tivadar M. (2019)
#' OasisR: An R Package to Bring Some Order to the World of Segregation Measurement.
#' \emph{Journal of Statistical Software},  89 (7), pp. 1-39
#' @references Duncan O. D. and Duncan B. (1955)
#' Residential Distribution and Occupational Stratification.
#' \emph{American Journal of Sociology}, 60 (5), pp. 493-503
#' @references Morrill B. (1991) On the measure of geographic
#' segregation. \emph{Geography research forum}, 11, pp. 25-36.
#' @references Wong D. W. S. (1998) Measuring multiethnic spatial
#' segregation. \emph{Urban Geography}, 19 (1), pp. 77-87.
#' @description Environmental Dissimilarity index measures
#' the dissimilarity between the distribution of a population
#' group \emph{x} and the one of an environmental (dis-)amenity \emph{a}
#' among spatial units. The environmental dissimilarity index has several versions:
#' "standard" aspatial version based on Duncan & Duncan (1955) segregation index;
#' adjusted versions with spatial interactions matrices based on contiguities
#' (Morrill, 1991; Tivadar, 2019), boundaries, or shapes (Wong, 1998; Tivadar, 2019);
#' or defined by the user.
#' @examples data(segdata, package = "OasisR")
#' # segdata - theoretical distributions on a 10x10 grid map
#' # We consider A1 - population distribution and A2 - amenity distribution
#' EDfunc (segdata@data$A1, segdata@data$A2)
#' EDfunc (segdata@data$A1, segdata@data$A2, vers = "contig", spatobj =segdata, queen = FALSE)
#' EDfunc (segdata@data$A1, segdata@data$A2, vers = "contig", spatobj =segdata, queen = FALSE, K = 3)
#' EDfunc (segdata@data$A1, segdata@data$A2, vers = "bound", spatobj =segdata)
#' EDfunc (segdata@data$A1, segdata@data$A2, vers = "shape", spatobj =segdata, ptype = 'all')
#' @seealso \code{\link{ECfunc}}, \code{\link{EnvResampleTest}},
#' \code{\link{EnvResamplePlot}}
#' @export



EDfunc <- function (x, a, vers = "standard", w = NULL, ar = NULL, per = NULL,
                    b = NULL, folder = NULL, shape = NULL, spatobj = NULL, queen = TRUE,
                    ptype = "int", K = 1, f = "exp", beta = 1)
{
  if (vers == "standard")
    return(0.5 * sum(abs((x/sum(x)) - (a/sum(a)))))
  if ((K == 1 & vers == "contig") | vers == "user") {
    if (is.null(w))
      w <- OasisR::contig(spatobj = spatobj, folder = folder, shape = shape, queen = queen)
    xsmat <- matrix(rep(t(x), length(x)), nrow = length(x),
                    byrow = FALSE)/(matrix(rep(t(x), length(x)), nrow = length(x),
                                           byrow = TRUE) + matrix(rep(t(x), length(x)), nrow = length(x),
                                                                  byrow = FALSE))
    asmat <- matrix(rep(t(a), length(a)), nrow = length(a),
                    byrow = FALSE)/(matrix(rep(t(a), length(a)), nrow = length(a),
                                           byrow = TRUE) + matrix(rep(t(a), length(a)), nrow = length(a),
                                                                  byrow = FALSE))
    smat <- w * abs(xsmat - asmat)
    smat[is.na(smat)] <- 0
    return((0.5 * sum(abs((x/sum(x)) - (a/sum(a))))) - sum(smat)/sum(w))
  }
  if (vers == "bound") {
    if (is.null(b))
      b <- OasisR::boundaries(spatobj = spatobj, folder = folder, shape = shape)
    if (is.null(w)) w <- b
    xsmat <- matrix(rep(t(x), length(x)), nrow = length(x),
                    byrow = FALSE)/(matrix(rep(t(x), length(x)), nrow = length(x),
                                           byrow = TRUE) + matrix(rep(t(x), length(x)), nrow = length(x),
                                                                  byrow = FALSE))
    asmat <- matrix(rep(t(a), length(a)), nrow = length(a),
                    byrow = FALSE)/(matrix(rep(t(a), length(a)), nrow = length(a),
                                           byrow = TRUE) + matrix(rep(t(a), length(a)), nrow = length(a),
                                                                  byrow = FALSE))
    smat <- w * abs(xsmat - asmat)
    smat[is.na(smat)] <- 0
    return((0.5 * sum(abs((x/sum(x)) - (a/sum(a))))) - sum(smat)/sum(b))
  }
  if (vers == "shape") {
    if (is.null(b))
      b <- OasisR::boundaries(spatobj = spatobj, folder = folder,
                              shape = shape)
    if (is.null(ar))
      ar <- OasisR::area(spatobj = spatobj, folder = folder,
                         shape = shape)
    if (is.null(per)) {
      if (ptype == "all")
        per <- OasisR::perimeter(spatobj = spatobj,
                                 folder = folder, shape = shape)
      if (ptype == "int")
        per <- rowSums(b)
    }
    w <- b * (matrix(rep(t(per/ar), length(x)), nrow = length(x),
                     byrow = TRUE) + matrix(rep(t(per/ar), length(x)),
                                            nrow = length(x), byrow = FALSE))/(2 * max(per/ar))
    xsmat <- matrix(rep(t(x), length(x)), nrow = length(x),
                    byrow = FALSE)/(matrix(rep(t(x), length(x)), nrow = length(x),
                                           byrow = TRUE) + matrix(rep(t(x), length(x)), nrow = length(x),
                                                                  byrow = FALSE))
    asmat <- matrix(rep(t(a), length(a)), nrow = length(a),
                    byrow = FALSE)/(matrix(rep(t(a), length(a)), nrow = length(a),
                                           byrow = TRUE) + matrix(rep(t(a), length(a)), nrow = length(a),
                                                                  byrow = FALSE))
    smat <- w * abs(xsmat - asmat)
    smat[is.na(smat)] <- 0
    return((0.5 * sum(abs((x/sum(x)) - (a/sum(a))))) - sum(smat)/sum(b))
  }
  if (K > 1 & vers == "contig") {
    if (is.null(w)) {
      if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape) else spatobj <- sf::st_as_sf(spatobj)
      ngb <- spdep::poly2nb(spatobj, queen = queen)
      ngbk <- spdep::nblag(ngb, K)
      w <- vector("list", K)
      for (k in 1:K) w[[k]] <- spdep::nb2mat(ngbk[[k]],
                                             style = "B", zero.policy = TRUE)
    }
    result <- (0.5 * sum(abs((x/sum(x)) - (a/sum(a)))))
    k <- 1
    cond <- TRUE
    while (cond) {
      if (f == "exp")
        interact <- exp(beta * (1 - k))
      if (f == "rec")
        interact <- 1/(k^beta)
      xsmat <- matrix(rep(t(x), length(x)), nrow = length(x),
                      byrow = FALSE)/(matrix(rep(t(x), length(x)),
                                             nrow = length(x), byrow = TRUE) + matrix(rep(t(x),
                                                                                          length(x)), nrow = length(x), byrow = FALSE))
      asmat <- matrix(rep(t(a), length(a)), nrow = length(a),
                      byrow = FALSE)/(matrix(rep(t(a), length(a)),
                                             nrow = length(a), byrow = TRUE) + matrix(rep(t(a),
                                                                                          length(a)), nrow = length(a), byrow = FALSE))
      smat <- w[[k]] * abs(xsmat - asmat)
      smat[is.na(smat)] <- 0
      result <- result - interact * sum(smat)/sum(w[[k]])
      k <- k + 1
      if (k > K)
        cond <- FALSE
    }
    return(result)
  }
}



#' A function to compute environmental centralization index
#'
#' @usage ECfunc (x, distmin = NULL, dist = NULL, K = NULL, kdist = NULL,
#' spatobj1 = NULL, folder1 = NULL, shape1 = NULL,
#' spatobj2 = NULL, folder2 = NULL, shape2 = NULL)
#' @param x - a matrix with the groups distributions across spatial units
#' @param distmin - an optional vector with the minimal distance between each spatial
#' unit and all the environmental localisations. If not provided, it will be computed
#' in the function
#' @param dist - an optional matrix with the distance between all spatial units
#' and environmental localisations. If not provided, it will be computed in the function
#' @param K - if provided, the version of the index constrained to the K nearest neighbors
#' @param kdist - if provided, the version of the index constrained to  the nearest neighbors
#' within a distance of kdist
#' @param spatobj1 - polygons spatial objects for population distribution to compute distances matrix
#' (necessary if distance not provided).
#' @param folder1 - a character vector with the folder (directory) name
#' indicating where the shapefile with geographical info of population distribution is located
#' on the drive (necessary if distance and spatial object are not provided).
#' @param shape1 - a character vector with the name of the shapefile
#' (without the .shp extension) with geographical info of population distribution
#' @param spatobj2 - points spatial objects for (dis-)amenity location to compute distances matrix
#' (necessary if distance not provided).
#' @param folder2 - a character vector with the folder (directory) name
#' indicating where the shapefile with geographical info of (dis-)amenity distribution is located
#' on the drive (necessary if distance and spatial object are not provided).
#' @param shape2 - a character vector with the name of the shapefile
#' (without the .shp extension) with geographical info of (dis-)amenity spatial location
#' @return The matrix with environmental centralization index values
#' @references Schaeffer Y. and Tivadar M. (2019)
#' Measuring Environmental Inequalities: Insights from the Residential
#' Segregation Literature. \emph{Ecological Economics}, 164, 106329
#' @references Tivadar M. (2019)
#' OasisR: An R Package to Bring Some Order to the World of Segregation Measurement.
#' \emph{Journal of Statistical Software},  89 (7), pp. 1-39
#' @references Duncan O. D. and Duncan B. (1955)
#' Residential Distribution and Occupational Stratification.
#' \emph{American Journal of Sociology}, 60 (5), pp. 493-503
#' @references Folch D.C and Rey S. J (2016) The centralization index:
#' A measure of local spatial segregation. \emph{Papers in Regional
#' Science}, 95 (3), pp. 555-576
#' @description Environmental Centralization index compares the spatial distribution of two
#'  social groups around a specific environmental (dis-)amenity, located at one or more points.
#' @examples data(segdata, package = "OasisR")
#' # segdata - theoretical distributions on a 10x10 grid map
#' # We consider A1 and A2 - two populations distribution and
#' # the amenities are located in the grid center
#' distance <- sf::st_distance(sf::st_centroid(sf::st_as_sf(segdata)),
#' sf::st_centroid(sf::st_union(sf::st_as_sf(segdata))))
#' ECfunc (segdata@data[,3:4], dist = distance)
#' @seealso \code{\link{EDfunc}}, \code{\link{EnvResampleTest}},
#' \code{\link{EnvResamplePlot}}
#' @export

ECfunc <- function (x, distmin = NULL, dist = NULL, K = NULL, kdist = NULL,
                    spatobj1 = NULL, folder1 = NULL, shape1 = NULL, spatobj2 = NULL,
                    folder2 = NULL, shape2 = NULL)
{
  x <- as.matrix(x)
  if (is.null(distmin)) {
    if (is.null(dist)) {
      if (is.null(spatobj1)) spatobj1 <- sf::st_read(dsn = folder1, layer = shape1) else spatobj1 <- sf::st_as_sf(spatobj1)
      if (is.null(spatobj2)) spatobj2 <- sf::st_read(dsn = folder2, layer = shape2) else spatobj2 <- sf::st_as_sf(spatobj2)
      centroids1 <- sf::st_centroid(spatobj1)
      dist <- sf::st_distance(centroids1, spatobj2)
      units(dist) <- NULL
    }
    distmin <- vector(length = nrow(dist))
    for (i in 1:nrow(dist)) distmin[i] <- min(dist[i, 1:ncol(dist)])
  }
  result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
  varTotal <- colSums(x)
  xprovi <- cbind(x, distmin)
  xprovi <- as.data.frame(xprovi[order(xprovi[, ncol(xprovi)]), ])
  xprovi2 <- xprovi[1:length(unique(xprovi$distmin)), ]
  xprovi2$distmin <- unique(xprovi$distmin)
  for (i in 1:ncol(x)) xprovi2[, i] <- tapply(xprovi[, i], xprovi$distmin, sum)
  xprovi <- xprovi2
  if (is.null(K) & is.null(kdist)) {
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
      XI1 <- cumsum(xprovi[, k1])[1:(nrow(xprovi) - 1)]/varTotal[k1]
      XI <- cumsum(xprovi[, k1])[2:nrow(xprovi)]/varTotal[k1]
      YI1 <- cumsum(xprovi[, k2])[1:(nrow(xprovi) - 1)]/varTotal[k2]
      YI <- cumsum(xprovi[, k2])[2:nrow(xprovi)]/varTotal[k2]
      result[k1, k2] <- XI1 %*% YI - XI %*% YI1
    }
    return(result)
  }
  else {
    if (!is.null(K)) {
      if (K >= nrow(xprovi2))
        K <- nrow(xprovi2) - 1
      xprovi <- xprovi[1:(K + 1), ]
    }
    if (!is.null(kdist))
      xprovi <- xprovi[xprovi$distmin <= kdist, ]
    xprovi <- xprovi[, -ncol(xprovi)]
    test <- TRUE
    varTotal <- colSums(xprovi)
    if (sum(varTotal) == 0)
      test <- FALSE
    for (i in length(varTotal)) if (varTotal[i] == 0 & sum(varTotal) > 0) {
      test <- FALSE
      for (j in 1:length(varTotal)) {
        result[j, i] <- 1
        result[i, j] <- -1
      }
    }
    if (nrow(xprovi) <= 1)
      test <- FALSE
    if (test)
      for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
        XI1 <- cumsum(xprovi[, k1])[1:(nrow(xprovi) -
                                         1)]/varTotal[k1]
        XI <- cumsum(xprovi[, k1])[2:nrow(xprovi)]/varTotal[k1]
        YI1 <- cumsum(xprovi[, k2])[1:(nrow(xprovi) -
                                         1)]/varTotal[k2]
        YI <- cumsum(xprovi[, k2])[2:nrow(xprovi)]/varTotal[k2]
        result[k1, k2] <- XI1 %*% YI - XI %*% YI1
      }
    return(result)
  }
}



#' A function to test environmental inequality indices by resampling
#'
#' @usage EnvResampleTest(x, a = NULL, fun, simtype = "MonteCarlo",
#' nsim = NULL, sampleunit = "unit", proba = NULL, setseed = FALSE,
#' perc = c(.05, .95), outl = FALSE, outmeth = "bp", sdtimes = 2, IQRrange = 1.5,
#' spatobj = NULL, folder = NULL, shape = NULL,
#' spatobj1 = NULL, folder1 = NULL, shape1 = NULL,
#' spatobj2 = NULL, folder2 = NULL, shape2 = NULL,
#' distmin = NULL, dist = NULL, K = NULL, kdist = NULL,
#' vers = "standard", w = NULL, b = NULL, ar = NULL, per = NULL,
#' queen = TRUE, ptype = "int", f = "exp", beta = 1)
#' @param x -  a vector of the population/group distribution across spatial units for EDfunc or
#' a matrix with the groups distributions across spatial units for ECfunc
#' @param a - a vector of the environmental variable spatial distribution for EDfunc
#' @param fun - a character vector with the function to be tested, fun = "EDfunc" or fun = "ECfunc"
#' @param simtype - a character vector with the type of simulation.
#' If simtype = 'MonteCarlo' (by default), the function produces a randomization test
#' using Monte Carlo simulations. If simtype = 'Jack', the function generates jackknife replications
#' @param nsim	- the number of simulations (equal to the number of observations for jackknife)
#' @param sampleunit - for jackknife replicant, the resampling is made only on spatial units.
#' For Monte Carlo simulations, the user can choose between "unit" for spatial units
#' resampling and "ind" for population resampling.
#' @param proba - for Monte Carlo simulations on population, proba is a vector with location probabilities.
#' If proba = NULL, the vector is equiprobable. If outliers are determined with jackknife technique,
#' proba indicates the probability (confidence interval) for scoring tests.
#' @param setseed - if TRUE (by default), specify zero seed for repetead simulation
#' @param perc - percentille values for jackknife simulations
#' @param outl	- logical parameter for jackknife simulations, if TRUE the function
#' provides the outliers obtained by jackknife iterations
#' @param outmeth - a character vector designing the outliers detection method:
#' outmeth = 'bp' (by default) for boxplot method; outmeth = 'sd' for standard deviation method;
#' outmeth = 'z' for normal scores method; outmeth = 't' for t Student scores method;
#' outmeth = 'chisq' for chi-squared scores method; outmeth = 'mad' for median absolute deviation
#' method. The estimations based on scoring methods are obtained using outliers package
#' @param sdtimes - multiplication factor of the standard deviation used for
#' outliers detection with jackknife simulations (2 by default)
#' @param IQRrange	- determines the boxplot thresholds (1.5 by default) as multiplication of IQR (Inter Quartile Range)
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) for EDfunc geographic functions
#' @param shape - a character vector with the name of the shapefile for EDfunc as alternative to spatobj
#' @param folder - a character vector with the folder (directory) containing the shapefile for EDfunc
#' @param spatobj1 - polygons spatial objects for population distribution to compute distances matrix for ECfunc
#' @param shape1 - a character vector with the name of the shapefile for ECfunc as alternative to spatobj1
#' @param folder1 - a character vector with the folder (directory) containing the shape1 for ECfunc
#' @param spatobj2 - points spatial objects for (dis-)amenity location to compute distances matrix for ECfunc
#' @param shape2 - a character vector with the name of the shapefile for ECfunc as alternative to spatobj2
#' @param folder2 - a character vector with the folder (directory) containing the shape2 for ECfunc
#' @param distmin - an optional vector for ECfunc with the minimal distance between each spatial
#' unit and all the environmental localisations
#' @param dist - an optional matrix for ECfunc with the distance between all spatial units
#' and environmental localisations.
#' @param K - if provided, the version of the index constrained to the K nearest neighbors
#' @param kdist - if provided, the version of the index constrained to  the nearest neighbors
#' within a distance of kdist
#' @param vers - the EDfunc version:
#' "standard" (by default) for aspatial environmental dissimilarity index (Duncan);
#' "contig" for adjusted index with a contiguity spatial interactions matrix (Morrill);
#' "bound" for adjusted index with a boundaries spatial interactions matrix (Wong);
#' "shape" for adjusted index with a boundaries and shape spatial interactions matrix (Wong);
#' "user" for adjusted index with any user spatial interactions matrix
#' @param w - an optional spatial weights matrix for EDfunc.
#' @param ar - an optional vector of spatial units area for EDfunc.
#' @param b - an optional shared border matrix for EDfunc.
#' @param per - an optional vector of spatial units perimeter for EDfunc.
#' @param queen - logical parameter for EDfunc defining criteria used for contiguity matrix computation,
#' TRUE for queen (by default), FALSE  for rook
#' @param ptype - a string variable for EDfunc giving two options for perimeter calculation for  Wong's indices:
#' "int" to use only interior borders of spatial units and
#' "all" to use entire borders, including to the exterior of the area
#' @param K - the order of contiguity matrix if "contig" version of EDfunc is chosen (K = 1 by default)
#' @param f - spatial decay function of contiguity matrix for EDfunc when K > 1, with
#' f = "exp" (by default) for exponential function of contiguity "distance"
#  and f = "rec" for 1/(k^beta)
#' @param beta - spatial decay intensity parameter for EDfunc (equal to 1 by default),
#' used only when the version with contiguity is chosen and K > 1
#' @return A list with:
#' - index's name
#' - simulation type
#' - statistics summary of the simulations
#' - simulated index distribution
#' - simulated population distribution
#' - matrix with outliers (jackknife)
#' - list with outliers values (jackknife)
#' @references Schaeffer Y. and Tivadar M. (2019)
#' Measuring Environmental Inequalities: Insights from the Residential
#' Segregation Literature. \emph{Ecological Economics}, 164, 106329
#' @references Tivadar M. (2019)
#' OasisR: An R Package to Bring Some Order to the World of Segregation Measurement.
#' \emph{Journal of Statistical Software},  89 (7), pp. 1-39
#' @description Resampling tests for environmental inequality indexes.
#' @examples data(segdata, package = "OasisR")
#' # segdata - theoretical distributions on a 10x10 grid map
#' # We consider A1 - population distribution and A2 - amenity distribution
#' testoutput <- EnvResampleTest (x = segdata@data$A1, a = segdata@data$A2, spatobj = segdata,
#' fun = "EDfunc", vers = "contig", queen = FALSE)
#' testoutput$Summary
#' hist(testoutput$IndexDist)
#' @seealso \code{\link{EDfunc}}, \code{\link{ECfunc}},
#' \code{\link{EnvResamplePlot}}
#' @export




EnvResampleTest <- function(x, a = NULL, fun, simtype = "MonteCarlo", nsim = NULL, sampleunit = "unit", proba = NULL,
                            setseed = FALSE, perc = c(.05, .95), outl = FALSE, outmeth = "bp", sdtimes = 2, IQRrange = 1.5,
                            spatobj = NULL, folder = NULL, shape = NULL,
                            spatobj1 = NULL, folder1 = NULL, shape1 = NULL,
                            spatobj2 = NULL, folder2 = NULL, shape2 = NULL,
                            distmin = NULL, dist = NULL, K = NULL, kdist = NULL,
                            vers = "standard", w = NULL, b = NULL, ar = NULL, per = NULL,
                            queen = TRUE, ptype = "int", f = "exp", beta = 1) {

  # INIT
  if (setseed) set.seed(0)
  x <- as.matrix(x)
  if (simtype == "Jack" & is.null(nsim)) nsim <- nrow(x)  else if (is.null(nsim)) nsim <-99
  ntot <- nsim + 1
  var <- 1:ncol(x)
  nvar <- length(var)
  if (fun == "EDfunc") {
    IndTest <- matrix(nrow = nvar, ncol = 5)
    resim <- matrix(nrow = nvar, ncol = nsim)
  }
  if (fun == "ECfunc ") {
    IndTest <- matrix(nrow = nvar*(nvar-1), ncol = 5)
    resim <- matrix(nrow = nvar*(nvar-1), ncol = nsim)
  }
  IndTest <- as.data.frame(IndTest)
  IndTest[,1] <- var
  xdistr <- vector("list", nsim)
  if (fun == "EDfunc" & is.null(K)) K <- 1
  if (fun == "EDfunc" & vers != "standard"){
    if (K == 1 & vers == "contig" & is.null(w))
      w <- OasisR::contig(spatobj = spatobj, folder = folder, shape = shape, queen = queen)
    if (vers == "bound") {
      if (is.null(b)) b <- OasisR::boundaries(spatobj = spatobj, folder = folder, shape = shape)
      if (is.null(w)) w <- b
      if (vers == "shape"){
        if (is.null(ar)) ar <- OasisR::area(spatobj = spatobj, folder = folder, shape = shape)
        if (is.null(per))
          if (ptype == "all") per <- OasisR::perimeter(spatobj = spatobj, folder = folder, shape = shape) else per <- rowSums(b)
          w <- b * (matrix(rep(t(per/ar), length(x)), nrow = length(x), byrow = TRUE) +
                      matrix(rep(t(per/ar), length(x)), nrow = length(x), byrow = FALSE)) / (2 * max(per/ar))
      }
    }
    if (vers == "bound" | vers == "shape") {
      b <- OasisR::boundaries(spatobj = spatobj, folder = folder, shape = shape)
      if (vers == "bound") w <- b
      if (vers == "shape"){
        if (is.null(ar)) ar <- OasisR::area(spatobj = spatobj, folder = folder, shape = shape)
        if (is.null(per))
          if (ptype == "all") per <- OasisR::perimeter(spatobj = spatobj, folder = folder, shape = shape) else per <- rowSums(b)
          w <- b * (matrix(rep(t(per/ar), length(x)), nrow = length(x), byrow = TRUE) +
                      matrix(rep(t(per/ar), length(x)), nrow = length(x), byrow = FALSE)) / (2 * max(per/ar))
      }
    }
    if (K > 1 & vers == "contig" & is.null(w)) {
      if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape)
      ngb <- spdep::poly2nb(spatobj, queen = queen)
      ngbk<-spdep::nblag(ngb, K)
      w <- vector("list", K)
      for (k in 1:K)
        w[[k]] <- spdep::nb2mat(ngbk[[k]], style = "B", zero.policy = TRUE)
      if (simtype == "Jack"){
        wjack <- vector("list", nsim)
        for (j in 1:nsim) {
          wjack[[j]] <- w
          for (k in 1:K)
            wjack[[j]][[k]] <- wjack[[j]][[k]][-j,-j]
        }
      }
    }
  }
  if (simtype == "Jack" & fun == "ECfunc "){
    if (is.null(distmin)) {
      if (is.null(dist)) {
        if (is.null(spatobj1))
          spatobj1 <- sf::st_read(dsn = folder1, layer = shape1)
        if (is.null(spatobj2))
          spatobj2 <- sf::st_read(dsn = folder2, layer = shape2)
        centroids1 <- sf::st_centroid(spatobj1)
        dist <- sf::st_distance(centroids1, spatobj2)
        units(dist) <- NULL
      }
      distmin <- vector(length = nrow(dist))
      for (i in 1:nrow(dist)) distmin[i] <- min(dist[i, 1:ncol(dist)])
    }
  }
  if (!is.null(a) & fun == "EDfunc") x <- as.matrix(cbind(x,a))

  # DISTRIBUTIONS

  if (simtype == "MonteCarlo" & sampleunit == "ind"){
    if (is.null(proba)) proba <- rep(1/nrow(x), nrow(x))
    for (i in 1:nsim) {
      xdistr[[i]] <- matrix(0, nrow = nrow(x), ncol = ncol(x))
      for (k in 1:ncol(x)) {
        xprovi <- table(sample(nrow(x), size = sum(x[, k]), replace = TRUE, prob = proba))
        dimv <- as.numeric(dimnames(xprovi)[[1]])
        xdistr[[i]][dimv, k] <- xprovi
      }
      if (!is.null(a) & fun == "EDfunc") xdistr[[i]][,ncol(xdistr[[i]])] <- x[,ncol(x)]
    }
  }
  if (simtype == "MonteCarlo" & sampleunit == "unit" & fun == "EDfunc")
    for (k in 1:nsim) {
      xdistr[[k]] <- matrix(nrow = nrow(x), ncol = (ncol(x)+1))
      neworder <- sample(c(1:nrow(x)), size = nrow(x))
      xdistr[[k]][,1:ncol(x)]<-x[neworder,]
      xdistr[[k]][,ncol(xdistr[[k]])]<-a[neworder]
      if (vers != "standard") xdistr[[k]] <- xdistr[[k]][,-ncol(xdistr[[k]])]
    }
  if (simtype == "MonteCarlo" & sampleunit == "unit" & fun == "ECfunc ")
    for (k in 1:nsim)
      xdistr[[k]]<-x[sample(c(1:nrow(x)), size = nrow(x)),]
  if (simtype == "Jack"){
    klist <- 1:nrow(x)
    for (k in 1:nsim)
      xdistr[[k]] <-x[-klist[k],]
  }

  # FUNCTIONS
  if (fun == "EDfunc") {
    if (simtype == "MonteCarlo")
      for (k in 1:nsim)
        resim[, k] <- EDfunc(x = xdistr[[k]][, -ncol(xdistr[[k]])], a = xdistr[[k]][,ncol(xdistr[[k]])], vers = vers, w = w, b = b, ar = ar, per = per, queen = queen, K = K, f = f, beta = beta, spatobj = spatobj)[var]
      if (simtype == "Jack")
        if (K > 1 & vers == "contig")
          for (k in 1:nsim)
            resim[, k] <- EDfunc(x = xdistr[[k]][, -ncol(xdistr[[k]])], a = xdistr[[k]][,ncol(xdistr[[k]])], vers = vers, w = wjack[[k]], queen = queen, K = K, f = f, beta = beta, spatobj = spatobj)[var]
          else for (k in 1:nsim)
            resim[, k] <- EDfunc(x = xdistr[[k]][, -ncol(xdistr[[k]])], a = xdistr[[k]][,ncol(xdistr[[k]])], vers = vers, w = w[-k,-k], b = b[-k,-k], ar = ar[-k], per = per[-k], queen = queen, K = K, f = f, beta = beta, spatobj = spatobj)[var]
          IndTest[, 2] <- EDfunc(x = x[,-ncol(x)], a = a, vers = vers, w = w, b = b, ar = ar, per = per, queen = queen, K = K, f = f, beta = beta, spatobj = spatobj)[var]
          x = as.matrix(x[,-ncol(x)])
  }
  if (fun == "ECfunc ") {
    for (k in 1:nsim) {
      xvect <- NULL
      if (simtype == "Jack") distmin2 <- distmin[-k] else distmin2 <- distmin
      resprovi <- ECfunc (xdistr[[k]], distmin = distmin2, dist = dist , K = K, kdist = kdist,
                          spatobj1 = spatobj1, folder1 = folder1, shape1 = shape1,
                          spatobj2 = spatobj2, folder2 = folder2, shape2 = shape2)
      for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
      resim[, k] <- xvect
    }
    xvect <- NULL
    resprovi <- ECfunc (x, distmin = distmin, dist = dist , K = K, kdist = kdist,
                        spatobj1 = spatobj1, folder1 = folder1, shape1 = shape1,
                        spatobj2 = spatobj2, folder2 = folder2, shape2 = shape2)
    for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i],
                                                        var[-i]])
    IndTest[, 2] <- xvect
  }

  # RESULTS
  if (simtype == "MonteCarlo"){
    names(IndTest) <- c("Var", fun, "Mean", "Rank", "P.Value")
    for (i in 1:nrow(IndTest)) {
      IndTest[i, 3] <- mean(resim[i, ])
      IndTest[i, 4] <- rank(c(IndTest[i, 2], resim[i, ]), ties.method = "min")[1]
      IndTest[i, 5] <- (ntot - IndTest[i, 4] + 1)/ntot
    }
  }
  if (simtype == "Jack"){
    term <- "th"
    if (substr(as.character(perc[1]), 3, 4) == "01") term <- "st"
    if (substr(as.character(perc[1]), 3, 4) == "02") term <- "nd"
    if (substr(as.character(perc[1]), 3, 4) == "03") term <- "rd"
    names(IndTest) <- c("Var", fun, paste0(substr(as.character(perc[1]), 4, 4), term, "_percentile"),
                        "Median", paste0(substr(as.character(perc[2]), 3, 4), "th_percentile"))
    for (i in 1:nrow(IndTest)) {
      IndTest[i, 3] <- stats::quantile(resim[i, ], perc[1])
      IndTest[i, 4] <- stats::quantile(resim[i, ], .5)
      IndTest[i, 5] <- stats::quantile(resim[i, ], perc[2])
    }
    jack.bias <- vector(length = nvar)
    jack.se <- vector(length = nvar)
    for (i in 1:nvar) {
      jack.bias[i] <- (nsim - 1) * (mean(resim[i,]) - IndTest[i,2])
      jack.se [i] <- sqrt(((nsim - 1)/nsim) * sum((resim[i,] - mean(resim[i,]))^2))
    }
    IndTest$JackBias <- jack.bias
    IndTest$JackSE <- jack.se
  }

  # OUTLIERS

  if (outl == TRUE & simtype == "Jack"){
    outl <- matrix(FALSE, nrow = ncol(x), ncol = nsim)
    if (outmeth == "sd"){
      for (i in 1:nrow(resim))
        outl[i, ] <- resim[i,] >= mean(resim[i, ]) + sdtimes * stats::sd(resim[i,]) |
          resim[i,] < mean(resim[i, ]) - sdtimes * stats::sd(resim[i,])
    }
    if (outmeth == "bp"){
      for (i in 1:nrow(resim))
        outl[i, ] <- is.element(resim[i,], grDevices::boxplot.stats(resim[i,],  coef = IQRrange )$out)
    }
    if (is.element(outmeth, c("z", "t", "chisq", "mad"))) {
      if (is.null(proba)) proba <- 0.9
      for (i in 1:nrow(resim))
        outl[i, ] <- outliers::scores(resim[i, ], type = outmeth, prob = proba)
    }
    outval <- vector("list", nrow(resim))
    for (i in 1:length(outval))
      outval[[i]] <- resim[i,][outl[i,]]
    graphics::boxplot(t(resim))
    outl <- t(outl)
    result <- list(fun,  simtype, IndTest, resim, xdistr, outl, outval)
    names(result) <- c("Index", "SimType", "Summary", "IndexDist", "RandomDist", "Outliers", "OutliersVal")
  } else {
    result <- list(fun, simtype, IndTest, resim, xdistr)
    names(result) <- c("Index", "SimType", "Summary", "IndexDist", "RandomDist")
  }
  return(result)
}




#' A function to test environmental inequality indices by resampling
#'
#' @usage EnvResamplePlot(ResampleTest, var = 1,  coldist = "red", colind = "blue", legend = TRUE,
#' legendpos = "top", cex.legend = 1, bty = "o")
#' @param ResampleTest - a ResampleTest object produced with ResampleTest function
#' @param var - the number of the variable to be plot
#' @param coldist - color used to plot the simulated distribution
#' @param colind	- color used to plot the index
#' @param legend	- logical parameter, to control the legend's plots
#' @param legendpos - a character string giving the legend's position: 'bottomright', 'bottom',
#' 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right' and 'center'.
#' @param cex.legend - a numerical value giving the amount by which plotting text and symbols
#' in legend should be magnified relative to the default.
#' @param bty - a character string which determines the type of box of the legend:
#' 'o' (by default), 'l', '7', 'c', 'u', or ']'
#' @return A plot with resampling distribution
#' corresponding upper case letter. A value of 'n' suppresses the box.
#' @references Schaeffer Y. and Tivadar M. (2019)
#' Measuring Environmental Inequalities: Insights from the Residential
#' Segregation Literature. \emph{Ecological Economics}, 164, 106329
#' @references Tivadar M. (2019)
#' OasisR: An R Package to Bring Some Order to the World of Segregation Measurement.
#' \emph{Journal of Statistical Software},  89 (7), pp. 1-39
#' @description Plot of resampling simulations results.
#' @examples data(segdata, package = "OasisR")
#' # segdata - theoretical distributions on a 10x10 grid map
#' # We consider A1 - population distribution and A2 - amenity distribution
#' testoutput <- EnvResampleTest (x = segdata@data$A1, a = segdata@data$A2, spatobj = segdata,
#' fun = "EDfunc", vers = "contig", queen = FALSE)
#' EnvResamplePlot(testoutput)
#' @seealso \code{\link{EDfunc}}, \code{\link{ECfunc}},
#' \code{\link{EnvResampleTest}}
#' @export




EnvResamplePlot <- function(ResampleTest, var = 1,  coldist = "red", colind = "blue", legend = TRUE,
                            legendpos = "top", cex.legend = 1, bty = "o")  {
  indexname <- ResampleTest$Index
  dens <- as.matrix(ResampleTest$IndexDist)[var, ]
  ind <- ResampleTest$Summary[var, 2]
  simtype <- ResampleTest$SimType
  if (simtype == "Boot") simtype <- "Bootstrap"
  if (simtype == "Jack") simtype <- "JackKnife"
  if (simtype == "MonteCarlo") simtype <- "Monte Carlo"
  nsim <- length(dens)
  ntot <- nsim + 1
  dens2 <- stats::density(dens)
  liminf <- min(dens2$x, ind)
  limsup <- max(dens2$x, ind)
  ylimit = c(0, ((max(dens2$y)) + 2))
  if (simtype == "Monte Carlo"){
    statname <- "mean"
    esper <- ResampleTest$Summary[var, 3]
  } else {
    statname <- "median"
    esper <- ResampleTest$Summary[var, 4]
  }
  plot(dens2, xlim = c(liminf, limsup), ylim = ylimit, lwd = 1,col = coldist, main = "", xlab = "Index values")
  graphics::segments(esper, 0, esper, max(dens2$y), lwd = 1, lty=3, col = coldist)
  graphics::segments(ind, 0, ind, max(dens2$y), lwd = 2, col = colind)
  graphics::mtext(paste0(simtype, " Test: ", indexname), side = 3, font = 2, line = 2)
  if (legend)
    legend(legendpos, c("Simulated distribution", paste("Simulated", statname), indexname),
           col = c(coldist, coldist, colind), lty = 1, lwd = c(2, 1, 2),
           bty = bty, cex = cex.legend)
}


