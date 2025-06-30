fusionSimulate <- function(n.point, n.area, n.grid, n.pred, dimension = 10,
                           psill = 5, phi = 1, nugget = 0, tau.sq = 1,
                           domain = NULL, point.beta = NULL, area.beta = NULL, nvar.pp = 1,
                           distributions, design.mat = matrix(c(1,1.5,2)),
                           pp.offset, seed){
    # checks ------------------------------------------------------------------

    if (missing("n.point")) n.point <- 0
    if (missing("n.area")) n.area <- 0
    if (missing("n.grid")){n.grid <- 0} else {if (missing("pp.offset")) pp.offset <- 1}
    if (missing("distributions")){
      if (n.point > 0 | n.area > 0) stop("distributions must be provided")
      distributions <- NULL
    } else {
      if (length(distributions) != length(point.beta) + length(area.beta)){
        stop(paste("the number of distributions does not match the length of fixed effect coefficients.",length(point.beta) + length(area.beta), "distributions need to be specified with", length(point.beta),
                   "for geostatistical data and", length(area.beta), "for lattice data."))
      }
      if (!all(distributions %in% c("normal","poisson","bernoulli"))) stop("the current implementations only support normal, Poisson and Bernoulli distributions")
    }

    if (n.point + n.area + n.grid < 1) stop("total sample size must be greater than zero.")
    if ("normal" %in% distributions){
      if (length(tau.sq) != sum(distributions == "normal")) stop(paste("the number of tau.sq provided does not match the number of normal distributions"))
    }
    if (length(unique(c(length(psill), length(phi),length(nugget)))) != 1) stop("Gaussian process parameters must have equal length.")
    if (!identical(length(psill), dim(design.mat)[2])) stop("number of colums of the design matrix must match the number of latent Gaussian processes.")
    if (any(rowSums(design.mat) == 0)) stop("at least one response variable is not associated with the latent process based on the design matrix, consider removing it in the simulation")
    #  if (!all.equal(length(psill), dim(design.mat)[1])) stop("Number of rows of the design matrix must match the number of dependent variables.")
    if (!is.null(domain) & !inherits(domain, "sf")) stop("domain must be of class sf")
    if (dimension <= 0) stop("dimension must be strictly positive")

    if (n.point != 0){
      if (missing("point.beta")){
        stop("coefficients point.beta for point data are missing")
      } else {
        if (length(unique(sapply(point.beta, function(x) length(x)))) != 1) stop("point.beta must be a list of equal vectors")
      }
    }

    if (n.area != 0){
      if (missing("area.beta")){
        stop("coefficients area.beta for lattice data are missing")
      } else {
        if (length(unique(sapply(area.beta, function(x) length(x)))) != 1) stop("area.beta must be a list of equal vectors")
      }
    }

    if (missing("seed")) seed <- round(rnorm(1) * 1e3, 0)


    # Simulation --------------------------------------------------------------

    size.latent <- 30^2
    if (missing("n.pred")) n.pred <- size.latent
    set.seed(seed)
    if (is.null(domain)){
      xy.latent <- data.frame(expand.grid(x = seq(0, dimension, length.out = sqrt(size.latent)),
                                          y = seq(0, dimension, length.out = sqrt(size.latent))))
      xy.point <- data.frame(x = runif(size.latent, 0, dimension), y = runif(size.latent, 0, dimension))
    } else {
      domain <- as(domain, "Spatial")
      xy.latent <- data.frame(spsample(domain, size.latent, "regular")@coords)
      xy.point <- data.frame(spsample(domain, size.latent, "random")@coords)
      names(xy.latent) <- names(xy.point) <- c("x","y")
    }

    n.w <- length(psill) # number of w
    if (n.point == 0){
      n.y.point <- 0
      p.point <- 0
    } else {
      n.y.point <- length(point.beta)
      p.point <- length(point.beta[[1]])
    }

    if (n.area == 0){
      n.y.area <- 0
      p.area <- 0
    } else {
      n.y.area <- length(area.beta)
      p.area <- length(area.beta[[1]])
    }

    if (n.grid == 0){
      n.y.pp <- 0
    } else {
      n.y.pp <- nvar.pp
    }

    if (nrow(design.mat) != n.y.point + n.y.area + n.y.pp) warning("the number of rows in design.mat exceeds the number of variables, redundant rows are ignored")

    noise <- numeric(length(distributions))
    if ("normal" %in% distributions)  noise[distributions == "normal"] <- tau.sq[1:sum(distributions == "normal")]
    # generate latent spatial process
    # resolution <- dimension*2/100 # result approximate 10,000 locations

    if (is.null(domain)){
      domain.coords = matrix(c(0, 0, 0, dimension, dimension, dimension, dimension, 0, 0, 0), ncol = 2, byrow = TRUE)
      domain <- SpatialPolygons(list(Polygons(list(Polygon(domain.coords)), 1)))
    }

    if (n.grid > 0){
      grd_lrg <- as.data.frame(makegrid(domain, n = n.grid^2))
      names(grd_lrg)       <- c("x", "y")
      coordinates(grd_lrg) <- c("x", "y")
      gridded(grd_lrg) <- TRUE
      grd_lrg@proj4string@projargs <- domain@proj4string@projargs
      centroids_grid <- SpatialPoints(coordinates(grd_lrg))
      xy.lgcp <- data.frame(centroids_grid@coords)
      names(xy.lgcp) <- c("x","y")
      n.grid <- nrow(xy.lgcp)
    } else {
      n.grid <- 0
      xy.lgcp <- NULL
    }
    xy.pred <- data.frame(spsample(domain, n.pred, "regular")@coords)
    names(xy.pred) <- c("x","y")
    xy <- rbind(xy.point, xy.lgcp, xy.pred, xy.latent)
    n.xy <- nrow(xy)
    for (i in 1:n.w){
      if (i == 1){
        g.dummy <- spam::rgrf(1, locs = xy, Covariance = "cov.exp", theta = c(phi[i], psill[i], nugget[i]))
        mrf <- data.frame(x = xy[,1], y = xy[,2], sim1 = g.dummy)
      } else {
        g.dummy2 <- spam::rgrf(1, locs = xy, Covariance = "cov.exp", theta = c(phi[i], psill[i], nugget[i]))
        mrf[,paste0("sim",i)] <- g.dummy2
      }
    }

    # generate representative points for LGCP
    if (n.grid > 0){
      total.area <- sum(sapply(domain@polygons, function(x) x@area))
      c.area <- prod(grd_lrg@grid@cellsize)
      # multiplicative effect
      w.pp <- lapply(1:n.y.pp, function(i)
        as.matrix(mrf[(nrow(xy.point)+1):(nrow(xy.point)+nrow(xy.lgcp)), paste0("sim",1:n.w)]) %*% design.mat[i + n.y.point + n.y.area,])
      lgcp.lambda <- lapply(1:n.y.pp, function(i) exp(w.pp[[i]]))
      lgcp.coords <- lapply(1:n.y.pp, function(i) {
        lgcp.coord <- spsample(domain, rpois(1, total.area * max(lgcp.lambda[[i]]) * pp.offset), type = "random")
        lgcp.coord <- as.data.frame(lgcp.coord@coords[which(rbinom(length(lgcp.coord), 1, lgcp.lambda[[i]][over(lgcp.coord, grd_lrg)]/max(lgcp.lambda[[i]])) == 1),])
        if (nrow(lgcp.coord) < 10) stop("the density of point pattern data is too low, try to set 'pp.offset' to larger numbers.")
        if (nrow(lgcp.coord) > 1e6) stop("the density of point pattern data is too high, try to set 'pp.offset' to smaller numbers.")
        coordinates(lgcp.coord) <- ~x+y
        lgcp.coord@proj4string@projargs <- domain@proj4string@projargs
        return(lgcp.coord)
      })

      lgcp.grids <- lapply(1:n.y.pp, function(i) {
        # how many points in each grid
        lgcp.grid <- data.frame(table(over(lgcp.coords[[i]], grd_lrg)))
        colnames(lgcp.grid) <- c("idx", "number")
        lgcp.grid <- merge(data.frame(idx = 1:length(grd_lrg)), lgcp.grid, all.x = TRUE)
        lgcp.grid$number[is.na(lgcp.grid$number)] <- 0
        return(lgcp.grid)
      })
    }

    # generate aggregated latent process for area data
    if (n.area > 0){
      if (length(domain) <= 1){
        centroids <- data.frame(x = runif(n.area,0,dimension), y = runif(n.area,0,dimension))
        centroids$x[which.max(centroids$x)] <- centroids$y[which.max(centroids$y)] <- dimension
        centroids$x[which.min(centroids$x)] <- centroids$y[which.min(centroids$y)] <- 0
        coordinates(centroids) <- ~ x + y
        poly <- suppressWarnings(voronoiPolygons(centroids, domain)) # modified original function from package SDraw (archived package)
      } else {
        n.area <- length(domain)
        warning(paste0("n.area is set to ", n.area, " to match the number of polygons provided in 'domain'."))
        poly <- domain
      }
      coordinates(xy) <- ~x+y
      xy@proj4string <- domain@proj4string
      pips.index <- over(poly, xy, returnList = T)

      aD_matrix <- matrix(0,n.area,nrow(xy@coords))
      for (i in 1:n.area){
        aD_matrix[i,pips.index[[i]]] <- 1
      }
      aD_matrix.full <-  diag(1/rowSums(aD_matrix)) %*% aD_matrix  # aggregation matrix that computes average mrf per area
      #
      mrf.indep <- lapply(1:n.y.area, function(i) (as.matrix(mrf[,paste0("sim",1:n.w)]) %*% design.mat[ i + n.y.point,]))
      mean.mrf <- lapply(1:n.y.area, function(i) log(aD_matrix.full %*% exp(mrf.indep[[i]])))
      wbar <- lapply(mean.mrf, function(x) c(x))

      Xa <- cbind(rep(1,n.area),matrix(rnorm(n.area*(p.area-1),0,1), n.area, p.area-1))
      y.area <- sapply(1:n.y.area, function(i) genOutcome(distributions[i+n.y.point], n.area, Xa, area.beta[[i]], wbar[[i]], noise[i+n.y.point]))
    }

    sample.ind <- sample(nrow(xy.point),n.point)
    if (n.point > 0){
      # generate point response
      Xn <- cbind(rep(1,n.point),matrix(rnorm(n.point*(p.point-1),0,1), n.point, p.point-1))
      w <- lapply(1:n.y.point, function(i) as.matrix(mrf[sample.ind,paste0("sim",1:n.w)]) %*% design.mat[i,])
      y <- sapply(1:n.y.point, function(i) genOutcome(distributions[i], n.point, Xn, point.beta[[i]], w[[i]], noise[i]))
    }

    # Preparing Output --------------------------------------------------------

    # a fusionData class for Stan models
    dat <- list()
    if (n.point > 0) dat <- c(dat, Y_point = list(data.frame(y)), X_point = list(Xn))
    if (n.area > 0) dat <- c(dat, Y_area = list(data.frame(y.area)), X_area = list(Xa))
    if (n.grid > 0) dat <- c(dat, lgcp.coords = list(lapply(lgcp.coords, function(i){st_as_sf(i)})))
    domain <- as(domain,"sf")

    out <- list(seed = seed, data = dat, mrf = mrf, domain = domain, pred.loc = xy.pred,
                pred.ind = seq(n.xy - nrow(xy.pred) - nrow(xy.latent) + 1,  n.xy - nrow(xy.latent)))
    if (n.point > 0){out <- c(out, sample.ind = list(sample.ind))}
    if (n.area > 0){
      out <- c(out, mean.w = list(mean.mrf))
      out$poly <- st_as_sf(poly)}
    if (n.grid > 0){out <- c(out, lgcp.grid = list(xy.lgcp))}

    return(out)
}
