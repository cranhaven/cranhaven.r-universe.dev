fusionData <- function(geo.data, geo.formula,
                       lattice.data, lattice.formula,
                       pp.data, distributions, domain = NULL,
                       method = c("Stan", "INLA"),
                       proj4string = CRS(as.character(NA)),
                       stan.control = NULL){

  # some checks -------------------------------------------------------------
  method <- match.arg(method, choices = c("Stan", "INLA"))
  if (!method %in% c("Stan", "INLA")) stop("method must be one of 'Stan' or 'INLA'")

  if (method == "Stan"){
    stan.control.default <- list(n.neighbor = 5, n.sampling = 5, n.grid = 10)
    if (!is.null(stan.control)){
      if (!is.list(stan.control)){
        stop("'stan.control' should be a named list")
      }
      control.names <- c("n.neighbor", "n.sampling", "n.grid")
      control.idx <- match(names(stan.control), control.names)
      if (sum(is.na(control.idx)) > 1){
        stop(paste0("'control' list contains unknown names: ", names(stan.control)[is.na(control.idx)]))
      }
      control.missing.idx <- which(is.na(match(control.names, names(stan.control))))  # gives missing control names
      for (i in control.missing.idx){
        stan.control[[control.names[i]]] <- stan.control.default[[i]]
      }
    } else {
      stan.control <- list(n.neighbor = 5, n.sampling = 5, n.grid = 10)
    }
    n.neighbor <- stan.control[["n.neighbor"]]
    n.sampling <- stan.control[["n.sampling"]]
    n.grid <- stan.control[["n.grid"]]
  }

  if (!missing("geo.data")){
    if (class(geo.data)[1] == "data.frame"){
      if (all(c("x","y") %in% names(geo.data))){
        geo.data <- SpatialPointsDataFrame(coords = geo.data[,c("x","y")], data = geo.data, proj4string = proj4string)
      } else {
        stop("missing x and y coordinates in the data.frame.")
      }
    } else {
      if (!inherits(geo.data, c("data.frame","sf"))) stop("geo.data must be either a class of data.frame or sf")
      if (inherits(geo.data,"sf")) {
        geo.data <- as(geo.data, "Spatial")
        colnames(geo.data@coords) <- c("x","y")
      }
    }
    n.point <- nrow(geo.data)
    if (missing(geo.formula)){
      stop("geo.formula is missing for geo.data")
    } else {
      if (!inherits(geo.formula, "formula")) stop("geo.formula is not a valid formula")
    }
    p.point <- length(attr(terms(geo.formula), "term.labels")) + attr(terms(geo.formula), "intercept")
    n.point.vars <- length(all.vars(geo.formula)) - length(attr(terms(geo.formula), "term.labels"))
    y.point <- all.vars(geo.formula)[1:n.point.vars]
    x.point <- all.vars(geo.formula)[-(1:n.point.vars)]
    if (!all(c(x.point, y.point) %in% names(geo.data@data))) stop("at least one variable name in geo.formula is missing")
    n_point_var <- length(y.point)
  } else {
    n_point_var <- n.point <- 0
  }

  if (!missing("lattice.data")){
    if (!inherits(lattice.data, "sf")){
      stop("lattice.data must be of class sf")
    } else {
      lattice.data <- as(lattice.data,"Spatial")
      n.area <- length(lattice.data)
      if (missing(lattice.formula)){
        stop("lattice.formula is missing for lattice.data")
      } else {
        if (!inherits(lattice.formula, "formula")) stop("lattice.formula is not a valid formula")
      }
    }

    p.area <- length(attr(terms(lattice.formula), "term.labels")) + attr(terms(lattice.formula), "intercept")
    n.area.vars <- length(all.vars(lattice.formula)) - length(attr(terms(lattice.formula), "term.labels"))
    y.area <- all.vars(lattice.formula)[1:n.area.vars]
    x.area <- all.vars(lattice.formula)[-(1:n.area.vars)]
    if (!all(c(x.area, y.area) %in% names(lattice.data@data))) stop("at least one variable name in lattice.formula is missing")
    n_area_var <- length(y.area)
  } else {
    n_area_var <- n.area <- 0
  }

  if (!missing("pp.data")){
    if (inherits(pp.data, "list")){
      if (length(unique(sapply(pp.data, class))) != 1) stop("pp.data must be a list of the same class")
      n_pp_var <- length(pp.data)
      if (all(sapply(pp.data, class) == "data.frame")){
        pp.data <- lapply(pp.data, function(pp) {
          if (all(c("x","y") %in% names(pp))){
            SpatialPoints(coords = pp[,c("x","y")], proj4string = proj4string)
          } else {
            stop("missing x and y coordinates in the data.frame.")
          }
        })
      } else {
        if (!all(sapply(pp.data, class) == "sf")) stop("pp.data must be (a list of) either data.frame or sf object")
        if (all(sapply(pp.data, class) == "sf")) {
          pp.data <- lapply(pp.data, function(pp){as(pp, "Spatial"); colnames(pp@coords) <- c("x","y")})
        }
      }} else {
        n_pp_var <- 1 # number of point pattern-variate
        if (!inherits(pp.data,"sf") & !inherits(pp.data,"data.frame")) stop("pp.data must be (a list of) either data.frame or sf object")
        if (!inherits(pp.data, "sf")){
          if (all(c("x","y") %in% names(pp.data))){
            pp.data <- SpatialPoints(coords = pp.data[,c("x","y")], proj4string = proj4string)
          } else {
            stop("missing x and y coordinates in the data.frame.")
          }
        } else {
          if (inherits(pp.data, "sf")) {
            pp.data <- as(pp.data, "Spatial")
            colnames(pp.data@coords) <- c("x","y")
          }
        }
      }
  } else {
    n_pp_var <- n.grid <- 0
  }

  if (is.null(domain)){
    if (!missing("lattice.data")){
      domain <- SpatialPolygons(lattice.data@polygons, proj4string = lattice.data@proj4string)
      if (!missing("geo.data")){
        if (sum(is.na(over(geo.data, domain)))/length(geo.data) > 0.5) stop("majority of 'geo.data' falls outside of the  spatial domain of 'lattice.data', please check your data again before proceeding or provide a larger 'domain'")
      }
      if (!missing("pp.data")){
        if (sum(is.na(over(pp.data, domain)))/length(pp.data) > 0.5) stop("majority of 'pp.data' falls outside of the spatial domain of 'lattice.data', please check your data again before proceeding or provide a larger 'domain'")
      }
    } else {
      stop("'domain' must be provided if there is no lattice data")
    }
  } else {
    if (!inherits(domain, "sf")) {
      stop("'domain' must be a class of sf")
    } else {
      domain <- as(domain,"Spatial")          }
    if (!missing("geo.data")){
      if (sum(is.na(over(geo.data, domain)))/length(geo.data) > 0.5) stop("majority of 'geo.data' falls outside of the 'domain', please check your data again before proceeding or provide a larger 'domain'")
    }
    if (!missing("pp.data")){
      if (n_pp_var == 1){
        if (sum(is.na(over(pp.data, domain)))/length(pp.data) > 0.5) stop("majority of 'pp.data' falls outside of the 'domain', please check your data again before proceeding or provide a larger 'domain'")
      } else {
        for (i in 1:n_pp_var){
          if (sum(is.na(over(pp.data[[i]], domain)))/length(pp.data) > 0.5) stop("majority of 'pp.data' falls outside of the 'domain', please check your data again before proceeding or provide a larger 'domain'")
        }
      }
    }
  }



  # if all data have the same proj4string, then fine
  # else set all of them to proj4string arg
  proj4strings <- c(if(!missing(geo.data)){geo.data@proj4string},
                    if(!missing(lattice.data)){lattice.data@proj4string},
                    if(!missing(pp.data)){
                      if(inherits(pp.data, "list")){
                        sapply(pp.data, function(x) x@proj4string)
                      }else{pp.data@proj4string}})

  if (length(unique(proj4strings)) != 1){
    warning(paste0("not all proj4string are the same, setting them all to ", proj4string))
    if (!missing(geo.data)) geo.data@proj4string <- proj4string
    if (!missing(lattice.data)) lattice.data@proj4string <- proj4string
    if (!missing(pp.data)){
      if (inherits(pp.data, "list")){
        for (i in 1:length(pp.data)){pp.data[[i]]@proj4string <- proj4string}
      } else {
        pp.data@proj4string <- proj4string
      }
    }
  } else {
    proj4string <- unique(proj4strings)[[1]]
  }

  if (missing("distributions")){
    if (n_point_var > 0 | n_area_var > 0) stop("distributions must be provided")
    distributions <- NULL
  } else {
    if (length(distributions) != n_point_var + n_area_var){
      stop("the number of distributions must be equal to the total number of geostatistical and lattice response variables")
    }
  }

  distributions <- tolower(distributions)
  if (sum(is.na(match(distributions, c("normal","gaussian","poisson","bernoulli")))) > 0){
    stop("currently only 'normal', 'poisson' (count) and 'bernoulli' (binary) are supported. ")
  }
  distributions[distributions == "gaussian"] <- "normal"

  if (n_pp_var + n_area_var + n_point_var < 2) stop("spatial fusion model is not suitable for analyzing only a single spatial variable")

  # main  -------------------------------------------------------------------


  if (method == "INLA"){
    n.grid <- n_pp_var # no gridding is used in INLA, set it to the number of point pattern data
    dat.all <- list(distributions = distributions, n_point = n.point, n_area = n.area, n_grid = n.grid, domain = domain)

    if (n.point > 0){
      X_point <- model.matrix(geo.formula, geo.data@data)
      Y_point <- data.frame(geo.data@data[, y.point])
      names(Y_point) <- y.point

      dat.all <- c(dat.all,
                   geo.formula = list(geo.formula),
                   locs_point = list(geo.data@coords),
                   p_point = list(p.point),
                   n_point_var = list(length(y.point)),
                   Y_point = list(Y_point),
                   X_point = list(X_point))
    } else {
      dat.all <- c(dat.all,
                   n_point_var = list(0))
    }
    if (n.area > 0){
      X_area <- model.matrix(lattice.formula, lattice.data@data)
      Y_area <- data.frame(lattice.data@data[, y.area])
      names(Y_area) <- y.area
      names(X_area) <- if (attr(terms(lattice.formula), "intercept") == 1){c("intercept",x.area)}else{x.area}

      dat.all <- c(dat.all,
                   lattice.formula = list(lattice.formula),
                   poly = list(lattice.data),
                   p_area = list(p.area),
                   n_area_var = list(n_area_var),
                   Y_area = list(Y_area),
                   X_area = list(X_area))
    } else {
      dat.all <- c(dat.all,
                   n_area_var = list(0))

    }
    if (n.grid > 0){
      if (n_pp_var == 1){
        dat.all <- c(dat.all,
                     locs_pp1 = list(pp.data@coords),
                     n_pp_var = list(n_pp_var))
      } else {
        dat.all <- c(dat.all,
                     locs_pp = lapply(pp.data, function(x) x@coords),
                     n_pp_var = list(n_pp_var))
      }

    } else {
      dat.all <- c(dat.all,
                   n_pp_var = list(0))
    }

    class(dat.all) <- "dinla"
  } else {

    # get sampling points for lattice data
    if (n.area > 0){
      sample.locs <- data.frame(do.call(rbind,sapply(1:n.area, function(i) {
        polysample <- spsample(lattice.data@polygons[[i]], n = n.sampling, type = "random")@coords
        cbind(id=i, polysample)}
        ,simplify = F)))
      colnames(sample.locs)[2:3] <- c("x","y")
      A1 <- kronecker(diag(1,n.area), t(matrix(rep(1/n.sampling, n.sampling))))  # the non-0 part of A1, block diag
    }

    # get grid for pp.data
    if (n.grid > 0){
      grd_lrg <- as.data.frame(makegrid(domain, "regular", cellsize = c(diff(domain@bbox[1,])/n.grid, diff(domain@bbox[2,])/n.grid)))
      names(grd_lrg)       <- c("x", "y")
      coordinates(grd_lrg) <- c("x", "y")
      gridded(grd_lrg) <- TRUE
      grd_lrg@proj4string <- proj4string
      xy.lgcp <- SpatialPoints(coordinates(grd_lrg))
      if (n.grid^2 != length(xy.lgcp)) warnings(paste0("the actual number of grid used for point pattern data is ", length(xy.lgcp),"."))
      n.grid <- length(xy.lgcp)
      cell.area <- prod(diff(domain@bbox[1,]), diff(domain@bbox[2,]))/length(xy.lgcp)
      # how many points in each grid
      if (n_pp_var == 1){
        lgcp.grid <- data.frame(table(over(pp.data, grd_lrg)))
        colnames(lgcp.grid) <- c("idx", "number")
        lgcp.grid <- merge(data.frame(idx = 1:length(grd_lrg)), lgcp.grid, all.x = TRUE)
        lgcp.grid$number[is.na(lgcp.grid$number)] <- 0
      } else {
        lgcp.grid <- lapply(pp.data, function(pp) data.frame(table(over(pp, grd_lrg))))
        for (i in 1:n_pp_var){
          colnames(lgcp.grid[[i]]) <- c("idx", "number")
          lgcp.grid[[i]] <- merge(data.frame(idx = 1:length(grd_lrg)), lgcp.grid[[i]], all.x = TRUE)
          lgcp.grid[[i]]$number[is.na(lgcp.grid[[i]]$number)] <- 0
        }}
    }

    # preparing covariance matrices
    all.locs <- rbind(if(n.point >0){geo.data@coords}, if(n.grid > 0){xy.lgcp@coords}, if(n.area > 0){sample.locs[,c("x","y")]})
    ind_distM_all <- get_index_dist(all.locs, n.neighbor)
    neardistM_all <- t(sapply(1: (nrow(all.locs) - 1), get_neardistM, ind_distM_all$d, n.neighbor = n.neighbor))
    neardist_all <- t(sapply(1: (nrow(all.locs) - 1), get_neardist, ind_distM_all$d, n.neighbor = n.neighbor))
    nearind_all <- t(sapply(1: (nrow(all.locs) - 1), get_nearind, ind_distM_all$i, n.neighbor = n.neighbor))

    nei_all <- array(NA, dim = c(nrow(all.locs) - 1, n.neighbor, n.neighbor))
    site_nei_all <- array(NA, dim = c(nrow(all.locs) - 1, n.neighbor))
    for (i in 2:nrow(all.locs)){
      dim <- if (i < (n.neighbor+1)){i-1} else {n.neighbor}
      h = 0;
      nei_all[i-1,,] <- diag(0, n.neighbor) # m by m
      for (j in 1:(dim-1)){
        for (k in (j+1):dim){
          h = h + 1
          nei_all[i-1, j, k] <- neardistM_all[i-1, h]
          nei_all[i-1, k, j] <- nei_all[i-1, j, k]
        }
      }
      site_nei_all[i-1,] <- neardist_all[i-1, ]
    }
    if (n.point + n.grid > 0){
      if (n.area > 0){
        # for sampling points
        nearind_sample <- nearind_all[(n.point+n.grid):(nrow(all.locs)-1),]
        C_nei <- nei_all[(n.point + n.grid):(nrow(all.locs)-1),,]
        C_site_nei <- site_nei_all[(n.point + n.grid):(nrow(all.locs)-1),]
      } else {
        nearind_sample <- NULL
        C_nei <- NULL
        C_site_nei <- NULL
      }
      # for geostatistical and point pattern data
      nearind <- nearind_all[1:(n.point + n.grid -1), ]
      sC_nei_all <- nei_all[1:(n.point + n.grid-1),,]
      sC_site_nei_all <- site_nei_all[1:(n.point + n.grid-1),]
    } else {
      nearind_sample <- nearind_all
      nearind <- NULL
      # for sampling points
      C_nei <- nei_all
      C_site_nei <- site_nei_all
      # for geostatistical and point pattern data
      sC_nei_all <- NULL
      sC_site_nei_all <- NULL
    }

    dat.all <- list(distributions = distributions,
                    n_point = n.point, n_area = n.area, n_grid = n.grid, n_neighbor = n.neighbor, n_sample = n.area * n.sampling,
                    locs = all.locs, nearind = nearind, nearind_sample = nearind_sample, C_nei = C_nei, C_site_nei = C_site_nei,
                    sC_nei = sC_nei_all, sC_site_nei = sC_site_nei_all)
    if (n.point > 0){
      X_point <- model.matrix(geo.formula, geo.data@data)
      Y_point <- data.frame(geo.data@data[, y.point])
      names(Y_point) <- y.point

      dat.all <- c(dat.all,
                   geo.formula = list(geo.formula),
                   p_point = list(p.point),
                   n_point_var = list(n_point_var),
                   Y_point = list(Y_point),
                   X_point = list(X_point))
    } else {
      dat.all <- c(dat.all,
                   n_point_var = list(n_point_var))
    }
    if (n.area > 0){
      X_area <- model.matrix(lattice.formula, lattice.data@data)
      Y_area <- data.frame(lattice.data@data[, y.area])
      names(Y_area) <- y.area

      dat.all <- c(dat.all,
                   lattice.formula = list(lattice.formula),
                   p_area = list(p.area),
                   n_area_var = list(n_area_var),
                   Y_area = list(Y_area),
                   X_area = list(X_area),
                   A1 = list(A1))
    } else {
      dat.all <- c(dat.all,
                   n_area_var = list(n_area_var))
    }
    if (n.grid > 0){
      Y_pp <- if (inherits(lgcp.grid, "data.frame")){
        data.frame(lgcp.grid$number)
      } else {sapply(lgcp.grid, function(x) x$number)}

      dat.all <- c(dat.all,
                   n_pp_var = list(n_pp_var),
                   Y_pp = list(Y_pp),
                   area = list(cell.area),
                   grd_lrg = list(grd_lrg))
    } else {
      dat.all <- c(dat.all,
                   n_pp_var = list(0))
    }
    class(dat.all) <- "dstan"
    if (dat.all$n_point + dat.all$n_sample + dat.all$n_grid > 1000) print("NOTE: the number of locations to be modelled is large, please expect the computations of spatial fusion modelling to last for > 1 day. Set 'method' to 'INLA' for faster computation.")
  }
  return(dat.all)
}
