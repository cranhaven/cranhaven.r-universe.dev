fusion.dinla <- function(data, n.latent = 1, bans = 0, pp.offset, verbose = FALSE, alpha = 3/2,
                         prior.range, prior.sigma, prior.args, mesh.locs, mesh.max.edge, mesh.args, inla.args, ...){


  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' needed for this function to work. Please install it first, visit https://www.r-inla.org/download-install",
         call. = FALSE)
  }

  # INLA:::inla.dynload.workaround()
  # if (Sys.info()["sysname"] == "Linux") {
  #   mkl = INLA::inla.getOption("mkl")
  #   if (is.null(mkl))
  #     mkl = FALSE
  #   d = dirname(system.file(paste("bin/linux/", ifelse(.Machine$sizeof.pointer == 4, "32", "64"),  "bit/inla.", "run", sep = ""), package = "INLA"))
  #   INLA::inla.setOption(inla.call = paste(d, "/inla.", if (mkl)
  #     "mkl."
  #     else "", "static", sep = ""))
  #   INLA::inla.setOption(fmesher.call = paste(d, "/fmesher.static", sep = ""))
  # }

  n.var <- data$n_point_var + data$n_area_var + data$n_pp_var

  # checks  -----------------------------------------------------------------
  if (identical(bans, 0)) bans <- matrix(0, n.var, n.latent)


  if (data$n_point > 0){
    for (i in 1:data$n_point_var){
      data[[paste0("Y_point",i)]] <- data$Y_point[,i]
    }
  }
  if (data$n_area > 0){
    for (i in 1:data$n_area_var){
      data[[paste0("Y_area",i)]] <- data$Y_area[,i]
    }
  }

  if (missing("mesh.locs")){
    stop("mesh.locs must be provided or loc.domain must be provided in the named list for mesh.args")
  } else {
    if (!any(class(mesh.locs) %in% c("matrix", "SpatialPoints", "SpatialPointsDataFrame"))) stop("mesh.locs must be a class of matrix, SpatialPoints or SpatialPointsDataFrame")
  }

  if (missing("mesh.args")) mesh.args <- NULL
  if (missing("prior.args")) prior.args <- NULL
  if (missing("inla.args")) inla.args <- NULL

  if (data$n_grid > 0){
    if (!missing("pp.offset")){
      if (length(pp.offset) != data$n_pp_var) stop("the length of pp.offset does not match the number of point pattern variables")
    } else {
      pp.offset <- rep(1, times = data$n_pp_var)
    }
  }

# mesh and prior ----------------------------------------------------------

  X_point <- data$X_point
  locs_point <- data$locs_point
  X_area <- data$X_area


  mesh.point <- do.call(INLA::inla.mesh.2d, args = c(list(loc = mesh.locs, max.edge = mesh.max.edge), mesh.args))

  spde = do.call(INLA::inla.spde2.pcmatern, args = c(list(mesh.point, alpha = alpha, prior.range = prior.range, prior.sigma = prior.sigma), prior.args))

# prepare data ------------------------------------------------------------

index.bans <- t(outer(1:n.latent, 1:n.var, FUN = "paste0"))

  if (data$n_point > 0){
    A.point = INLA::inla.spde.make.A(mesh = mesh.point, loc = locs_point)
    Xcov <- as.matrix(X_point)
    for (i in 1:data$n_point_var){
      temp <- rep("NA", n.var)
      temp[i] <- paste0("data$Y_point",i)
      y.list.point <- eval(parse(text = paste0("cbind(",paste0(temp, collapse = ", "),")")))
      effect.list.point <- eval(parse(text = paste0("list(",
                                                    paste0("s", index.bans[i,
                                                                           which(bans[i,] == 0)],"= 1:mesh.point$n", collapse = ", "),
                                                    ", beta_p", i, " = Xcov)")))
      A.list.point <- eval(parse(text = paste0("list(",
                                               paste0(rep("A.point", length(which(bans[i,] == 0))), collapse = ","), ", 1)")))

      assign(paste0("stack.point", i),
             INLA::inla.stack(tag = 'point', data = list(y = y.list.point), effects = effect.list.point, A = A.list.point))
    }
  }

  if (data$n_area > 0){
    mesh.locs <- SpatialPoints(mesh.point$loc, proj4string = data$poly@proj4string)
    # mesh coordinates within polygons = mesh.in, polygon indices with each mesh = mesh.in.id
    mesh.in <- mesh.point$loc[as.vector(which(!is.na(over(mesh.locs, data$poly)[,1]))),]
    mesh.in.locs <- SpatialPoints(mesh.in, proj4string = data$poly@proj4string)
    mesh.in.id <- as.numeric(sapply(over(mesh.in.locs, data$poly, returnList = TRUE), rownames))
    A.area = INLA::inla.spde.make.A(mesh = mesh.point, loc = mesh.in, block = mesh.in.id, block.rescale = "sum")
    Xcov.area <- as.matrix(X_area)

    for (i in 1:data$n_area_var){
      temp <- rep("NA", n.var)
      temp[data$n_point_var + i] <- paste0("data$Y_area",i)
      y.list.area <- eval(parse(text = paste0("cbind(",paste0(temp, collapse = ", "),")")))
      effect.list.area <- eval(parse(text = paste0("list(",
                                                   paste0("s",index.bans[data$n_point_var + i,
                                                                         which(bans[data$n_point_var + i,] == 0)], "= 1:mesh.point$n", collapse = ", "),
                                                   ", beta_a",i," = Xcov.area)")))
      A.list.area <- eval(parse(text = paste0("list(",
                                              paste0(rep("A.area", length(which(bans[data$n_point_var + i,] == 0))), collapse = ","), ", 1)")))

      assign(paste0("stack.area", i),
             INLA::inla.stack(tag = 'area', data = list(y = y.list.area), effects = effect.list.area, A = A.list.area))
    }
  }

  if (data$n_grid > 0){

    # mesh coordinates within polygons = mesh.in set to 0
    temp.y <- rep(NA, times = mesh.point$n)
    temp.y[!is.na(over(SpatialPoints(mesh.point$loc, proj4string = data$domain@proj4string), data$domain))] <- 0
    ForOffset <- list()

    for (i in 1:data$n_pp_var){
      A.pp <- rbind(diag(rep(1, mesh.point$n)), INLA::inla.spde.make.A(mesh = mesh.point,
                                                                       loc = eval(parse(text = paste0("data$locs_pp",i)))))
      assign(paste0("Y_pp",i), c(temp.y, rep(1, times = nrow(eval(parse(text = paste0("data$locs_pp",i)))))))

      temp <- rep("NA", n.var)
      temp[data$n_point_var + data$n_area_var + i] <- paste0("Y_pp",i)
      y.list.pp <- eval(parse(text = paste0("cbind(",paste0(temp, collapse = ", "),")")))
      effect.list.pp <- eval(parse(text = paste0("c(list(",
                                                   paste0("s", index.bans[data$n_point_var + data$n_area_var + i,
                                                                          which(bans[data$n_point_var + data$n_area_var + i,] == 0)],
                                                          "= 1:mesh.point$n", collapse = ", "),")",
                                                   paste0(", list(intercept = rep(1, length(Y_pp",i,"))))"))))
      A.list.pp <- eval(parse(text = paste0("list(",
                                              paste0(rep("A.pp", length(which(bans[data$n_point_var + data$n_area_var + i,] == 0)
                                                                               )), collapse = ","), ", 1)")))

      assign(paste0("stack.pp", i),
             stack.pp <- INLA::inla.stack(tag = "pp", data = list(y = y.list.pp), effects = effect.list.pp, A = A.list.pp))


      # a vector of the areas of the Voronoi polygons, and it is 0 on the cases (because they are points)
      e.pp <- c(spde$param.inla$M0@x, rep(0, nrow(eval(parse(text = paste0("data$locs_pp",i))))))
      ForOffset[[i]] <- c(!is.na(temp.y), rep(0, times = nrow(eval(parse(text = paste0("data$locs_pp",i)))))) * pp.offset * e.pp
    }
  }

  bans.copy <- bans
  index.ori <- character(n.latent)
  for (i in 1:n.latent){
    index.ori[i] <- index.bans[[which(bans.copy[,i] == 0)[1],i]]
    bans.copy[which(bans.copy[,i] == 0)[1],i] <- 1
  }

  # spatial component copies
  index.copy <- index.bans[which(bans.copy == 0, arr.ind = T)]
  # corresponding original components
  index.copy.o <- as.numeric(sapply(index.copy, function(x) substr(x,1,1)))

  assign("formula", eval(parse(text = paste0("y ~ -1 + ", if(data$n_point_var>0){paste0(paste0("beta_p",1:data$n_point_var, collapse = " + "), " + ")},
                                             if(data$n_area_var>0){paste0(paste0("beta_a",1:data$n_area_var, collapse = " + "), " + ")},
                                             paste0("f(s",index.ori,", model = spde)", collapse = " + "),"+",
                                             paste0("f(s",index.copy,", copy ='s", index.ori[index.copy.o], "', fixed = FALSE)", collapse = " + "))))
  )

  stack.full <- eval(parse(text = paste0('INLA::inla.stack(',
                                         paste(
                                           c(if(data$n_point_var>0){
                                             paste0("stack.point",1:data$n_point_var, collapse = ", ")},
                                           if(data$n_area_var>0){
                                             paste0("stack.area",1:data$n_area_var, collapse = ", ")},
                                           if(data$n_pp_var>0){
                                             paste0("stack.pp",1:data$n_pp_var, collapse = ", ")}), collapse = ","),')')))

  E <- c(rep(0, data$n_point * data$n_point_var), rep(1, data$n_area * data$n_area_var), if(data$n_pp_var){unlist(ForOffset)})

  data$distributions[data$distributions == "bernoulli"] <- "binomial" # default INLA sets Ntrials to one

# main modeling -----------------------------------------------------------

  result <- do.call(INLA::inla, args = c(list(formula, data = INLA::inla.stack.data(stack.full),
                 control.predictor = list(A = INLA::inla.stack.A(stack.full), compute = T),
                 family = c(data$distributions, rep("poisson", data$n_pp_var)),
                 E = if(data$n_pp_var>0){E}else{NULL},
                 verbose = verbose), inla.args))
  data$n_w <- n.latent
  out <- list(model = result, mesh = mesh.point, data = data, priors = list(prior.range = prior.range, prior.sigma = prior.sigma))
  class(out) <- "fusionModel"
  return(out)
}
