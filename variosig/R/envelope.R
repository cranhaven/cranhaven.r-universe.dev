envelope <- function(vario, ...){
  if (!class(vario)[1] %in% c("gstatVariogram","variogram")) stop(
"The method 'envelope' must be applied to an object of class either 'gstatVariogram' or'variogram'")
  UseMethod("envelope",vario)
}

envelope.gstatVariogram <- function(vario, data, locations = coordinates(data), formula = NULL,
                                    cluster = FALSE, n.cluster = NULL,
                                    nsim = 999, conf.level = 0.95, save.sim = FALSE, ...) {
  dots <- list(...)
  variogramDefault <- vario
  if (cluster & is.null(n.cluster)) stop("The number of clusters is not specified.")
  if (!is.null(formula)){
    if (inherits(formula, "formula")){
      dataValues <- lm(formula, data = data)$residuals
      dots$object <- NULL
    }}

  simulation <- list()

  if (cluster){
    simulation$ID <- as.factor(kmeans(locations, centers = n.cluster)$cluster)
  } else {
    simulation$ID <- rep(1, length(dataValues))
  }

  dat <- data.frame(dataValues, locations, ID = simulation$ID)
  dat <- dat[order(dat$ID), ]

  simulation$data <- lapply(1:nsim, function(i)
    do.call(rbind, lapply(split(dat, dat$ID), function(x) x[sample(nrow(x)),])))

  simulation$variogram <- cbind(variogramDefault$gamma, sapply(1:nsim, function(i){
      data.temp <- data.frame(simulation$data[[i]][,c(2:3)], dat[,1])
      names(data.temp) <- c("x","y","residuals")
      sp::coordinates(data.temp) = ~ x + y
      gstat::variogram(residuals~1, locations, data.temp, ...)$gamma
    }))

  simulation$upper <- apply(simulation$variogram, 1, quantile, probs = 1-(1-conf.level)/2)
  simulation$lower <- apply(simulation$variogram, 1, quantile, probs = (1-conf.level)/2)
  simulation$data.values <- dataValues
  simulation$variogram0 <- variogramDefault
  simulation$conf.level <- conf.level
  if (!save.sim){simulation$data <- NULL}
  return(simulation)
}

envelope.variogram <- function(vario, data, locations = data$coords, trend = NULL,
                               cluster = FALSE, n.cluster = NULL,
                               nsim = 999, conf.level = 0.95, save.sim = FALSE, ...) {
  if (requireNamespace("geoR", quietly = TRUE)) {
    dots <- list(...)
    dataValue <- data$data
    variogramDefault <- vario
    if (cluster & is.null(n.cluster)) stop("The number of clusters is not specified.")
    if (!is.null(trend)){
      dataValues <- lm(dataValue ~ trend-1)$residuals
    } else {
      dataValues <- dataValue
    }
    simulation <- list()

    if (cluster){
      simulation$ID <- as.factor(stats::kmeans(locations, centers = n.cluster)$cluster)
    } else {
      simulation$ID <- rep(1, length(dataValues))
    }

    dat <- data.frame(dataValue, locations, ID = simulation$ID)
    dat <- dat[order(dat$ID), ]

    simulation$data <- lapply(1:nsim, function(i)
      do.call(rbind, lapply(split(dat, dat$ID), function(x) x[sample(nrow(x)),])))

    simulation$variogram <- cbind(variogramDefault$v, sapply(1:nsim, function(i){
      data.temp <- data.frame(simulation$data[[i]][,c(2:3)], dat[,1])
      data.temp <- geoR::as.geodata(data.temp, coords.col = 1:2, data.col = 3)
      geoR::variog(data.temp, messages = FALSE, ...)$v
    }))

    simulation$upper <- apply(simulation$variogram, 1, quantile, probs = 1-(1-conf.level)/2)
    simulation$lower <- apply(simulation$variogram, 1, quantile, probs = (1-conf.level)/2)
    simulation$data.values <- dataValues
    simulation$variogram0 <- variogramDefault
    simulation$conf.level <- conf.level
    if (!save.sim){simulation$data <- NULL}
    return(simulation)
  } else {
    stop("The 'geoR' package is not installed, please install it first to use the envelope() function with a 'variogram' class.")
  }
}

