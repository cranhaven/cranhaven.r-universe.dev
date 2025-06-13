#' @include colorpatch_methods.R
NULL

#' Computes the distance of to colors within a certain colorspace
#'
#' @param x First color to be compared
#' @param y Second color to be compared
#' @param color.space Defaults to "LAB" (can be anything within the colorspace package) see [colorspace::color-class]
#'
#' @return L2 distance of the two colors within the given coordinate space
#' 
#' @importClassesFrom colorspace "color"
#' @import methods
#' 
#' @export
#'
#' @examples
#' library(colorspace)
#' library(colorpatch)
#' DistColor(sRGB(0.1,0.5,0), sRGB(0.2,0.7,1.0), "LUV")
#' @seealso [colorspace::color-class], [DistColorFun()]
DistColor <- function(x, y, color.space = "LAB") {
  x.unif <- colorspace::coords(methods::as(x, color.space))
  y.unif <- colorspace::coords(methods::as(y, color.space))
  return(sqrt(sum( (x.unif - y.unif) ^ 2 )))
}

#' Creates a color distance function
#'
#' @param color.space Color space to be used (see [colorspace::color-class])
#'
#' @return A function mapping two color values of a color class [colorspace::color-class] to a numeric value.
#' @export
#'
#' @examples
#' library(colorspace)
#' library(colorpatch)
#' fn <- DistColorFun("LUV")
#' a <- sRGB(1,0,0)
#' b <- sRGB(0.8,0.1,0)
#' my.distance <- fn(a,b)
DistColorFun <- function(color.space = "LAB") {
  return(function(x, y) {
    DistColor(x, y, color.space)
    })
}

#' Finds a uniform color sequence within a non-uniform palette by subsampling that palette
#'
#' @param P input color palette (must be a class derived from [colorspace::color-class])
#' @param n.out number of output colors (must be less than length(P))
#' @param reverse shall the searching be performed from the end of the palette to the beginning
#' @param delta the perceptual difference to be achieved between two adjecent colors
#' @param col.dist.fun function mapping two colors to a numeric distance
#'
#' @return a optimized palette (sub-set of P)
#' @export
FindUniformSequence <- function(P, n.out, reverse = FALSE, delta = NULL,
                                col.dist.fun = DistColorFun("LAB")) {
  n.in <- length(P)
  if (n.out > n.in) {
    stop("n_out must be smaller than nrow(P)")
  }

  if (is.null(delta)) {
    delta <- col.dist.fun(P[1], P[n.in]) / (n.out - 1)
  }
  if (reverse) {
    # search from the end
    col.index <- c(n.in)
    col <- P[n.in]
    k <- n.in
    while (k > 0 && length(col.index) < n.out) {
      # compute all distances to color c
      d <- apply.color(P[1:k], function(x) col.dist.fun(x, col))

      # find minimum
      min.index <- which.min(abs(d - delta))
      col <- P[min.index]
      col.index <- c(min.index, col.index)
      k <- min.index - 1
    }
  } else {
    # search from the beginning
    col.index <- c(1)
    col <- P[1]
    k <- 2

    while (k <= n.in && length(col.index) < n.out) {
      # compute all distances to color c
      d <- apply.color(P[k:n.in], function(x) col.dist.fun(x, col))

      # find minimum
      min.index <- which.min(abs(d - delta))
      min.index <- min.index + k - 1
      col <- P[min.index]
      col.index <- c(col.index, min.index)
      k <- min.index + 1
    }
  }

  if (length(col.index) < n.out) {
    warning("number of output colors is smaller than the required number")
  }
  return(P[col.index])
}

#' Creates a linear color space between two colors
#'
#' @param color1 the first color (must be of the class [colorspace::color-class])
#' @param color2 the second color (must be of the class [colorspace::color-class])
#' @param n.out number of output colors
#'
#' @return a palette
#' @export
#'
#' @examples
#' library(colorspace)
#' library(colorpatch)
#' pal <- LinColorSpace(sRGB(0,1,0), sRGB(0,0.1,0), 32)
#' pal <- append(pal, sRGB(0,0,0))
#' pal <- append(pal, LinColorSpace(sRGB(0.1,0,0), sRGB(1,0,0), 32))
#' PlotUniformity(pal)
#' print(pal)
LinColorSpace <- function(color1, color2, n.out) {
  alpha <- seq(0, 1, length.out = n.out)
  return(colorspace::mixcolor(alpha, color1, color2))
}


#' Optimizes a bicolor palette
#'
#' @param neg.col.min color representing the negative mininum value
#' @param neg.col.max color representing the negative maximum value
#' @param pos.col.min color for the positive minimum value
#' @param pos.col.max color representing the positive maximum value
#' @param center.col center color which maps to 0 (default: black)
#' @param n.out size of each half-palette
#' @param oversampling the oversampling rate
#' @param col.dist.fun color distance function (default: DistColorFun("LAB")) for 
#' optimizing the palette
#' @param reverse shall the palette be searched starting from the minimum color to the maximum
#' (`reverse=FALSE`) or vice versa - defaults to `FALSE`
#'
#' @return bicolor palette
#' @export
#' @examples
#' pal <- OptimizeBiColor(n.out = 8, oversampling = 32)
#' PlotUniformity(pal)
OptimizeBiColor <- function(neg.col.min = colorspace::sRGB(0, 0.01, 0),
                           neg.col.max = colorspace::sRGB(0, 1, 0),
                           pos.col.min = colorspace::sRGB(0.01, 0, 0),
                           pos.col.max = colorspace::sRGB(1, 0, 0),
                           center.col = colorspace::sRGB(0, 0, 0),
                           n.out = 64,
                           oversampling = 128,
                           col.dist.fun = DistColorFun("LAB"),
                           reverse = FALSE) {
  nin <- oversampling * n.out
  neg <- LinColorSpace(neg.col.min, neg.col.max, nin)
  pos <- LinColorSpace(pos.col.min, pos.col.max, nin)
  delta <- min(c(col.dist.fun(neg[1], neg[nin]),
                col.dist.fun(pos[1], pos[nin]))) / (nin - 1)
  message("Optimizing negative side")
  unif.neg <- FindUniformSequence(neg, n.out,
                                  delta = delta, 
                                  col.dist.fun = col.dist.fun,
                                  reverse = reverse)
  message("Optimizing positive side")
  unif.pos <- FindUniformSequence(pos, n.out,
                                  delta = delta, 
                                  col.dist.fun = col.dist.fun,
                                  reverse = reverse)
  pal <- append(rev(unif.neg), center.col)
  pal <- append(pal, unif.pos)
  return(pal)
}

#' Creates color palettes and saves them as files
#'
#' @param col.dist.fun Color distance function.
#' @param ... Additional arguments forwarded to [colorpatch::OptimizeBiColor()].
#' @return Nothing - this function is used for its side effects (creating files in data).
#' @export
GeneratePalettes <- function (col.dist.fun = DistColorFun("LAB"), ...) {
  eta <- 0.01
  utils::timestamp()
  message("Generating green/black/red RGB")
  GreenRedRGB <- LinColorSpace(colorspace::sRGB(0, 1, 0), colorspace::sRGB(0, eta, 0), 64)
  GreenRedRGB <- append(GreenRedRGB, colorspace::sRGB(0, 0, 0))
  GreenRedRGB <- append(GreenRedRGB, 
                        LinColorSpace(colorspace::sRGB(eta, 0, 0), colorspace::sRGB(1, 0, 0), 64))
  save(GreenRedRGB, file = "./data/GreenRedRGB.RData")
  
  utils::timestamp()
  message("Generating green/black/red LAB")
  OptimGreenRedLAB <- OptimizeBiColor(
                           neg.col.min = colorspace::sRGB(0, eta, 0),
                           neg.col.max = colorspace::sRGB(0, 1, 0),
                           pos.col.min = colorspace::sRGB(eta, 0, 0),
                           pos.col.max = colorspace::sRGB(1, 0, 0),
                           center.col = colorspace::sRGB(0, 0, 0),
                           col.dist.fun = col.dist.fun)
  save(OptimGreenRedLAB, file="./data/OptimGreenRedLAB.RData")
  
  utils::timestamp()
  message("Generating blue/black/yellow LAB")
  OptimBlueYellowLAB <- OptimizeBiColor(
                           neg.col.min = colorspace::sRGB(0, 0, eta),
                           neg.col.max = colorspace::sRGB(0, 0, 1),
                           pos.col.min = colorspace::sRGB(0, eta, eta),
                           pos.col.max = colorspace::sRGB(0, 1, 1),
                           center.col = colorspace::sRGB(0, 0, 0),
                           col.dist.fun = col.dist.fun)
  save(OptimBlueYellowLAB, file = "./data/OptimBlueYellowLAB.RData")
  utils::timestamp()
  message("finished.")
}

Quantize <- function(u, n) {
  return(as.integer(floor(u*n+0.5)))
}

ColorHexFun <- function(cols) {
  n <- length(cols)
  n2 <- floor((n - 1) / 2)
  return(function(x) {
    x <- max(min(1,x),-1)
    return(cols[n2 + 1 + Quantize(x, n2)])
  })
}

#' Creates a color mapping function
#'
#' @param pal the color palette 
#' @param xmin minimum value to be mapped to the first entry of the palette
#' @param xmax maximum value to be mapped to the last entry of the palette
#' @param coerce.fun the color coercing function (e.g. for ggplot2 [colorspace::hex()] is recommended)
#'
#' @return a function mapping a value to a color
#' @export
#'
#' @examples
#' data("OptimGreenRedLAB")
#' fn <- ColorRgbFun(OptimGreenRedLAB)
ColorRgbFun <- function(pal, xmin = -1, xmax = 1, coerce.fun = colorspace::hex) {
  n <- length(pal)
  x <- seq(xmin, xmax, length.out = n)
  return(function(value) {
    yy <- apply(colorspace::coords(methods::as(pal,"sRGB")),
                2,
                function(v) stats::approx(x, v, value, rule = 2))
    return(coerce.fun(colorspace::sRGB(R = yy$R$y, G = yy$G$y, B = yy$B$y)))
  })
}

#' Reads a sRGB color table as CSV file
#'
#' @param file.name the color file
#'
#' @return a colorspace palette
#' @export
ReadArraySRGB <- function(file.name) {
  X <- utils::read.table(file.name)
  names(X) <- c("R", "G", "B")
  return(colorspace::sRGB(R = X$R, G = X$G, B = X$B))
}


#' Computes the perceptional distance between two neighboring colors
#'
#' @param pal the color palette
#' @param color.space color space in which the distance shall be computed (default "LAB")
#'
#' @return a vector of distances
#' @export
#'
#' @examples
#' data("OptimGreenRedLAB")
#' dd <- ColorDistance(OptimGreenRedLAB)
ColorDistance <- function(pal, color.space = "LAB") {
  labs <- methods::as(pal, color.space)
  coords <- colorspace::coords(labs)
  sqrt(apply(diff(coords) ^ 2, 1, sum))
}

ColorDistance2 <- function(color1, color2, color.space = "LAB") {
  u1 <- colorspace::coords(methods::as(color1, color.space))
  u2 <- colorspace::coords(methods::as(color2, color.space))
  sqrt(sum((u1 - u2) ^ 2))
}

#' Plots the uniformity of a color palette
#'
#' @param pal A colorspace palette
#' @param color.space the color space (see [colorspace::color-class])
#'
#' @return a ggplot instance
#' @export
#'
#' @examples
#' data("OptimGreenRedLAB")
#' p <- PlotUniformity(OptimGreenRedLAB)
#' plot(p)
PlotUniformity <- function(pal, color.space = "LAB") {
  n <- (length(pal) - 1) / 2
  dd <- ColorDistance(pal, color.space)
  # The following is necessary for supressing the "no visible binding for global variable" message
  # from R CHECK.
  # See http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  index <- distance <- side <- NULL
  df <- data.frame(index = c(-n:-1,1:n),
                   distance = dd,
                   side = as.factor(c(rep(-1, n), rep(+1, n))))
  
  # The following is necessary for supressing the "no visible binding for global variable" message
  # from R CHECK.
  # See http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  index <- distance <- color <- NULL
  ggplot2::ggplot(df, 
                  ggplot2::aes(index, distance, color = side)) + 
    ggplot2::geom_line() + 
    ggplot2::geom_point()
}

#' Computes the symmetry of a given bi-variate color palette
#'
#' @param pal A two-sided input palette [colorspace::color-class]
#' @param color.space Color space where the distances shall be computed (default "LAB")
#'
#' @return a data frame with index, side (pos/neg) and distance
#' @export
#'
#' @examples
#' data("OptimGreenRedLAB")
#' df <- ComputeSymmetry(OptimGreenRedLAB)
#' print(df)
ComputeSymmetry <- function(pal, color.space = "LAB") {
  n <- (length(pal) - 1) / 2
  c0 <- pal[n + 1]
  pal <- append(pal[1:n],pal[(n+2):length(pal)])
  dd <- unlist(lapply(methods::as(pal, "list"), 
                  function(x) {
                    ColorDistance2(x, c0, color.space = color.space)
                  }))
  data.frame(index = c(n:1,1:n),
             side = as.factor(c(rep("neg", n), rep("pos", n))),
             distance = dd)
}
  
#' Plots the symmetry of a bivariate color scale
#'
#' @param pal A two-sided input palette [colorspace::color-class]
#' @param color.space Color space where the distances shall be computed (default "LAB")
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' data("OptimGreenRedLAB")
#' PlotSymmetry(OptimGreenRedLAB)
PlotSymmetry <- function(pal, color.space = "LAB") {
  df <- ComputeSymmetry(pal, color.space = color.space)
  # The following is necessary for supressing the "no visible binding for global variable" message
  # from R CHECK.
  # See http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  index <- distance <- side <- NULL
  ggplot2::ggplot(df, 
                  ggplot2::aes(x = index, y = distance, color = side)) + 
    ggplot2::geom_line() + 
    ggplot2::geom_point()
}

#' Linear interpolation within a [colorspace::color-class] palette
#'
#' This function can be used together with [ggplot2] for mapping values onto [colorspace::color-class] palettes.
#' The color is then coerced with `coerce.fun`.
#' 
#' @param pal The input palette (must be of class [colorspace::color-class])
#' @param xmin minimum of the numeric range to be mapped onto `pal`
#' @param xmax maximum of the numeric range to be mapped onto `pal`
#' @param coerce.fun each color will be coerced by this function (defaults to [colorspace::hex()])
#'
#' @return A function mapping a numeric value `value` onto a color value.
#' @export
#'
#' @examples
#' library(colorspace)
#' library(colorpatch)
#' data("OptimGreenRedLAB")
#' fn <- InterpolateColorFun(OptimGreenRedLAB)
#' cols <- fn(seq(-1, 1, by = 0.1))
#' specplot(cols)
InterpolateColorFun <- function(pal, xmin = -1, xmax = +1, coerce.fun = colorspace::hex) {
  stopifnot(methods::isClass(class(pal),"color"))
  n <- length(pal)
  n2 <- floor((n - 1) / 2)
  x <- seq(xmin, xmax, length.out = n)
  return (function(value) {
    yy <- apply(colorspace::coords(pal),
                2,
                function(v) stats::approx(x, v, value, rule = 2))
    return(coerce.fun(colorspace::sRGB(R = yy$R$y, G = yy$G$y, B = yy$B$y)))
  })
}

#' Creates a color function mapping (ratio, conf) tuples to a single color
#'
#' @param palette name of the palette (see [data()]) - defaults to "OptimGreenRedLAB"
#'
#' @return A function mapping (ratio, conf) to a color.
#' @export
#'
#' @examples
#' fn <- ColorPatchColorFun("OptimBlueYelloLAB")
ColorPatchColorFun <- function(palette = "OptimGreenRedLAB") {
  env <- new.env()
  utils::data(list = palette, package = "colorpatch", envir = env)
  fn <- InterpolateColorFun(pal = env[[palette]])
  return (function(ratio, conf) fn(ratio))
}

#' Creates a size function mapping (ratio, conf) to a single color
#'
#' @param type defaults to "linear"
#'
#' @return A function mapping (ratio, conf) to a size.
#' @export
#'
ColorPatchSizeFun <- function(type = "linear") {
  if (type == "linear") {
    return(function(ratio, conf) conf)
  }
  stop("Invalid type in ColorpatchSizeFun: ", type)
}

#' Creates a color function mapping ratio/conf values to a HSV colorspace
#'
#' @param coerce.fn coerces each HSV color with this function (defaults [colorspace::hex()])
#' @param hue.offset hue offset (defaults to 60)
#' @param hue.scale hue scale (defaults to 60)
#' @param saturation HSV saturation (defaults to 1)
#'
#' @return a color mapping function (ratio,conf) -> color
#' @export
#'
HsvColorFun <- function(coerce.fn = colorspace::hex, hue.offset = 60, hue.scale = -60, saturation = 1) {
  return (function(ratio,conf) {
    return (coerce.fn(colorspace::HSV(H = (hue.offset + hue.scale * ratio) %% 360,
                                      S = saturation,
                                      V = conf)))
  })
}

#' Creates a size function mapping ratio/conf to a patch size for bicolorings
#'
#' @return a size mapping function (ratio,conf) -> size
#' @export
#'
HsvSizeFun <- function() {
  return (function(ratio,conf) {
    1
  })
}

#' Transforms a ratio/conf data set to a ggplot dataframe
#'
#' @param dat must be a list with two matrices `ratio` and `conf`
#'
#' @return a data frame
#' @export
ToDataFrame <- function(dat) {
  stopifnot(all(dim(dat$ratio) == dim(dat$conf)))
  nrows <- nrow(dat$ratio)
  ncols <- ncol(dat$ratio)
  data.frame(
    ratio = as.vector(dat$ratio),
    conf = as.vector(dat$conf),
    x = as.vector(sapply(1:ncols,function(x) rep(x, nrows))),
    y = rep(1:nrows,ncols) 
  )
}

RandomDataUniform <- function(nrow = 30, ncol = 12) {
  n <- nrow*ncol
  ratio <- matrix(stats::runif(n, -1, 1), nrow = nrow, ncol = ncol)
  conf <- matrix(stats::runif(n, 0, 1), nrow = nrow, ncol = ncol)
  return(list(ratio = ratio, conf = conf))
}

RandomDataNorm <- function(nrow = 30, ncol = 12) {
  n <- nrow * ncol
  ratio <- matrix(stats::rnorm(n) * 2 - 1, nrow = nrow, ncol = ncol)
  conf <- matrix(stats::rnorm(n) ^ 2, nrow = nrow, ncol = ncol)

  ratio <- ratio / max(max(ratio), max(-ratio))
  conf <- conf / max(conf)
  return(list(ratio = ratio, conf = conf))
}

#' Creates demonstration data of the colorpatch package
#'
#' @param nrow number of rows (default 30)
#' @param ncol number of columns (default 12)
#'
#' @return the data set
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(colorpatch)
#' dat <- CreateExampleData()
#' df <- ToDataFrame(dat)
#' p <- ggplot(df, aes(x = x, y = y, ratio = ratio, conf = conf)) 
#' p <- p + theme_colorpatch() + coord_fixed(ratio = 1)  + stat_colorpatch()
#' plot(p)
CreateExampleData <- function(nrow = 30, ncol = 12) {
  dat <- RandomDataNorm(nrow, ncol)
  return(dat)
}

#' Creates clustered random data
#'
#' @param nrow Number of rows (default: 30)
#' @param ncol Number of columns (default: 12)
#' @param nrow.clusters Number of row cluster
#' @param ncol.clusters Number of column clusters (default: 2)
#' @param alpha Scaling facor (default: 4)
#'
#' @return A data set with $ratio and $conf values
#' @export
CreateClusteredData <- function(nrow = 30, 
                                ncol = 12,
                                nrow.clusters = 2,
                                ncol.clusters = 2,
                                alpha = 4) {
  rowGroup <- sample(1:nrow.clusters, nrow, replace = TRUE)
  rowGroup <- sort(rowGroup)
  colGroup <- sample(1:ncol.clusters, ncol, replace = TRUE)
  colGroup <- sort(colGroup)
  groupRow <- as.vector(sample(-1:1, nrow.clusters, replace = TRUE))
  groupCol <- as.vector(sample(-1:1, ncol.clusters, replace = TRUE))
  regulationGroup <- groupRow %*% matrix(1, ncol = ncol.clusters) +
                     matrix(1, nrow = nrow.clusters) %*% t(groupCol)
  
  regulation <- regulationGroup[rowGroup, colGroup]
  dat <- list()
  A <- matrix(stats::rlnorm(nrow*ncol), nrow = nrow, ncol = ncol)
  B <- matrix(stats::rlnorm(nrow*ncol), nrow = nrow, ncol = ncol)
  idx <- which(regulation == +1)
  A[idx] <- alpha * A[idx]
  idx <- which(regulation == -1)
  B[idx] <- alpha * B[idx]
  
  rowConf <- matrix(stats::runif(nrow, min = 0.05), nrow = nrow, ncol = ncol)
  colConf <- matrix(stats::runif(ncol, min = 0.05), nrow = nrow, ncol = ncol, byrow = TRUE)
  
  A <- A * rowConf * colConf
  B <- B * rowConf * colConf
  
  dat$ratio <- log2(A / B)
  dat$conf <-  abs(0.5 * log2(A * B))
  dat$regulation <- regulation
  dat$regulationGroup <- regulationGroup
  dat$rowGroup <- rowGroup
  dat$colGroup <- colGroup
  return(dat)
}

#' Orders rows and column distances with [stats::hclust()]
#'
#' @param row.dist row distances
#' @param col.dist column distances
#' @param ... optional parameters forwarded to the [stats::hclust()] function
#' @return a list with irow and icol containing the orders of rows and columns
#' @export
#'
OrderDataHclust <- function(row.dist, col.dist, ...) {
  irow <- stats::hclust(row.dist)$order
  icol <- stats::hclust(col.dist)$order
  list(irow = irow, icol = icol)
}

GetTspDistances <- function(dist, tour) {
  stopifnot(methods::is(dist, "dist") || methods::is(dist, "matrix"))
  stopifnot(methods::is(tour, "TOUR") || methods::is(tour, "integer"))
  dd <- as.matrix(dist)
  idx <- as.integer(tour)
  if (methods::is(tour, "TOUR")) {
    idx <- c(idx, idx[1])
  }
  n <- length(idx)
  unlist(mapply(function(i, j) dd[i, j],
                idx[1:(n-1)],
                idx[2:n]))
}

#' Orders a data set given a distance matrix with TSP
#'
#' @param dist distance object or distance matrix
#' @param ... extra arguments fed to [TSP::solve_TSP()]
#'
#' @return a path (vector of integers)
#' @export
OrderWithTSP <- function(dist, ...) {
  tsp <- TSP::TSP(dist)
  tour <- TSP::solve_TSP(tsp, ...)
  # find the maximum cut
  d <- GetTspDistances(dist, tour)
  imax <- 1 + (which.max(d) %% length(tour))
  i <- as.integer(tour)
  TSP::cut_tour(tour, i[imax], exclude_cut = TRUE)
}

#' Orders rows and column distances with traveling salesman ordering [TSP]
#'
#' @param row.dist row distances
#' @param col.dist column distances
#' @param ... optional parameters fed to the [TSP::solve_TSP()] function
#' @return  a list with irow and icol containing the orders of rows and columns
#' @export
#'
OrderDataTSP <- function(row.dist, col.dist, ...) {
  list(irow = OrderWithTSP(row.dist, ...),
       icol = OrderWithTSP(col.dist, ...))
}


#' Orders rows and columns of data.
#'
#' @param dat Ratio data
#' @param orderFn Ordering method (default: [OrderDataHclust])
#' @param distFn Distance function (Idefault [stats::dist])
#' @return ordered data
#' @export
#'
OrderData <- function(dat, orderFn = OrderDataHclust, distFn = stats::dist) {
  row.dist <- distFn(dat$ratio)
  col.dist <- distFn(t(dat$ratio))
  ord <- orderFn(row.dist, col.dist)
  dat$ratio <- dat$ratio[ord$irow, ord$icol]
  dat$conf <- dat$conf[ord$irow, ord$icol]
  dat$irow <- ord$irow
  dat$icol <- ord$icol
  dat$row.dist <- row.dist
  dat$col.dist <- col.dist
  return(dat)
}
