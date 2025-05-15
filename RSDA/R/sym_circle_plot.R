#' Symbolic Circle of Correlations
#' @name sym.circle.plot
#' @aliases sym.circle.plot
#' @author Oldemar Rodriguez Rojas
#' @description Plot the symbolic circle of correlations.
#' @usage sym.circle.plot(prin.corre)
#' @param prin.corre A symbolic interval data matrix with correlations between the variables and the
#' principals componets, both of interval type.
#'
#' @return Plot the symbolic circle
#' @references
#' Rodriguez O. (2012). The Duality Problem in Interval Principal Components Analysis.
#' The 3rd Workshop in Symbolic Data Analysis, Madrid.
#'
#' @examples
#' data(oils)
#' res <- sym.pca(oils, "centers")
#' sym.circle.plot(res$Sym.Prin.Correlations)
#' @keywords Symbolic Circle
#' @export
#' @importFrom graphics abline symbols plot
sym.circle.plot <- function(prin.corre) {
  v <- c(
    "green", "red", "blue", "cyan", "brown", "yellow", "pink", "purple", "orange",
    "gray"
  )
  msg <- paste("Correlation Circle")
  graphics::plot(-1.5:1.5, -1.5:1.5, type = "n", xlab = "C1", ylab = "C2", main = msg)
  graphics::abline(h = 0, lty = 3)
  graphics::abline(v = 0, lty = 3)
  graphics::symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
  c1 <- 1
  c2 <- 2
  n <- dim(prin.corre)[1]
  f <- dim(prin.corre)[2]
  CRTI <- prin.corre[, colnames(prin.corre) != "varname"]
  vars <- prin.corre$varname
  for (k in 1:n) {
    x1 <- min(CRTI[k, c1], CRTI[k, c2])
    x2 <- max(CRTI[k, c1], CRTI[k, c2])
    y1 <- min(CRTI[k, c2 + 1], CRTI[k, c2 + 2])
    y2 <- max(CRTI[k, c2 + 1], CRTI[k, c2 + 2])
    if (((x1 > 0) && (x2 > 0) && (y1 > 0) && (y2 > 0)) || ((x1 < 0) && (x2 < 0) &&
      (y1 < 0) && (y2 < 0))) {
      plotX.slice(x1, y2, x2, y1, v, vars, k)
    }
    if (((x1 < 0) && (x2 < 0) && (y1 > 0) && (y2 > 0)) || ((x1 > 0) && (x2 > 0) &&
      (y1 < 0) && (y2 < 0))) {
      plotX.slice(x1, y1, x2, y2, v, vars, k)
    }
    if ((y1 > 0) && (y2 > 0) && (x1 < 0) && (x2 > 0)) {
      plotX.slice(x1, y1, x2, y1, v, vars, k)
    }
    if ((y1 < 0) && (y2 < 0) && (x1 < 0) && (x2 > 0)) {
      plotX.slice(x1, y2, x2, y2, v, vars, k)
    }
    if ((x1 > 0) && (x2 > 0) && (y1 < 0) && (y2 > 0)) {
      plotX.slice(x1, y1, x1, y2, v, vars, k)
    }
    if ((x1 < 0) && (x2 < 0) && (y1 < 0) && (y2 > 0)) {
      plotX.slice(x2, y1, x2, y2, v, vars, k)
    }
    if ((x1 < 0) && (x2 > 0) && (y1 < 0) && (y2 > 0)) {
      plotX.slice(x2, y1, x2, y2, v, vars, k)
    }
  }
}

#' plotX.slice
#' @keywords internal
#' @importFrom  graphics text polygon
plotX.slice <- function(xx1, yy1, xx2, yy2, vv, vvars, kk) {
  radio <- sqrt(xx1^2 + yy1^2)
  a <- xx1
  b <- yy1
  cx1 <- 0
  cy1 <- 0
  if (radio <= 1) {
    cx1 <- xx1
    cy1 <- yy1
    if (xx1 > 0) {
      graphics::text(xx1 + 0.15, yy1, vvars[kk], col = vv[kk])
    } else {
      graphics::text(xx1 - 0.15, yy1, vvars[kk], col = vv[kk])
    }
  } else {
    na <- -(a / sqrt(a^2 + b^2))
    nb <- -(b / sqrt(a^2 + b^2))
    sig1 <- a * na
    sig2 <- b * nb
    if ((sig1 < 0) & (sig2 < 0)) {
      na <- -na
      nb <- -nb
    }
    cx1 <- na
    cy1 <- nb
    if (na > 0) {
      graphics::text(na + 0.15, nb, vvars[kk], col = vv[kk])
    } else {
      graphics::text(na - 0.15, nb, vvars[kk], col = vv[kk])
    }
  }
  radio <- sqrt(xx2^2 + yy2^2)
  a <- xx2
  b <- yy2
  cx2 <- 0
  cy2 <- 0
  if (radio <= 1) {
    cx2 <- xx2
    cy2 <- yy2
  } else {
    na <- -(a / sqrt(a^2 + b^2))
    nb <- -(b / sqrt(a^2 + b^2))
    sig1 <- a * na
    sig2 <- b * nb
    if ((sig1 < 0) & (sig2 < 0)) {
      na <- -na
      nb <- -nb
    }
    cx2 <- na
    cy2 <- nb
  }
  fxx <- c(0, cx1, cx2, 0)
  fyy <- c(0, cy1, cy2, 0)
  graphics::polygon(fxx, fyy, col = vv[kk], border = vv[kk])
}
