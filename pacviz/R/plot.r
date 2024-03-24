#' @title Pac-Man plotting function
#'
#' @description A method of plotting traditional Cartesian data, based on a restricted
#' radial coordinate system, while preserving the information.
#' @param x,y Numeric data
#' @param title Figure title
#' @param taxis,raxis Vector with the first entry being the axis label and the second entry being units
#' @param color1 Color value as string or rgb
#' @return Pac-Man SVM
#' @keywords visualization
#' @import plotrix circlize
#' @importFrom graphics par text mtext rect abline plot
#' @importFrom stats coef lm nls resid predict sigma rstandard median
#' @examples
#' # Generic Pac-Man plot
#' data("cars")
#' pac.plot(cars$dist,cars$speed, 'Example 1', c("Distance", "m"), c("Speed", "m/s"))
#' @export
pac.plot <- function(x,y, title, taxis, raxis, color1 = "gold") {
  # Revert margin settings back to default after exit
  oldpar <- par(mar = par()$mar, oma = par()$oma)
  on.exit(par(oldpar))
    # Maping function for the angular axis
  t <- linMap(x, 40, 320)
  # Mapping function for the radial axis
  r <- linMap(y, 1, 0)
  # gets formatted units for the angular axis
  tunit <- unit_format(taxis[2])
  # Angular axis label positions
  lp = linMap(seq.int(min(x, na.rm=TRUE), max(x, na.rm=TRUE)), 40, 320)
  # Angular axis labels
  ln = rev(seq.int(min(x, na.rm = TRUE), max(x, na.rm = TRUE),1))

  # 6 equal divisions
  divl  <- seq.int(round(min(y, na.rm = TRUE), 1), round(max(y, na.rm = TRUE),1), length.out = 6)
  divs  <- seq.int(round(min(abs(r), na.rm = TRUE), 1), round(max(abs(r), na.rm = TRUE),1), length.out = 6)

  n     <- abs(divs[6]/10)

  m <- 12
  # Plots the residual against an angular position
  par(oma = c(0, 0, 3, 0), cex = 0.9)
  polar.plot(0, labels = "", show.grid = FALSE, show.grid.labels = FALSE,show.radial.grid = FALSE)
  title(paste("\n\n", title, sep = ""), outer = TRUE)
  # Generates 'tick marks' for angular axis
  ## Major tick marks
  for (i in seq(1,length(lp), length.out=m)){
    polar.plot(c(0, divs[6] + n/2), c(lp[i], lp[i]), lwd = 1, rp.type = "p", line.col = "Black",add = TRUE)
  }
  ## Minor tick marks
  for (i in seq(1, length(lp))) {
      polar.plot(c(0, divs[6] + n/4), c(lp[i], lp[i]), lwd = 1, rp.type = "p", line.col = "Black",add = TRUE)
  }

  # Generates angular labels
  for (i in seq.int(1,length(lp),length.out=m)) {
      text <- sprintf("%.1f", round(ln[i], 1))
      if (i <= round(length(lp)/2, 0)) {
          arctext(text, middle = (lp[i] * pi)/(180), radius = divs[6] + n, clockwise = FALSE)
      } else{
          arctext(text, middle = (lp[i] * pi)/(180), radius = divs[6] + n, clockwise = TRUE)
      }
  }
  # Angular axis title  (w/ units)
  arctext(paste(taxis[1], paste(tunit$unit, "]", sep=""), sep=" ["), middle = 0, radius = divs[6] + n, clockwise = TRUE)

  # Alternating Colors
  for (i in 6:1) {
      if ((i%%2) == 0) {
          color <- color1
      } else {
          color <- "White"
      }
      draw.circle(0, 0, radius = abs(divs[i]), col = color)
    }
  # Creates region for axis space
  draw.sector(40, -40, col="white")
  for (i in 6:1){
    rlab <- mean(c(abs(divs[i + 1]), abs(divs[i])))
    text(rlab, 0, srt=0, labels = divl[i + 1])
    draw.sector(-1, -40, rou1=divs[i], rou2=divs[i])

  }
  text(mean(c(abs(divs[2 + 1]), abs(divs[4]))),par("usr")[1] + 0.55 * diff(par("usr")[1:2]), srt=0, labels=paste(raxis[1], paste(raxis[2], "]", sep=""), sep=" ["))
  polar.plot(r,t, rp.type = "s", point.col="black", add=TRUE, point.symbol=16)
}
