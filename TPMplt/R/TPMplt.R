#' Plot 2d thermal process maps
#'
#' @description Plot a 2d thermal process maps: logarithm strain rate as y axis while celsius temperature as x axis. Contours denotes
#' the power dissipation efficiency factor, while the background with gradual colors represents rheological stability.
#' @param x Regression results from modeling functions such as \code{\link[TPMplt:SVRModel]{SVRModel}}.
#' @param xloc Location for annotatin in x axis. The default value is 0.09.
#' @param yloc Location for annotatin in y axis. The default value is 0.03.
#' @param clrctrl Colour vector to control eta background, the default value uses rainbow palette.
#'
#' @import ggplot2 metR
#' @return A 2d thermal processing-map with logarithm strain rate as its y axis while celsius temperature as its x axis. Strain conditon
#' is showed in top-left in the figure. Power dissipation efficiency factor eta is denoted by gradient blue contours, and the rheological
#' stability coefficient are represented by gradient background.
#' @export TPM2dplt
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' TPM2dplt(PLTbd)
#' @keywords PLTbuilder Processing-map
TPM2dplt <- function(x, xloc=0.09, yloc=0.03, clrctrl=rev(rainbow(7))[-1]){
  # input data check
  if(!any(class(x)=="PLTbuilder")){
    stop("input data must be a data frame with the attribute of PLTbuilder, returned from related functions", call. = FALSE)
  }

  # plot theme
  theme_zc <- theme(plot.title = element_text(size = 12.5, face = "bold"),
                    axis.text = element_text(size = 11), axis.title = element_text(size = 12, face = "bold"),
                    axis.text.y = element_text(margin = margin(0, 6, 0, 0, "pt")), axis.text.x = element_text(margin = margin(6, 0, 0, 0, "pt")),
                    axis.ticks = element_line(size = 1), axis.ticks.length = unit(-0.16, "cm"),
                    panel.background = element_rect(color = "black", size = 1))
  legendtheme_zc <- theme(legend.key.size = unit(0.18, "inches"), legend.key.height = unit(0.18, "inches"),
                          legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))
  my_theme <- theme_zc + legendtheme_zc

  # make plot
  data1 <- subset(x[[1]], x[[1]][,3]=="xi")
  data2 <- subset(x[[1]], x[[1]][,3]=="eta")
  level <- NULL

  xily <- ggplot(data = data1, aes(data1[,1], data1[,2], z = data1[,4]))
  xily <- xily + geom_raster(aes(fill = data1[,4]), interpolate = TRUE) + scale_fill_gradientn(name=expression(xi), colours=clrctrl)

  ath_etaly <- xily + geom_contour(data = data2, aes(data2[,1], data2[,2], z=data2[,4], colour = after_stat(level)), inherit.aes = TRUE) +
    metR::geom_text_contour(aes(z = data2[,4]), stroke = 0.15)
  ath_etaly <- ath_etaly + my_theme + xlab("Temperature (Celsius)") + ylab("LogStrainRate (log(s^(-1)))") + labs(color="eta")

  # add annotation
  SR <- x[[2]]
  locx <- min(x[[1]][,1]) + (max(x[[1]][,1])-min(x[[1]][,1]))*xloc
  locy <- max(x[[1]][,2]) - (max(x[[1]][,2])-min(x[[1]][,2]))*yloc
  result <- ath_etaly + annotate("text", x=locx, y=locy, label=paste("Strain: ", SR, sep = ""), colour = "black") +
    labs(title = expression(paste("Guide: Background=", xi, "; Contours=", eta, collapse  = "")))
  return(result)
}


#' Build matrix for 3D plots
#'
#' @param x An "PLTbuilder" object.
#' @param grp "eta" or "xi" to determine which group is extracted as the subset.
#'
#' @return A matrix for 3d surface plot.
#' @export surfacebld
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' surfacebld(PLTbd, "eta")
#' @keywords internal
surfacebld <- function(x, grp=c("eta", "xi")){
  tt <- subset(x[[1]], x[[1]][,3] == grp)
  clnslt <- c(4,1,2)
  m <- cbind(tt[,clnslt[1]], tt[,clnslt[2]], tt[,clnslt[3]])
  len <- length(m[,1])
  levx <- as.numeric(levels(factor(m[,2])))
  levy <- as.numeric(levels(factor(m[,3])))
  cvrtm <- matrix(NA, nrow = length(levx), ncol = length(levy))

  i <- 1
  for (i in 1:len) {
    x_cor <- which(levx==m[i,2])
    y_cor <- which(abs(levy-m[i,3]) < 0.000002)
    cvrtm[x_cor, y_cor] <- m[i,1]
  }
  result <- list(result=cvrtm, levx=levx, levy=levy)
  return(result)
}


#' 3D plots without labels
#'
#' @param x An "PLTbuilder" object.
#' @param gain A positive integer to gain gradual colours. Default value is 100.
#' @param division Subdivision numbers for x, y and z axises.
#' @param clrctrl Color control. Optinal value are "rainbow", "heat", "terrain", "topo" and "cm".
#' @param zeroplane Boolean value to control for adding the plane of z=0. Default setting is TRUE.
#'
#' @import rgl grDevices
#' @return A surface 3d plot.
#' @export basic3d
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' PLT3dbd <- surfacebld(PLTbd, "eta")
#' basic3d(PLT3dbd, clrctrl="cm")
#' @keywords internal
basic3d <- function(x, gain=100, division=5, clrctrl=c("heat", "terrain", "topo", "cm", "rainbow"), zeroplane=TRUE){
  levx <- x[[2]]
  levy <- x[[3]]
  x <- x[[1]]
  eta.gained <- x*gain
  T.index <- (1:nrow(x))
  lgSR.index <- (1:ncol(x))

  zlim <- range(eta.gained)
  zlen <- zlim[2] - zlim[1] + 1

  # height color lookup table
  if (clrctrl=="heat"){
    colorlut <- heat.colors(zlen)
  } else if (clrctrl=="terrain") {
    colorlut <- terrain.colors(zlen)
  } else if (clrctrl=="topo") {
    colorlut <- topo.colors(zlen)
  } else if (clrctrl=="cm") {
    colorlut <- cm.colors(zlen)
  } else if (clrctrl=="rainbow") {
    colorlut <- rainbow(zlen)
  }
  col <- colorlut[(eta.gained-zlim[1]+1)] # assign colors to heights for each point

  T.index1 <- T.index*zlim[2]/max(T.index)
  lgSR.index1 <- lgSR.index*zlim[2]/max(lgSR.index)


  surface3d(T.index1, lgSR.index1, x*gain, color=col, back="lines")
  grid3d(c("x", "y+", "z"), n =20)

  lgSR.name <- round(levy[1:ncol(x)], 2)
  lgSR.label <- as.integer(seq(1, length(lgSR.name), length = division))

  axis3d('y+',at = lgSR.index1[lgSR.label], lgSR.name[lgSR.label], cex = 1)
  axis3d('y',at = lgSR.index1[lgSR.label], lgSR.name[lgSR.label], cex = 1)
  eta.label <- seq(min(zlim), max(zlim), length=division)
  axis3d('z',pos=c(0, 0, NA), at=eta.label, round(eta.label/gain, 2), cex =1)

  T.label <- as.integer(seq(1, length(T.index), length = division))
  axes3d('x', at=T.index1[T.label], levx[T.label], cex = 1)

  if(zeroplane==TRUE){
    planes3d(a=0, b=0, c=1, d=0, alpha=0.6)
  }
}

#' Plot 3d thermal processing-maps
#'
#' @description Return a 3d thermal process result consisted of 3d surfaces for power dissipation efficiency eta and rheological stability coefficient
#' xi respectively.
#' @param x Regression results from modeling functions such as \code{\link[TPMplt:SVRModel]{SVRModel}}.
#' @param dvs A positive integer to set the divisions for x, y and z labels in two 3d surface plots. The default value is 5.
#' @param etaclr Colour control for eta. Optional value are "rainbow", "heat", "terrain", "topo" and "cm". "heat" is default value.
#' @param xiclr Colour control for xi. Optional value are "rainbow", "heat", "terrain", "topo" and "cm". "cm" is default value.
#'
#' @import rgl
#' @return Two 3d surface plots: the left one denotes power dissipation efficiency factor eta, while the right one is for rheological stability
#' xi. A zero plane, z=0, for xi value is added in the right plots for determining unstable region.
#' @export TPM3dplt
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' TPM3dplt(PLTbd)
#' @keywords PLTbuilder Processing-map
TPM3dplt <- function(x, dvs=5, etaclr="heat", xiclr="cm"){
  # input data check
  if(!any(class(x)=="PLTbuilder")){
    stop("input data must be a data frame with the attribute of PLTbuilder, returned from related functions", call. = FALSE)
  }

  mfrow3d(1, 2, sharedMouse = TRUE)
  eta <- surfacebld(x, "eta")
  basic3d(eta, 100, division = dvs, clrctrl = etaclr, zeroplane = FALSE)
  title.name <- "Surface3d: power dissipation efficiency"
  title.sub <- paste("(Strain=", x[[2]], ")", sep = "")
  title3d(main = title.name, sub = title.sub, xlab = "Temperature (Celsius)", ylab = "LogStrainRate (log(s^(-1)))", zlab = "eta")
  next3d()
  xi <- surfacebld(x, "xi")
  basic3d(xi, 100, division = dvs, clrctrl = xiclr)
  title.name <- "Surface3d: rheological stability"
  title.sub <- paste("(Strain=", x[[2]], ")", sep = "")
  title3d(main = title.name, sub = title.sub, xlab = "Temperature (Celsius)", ylab = "LogStrainRate (log(s^(-1)))", zlab = "xi")
}
