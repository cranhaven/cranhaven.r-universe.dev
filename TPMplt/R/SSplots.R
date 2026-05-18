#' Automatic completion for Stress-Strain plots
#'
#' @param x A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors in column names
#' should be separated by "-" symbol, and factors for temperatures and strain rates should be saved in pure numeric style.
#' @param grpby Layer for variables to be grouped.
#' @param manual A positive integer vector with length 3. The first value is layer for Stress and Strain, the sencond value
#' is for Strain's level in this layer, and the third is Stress's level. Default setting is NULL, to call
#' \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} automatically fill this argument.
#' @param legendscale A positive value to determine the scale of legend. Default value is 0.7
#' @param ... Arguments be passed to \code{\link[graphics:par]{par}} for plots arrangements. \code{mfrow} and \code{mfcol} are
#' recommanded.
#'
#' @return Multiple Stress-Strain plots according to specified group methods.
#' @import VBTree graphics
#' @export SSplots
#'
#' @examples
#' require(VBTree)
#' # Find locations for temperature and strain rate:
#' # temperature in layer2, strain rate in layer3;
#' # Strain in layer1 level1, Stress in layer1 level2.
#' dl2vbt(chrvec2dl(colnames(TPMdata)))
#'
#' # Attention: Zoom your Plots panes large enough to ensure
#' # correct output!
#'
#' # Plot multiple Stress-Strain curves, grouped by strain rate:
#' SSplots(TPMdata, 3, mfrow=c(3, 3))
#'
#' # Plot multiple Stress-Strain curves, grouped by temperature:
#' SSplots(TPMdata, 2, mfrow=c(2, 2))
#'
#' # Manual setting, for Stress-Stain plots:
#' SSplots(TPMdata, 2, manual=c(1, 1, 2), mfrow=c(2, 2))
#' @keywords SSplots lyIDdetector
SSplots <- function(x, grpby, manual=NULL, legendscale=0.7, ...){

  # # test section
  # x <- TPMdata
  # grpby <- 3
  # manual <- NULL
  # legendscale <- 0.65

  vbt <- dl2vbt(chrvec2dl(colnames(x)))
  vbt

  if(is.null(manual)){
    lyIDs <- lyIDdetector(x)
    lySS <- lyIDs[[1]]
    strainlevel <- lyIDs[[2]]
    stresslevel <- lyIDs[[3]]
  } else {
    lyIDs <- list("layer"=manual[1], "strainID"=manual[2], "stressID"=manual[3])
    class(lyIDs) <- "pointer"
    lySS <- manual[1]
    strainlevel <- manual[2]
    stresslevel <- manual[3]
  }

  grps <- vbt[[2]][grpby]
  plts <- prod(vbt[[2]][-lyIDs[[1]]])/grps

  grparr <- array(NA, dim = c(plts, grps, 2)) # c(,,1) for strain, c(,,2) for stress

  initinq <- rep(-1, length(vbt[[2]]))
  straininq <- initinq
  straininq[lySS] <- strainlevel
  stressinq <- initinq
  stressinq[lySS] <- stresslevel

  i <- 1
  for (i in 1:grps) {
    # mapping talbe for strain
    visit <- straininq
    visit[grpby] <- i
    grparr[, i, 1] <- as.vector(vbt2arr(vbtsub(vbt, visit)))
    # mapping talbe for stress
    visit <- stressinq
    visit[grpby] <- i
    grparr[, i, 2] <- as.vector(vbt2arr(vbtsub(vbt, visit)))
  }

  cus_par <- par(...=...) # plots arrangement
  on.exit(par(cus_par))

  i <- 1
  for (i in 1:plts) {
    # make scales
    j <- 1
    xtemp <- c()
    ytemp <- xtemp
    for (j in 1:grps) {
      xtemp <- append(xtemp, x[, grparr[i,j,1]])
      ytemp <- append(ytemp, x[, grparr[i,j,2]])
    }
    xscale <- c(min(unlist(xtemp)), max(unlist(xtemp)))
    yscale <- c(min(unlist(ytemp)), max(unlist(ytemp)))

    # plots
    j <- 1
    ptlegend <- c()
    for (j in 1:grps) {
      # make title and legend
      labtable <- unlist(chrvec2dl(grparr[i,j,1]))
      notation <- paste(labtable[grpby], collapse = "-")
      ptlegend <- append(ptlegend, notation)
      mainlab <- paste(labtable[-c(lySS, grpby)], collapse = "-")

      # plots in groups
      clrs <- palette()[j+1]
      cus_par <- par(cex=legendscale)
      on.exit(par(cus_par))
      plot(x=x[, grparr[i,j,1]], y=x[, grparr[i,j,2]], pch=20, type = "l", col=clrs,
           xlab = "Strain", ylab = "Stress", xlim = xscale, ylim = yscale, main = mainlab)
      if (j==grps){
        legend("topleft", ptlegend, fill = palette()[2:(grps+1)], text.font = 2, seg.len = 0.3, cex = legendscale,
               horiz = TRUE, bg = "transparent", box.lty = 0, x.intersp=0.1, text.width = (c(1:grps)-1)*0.0002)
        cus_par <- par(new=FALSE)
        on.exit(par(cus_par))
      } else {
        cus_par <- par(new=TRUE)
        on.exit(par(cus_par))
      }
    }
  }
}
