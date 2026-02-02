## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(sarp.snowprofile.alignment)

## ----avgSPdisp, eval=FALSE, echo=TRUE-----------------------------------------
#  ## compute the average profile (here with default settings)
#  avgSP <- averageSP(SPgroup2)
#  ## plot profile set and resulting average profile
#  plot(SPgroup2, SortMethod = 'hs', xticklabels = 'originalIndices')
#  plot(avgSP$avg)

## ----avgSPcalc, eval=TRUE, echo=FALSE, fig.asp=0.7, dpi = 300-----------------
avgSP <- averageSP(SPgroup2, n = 1, progressbar = FALSE)
layout(matrix(c(1, 2), ncol = 2), widths = c(2.5, 1))
par(cex = 0.3)
plot(SPgroup2, SortMethod = 'hs', xticklabels = 'originalIndices', ylim = c(0, 150), main = "individual profiles of example set 'SPgroup2'")
par(mar = c(5.1, 2.1, 2.1, 1.1))
plot(avgSP$avg, main = "average profile", ymax = 150)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("figures/averageSPdistributions.png")

## ----echo=TRUE, eval=TRUE, fig.asp=0.5, dpi = 300-----------------------------
## identify layer of interest: we need its row index
deepestSH_index <- min(findPWL(avgSP$avg, pwl_gtype = "SH"))  # this is the deepest SH of the profile, in our specific case exactly what we want
## alternatively, you can (additionally) query for its date, 
## or you can identify it manually by investigating the profile layers data frame:
# View(avgSP$avg$layers)

## backtrack this one layer
backtrackedlayers <- backtrackLayers(avgSP$avg, layer = deepestSH_index, profileSet = avgSP$set)
## analyze result:
str(backtrackedlayers)  
## the backtrackedLayers object is a list of data frames, one data frame for each averaged layer (in our case 1); 
## list elements are named by the height (cm) of the averaged layer

## ----fig.asp=0.3, dpi = 300---------------------------------------------------
## compute whatever distribution you're interested: e.g., depth histogram
par(cex = 0.3)
hist(backtrackedlayers[[1]]$depth, 
     main = "Depth distribution of deepest SH layer", xlab = "Depth (cm)")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
## backtrack all layers by not providing a row index
backtrackedlayers <- backtrackLayers(avgSP$avg, profileSet = avgSP$set)

## -----------------------------------------------------------------------------
## identify proportion of underlying layers with good/fair/poor stability 
## (here with RTA, but feel free to choose any other index or thresholds!)
transitional <- sapply(backtrackedlayers, function(bti) {
  sum(bti$rta >= 0.6)/ length(bti$rta)
  })  # proportion of layers fair stability
poor <- sapply(backtrackedlayers, function(bti) {
  sum(bti$rta >= 0.8)/ length(bti$rta)
  })  # proportion of layers poor stability

## ----echo=TRUE, eval=TRUE, fig.asp=0.7, dpi = 300-----------------------------
## visualize profile
layout(matrix(c(2, 1), 1, 2, byrow = TRUE), c(1.2, 1.8))
par(mar = c(9.1, 0, 2.1, 2.1), bg = "transparent", cex = 0.3)
plot(avgSP$avg, axes = FALSE, xlab = "")
axis(1, at = seq(5), labels = c("F", "4F", "1F", "P", "K"))
mtext("Hardness", side = 1, line = 5.5, cex = 0.3)
##
## stacked barplot for stability distributions
par(mar = c(9.1, 5.1, 2.1, 0))
x0 <- 0
ygrid <- as.numeric(names(backtrackedlayers))  # the names of the list define the height grid
cols <- c("gray90", "gray70", "gray20")
stackedhist <- rbind(data.frame(xleft = 0, xright = 1-transitional, ytop = ygrid, 
                                ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[1]),
                     data.frame(xleft = 1-transitional, xright = (1-transitional) + transitional, 
                                ytop = ygrid, ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[2]),
                     data.frame(xleft = 1-poor, xright = 1, ytop = ygrid, 
                                ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[3]))
plot(ygrid, xlim = c(0, 1), ylim = c(0, max(ygrid)), frame.plot = FALSE, type = 'n', 
     axes = FALSE, xlab = "", ylab = "Height (cm)")
mtext("Proportion of  \nindividual profiles", side = 1, line = 5.5, cex = 0.3)
rect(xleft = x0+stackedhist$xleft, ybottom = stackedhist$ybottom, 
     xright = x0+stackedhist$xright, ytop = stackedhist$ytop, 
     col = stackedhist$col, border = NA)
xaxisticks <- c(0, 0.2, 0.5, 0.8, 1)
axis(1, at = xaxisticks, labels = rev(xaxisticks))
axis(2, at = pretty(ygrid))
abline(v = 0.2, lty = "dotted", col = "gray")
abline(v = 0.5, lty = "dotted", col = "gray")
abline(v = 0.8, lty = "dotted", col = "gray")

## ----timeseries---------------------------------------------------------------
## labeling of weak layers; again you can choose your own rules and thresholds!
SPspacetime <- snowprofileSet(lapply(SPspacetime, function(sp) {
  labelPWL(sp, pwl_gtype = c("SH", "DH", "FC", "FCxr"), threshold_RTA = 0.8)
  }))  # label weak layers in each profile of the profile set 'SPspacetime'

## average along several days
avgSP <- averageSPalongSeason(SPspacetime)

## ----dpi=300, fig.asp=0.7-----------------------------------------------------
## explore the average timeseries object:
names(avgSP)
avgSP$call
avgSP$meta

## visualize the time series
par(cex = 0.3)
plot(avgSP$avgs, main = "Timeseries of average profile with median HS and median new snow amounts highligted", xlab = "Daily progression")
lines(avgSP$meta$date, avgSP$meta$hs_median)
lines(avgSP$meta$date, avgSP$meta$hs - avgSP$meta$thicknessPPDF_median, lty = "dashed")

## ----fig.asp=0.7, dpi = 300---------------------------------------------------
## brief helper function for median vertical locations of specific layers (i.e., height or depth)
medianVLOC <- function(avgObj, pwldate, vloc = "Depth", pwlgt = c("SH", "DH"), date_range_earl = -5, draw = TRUE) {
  mvl <- unname(do.call("c", lapply(avgObj$avgs, function(avg) {
    median(avg$layers[, paste0("medianPredominant", vloc)][findPWL(avg, pwl_gtype = pwlgt, pwl_date = pwldate, 
                                                                   date_range_earlier = as.difftime(date_range_earl, units = "days"))])
  })))
  if (draw) {
    if (vloc == "Depth") lines(avgObj$meta$date, avgObj$meta$hs_median - mvl, lty = "dotted", lwd = 2)
    else if (vloc == "Height") lines(avgObj$meta$date, mvl, lty = "dotted", lwd = 2)
  }
  return(mvl)
}

## plot time series...
par(cex = 0.3)
plot(avgSP$avgs, main = "Time series with median depth of middle Nov 22 DH layer highlighted", xlab = "Daily progression")
## ... and apply above function to the Nov 22 weak layer
medianDepth_NOV22 <- medianVLOC(avgSP, "2018-11-23", vloc = "Depth")

## ----fig.asp=0.7, dpi=300-----------------------------------------------------
## rename the variable ppu_all to 'percentage' for subsequent plotting
avgSP$avgs <- snowprofileSet(lapply(avgSP$avgs, function(avg) {
  avg$layers$percentage <- avg$layers$ppu_all
  avg
}))

## overplot the grain type time series with the stability distribution
par(cex = 0.3)
plot(avgSP$avgs[avgSP$meta$date>="2018-09-20"], colAlpha = 0.5, main = "time series of average profile overplotted with stability distribution", xlab = "Daily progression")
plot(avgSP$avgs[avgSP$meta$date>="2018-09-20"], ColParam = "percentage", add = TRUE)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("figures/averageSPseason_stability.png")

