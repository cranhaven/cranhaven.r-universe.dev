## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(fig.width=8, 
                      fig.height=4,
                      collapse = TRUE,
                      comment = "#>"
)

## ----setup, warning=FALSE-----------------------------------------------------
library(MALDIcellassay)
library(MALDIquant)

## ----loadData-----------------------------------------------------------------
data("Blank2022spec")

## ----qualityCheck-------------------------------------------------------------
MALDIquant::plot(Blank2022spec[[1]], main = "0uM, replicate 1")

## ----baseline-----------------------------------------------------------------
conc <- as.numeric(names(Blank2022spec))
spec_prc <- MALDIquant::removeBaseline(Blank2022spec)
avg <- MALDIquant::averageMassSpectra(spec_prc, labels = conc)
MALDIquant::plot(avg[[1]], main = "Overview of mean spectra", xlim = c(755, 765))
for(i in 2:length(avg)) {
  MALDIquant::lines(avg[[i]], col = i)
}
legend("topright", legend = paste0(names(avg), "uM"), col = 1:8, lty=1)

## ----mzShift------------------------------------------------------------------
peaks <- MALDIquant::detectPeaks(Blank2022spec, method = "SuperSmoother", SNR = 5)
names(peaks) <- names(Blank2022spec)
mz_shift <- getMzShift(peaks = peaks, targetMz = 760.585, tol = 500)
summary(mz_shift$mzshift)


## ----recal--------------------------------------------------------------------
spec_align <- shiftMassAxis(Blank2022spec, mz_shift$mzshift)


## ----norm---------------------------------------------------------------------
peaks_align <- MALDIquant::detectPeaks(spec_align, method = "SuperSmoother", SNR = 3)
norm <- getNormFactors(peaksdf = peaks2df(peaks_align), targetMz = 760.585, tol = 100)
summary(norm$norm_factor)
spec_rdy <- normalizeByFactor(spec_align, norm$norm_factor)


## ----finalCheck---------------------------------------------------------------
avg_rdy <- MALDIquant::averageMassSpectra(spec_rdy, labels = conc)
MALDIquant::plot(avg_rdy[[1]], main = "Overview of mean spectra", xlim = c(755, 765))
for(i in 2:length(avg_rdy)) {
  MALDIquant::lines(avg_rdy[[i]], col = i)
}
legend("topright", legend = paste0(names(avg), "uM"), col = 1:8, lty=1)

## ----intmat-------------------------------------------------------------------
peaks_rdy <- MALDIquant::detectPeaks(avg_rdy, method = "SuperSmoother", SNR = 3)
peaks_rdy <- MALDIquant::binPeaks(peaks_rdy)
intmat <- MALDIquant::intensityMatrix(peaks_rdy, avg_rdy)
dim(intmat)

## ----highVar------------------------------------------------------------------
vars <- apply(intmat, 2, var)
idx <- which(vars > mean(vars))
highVarIntmat <- intmat[,idx]
dim(highVarIntmat)

## ----fit----------------------------------------------------------------------
concLog <- log10(unique(conc))
    if(any(concLog == -Inf)) {
      concLog[which(concLog == -Inf)] <- (min(concLog[which(!concLog == -Inf)])-1)
    }
resp <- nplr::convertToProp(y = intmat[,10])
model <- nplr::nplr(x = concLog, y = resp, useLog = FALSE, npars = 4)
title <- paste0("m/z =", round(as.numeric(colnames(intmat)[12]), 2))
plot(model, main = title)

## ----fitCruve, eval = FALSE---------------------------------------------------
#  fitCurve(spec = Blank2022spec,
#           SinglePointRecal = TRUE,
#           normMz = 760.585,
#           alignTol = 0.1,
#           normTol = 0.1)

## ----Rsession-----------------------------------------------------------------
sessionInfo()

