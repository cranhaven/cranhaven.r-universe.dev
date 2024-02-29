## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -------------------------------------------------------------------------------------------------
old.opt <- options(width=100)
data(Mono27ac, package="PeakSegDisk", envir=environment())
library(data.table)
penalty <- 1e6
expr.list <- c(
  if(requireNamespace("PeakSegDisk"))atime::atime_grid(
    real=PeakSegDisk::PeakSegFPOP_df(real, penalty),
    synthetic=PeakSegDisk::PeakSegFPOP_df(synthetic, penalty)),
  atime::atime_grid(mean=mean(real$count)))
atime.list <- atime::atime(
  N=as.integer(10^seq(1, 3, by=0.5)),
  setup={
    real <- Mono27ac$coverage[1:N]
    synthetic <- data.table(real)[, count := 1:.N]
  },
  expr.list=expr.list,
  seconds.limit=Inf,
  result=TRUE)
plot(atime.list)

## -------------------------------------------------------------------------------------------------
atime.list$measurements[, intervals := sapply(
  result, function(L)if(is.numeric(L))NA else L$loss$mean.intervals)]
best.list <- atime::references_best(atime.list, more.units="intervals")
plot(best.list)

## -----------------------------------------------------------------------------
options(old.opt)

## -----------------------------------------------------------------------------
(data.grid.exprs <- c(
  if(requireNamespace("PeakSegDisk"))atime::atime_grid(
    list(DATA=c("real","synthetic")),
    PeakSegDisk=PeakSegDisk::PeakSegFPOP_df(data.list[[DATA]], penalty)),
  atime::atime_grid(mean=mean(data.list$real$count))))
data.grid.result <- atime::atime(
  N=as.integer(10^seq(1, 3, by=0.5)),
  setup={
    real <- Mono27ac$coverage[1:N]
    data.list <- list(
      real=real, 
      synthetic=data.table(real)[, count := 1:.N])
  },
  seconds.limit = Inf,
  expr.list=data.grid.exprs)
plot(data.grid.result)

