## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(data.table)
seg.result <- atime::atime(
  N=2^seq(2, 20),
  setup={
    max.segs <- as.integer(N/2)
    max.changes <- max.segs-1L
    set.seed(1)
    data.vec <- 1:N
  },
  "changepoint\n::cpt.mean"={
    cpt.fit <- changepoint::cpt.mean(data.vec, method="BinSeg", Q=max.changes)
    sort(c(N,cpt.fit@cpts.full[max.changes,]))
  },
  "binsegRcpp\nmultiset"={
    binseg.fit <- binsegRcpp::binseg(
      "mean_norm", data.vec, max.segs, container.str="multiset")
    sort(binseg.fit$splits$end)
  },
  "fpop::\nmultiBinSeg"={
    mbs.fit <- fpop::multiBinSeg(data.vec, max.changes)
    sort(c(mbs.fit$t.est, N))
  },
  "wbs::sbs"={
    wbs.fit <- wbs::sbs(data.vec)
    split.dt <- data.table(wbs.fit$res)[order(-min.th, scale)]
    sort(split.dt[, c(N, cpt)][1:max.segs])
  },
  "binsegRcpp\nlist"={
    binseg.fit <- binsegRcpp::binseg(
      "mean_norm", data.vec, max.segs, container.str="list")
    sort(binseg.fit$splits$end)
  })
plot(seg.result)

## -----------------------------------------------------------------------------
seg.best <- atime::references_best(seg.result)
plot(seg.best)

## -----------------------------------------------------------------------------
(seg.pred <- predict(seg.best))
plot(seg.pred)

## -----------------------------------------------------------------------------
my.refs <- list(
  "N \\log N"=function(N)log10(N) + log10(log(N)),
  "N^2"=function(N)2*log10(N),
  "N^3"=function(N)3*log10(N))
my.best <- atime::references_best(seg.result, fun.list=my.refs)
plot(my.best)

