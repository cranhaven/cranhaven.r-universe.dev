# test

parlist <- confidenceSim::getparlist(
  looks=seq(500,1000,100),
  perpetual=FALSE,
  alloc.ratio=c(1,1),
  num.per.block=c(1,1),
  final.visit=0,
  as.type="asOF",
  multiarm.mode="CONFIDENCE-BASED",
  lmb.threshold=0.95,
  lmb.conf.thresh=0.9,
  outcome.type='BINARY',
  estimator.type='odds ratio',
  resprate=c(0.3,0.5),
  ppm=rep(15, 300))
looks <- parlist$num.looks

testthat::expect_equal(looks, 6)

