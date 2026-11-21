

####################
###  UNIT TESTS  ###
####################
#
# For exported "pid" functions 
# and their subordinated modules.


test_that("pid.cvm() and pid.dc() return identical objects for group-ICA with one common factor", {
  # prepare data #
  data("PCAP")
  names_k = c("g", "k", "l", "y")  # variable names
  names_i = levels(PCAP$id_i)      # country names 
  L.data  = sapply(names_i, FUN=function(i) 
     ts(PCAP[PCAP$id_i==i, names_k], start=1960, end=2019, frequency=1), simplify=FALSE)
    
  # estimate #
  L.vars = lapply(L.data, FUN=function(x) vars::VAR(x, p=1, type="both"))
  R.pcvm = pid.cvm(L.vars, combine="group", n.factors=1)
  R.pdc  = pid.dc(L.vars,  combine="group", n.factors=1)
  
  # check #
  R.pcvm$args_pid = NULL
  R.pdc$args_pid  = NULL
  R.pcvm$L.varx = lapply(R.pcvm$L.varx, function(x_i) x_i$args_id = NULL)
  R.pdc$L.varx  = lapply(R.pdc$L.varx,  function(x_i) x_i$args_id = NULL)
  expect_equal(R.pcvm, R.pdc)
})


test_that("pid.dc(c('pool','indiv')) can reproduce the id.dc() example from svars package under N=1", {
  # estimate #
  library(svars)
  L.vars = list(USA = vars::VAR(USA, p=6, type="const"))
  
  # identify #
  set.seed(23211)
  R.pool = pid.dc(L.vars, combine="pool",  n.iterations=100, PIT=FALSE)
  set.seed(23211)
  R.indi = pid.dc(L.vars, combine="indiv", n.iterations=100, PIT=FALSE)
  set.seed(23211)
  R.svar = svars::id.dc(L.vars$USA, PIT=FALSE)
  
  # baseline shocks #
  R.varx = as.varx(L.vars$USA)
  R.eps  = t(solve(t(chol(R.varx$SIGMA))) %*% R.varx$resid)
  
  # check #
  R.svar$B = aux_sico(R.svar$B)$B
  expect_equivalent(R.svar$B, R.indi$B, tolerance=5e-05)  # identical on Linux Ubuntu (22.04)
  expect_equivalent(R.svar$B, R.pool$B, tolerance=5e-05)  # win-builder needs higher tolerance than other platforms
  expect_equivalent(R.eps, R.pool$eps)  
  ### View(R.eps - R.pool$eps)  # small rounding errors lead to slightly different rotation angles
})


test_that("pid.cvm(c('pool','indiv')) can reproduce the id.cvm() example from svars package under N=1", {
  # estimate #
  library(svars)
  L.vars = list(USA = vars::VAR(USA, p=6, type="const"))
  
  # identify #
  set.seed(23211)
  dd = copula::indepTestSim(L.vars$USA$obs, L.vars$USA$K, verbose=FALSE)
  set.seed(23211)
  R.pool = pid.cvm(L.vars, combine="pool",  dd=dd, itermax=25, steptol=100, iter2=5)
  set.seed(23211)
  R.indi = pid.cvm(L.vars, combine="indiv", dd=dd, itermax=25, steptol=100, iter2=5)
  set.seed(23211)
  R.svar = svars::id.cvm(L.vars$USA, dd=dd, itermax=25, steptol=100, iter2=5)
  
  # check #
  R.svar$B = aux_sico(R.svar$B)$B
  expect_equivalent(R.svar$B, R.indi$B)
  expect_equivalent(R.svar$B, R.pool$B)
})


test_that("aux_cvmICA() is correctly grafted", {
  # prepare data #
  data("USA")
  
  # joint arguments and model estimation #
  set.seed(23211)
  v1 = vars::VAR(USA, lag.max = 10, ic = "AIC" )
  dd = copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)
  dim_p   = unname(v1$p)  # lag-order
  itermax = 500/10
  steptol = 100/10
  iter2   = 75/15
  n.ahead = 10
  n.boot  = 10
  n.cores = 1
  
  # pvars: identify and bootstrap #
  set.seed(23211)
  R.varx = as.varx.varest(v1)
  R.chol = t(chol(R.varx$SIGMA))
  R.eps  = solve(R.chol) %*% R.varx$resid  # pre-whitened shocks form baseline decomposition
  R.ICA  = aux_cvmICA(t(R.eps), dd=dd, itermax=itermax, steptol=steptol, iter2=iter2)
  R.varx$B = R.chol %*% R.ICA$W
  #R.varx$arg_id = list(dd=dd, itermax=30, steptol=20, iter2=5)
  #R.boot1 = sboot.mb(R.varx, b.length=1, n.ahead=n.ahead, n.boot=n.boot, n.cores=n.cores)
  
  # svars: identify and bootstrap #
  set.seed(23211)
  R.svars = svars::id.cvm(v1, dd=dd, itermax=itermax, steptol=steptol, iter2=iter2)
  #R.boot2 = svars::mb.boot(R.svars, b.length=1, n.ahead=n.ahead, nboot=n.boot, nc=n.cores,
  #                         dd=dd, itermax=30, steptol=20, iter2=5)
  
  # check #
  expect_equal(R.ICA$theta, R.svars$rotation_angles)
  expect_equal(R.varx$B, R.svars$B)
  #expect_equal(R.boot1, R.boot2)
})


test_that("aux_UGroup() and aux_UPool() provide whitened residuals", {
  # prepare data and estimate #
  data("PCAP")
  names_k = c("g", "k", "l", "y")  # variable names
  names_i = levels(PCAP$id_i)      # country names 
  L.data = sapply(names_i, FUN=function(i) ts(PCAP[PCAP$id_i==i, names_k], start=1960, end=2019, frequency=1), simplify=FALSE)
  L.cajo = lapply(L.data, FUN=function(x) urca::ca.jo(x, K=2, ecdet="trend", spec="transitory", type="trace"))
  L.vars = lapply(L.cajo, FUN=function(x) vars::vec2var(x, r=3))
  
  # apply #
  L.varx  = as.pvarx(L.vars)$L.varx
  L.Ucov  = lapply(L.varx, FUN=function(x) x$OMEGA) # use MLE covariance matrix
  L.resid = lapply(L.varx, FUN=function(x) x$resid)
  dim_K   = nrow(L.varx[[1]]$A)
  R.group = aux_UGroup(L.resid, n.factors=dim_K)
  R.pool  = aux_UPool(L.resid, L.Ucov)
  
  # check #
  our_UGroup = t(R.group$eps) %*% (R.group$eps) / nrow(R.group$eps)
  our_UPool  = t(R.pool$eps)  %*% (R.pool$eps)  / nrow(R.pool$eps) 
  ### Do not correct for degrees-of-freedom when using MLE covariance matrix.
  expect_equivalent(our_UGroup, diag(dim_K))
  expect_equivalent(our_UPool,  diag(dim_K))
})


