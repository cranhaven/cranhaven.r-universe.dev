

####################
###  UNIT TESTS  ###
####################
#
# For exported "svarfevd", "svarirf",  
# "sboot" functions, and supporting modules.


test_that("irf.*() and fevd.*() provide the same results for 'svars', 'L.varx', and 'pvarx'", {
  # prepare data #
  data("PCAP")
  names_k = c("g", "k", "l", "y")  # variable names
  names_i = levels(PCAP$id_i)      # country names 
  L.data  = sapply(names_i[1], FUN=function(i) 
    ts(PCAP[PCAP$id_i==i, names_k], start=1960, end=2019, frequency=1), simplify=FALSE)
  
  # calculate single SVAR #
  ### Note that svars (version 1.3.11) does not count the number of deterministic regressors 
  ### ... when calculating the OLS covariance matrix of residuals. Only with a 
  ### ... single deterministic regressor, the results are identical to vars and pvars!
  L.vars  = lapply(L.data, FUN=function(x) vars::VAR(x, p=2, type="const"))
  R.svars = svars::id.chol(L.vars[[1]], order_k=names_k)
  R.pid   = pid.chol(L.vars, order_k=names_k)
  colnames(R.svars$B) = colnames(R.pid$L.varx[[1]]$B)  ### svars::id.*() do not define names_s for shocks.
  L.svars = list(AUS=R.svars)
  
  # calculate FEVD #
  n.ahead = 3
  R.fevd_svars = svars:::fevd.svars(R.svars, n.ahead=n.ahead)
  R.fevd_varx  = fevd.id(R.svars, n.ahead=n.ahead)
  
  # calculate IRF #
  R.irf_svars = svars:::irf.svars(R.svars, n.ahead=n.ahead)
  R.irf_varx  = irf.varx(R.svars,  n.ahead=n.ahead)
  R.irf_pid   = irf.pvarx(R.pid,   n.ahead=n.ahead)
  R.irf_pid_L = irf.pvarx(L.svars, n.ahead=n.ahead)
  R.irf_pid_i = irf.pvarx(R.pid,   n.ahead=n.ahead, idx_i=1)
  R.irf_id_i  = irf.varx(R.pid$L.varx[[1]], n.ahead=n.ahead)
  
  # check #
  ### Note that irf.svars (version 1.3.8) produces IRF over the interval [0, n.ahead-1]!
  idx_row = 1:n.ahead
  expect_equal(R.svars$B, R.pid$L.varx[[1]]$B)
  expect_equivalent(R.fevd_svars, R.fevd_varx)
  expect_equal(R.irf_svars$irf[idx_row, ], R.irf_pid$irf[idx_row, ])
  expect_equal(R.irf_varx, R.irf_pid)
  expect_equal(R.irf_pid,  R.irf_pid_L)
  expect_equal(R.irf_varx, R.irf_pid_i)
  expect_equal(R.irf_id_i, R.irf_pid_i)
})


test_that("PP.*() provide the same persistence profiles for objects of class 'varx' and 'vec2var'", {
  tolerant = 25e-08  # vec2var() is based on equation-wise lm().
  
  # prepare data #
  data("PCAP")
  names_k = c("g", "k", "l", "y")  # variable names
  names_i = levels(PCAP$id_i)      # country names 
  L.data  = sapply(names_i, FUN=function(i) ts(PCAP[PCAP$id_i==i, names_k], start=1960, end=2019, frequency=1), simplify=FALSE)
  
  # joint specifications #
  dim_r = 2
  dim_p = 2
  shock = cbind(c(1, 0, 0, 0), c(0, 0, 1, 0), c(0, 0, 1, 1))
  n.ahead = 10
  n.season = 4
  
  # calculate single reduced-rank VAR #
  R.vecm = VECM(y=L.data$DNK, dim_p=dim_p, dim_r=dim_r, type="Case4", t_D2=list(n.season=n.season))
  R.cajo = urca::ca.jo(L.data$DNK, ecdet="trend", type="eigen", K=dim_p, spec="transitory", season=n.season)
  R.vars = vars::vec2var(R.cajo, r=dim_r)
  
  # calculate persistence profiles #
  R.ppv_varx = PP.variable(R.vecm, n.ahead = n.ahead, shock = shock)
  R.pps_varx = PP.system(R.vecm,   n.ahead = n.ahead)
  R.ppv_vars = PP.variable(R.vars, n.ahead = n.ahead, shock = shock)
  R.pps_vars = PP.system(R.vars,   n.ahead = n.ahead)
  
  # check #
  expect_equivalent(R.vecm$resid, t(R.vars$resid))
  expect_equal(R.ppv_varx, R.ppv_vars, tolerance=tolerant)
  expect_equal(R.pps_varx, R.pps_vars, tolerance=tolerant)
})


test_that("aux_check() warns or stops in case of incorrect arguments", {
  # prepare data #
  data("ERPT")
  names_k = c("lpm5", "lfp5", "llcusd")  # variable names for "Chemicals and related products"
  names_i = levels(ERPT$id_i)[c(1,6,2,5,4,3,7)]  # ordered country names
  dim_N   = length(names_i)
  
  # arguments #
  L.data  = sapply(names_i, FUN=function(i) ts(ERPT[ERPT$id_i==i, names_k], start=c(1995, 1), frequency=12), simplify=FALSE)
  L.dataF = L.data; L.dataF[[2]] = L.data[[2]][ , -2, drop=FALSE]
  R.lags  = c(3, 3, 3, 4, 3, 3, 3); names(R.lags)=names_i  # lags of VAR model by MAIC
  R.lagsF = R.lags; names(R.lagsF) = names_i[dim_N:1]  # lags with false names
  t_DF    = list(t_impulse=20, t_error=10)
  DF_sq   = matrix((1:100)^2, nrow=1)
  
  # test the stops and warnings: pvarx #
  expect_error(pvarx.VAR(L.dataF, lags=R.lags, type="both"),
               "The number of variables in 'L.data' must be the same for all individuals.")
  expect_error(pvarx.VAR(DF_sq, lags=R.lags, type="both"),
               "Argument 'L.data' must be a list of N individual data.frame objects.")
  expect_error(pvarx.VAR(L.data, lags=R.lags, type="both", D=list(A=DF_sq, B=DF_sq)),
               "Argument 'D' must be either a single data.frame object or a list of N individual data.frame objects.")
  
  # test the stops and warnings: pcoint # 
  expect_error(pcoint.BR(L.dataF, lags=R.lags, type="Case4"),
               "The number of variables in 'L.data' must be the same for all individuals.")
  expect_error(pcoint.JO(L.data, lags=1:2, type="Case4"),
               "Argument 'lags' must be either a single integer or a vector of N integers specific to each individual.")
  expect_warning(pcoint.SL(L.data, lags=R.lagsF, type="SL_trend"),
                 "Arguments 'L.data' and lags have mismatching names for individuals.")
  expect_warning(pcoint.SL(L.data, lags=2, type="SL_trend", t_D=list(t_impulse=10), n.factors=2),
                 "The factor estimation by PCA ignores period-specific deterministic regressors.")
  expect_warning(pcoint.CAIN(L.data, lags=2, type="SL_trend", t_D=t_DF),
                 "Unrecognized elements in 't_D' have been removed: t_error")
})


test_that("speci.VAR() can reproduce basic examples from vars and urca package.", {
  # prepare data #
  library("urca")
  library("vars")
  data("denmark")
  sjd = denmark[, c("LRM", "LRY", "IBO", "IDE")]
  
  # use a single lag-order to determine only the break period #
  R.vars  = vars::VAR(sjd, type="both", p=1, season=4)
  R.speci = speci.VAR(R.vars, lag_set=2, dim_m=1, trim=3, add_dummy=FALSE)
  
  # perform cointegration test procedure #
  R.t_D   = list(t_shift=8, n.season=4)
  R.coint = coint.SL(sjd, dim_p=2, type_SL="SL_trend", t_D=R.t_D)
  R.urca  = urca::cajolst(sjd, trend=TRUE, K=2, season=4)
  
  # use m=0 to determine only the lag-order #
  data(Canada)
  R.vars_ic = vars::VARselect(Canada, lag.max=5, type="const")
  R.spec_ic = speci.VAR(vars::VAR(Canada, p=1, type="const"), lag_set=1:5, dim_m=0)
  
  # joint determination of lag-order and break period #
  R.spec_jnt = speci.VAR(R.vars, lag_set=1:2, dim_m=2, trim=0.2)
  ### has no original results to replicate and test against.
  
  # check #
  expect_equivalent(R.speci$selection[2,2], R.urca@bp)
  ### expect_equal(rev(R.coint$stats_TR),       R.urca@teststat)
  ### Trace statistics are not equal because cajolst() uses just OLS-detrending,
  ### ... while coint.SL() uses GLS-detrending ('urca' version 1.3-4).
  expect_equivalent(t(R.spec_ic$df[ ,-1]),  R.vars_ic$criteria)
  expect_equivalent(R.spec_ic$selection,    R.vars_ic$selection)
  
  # test the stops and warnings #
  R.vars_none = VAR(sjd, type="none", p=1, season=4)
  expect_warning(speci.VAR(R.vars_none, lag_set=2, dim_m=1, trim=3, type_break="const"),
                 "'type_break' pertains the constant, which is missing in the provided VAR model 'x'.")
  expect_warning(speci.VAR(R.vars_none, lag_set=2, dim_m=1, trim=4, type_break="trend"),
                 "'type_break' pertains the linear trend, which is missing in the provided VAR model 'x'.")
})


test_that("sboot.pmb() under N=1 and sboot.mb() return identical Chol-VAR and BQ-VAR results", {
  # prepare data and arguments #
  data("PCAP")
  names_k = c("g", "k", "l", "y")  # variable names
  names_i = levels(PCAP$id_i)      # country names 
  L.data  = sapply(names_i, FUN=function(i) 
    ts(PCAP[PCAP$id_i==i, names_k], start=1960, end=2019, frequency=1), 
    simplify=FALSE)
  dim_p   = 2
  n.boot  = 3
  n.ahead = 3
  
  # estimate, identify, and bootstrap panel VAR #
  set.seed(8349)
  L.vars = lapply(L.data[1], FUN=function(x) vars::VAR(x, p=dim_p, type="none"))
  L.idBQ = lapply(L.vars[1], FUN=function(x) vars::BQ(x))
  R.pbBQ = sboot.pmb(L.idBQ, b.dim=c(5, FALSE), n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  
  set.seed(8349)
  R.pvar = pvarx.VAR(L.data[1], lags=dim_p, type="none")
  R.pid  = pid.chol(R.pvar)
  R.pmb  = sboot.pmb(R.pid, b.dim=c(5, FALSE), n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  R.pmb2 = sboot.pmb(R.pmb, b.dim=c(5, FALSE), n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  
  # estimate, identify, and bootstrap individual VAR #
  set.seed(8349)
  R.id  = R.pid$L.varx[[1]]
  R.mb  = sboot.mb(R.id, b.length=5, n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  R.mb2 = sboot.mb(R.mb, b.length=5, n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  
  set.seed(8349)
  R.idBQ = L.idBQ[[1]]
  R.mbBQ = sboot.mb(R.idBQ, b.length=5, n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  
  ###tolerant = 1.04e-04  
  ### Note that vars::VAR() is based on equation-wise lm() instead of multivariate LS. 
  ### ... The resulting computational differences are most notable in some bootstrapped IRF.
  ###R.svars = svars::id.chol(L.vars[[1]])  # dim_Kpn == Tob - 1 - k * p under type="const"
  ### Note that svars (version 1.3.11) does not count the number of deterministic regressors 
  ### ... when calculating the OLS covariance matrix of residuals. Only with a 
  ### ... single deterministic regressor, the results are identical to vars and pvars!
  ###colnames(R.svars$B) = colnames(R.pid$B)  # no automatic naming by id.chol()
  
  # check #
  expect_equal(R.pmb$true, R.mb$true)
  expect_equal(R.pmb$bootstrap, R.mb$bootstrap)
  expect_equal(R.pmb$A, R.mb$A)
  expect_equal(R.pmb$B, R.mb$B)
  
  expect_equal(R.pmb2$true, R.mb2$true)
  expect_equal(R.pmb2$bootstrap, R.mb2$bootstrap)
  expect_equal(R.pmb2$A, R.mb2$A)
  expect_equal(R.pmb2$B, R.mb2$B)
  
  expect_equal(R.pbBQ$true, R.mbBQ$true)
  expect_equal(R.pbBQ$bootstrap, R.mbBQ$bootstrap)
  expect_equal(R.pbBQ$A, R.mbBQ$A)
  expect_equal(R.pbBQ$B, R.mbBQ$B)
})


test_that("sboot.pmb() under N=1 and sboot.mb() return identical SVECM results", {
  # prepare data #
  data("PCAP")
  names_k = c("g", "k", "l", "y")  # variable names
  names_i = levels(PCAP$id_i)      # country names 
  L.data  = sapply(names_i, FUN=function(i) 
    ts(PCAP[PCAP$id_i==i, names_k], start=1960, end=2019, frequency=1), 
    simplify=FALSE)
  
  # arguments #
  type = "Case4"
  t_D1 = list(t_break=c(23, 49), t_shift=30)
  t_D2 = list(t_impulse=c(10), t_blip=12)
  ### A single element in t_D1 or t_D2 would be caught by aux_check() in pvarx.VEC(), ...
  ### ... but a panel model with single individual is not relevant anyway.
  LR = matrix(NA, nrow=4, ncol=4); LR[  , 1:2] = 0  # transitory shocks from g_t and k_t 
  SR = matrix(NA, nrow=4, ncol=4); SR[ 1, 2] = 0; SR[ 3, 4] = 0  # no instantaneous reaction of g_t to k_t resp. l_t to y_t 
  dim_r   = 2
  dim_p   = 2
  n.boot  = 3
  n.ahead = 3
  
  # estimate, identify, and bootstrap panel VAR #
  set.seed(8349)
  L.vecm = lapply(L.data["DNK"], FUN=function(x) VECM(x, dim_r=dim_r, dim_p=dim_p, type=type, t_D1=t_D1, t_D2=t_D2))
  L.id   = lapply(L.vecm["DNK"], FUN=function(x) id.grt(x, LR=LR, SR=SR))
  R.mb   = sboot.mb(L.id[["DNK"]], b.length=5, n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  
  set.seed(8349)
  R.pvec = pvarx.VEC(L.data["DNK"], dim_r=dim_r, lags=dim_p, type=type, t_D1=t_D1, t_D2=t_D2)
  R.pid  = pid.grt(R.pvec, LR=LR, SR=SR)
  R.pmb  = sboot.pmb(R.pid, b.dim=c(5, FALSE), n.boot=n.boot, n.ahead=n.ahead, n.cores=1)
  
  # extract a single individual from the panel application
  R.mg = sboot.mg(R.pid, n.ahead=n.ahead, idx_i="DNK")
  ### sboot.pmb could therewith show the result for a single ... 
  ### ... individual within the sampling of the complete panel ...
  ### ... (including potential selective pooled estimation).
  
  # check #
  expect_equal(R.mb$true, R.pmb$true)
  expect_equal(R.mb$bootstrap, R.pmb$bootstrap)
  expect_equal(R.mb$beta, R.pmb$beta)
  expect_equal(R.mb$A, R.pmb$A)
  expect_equal(R.mb$B, R.pmb$B)
})


test_that("sboot.pmb() return identical SVECM results under weighting and subsetting", {
  # prepare data #
  data("PCAP")
  names_k = c("g", "k", "l", "y")  # variable names
  names_i = levels(PCAP$id_i)      # country names 
  L.data  = sapply(names_i, FUN=function(i) 
    ts(PCAP[PCAP$id_i==i, names_k], start=1960, end=2019, frequency=1), 
    simplify=FALSE)
  
  # arguments #
  type = "Case4"
  t_D1 = list(t_break=c(23, 49), t_shift=30)
  t_D2 = list(t_impulse=c(10), t_blip=12)
  LR = matrix(NA, nrow=4, ncol=4); LR[  , 1:2] = 0  # transitory shocks from g_t and k_t 
  SR = matrix(NA, nrow=4, ncol=4); SR[ 1, 2] = 0; SR[ 3, 4] = 0  # no instantaneous reaction of g_t to k_t resp. l_t to y_t 
  dim_r   = 2
  dim_p   = 2
  n.boot  = 3
  n.ahead = 3
  b.dim   = c(FALSE, 2)
  w1 = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  w2 = c(1, 1, 0, 0, 0)
  
  # estimate, identify, and bootstrap panel VAR #
  R.pvec = pvarx.VEC(L.data[1:5], dim_r=dim_r, lags=dim_p, type=type, t_D1=t_D1, t_D2=t_D2)
  R.pid  = pid.grt(R.pvec, LR=LR, SR=SR)
  set.seed(8349)
  R.pmb1 = sboot.pmb(R.pid, b.dim=b.dim, n.boot=n.boot, n.ahead=n.ahead, n.cores=1, fix_beta=FALSE, w=w1)
  set.seed(8349)
  R.pmb2 = sboot.pmb(R.pid, b.dim=b.dim, n.boot=n.boot, n.ahead=n.ahead, n.cores=1, fix_beta=FALSE, w=w2)
  
  # check #
  expect_equal(R.pmb1$true, R.pmb2$true)
  expect_equal(R.pmb1$bootstrap, R.pmb2$bootstrap)
  expect_equal(R.pmb1$beta, R.pmb2$beta)
  expect_equal(R.pmb1$A, R.pmb2$A)
  expect_equal(R.pmb1$B, R.pmb2$B)
})


