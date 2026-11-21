

####################
###  UNIT TESTS  ###
####################
#
# For exported "pcoint" functions 
# and their subordinated modules.


test_that("pcoint.s' PANIC can reproduce 'MERM' in Oersal,Arsova 2017:67, Ch.5", {
  # prepare data
  data("MERM")
  names_k = colnames(MERM)[-(1:2)] # variable names
  names_i = levels(MERM$id_i)      # country names
  L.data  = sapply(names_i, FUN=function(i) ts(MERM[MERM$id_i==i, names_k], start=c(1995, 1), frequency=12), simplify=FALSE)
  
  # Oersal,Arsova 2017:67, Tab.5 #
  R.lags = c(2, 2, 2, 2, 1, 2, 2, 4, 2, 3, 2, 2, 2, 2, 2, 1, 1, 2, 2); names(R.lags)=names_i  # individual lags by AIC (lag_max=4)
  R.pcsl = pcoint.SL(L.data, lags=R.lags, type="SL_trend", n.factors=8)  # eight factors by Onatski's (2010) criterion
  R.pcjo = pcoint.JO(L.data, lags=R.lags, type="Case4",    n.factors=8)
  
  dim_K = length(names_k)
  their_indiv = rbind(Brazil = c(0.114, 0.829, 0.473, 0.256),
                      Canada = c(0.070, 0.404, 0.747, 0.940),
                      Sweden = c(0.524, 0.998, 0.969, 0.851))
  colnames(their_indiv) = paste0("r_H0 = ", 0:(dim_K-1))
  our_indiv   = R.pcsl$individual$pvals[rownames(their_indiv), ]
  expect_equal(our_indiv, their_indiv, tolerance = 0.005)
  
  their_panel = rbind(LRbar   = c( 2.31, -1.35, -2.64, -2.10),
                      Choi_Pm = c( 3.74, -0.98, -2.40, -2.02),
                      Choi_Z  = c(-1.91,  1.49,  2.64,  2.12))
  our_panel   = R.pcsl$panel$stats[rownames(their_panel), ]
  colnames(their_panel) = colnames(our_panel)
  expect_equal(our_panel, their_panel, tolerance = 0.05)
  expect_equal(R.pcsl$CSD, R.pcjo$CSD)
  
  # Oersal,Arsova 2017:67, Tab.6 #
  R.Ftsl = coint.SL(y=R.pcsl$CSD$Ft, dim_p=2, type_SL="SL_trend")  # lag-order by AIC
  R.Ftjo = coint.JO(y=R.pcsl$CSD$Ft, dim_p=2, type="Case4")
  ours   = cbind(SL_stats = R.Ftsl$stats_TR, 
                 SL_pvals = R.Ftsl$pvals_TR, 
                 JO_stats = R.Ftjo$stats_TR,
                 JO_pvals = R.Ftjo$pvals_TR)
  theirs = cbind(SL_stats = c(202.45, 116.54, 85.13, 47.89, 27.17, 14.74, 3.72, 1.41),
                 SL_pvals = c(0.000, 0.080, 0.125, 0.622, 0.794, 0.784, 0.955, 0.684),
                 JO_stats = c(251.61, 177.37, 119.95, 81.59, 50.87, 31.4, 16.87, 7.16),
                 JO_pvals = c(0.000, 0.001, 0.034, 0.147, 0.379, 0.427, 0.433, 0.338))
  expect_equal(ours, theirs, tolerance = 0.005)
})


test_that("SL-procedures can reproduce 'ERPT' in Oersal,Arsova 2016:13, Ch.6", {
  # prepare data #
  data("ERPT")
  names_k = c("lpm5", "lfp5", "llcusd")  # variable names for "Chemicals and related products"
  names_i = levels(ERPT$id_i)[c(1,6,2,5,4,3,7)]  # ordered country names
  L.data  = sapply(names_i, FUN=function(i) ts(ERPT[ERPT$id_i==i, names_k], start=c(1995, 1), frequency=12), simplify=FALSE)
  
  # Oersal,Arsova 2016:21, Tab.6 (only for individual results) #
  R.lags = c(3, 3, 3, 4, 3, 3, 3); names(R.lags)=names_i  # lags of VAR model by MAIC
  R.cain = pcoint.CAIN(L.data, lags=R.lags, type="SL_trend")
  R.pcsl = pcoint.SL(L.data,   lags=R.lags, type="SL_trend")
  expect_equal(R.cain$individual$stats, R.pcsl$individual$stats)
  expect_equal(R.cain$individual$pvals, R.pcsl$individual$pvals)
  
  # Oersal,Arsova 2016:22, Tab.7/8 #
  R.lags = c(3, 3, 3, 4, 4, 3, 4); names(R.lags)=names_i  # lags of VAR model by MAIC
  R.t_D  = list(t_break=89)  #  a level shift and trend break in 2002_May for all countries
  R.cain = pcoint.CAIN(L.data, lags=R.lags, t_D=R.t_D, type="SL_trend")
  
  # identity irrespective of the 't_D' specification form #
  L.t_D = sapply(names_i, function(i) list(t_break=89), simplify=FALSE)
  L.t_D$France$t_shift = 89
  R.cain_LtD = pcoint.CAIN(L.data, lags=R.lags, t_D=L.t_D, type="SL_trend")
  
  # check #
  our_France   = unname(R.cain$individual$pvals["France", ])
  their_France = c(0.02, 0.25, 0.43)
  expect_equal(our_France, their_France, tolerance = 0.005)
  
  dim_K = length(names_k)
  our_panel = R.cain$panel$pvals
  their_panel = rbind(Hartung_K1 = c(0.02, 0.07, 0.63),
                      Hartung_K2 = c(0.02, 0.06, 0.63),
                      CAIN       = c(0.00, 0.02, 0.70))
  colnames(their_panel) = paste0("r_H0 = ", 0:(dim_K-1))
  expect_equal(our_panel, their_panel, tolerance = 0.005)
  
  R.cain$individual$t_D = NULL
  R.cain_LtD$individual$t_D = NULL
  expect_equal(R.cain, R.cain_LtD)
})


#test_that("PMSB functions can reproduce 'MDEM' in Silvestre,Surdeanu 2011:27, Ch.6,Tab.11(A)", {
  # data("MDEM")
  # names_k = c("m1", "gdp", "R") # variable names
  # names_i = levels(MDEM$id_i)   # country names
  # L.data = sapply(names_i, FUN=function(i) MDEM[MDEM$id_i==i, names_k], simplify=FALSE)
  # L.data = sapply(names_i, FUN=function(i) ts(MDEM[MDEM$id_i==i, names_k], start=1957, frequency=1), simplify=FALSE)
  # 
  # R.pcsl = pcoint.SL(L.data,  lags=3, n.factors=3, type="SL_trend")
  # R.pcjo = pcoint.JO(L.data,  lags=3, n.factors=3, type="Case4")
  # R.pmsb = pcoint.MSB(L.data, lag_max=6, n.factors=3, type="MSB_trend")  # three common factors by panel BIC 
#}


test_that("pcoint functions under cross-sectional indepencede are equal to their branch of coint function",{
  # prepare data and arguments #
  data("MERM")
  names_k = colnames(MERM)[-(1:2)] # variable names
  names_i = levels(MERM$id_i)      # country names
  L.data = sapply(names_i, FUN=function(i) ts(MERM[MERM$id_i==i, names_k], start=c(1995, 1), frequency=12), simplify=FALSE)
  R.lags = c(2, 2, 2, 2, 1, 2, 2, 4, 2, 3, 2, 2, 2, 2, 2, 1, 1, 2, 2); names(R.lags)=names_i
  
  # perform tests #
  R.pcjo = pcoint.JO(L.data, lags=R.lags, type="Case2")
  R.pcsl = pcoint.SL(L.data, lags=R.lags, type="SL_mean")
  L.cojo = lapply(names_i, function(i) coint.JO(L.data[[i]], dim_p=R.lags[i], type="Case2"))
  L.cosl = lapply(names_i, function(i) coint.SL(L.data[[i]], dim_p=R.lags[i], type_SL="SL_mean"))
  
  # check #
  R.jo = R.sl = list()
  #R.jo$stats = t(sapply(L.cojo, FUN=function(x) x$stats_TR))  # matrix of trace statistics (K x N) with ordering r_H0 = 0,...,K-1
  R.jo$pvals = t(sapply(L.cojo, FUN=function(x) x$pvals_TR))  # matrix of p-values (K x N) with ordering r_H0 = 0,...,K-1
  #R.sl$stats = t(sapply(L.cosl, FUN=function(x) x$stats_TR))  # matrix of trace statistics (K x N) with ordering r_H0 = 0,...,K-1
  R.sl$pvals = t(sapply(L.cosl, FUN=function(x) x$pvals_TR))  # matrix of p-values (K x N) with ordering r_H0 = 0,...,K-1
  
  expect_equal(unname(R.pcjo$individual$pvals), R.jo$pvals)
  expect_equal(unname(R.pcsl$individual$pvals), R.sl$pvals)
})


