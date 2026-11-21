

####################
###  UNIT TESTS  ###
####################
#
# For exported "coint" functions 
# and their auxiliary modules.



test_that("OLS of the transformed model and GLS are identical in aux_GLStrend.", {
  # prepare data #
  data("ERPT")
  names_k = c("lpm5", "lfp5", "llcusd")  # variable names for "Chemicals and related products"
  names_i = levels(ERPT$id_i)[c(1,6,2,5,4,3,7)]  # ordered country names
  L.data = sapply(names_i, FUN=function(i) ts(ERPT[ERPT$id_i==i, names_k], start=c(1995, 1), frequency=12), simplify=FALSE)
  
  # GLSE of the deterministic term #
  for(dim_p in 0:3){
    R.vecm = VECM(y=L.data$France, dim_r = 2, dim_p = 4, type = "Case4", t_D1=list(t_break=89))
    D_VAR  = aux_dummy(dim_T=R.vecm$dim_T+R.vecm$dim_p, t_break=R.vecm$t_D1$t_break, t_shift=R.vecm$t_D1$t_break, type="both")
    #alpha_oc = if(R.vecm$dim_r==0){ diag(dim_K) }else{ R.vecm$RRR$S00inv %*% R.vecm$RRR$S01 %*% R.vecm$RRR$V[ ,(R.vecm$dim_r+1):R.vecm$dim_K] }
    alpha_oc = MASS::Null(R.vecm$VECM$alpha)
    R.GLSd1 = aux_GLStrend(y=L.data$France, OMEGA=R.vecm$VECM$OMEGA, A=R.vecm$A, D=D_VAR, dim_p=R.vecm$dim_p)
    R.GLSd2 = aux_GLStrend(y=L.data$France, alpha=R.vecm$VECM$alpha, alpha_oc=alpha_oc,
                           OMEGA=R.vecm$VECM$OMEGA, A=R.vecm$A, D=D_VAR, dim_p=R.vecm$dim_p)
    
    # check #
    expect_equal(R.GLSd2$Q%*%t(R.GLSd2$Q), solve(R.vecm$VECM$OMEGA))
    expect_equal(R.GLSd1$MU, R.GLSd2$MU)
    
    # OLSE of the deterministic term #
    #R.GLSd1 = aux_GLStrend(y=L.data$France, OMEGA=diag(R.vecm$dim_K), A=R.vecm$A, D=D_VAR, dim_p=R.vecm$dim_p)
  }
})


test_that("aux_CointMoments can reproduce the examples of Doornik 1998:587, 
  Johansen et al. 2000:235, Trenkler et al. 2008:350, and  Kurita,Nielsen 2019:19", {
  # Johansen procedure with a weakly exogenous variable #
  our_JO   = aux_CointMoments(dim_K=3, dim_L=1, type="Case4")[ , 1:2]  # remove moments for ME
  their_JO = cbind(TR_EZ = c(36.52, 20.43, 8.265),
                   TR_VZ = c(59.55, 34.23, 14.40))  # Doornik 1998:587, Ch.10.2 for r_H0=0:2
  rownames(their_JO) = rownames(our_JO)
  expect_equal(our_JO, their_JO, tolerance = 0.005)
  
  # JMN (2000) procedure #
  moments = aux_CointMoments(dim_K=5, dim_T=91, t_D1=list(t_break=c(27,77)), type="JMN_Case4")
  m = moments[ , 1, drop=FALSE]  # means for TR
  v = moments[ , 2, drop=FALSE]  # variances for TR
  LR.stats  = c(256.46, 157.43, 71.96, 29.50, 10.42)  # Johansen et al. 2000:235, Tab.8
  our_JMN   = 1-pgamma(LR.stats, shape=m^2/v, rate=m/v)
  their_JMN = c(0, 0, 0.043, 0.621, 0.745)
  expect_equal(our_JMN, their_JMN, tolerance = 0.005)
  
  # TSL (2008) procedure #
  our_TSL   = aux_CointMoments(dim_K=3, dim_T=184, t_D1=list(t_break=125), r_H0=1, type="TSL_trend")[ , 1:2]  # remove moments for ME
  their_TSL = c(TR_EZ=11.3009, TR_VZ=17.5418)  # Trenkler et al. 2008:350
  expect_equal(our_TSL, their_TSL, tolerance = 0.0005)
  
  # KN (2019) procedure #
  # counted signs of coefficients in Kurita,Nielsen 2019:21/22, Tab.A1/A2
  expect_equal( colSums(coint_rscoef$KN_Case2 < 0), c(TR_lam=14, TR_del=17, TR_cov=18) )
  expect_equal( colSums(coint_rscoef$KN_Case2 > 0), c(TR_lam=17, TR_del=17, TR_cov=16) )
  expect_equal( colSums(coint_rscoef$KN_Case4 < 0), c(TR_lam=17, TR_del=16, TR_cov=13) )
  expect_equal( colSums(coint_rscoef$KN_Case4 > 0), c(TR_lam=18, TR_del=18, TR_cov=11) )
  
  # Kurita,Nielsen 2019:19, Ch.5, Tab.4 and supplementary spreadsheet
  moments = aux_CointMoments(dim_K=2, dim_L=3, dim_T=94, type="KN_Case4", t_D1=list(t_break=71))[ , 1:2]  # remove moments for ME
  m = moments[ , 1, drop=FALSE]  # means for TR
  v = moments[ , 2, drop=FALSE]  # variances for TR
  LR.stats = c(56.610, 21.964)
  KN_pvals = 1-pgamma(LR.stats, shape=m^2/v, rate=m/v)
  KN_cvals = c(qgamma(0.95, shape=m^2/v, rate=m/v))
  our_KN   = cbind(KN_pvals, KN_cvals)
  their_KN = cbind(c(0.014, 0.148), c(50.864, 26.334))
  dimnames(their_KN) = dimnames(our_KN)
  expect_equal(our_KN, their_KN, tolerance = 0.005)
  
  # approximate equivalence of JMN (2000) and KN (2019) 
  JMN_moments = aux_CointMoments(dim_K=4, dim_T=100, t_D1=list(t_break=c(25,75)), type="JMN_Case4")[ ,1:2]
  KN_moments  = aux_CointMoments(dim_K=4, dim_T=100, t_D1=list(t_break=c(25,75)), type="KN_Case4")[ ,1:2]
  zero_matrix = matrix(0, nrow=4, ncol=2, dimnames=dimnames(JMN_moments))
  expect_equal((JMN_moments-KN_moments)/JMN_moments, zero_matrix, tolerance = 0.09)
})


test_that("coint.JO() and VECM() can reproduce the basic examples with four seasons from urca package", {
  # prepare data #
  library("urca")
  data(denmark)
  sjd = denmark[, c("LRM", "LRY", "IBO", "IDE")]
  dim_K = ncol(sjd)  # number of endogenous variables
  
  # perform Johansen procedure on full VECM #
  sjd.vecm = urca::ca.jo(sjd, ecdet="const", type="eigen", K=2, spec="transitory", season=4)
  sjd.vars = vars::vec2var(sjd.vecm, r=1)
  R.JOrank = coint.JO(y=sjd,      dim_p=2, type="Case2", t_D2=list(n.season=4))
  R.JOvecm = VECM(y=sjd, dim_r=1, dim_p=2, type="Case2", t_D2=list(n.season=4))
  
  # perform Johansen procedure on partial VECM with structural shift #
  R.KNrank = coint.JO(y=sjd[ , c("LRM"), drop=FALSE],   dim_p=2, 
                      x=sjd[ , c("LRY", "IBO", "IDE")], dim_q=2, 
                      type="Case2", t_D1=list(t_shift=36), t_D2=list(n.season=4))
  R.KNvecm = VECM(y=sjd[ , c("LRM"), drop=FALSE],   dim_p=2, 
                  x=sjd[ , c("LRY", "IBO", "IDE")], dim_q=2, dim_r=1, 
                  type="Case2", t_D1=list(t_shift=36), t_D2=list(n.season=4))
  
  # check #
  our_statsME  = R.JOrank$stats_ME
  urca_statsME = sort(sjd.vecm@teststat, decreasing=TRUE)
  urca_resids  = t(resid(sjd.vars)); dimnames(urca_resids) = dimnames(R.JOvecm$resid)
  
  expect_equal(our_statsME,    urca_statsME)
  expect_equal(R.JOvecm$resid, urca_resids)
  expect_equal(R.JOrank$lambda[1:dim_K], R.JOvecm$RRR$lambda[1:dim_K])
  expect_equal(R.KNrank$lambda[1],       R.KNvecm$RRR$lambda[1])
})


### reproduce Oersal,Arsova 2016:22, Tab.7.5/8 ###
# R.JOrank  = coint.JO(y=sjd, dim_p=3, type="Case4")
# R.TSLrank = coint.SL(y=L.data$France, dim_p=3, type_SL="SL_trend", t_D=list(t_break=89))
# R.MSBrank = coint.MSB(eit=sjd, lag_max = 5, MIC="AIC", type_MSB = "MSB_trend")

