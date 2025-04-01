## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
  )
  library(twangRDC) 
  data(nola_south)

## -----------------------------------------------------------------------------
  library(twangRDC) 
  data(nola_south)

## -----------------------------------------------------------------------------
  # factors need to be coded as such
  nola_south$metarea = as.factor(nola_south$metarea)
  nola_south$c00_age12 = as.factor(nola_south$c00_age12)
  nola_south$c00_race = as.factor(nola_south$c00_race)
  nola_south$c00_sex = as.factor(nola_south$c00_sex)

## -----------------------------------------------------------------------------
  # only consider Orleans parish 
  nola_only = subset(nola_south , metarea==556)

## ---- results='hide' , warning=FALSE------------------------------------------
  # set the model parameters
  params = list(eta = .1 , max_depth = 5 , min_child_weight=50)

  # fit the xgboost model
  res.pik = ps.xgb(sim_pik ~ c00_age12 + c00_race + c00_sex , 
               strata="tract_id_str",
               data=nola_only,
               params=params,
               max.steps=50,
               iters.per.step=100,
               min.iter=1000,
               id.var="id",
               linkage = TRUE)

## ---- fig.show='hold',fig.cap = "Figure 1: Convergence of gradient boosted model for linkage failure."----
  plot(res.pik)

## ---- eval=FALSE--------------------------------------------------------------
#    bal.table(res.pik)

## ---- echo=FALSE, results='asis'----------------------------------------------
  knitr::kable(bal.table(res.pik),digits=3)

## ---- eval=FALSE--------------------------------------------------------------
#    bal.table(res.pik , type='strata' , include.var=TRUE , n=3 , decreasing = T)

## ---- echo=FALSE, results='asis'----------------------------------------------
  knitr::kable(bal.table(res.pik , type='strata' , include.var=TRUE , n=3 , decreasing = T),digits=3)

## ---- eval=FALSE--------------------------------------------------------------
#    # extract weights
#    w = get.weights(res.pik)
#  
#    # merge weights into data
#    dta=merge(dta , w , by='id' , all=TRUE)

## ---- results='hide' , warning=FALSE------------------------------------------
  # set the model parameters
  params = list(eta = .3 , max_depth = 5 , subsample = 1 , 
                max_delta_step=0 , gamma=0 , lambda=0 , alpha=0, 
                min_child_weight=50 , objective = "binary:logistic")

  # fit the xgboost model
  res.ps = ps.xgb(nola_rec ~ c00_age12:c00_sex + c00_race , 
               data=nola_south,
               params=params,
               max.steps=25,
               iters.per.step=100,
               min.iter=1000,
               min.width = 500,
               id.var="id",
               linkage = FALSE)

## ---- fig.show='hold',fig.cap = "Figure 2: Convergence of gradient boosted model for comparison group construction."----
plot(res.ps)

## ---- eval=FALSE--------------------------------------------------------------
#    bal.table(res.ps)

## ---- echo=FALSE, results='asis'----------------------------------------------
  knitr::kable(bal.table(res.ps),digits=4)

