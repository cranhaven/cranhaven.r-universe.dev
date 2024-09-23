## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)


## ----eval=FALSE---------------------------------------------------------------
#  Strat_Col <- "e"
#  e <- RunCoxRegression_STRATA(df, time1, time2, event, names, term_n, tform, keep_constant,
#                               a_n, modelform, fir, der_iden, control,Strat_Col)

## ----eval=FALSE---------------------------------------------------------------
#  Strat_Col <- c("e")
#  e <-RunPoissonRegression_STRATA(df, pyr, event, names, term_n, tform, keep_constant,
#                                  a_n, modelform, fir, der_iden, control,Strat_Col)

## ----eval=FALSE---------------------------------------------------------------
#  e <- RunCoxRegression_Basic(df, time1, time2, event, names,
#                              keep_constant, a_n, der_iden, control)

## ----eval=FALSE---------------------------------------------------------------
#  e <- RunCoxRegression_Single(df, time1, time2, event, names, term_n, tform,
#                               a_n, modelform, fir, control)
#  
#  e <- RunPoissonRegression_Single(df, pyr, event, names, term_n, tform,
#                                   a_n, modelform, fir, control)

## ----eval=FALSE---------------------------------------------------------------
#  df$censor <- (df$lung==0) #censoring column made
#  event <- "censor" #event type switched to censoring
#  
#  plot_options <- list("name"="run_2","verbose"=FALSE,"studyID"="studyID","age_unit"="years")
#  #modified plotting function used to get censoring weights
#  dft <- GetCensWeight(df, time1, time2, event, names, term_n, tform, keep_constant,
#                       a_n, modelform, fir, control, plot_options) #generates a survival curve
#  t_ref <- dft$t
#  surv_ref <- dft$surv
#  t_c <- df$t1
#  cens_weight <- approx(t_ref, surv_ref, t_c,rule=2)$y
#  #the surviving proportions used as censoring weight
#  event <- "lung" #event switched back
#  
#  e <- RunCoxRegression_CR(df, time1, time2, event, names, term_n, tform, keep_constant,
#                           a_n, modelform, fir, der_iden, control,cens_weight)

## ----eval=TRUE----------------------------------------------------------------
a <- c(0,0,0,1,1,1)
b <- c(1,1,1,2,2,2)
c <- c(0,1,2,2,1,0)
d <- c(1,1,0,0,1,1)
e <- c(0,1,1,1,0,0)
df <- data.table('t0'=a,'t1'=b,'e0'=c,'e1'=d,'fac'=e)
time1 <- "t0"
time2 <- "t1"
df$pyr <- df$t1 - df$t0
pyr <- "pyr"
events <- c('e0','e1')

## ----eval=TRUE----------------------------------------------------------------
names_e0 <- c('fac')
names_e1 <- c('fac')
names_shared <- c('t0','t0')
term_n_e0 <- c(0)
term_n_e1 <- c(0)
term_n_shared <- c(0,0)
tform_e0 <- c("loglin")
tform_e1 <- c("loglin")
tform_shared <- c("quad_slope","loglin_top")
keep_constant_e0 <- c(0)
keep_constant_e1 <- c(0)
keep_constant_shared <- c(0,0)
a_n_e0 <- c(-0.1)
a_n_e1 <- c(0.1)
a_n_shared <- c(0.001, -0.02)
name_list <- list('shared'=names_shared,'e0'=names_e0,'e1'=names_e1)
term_n_list <- list('shared'=term_n_shared,'e0'=term_n_e0,'e1'=term_n_e1)
tform_list <- list('shared'=tform_shared,'e0'=tform_e0,'e1'=tform_e1)
keep_constant_list <- list('shared'=keep_constant_shared,
                           'e0'=keep_constant_e0,'e1'=keep_constant_e1)
a_n_list <- list('shared'=a_n_shared,'e0'=a_n_e0,'e1'=a_n_e1)

## ----eval=TRUE----------------------------------------------------------------
Joint_Multiple_Events(df, events, name_list, term_n_list,
                      tform_list, keep_constant_list, a_n_list)

## ----eval=TRUE----------------------------------------------------------------
der_iden <- 0
modelform <- "M"
fir <- 0
control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 10,'halfmax' = 5,'epsilon' = 1e-3,
   'dbeta_max' = 0.5,'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
   'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
guesses_control <- list("maxiter"=10,"guesses"=10,"lin_min"=0.001,"lin_max"=1,
    "loglin_min"=-1,"loglin_max"=1, "lin_method"="uniform","loglin_method"="uniform",
     strata=FALSE)
Strat_Col <- 'f'
RunPoissonRegression_Joint_Omnibus(df, pyr, events, name_list, term_n_list,
                                        tform_list, keep_constant_list, a_n_list,
                                        modelform, fir, der_iden, control,Strat_Col)

## ----eval=FALSE---------------------------------------------------------------
#  
#  a_n <- list(c(1,1,1),c(1,2,1),c(1,2,2),c(2,1,1))
#  
#  control$maxiter <- 5 # runs each (4) starts 1 iteration, and then runs the best 5 iterations
#  control$maxiters <- c(1,1,1,1,5) # runs each (4) starts 1 iteration, and then runs the best 5 iterations
#  control$maxiters <- c(5,5,5,5,5) # runs each (4) starts 5 iterations, and then runs the best 5 iterations
#  
#  e <- RunCoxRegression_Omnibus(df, time1, time2, event,
#                                names, term_n, tform, keep_constant,
#                                a_n, modelform,control=control)

