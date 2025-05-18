## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----load, echo=FALSE---------------------------------------------------------
library(PRSPGx)

## ---- out.width = "500px", echo=FALSE, fig.cap="Table 1: Overview of PRS-DIS and PRS-PGx methods."----
knitr::include_graphics("overview.jpeg")

## ----eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE-----------------------
## Simulated sample example
data(PRSPGx.example); attach(PRSPGx.example)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
## Training
## Individual-level data, prepared only for PRS-PGx-Lasso
Y_train <- Y[1:3000]; T_train <- Tr[1:3000]; G_train <- G[1:3000,]

## Testing
## Individual-level data
Y_test <- Y[3001:4000]; T_test <- Tr[3001:4000]; G_test <- G[3001:4000,]

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
## Performance Evaluation
run_eval <- function(coef_est, Y_test, T_test, G_test){
  ## Prognostic score
  prog_score = as.vector(as.matrix(G_test)%*%coef_est$coef.G)
  ## Predictive score
  pred_score = as.vector(as.matrix(G_test)%*%coef_est$coef.TG)

  ## Performance evaluation
  fit <- summary(lm(Y_test ~ T_test + prog_score + T_test:pred_score))
  ## prediction accuracy: r2
  r2 = fit$adj.r.squared
  ## p-value of the interaction effect
  inter_pvalue = fit$coefficients[4,4]
  
  result <- c(r2=r2, inter_pvalue=inter_pvalue)
  return(result)
}

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
coef_est <- PRS_Dis_CT(DIS_GWAS, G_reference, pcutoff = 0.1, clumping = TRUE)

## -----------------------------------------------------------------------------
## Performance Evaluation
run_eval(coef_est, Y_test, T_test, G_test)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  coef_est <- PRS_Dis_LDpred2(DIS_GWAS, G_reference, pcausal = 0.1, h2 = 0.4)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
githubURL <- "https://github.com/zhaiso1/PRSPGx/blob/main/coef_est_LDpred2.rda?raw=true"
load(url(githubURL))

## -----------------------------------------------------------------------------
## Performance Evaluation
run_eval(coef_est, Y_test, T_test, G_test)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
coef_est <- PRS_PGx_CT(PGx_GWAS, G_reference, pcutoff = 0.01, clumping = TRUE)

## -----------------------------------------------------------------------------
## Performance Evaluation
run_eval(coef_est, Y_test, T_test, G_test)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
## PRS-PGx-L (method = 1)
coef_est <- PRS_PGx_Lasso(Y_train, T_train, G_train, lambda = 1.1, method = 1)

## -----------------------------------------------------------------------------
## Performance Evaluation
run_eval(coef_est, Y_test, T_test, G_test)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
## PRS-PGx-GL (method = 2)
coef_est <- PRS_PGx_Lasso(Y_train, T_train, G_train, lambda = 0.5, method = 2)

## -----------------------------------------------------------------------------
## Performance Evaluation
run_eval(coef_est, Y_test, T_test, G_test)

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
## PRS-PGx-SGL (method = 3)
coef_est <- PRS_PGx_Lasso(Y_train, T_train, G_train, lambda = 0.02, method = 3, alpha = 0.5)

## -----------------------------------------------------------------------------
## Performance Evaluation
run_eval(coef_est, Y_test, T_test, G_test)

## ---- out.width = "500px", echo=FALSE, fig.cap="Table 2: PRS-PGx-Bayes algorithm."----
knitr::include_graphics("algorithm.jpeg")

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  paras = c(3, 5)
#  coef_est <- PRS_PGx_Bayes(PGx_GWAS, G_reference, n.itr = 100, n.burnin = 50, n.gap = 5, paras = paras)

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
githubURL <- "https://github.com/zhaiso1/PRSPGx/blob/main/coef_est_Bayes.rda?raw=true"
load(url(githubURL))

## -----------------------------------------------------------------------------
## Performance Evaluation
run_eval(coef_est, Y_test, T_test, G_test)

