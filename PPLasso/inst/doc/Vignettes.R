## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(message = FALSE,warning = FALSE)
library(PPLasso)
library(ggplot2)
library(cvCovEst)
set.seed(123456)

## ----generate Sigma-----------------------------------------------------------
p <- 50 # number of variables 
d <- 10 # number of actives
n <- 50 # number of samples
actives <- 1:d
nonacts <- c(1:p)[-actives]
Sigma <- matrix(0, p, p)
Sigma[actives, actives] <- 0.3
Sigma[-actives, actives] <- 0.5
Sigma[actives, -actives] <- 0.5
Sigma[-actives, -actives] <- 0.7
diag(Sigma) <- rep(1,p)
actives_pred <- 1:5

## ----X------------------------------------------------------------------------
X_bm <- MASS::mvrnorm(n = n, mu=rep(0,p), Sigma, tol = 1e-6, empirical = FALSE)
colnames(X_bm) <- paste0("X",(1:p))
n1=n2=n/2 # 1:1 randomized
beta1 <- rep(0,p)
beta1[actives] <- 1
beta2 <- beta1
beta2[actives_pred] <- 2
beta <- c(beta1, beta2)
TRT1 <- c(rep(1,n1), rep(0,n2))
TRT2 <- c(rep(0,n1), rep(1,n2))
Y <- cbind(X_bm*TRT1,X_bm*TRT2)%*%beta+TRT2+rnorm(n,0,1)

## ----est Sigma----------------------------------------------------------------
cv_cov_est_out <- cvCovEst(
      dat = X_bm,
      estimators = c(
        linearShrinkLWEst, denseLinearShrinkEst,
        thresholdingEst, poetEst, sampleCovEst
      ),
      estimator_params = list(
        thresholdingEst = list(gamma = c(0.2, 0.4)),
        poetEst = list(lambda = c(0.1, 0.2), k = c(1L, 2L))
      ),
      cv_loss = cvMatrixFrobeniusLoss,
      cv_scheme = "v_fold",
      v_folds = 5
    )
Sigma_est <- cov2cor(cv_cov_est_out$estimate) 

## ----WLasso model, warning = FALSE, message = FALSE---------------------------
mod <- ProgPredLasso(X1 = X_bm[1:n1, ], X2 = X_bm[(n1+1):n, ], Y = Y, cor_matrix = Sigma_est)

## -----------------------------------------------------------------------------
#alpha1
 mod$beta.min[1]

## -----------------------------------------------------------------------------
#alpha2 
 mod$beta.min[2]

## ----variable selection, figures-side, fig.show="hold", out.width="50%",echo=FALSE,fig.cap="\\label{fig:fig1}Left: Identified prognostic biomarkers. Right: Identified predictive biomarkers."----
beta_min <- mod$beta.min[-c(1,2)]
df_beta <- data.frame(beta_est=beta_min, Status = ifelse(c(beta1, beta2-beta1)==0, "non-active", "active"))
df_prog <- data.frame(beta_est=beta_min[1:p], Status = ifelse(beta1==0, "false positive", "true prognostic"), index=c(1:p))

df_pred <- data.frame(beta_est=beta_min[(p+1):(2*p)], Status = ifelse(c(beta2-beta1)==0, "false positive", "true predictive"), index=c(1:p))

df_plot_prog <- df_prog[which(df_prog$beta_est!=0), ]
df_plot_pred <- df_pred[which(df_pred$beta_est!=0), ]

ggplot2::ggplot(data=df_plot_prog, mapping=aes(y=beta_est, x=index, color=Status))+geom_point()+
  theme_bw()+ylab("Estimated coefficients")+xlab("Indices of selected variables")

ggplot2::ggplot(data=df_plot_pred, mapping=aes(y=beta_est, x=index, color=Status))+geom_point()+
  theme_bw()+ylab("Estimated coefficients")+xlab("Indices of selected variables")

## -----------------------------------------------------------------------------
which(beta_min[1:p]!=0)

## -----------------------------------------------------------------------------
which(beta_min[(p+1):(2*p)]!=0)

