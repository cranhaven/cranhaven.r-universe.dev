## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE, echo=FALSE-----------------------------------------
library(rSDR)
library(ManifoldOptim)
library(Matrix)
library(rstiefel)
library(RcppNumerical)
library(expm)
library(ggplot2)
library(future) #parallel
library(future.apply) #parallel
library(fdm2id) #ionosphere dataset

## ----echo=TRUE----------------------------------------------------------------
#
# # Load rSDR package
# if (!requireNamespace("rSDR", quietly = TRUE)) {
#   if (!requireNamespace("devtools", quietly = TRUE)){   install.packages("devtools")
# }
#   library(devtools)
#   devtools::install_github("scchen1/rSDR")
#   # or
#
#   #a local zip file (`rSDR-main.zip`) downloaded from GitHub
#   #devtools::install_local(path='yourpath/rSDR-main.zip')
# }



## ----ionosphere, echo=TRUE----------------------------------------------------
#Load ionosphere data in fdm2id package
utils::data("ionosphere", package = "fdm2id")

X<-as.matrix(ionosphere[,c(1:33)])
Y<-ifelse(ionosphere[,34]=='b',0,1)
# Y will be used for rSDR
Y<-matrix(Y,length(Y),1)

#Y.factor will be used in plot_rSDR
Y.factor<-factor(ionosphere$V35,levels=c('b','g'),labels=c('Bad','Good'))

## ----echo=TRUE----------------------------------------------------------------
set.seed(2435)
sdr_result<-rSDR(X=X, Y=Y, d=3, alpha=0.3,maxiter=1000,tol=1e-7)


# An optimal of C is obtained by maximizing the target function using
# ManifoldOptim method
head(sdr_result$C_value)

#The value of cost function f is equal to the negative of the target function.
sdr_result$f_value

#beta=sigma_x^(-0.5)%*%C
head(sdr_result$beta)

#projected_data: This projects X onto the estimated SDR directions (X %*% beta),
#the projected matrix (data) n x d
head(sdr_result$projected_data)


## ----echo=TRUE,fig.height=3,fig.width=4, fig.cap='Plot for 3-dimensional reduction using rSDR'----
#Plot for 3-dimensional reduction using rSDR

plot_rSDR(projected_data=sdr_result$projected_data,Y=Y.factor,Y.name='group',colors=c("#374E55FF", "#DF8F44FF"))


## ----echo=TRUE----------------------------------------------------------------
# plan(multisession) will launch parallel workers running in the background
# to save running time. To shut down background workers launched this way, call
# plan(sequential)
# use all local cores except one
# future::plan(future::multisession, workers = future::availableCores() - 1)
# use 2 cores for parallel
future::plan("multisession", workers = 2)
set.seed(2435)
opt_results<-optimal_alpha_cv( alpha.v=c(0.3, 0.5, 0.7),X=X,Y=Y,d=3,kfolds=5)

# Optimal alpha using cross validation
opt_results$opt.alpha

# The cost function values by alpha (column) for each cross validation (row)
head(opt_results$f_test)

# The mean of cost function value by alpha
opt_results$f_test.mean

# The standard deviation of cost function value by alpha
opt_results$f_test.sd

# The reduced dimension
opt_results$d

# Number of folds
opt_results$kfolds

## ----fig.height=3,fig.width=3,echo=TRUE,fig.cap='Mean and standard deviation of cost function (k-fold cross-validation method)'----
plot_alpha(opt_results=opt_results)

## ----echo=TRUE----------------------------------------------------------------
# plan(multisession) will launch parallel workers running in the background
# to save running time. To shut down background workers launched this way, call
# plan(sequential)
# use all local cores except one
# future::plan(future::multisession, workers = future::availableCores() - 1)
# use 2 cores for parallel
future::plan("multisession", workers = 2)
set.seed(2435)
opt_results<-optimal_alpha_boot(alpha.v=c(0.3,0.5, 0.7),X=X,Y=Y,d=3,R=10)
#opt_results<-optimal_alpha_boot(alpha.v=c(0.3,0.4,0.5,0.6, 0.7),X=X,Y=Y,d=3,R=50)

# Optimal alpha using bootstrap method
opt_results$opt.alpha

# The cost function values by alpha (column) for R bootstrap replicates (row)
head(opt_results$f_test)

# The mean of cost function value by alpha
opt_results$f_test.mean

# The standard deviation of cost function value by alpha
opt_results$f_test.sd

# The reduced dimension
opt_results$d

# Number of bootstrap replicates
opt_results$R


## ----fig.height=3,fig.width=3,fig.cap='Mean and standard deviation of cost function (bootstrap method)'----
plot_alpha(opt_results=opt_results)


## -----------------------------------------------------------------------------
set.seed(2435)
sdr_result<-rSDR(X=X, Y=Y, d=2, alpha=0.3,maxiter=1000,tol=1e-7)

# An optimal of C is obtained by maximizing the target function using ManifoldOptim method
head(sdr_result$C_value)

#The value of cost function f is equal to the negative of the target function.
sdr_result$f_value

#beta=sigma_x^(-0.5)%*%C
head(sdr_result$beta)

#projected_data: This projects X onto the estimated SDR directions (X %*% beta), the projected matrix (data) n x d
head(sdr_result$projected_data)


## -----------------------------------------------------------------------------
#Plot for 2-dimensional reduction using rSDR
plot_rSDR(projected_data=sdr_result$projected_data,Y=Y.factor,Y.name='group',colors=c("#374E55FF", "#DF8F44FF"))

