## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library("ILSE")

## ----eval = FALSE-------------------------------------------------------------
#    set.seed(1)
#    n <- 100
#    p <- 6
#    X <- MASS::mvrnorm(n, rep(0, p), cor.mat(p, rho=0.5))
#    beta0 <- rep(c(1,-1), times=3)
#    Y <- -2+ X %*% beta0 + rnorm(n, sd=1)

## ----eval = FALSE-------------------------------------------------------------
#  ilse1 <- ilse(Y~X)
#  print(ilse1)

## ----eval = FALSE-------------------------------------------------------------
#  dat <- data.frame(Y=Y, X=X)
#  ilse1 <- ilse(Y~., data=dat)
#  print(ilse1)
#  Coef(ilse1) # access the coefficients
#  Fitted.values(ilse1)[1:5]
#  Residuals(ilse1)[1:5]
#  

## ----eval = FALSE-------------------------------------------------------------
#  s1 <- summary(ilse1)
#  s1

## ----eval = FALSE-------------------------------------------------------------
#  mis_rate <- 0.3
#  set.seed(1)
#  na_id <- sample(1:(n*p), n*p*mis_rate)
#  Xmis <- X
#  Xmis[na_id] <- NA
#  ncomp <- sum(complete.cases(Xmis))
#  message("Number of complete cases is ", ncomp, '\n')

## ----eval = FALSE-------------------------------------------------------------
#  lm1 <- lm(Y~Xmis)
#  s_cc <- summary.lm(lm1)
#  s_cc

## ----eval = FALSE-------------------------------------------------------------
#  ilse2 <- ilse(Y~Xmis+0, data=NULL, verbose=T)
#  print(ilse2)

## ----eval = FALSE-------------------------------------------------------------
#  ilse2 <- ilse(Y~Xmis, data=NULL, verbose=T)
#  print(ilse2)

## ----eval = FALSE-------------------------------------------------------------
#  s2 <- summary(ilse2, Nbt=20)
#  s2

## ----eval = FALSE-------------------------------------------------------------
#  fimllm <- fimlreg(Y~Xmis)
#  print(fimllm)
#  

## ----eval = FALSE-------------------------------------------------------------
#  s_fiml <- summary(fimllm, Nbt=20)
#  s_fiml

## ----eval = FALSE-------------------------------------------------------------
#  pMat <- cbind(CC=s_cc$coefficients[,4], ILSE=s2[,4], FIML=s_fiml[,4])
#  library(ggplot2)
#  df1 <- data.frame(Pval= as.vector(pMat[-1,]),
#                      Method =factor(rep(c('CC', "ILSE", "FIML"),each=p)),
#                      covariate= factor(rep(paste0("X", 1:p), times=3)))
#  ggplot(data=df1, aes(x=covariate, y=Pval, fill=Method)) + geom_bar(position = "dodge", stat="identity",width = 0.5) + geom_hline(yintercept = 0.05, color='red') + geom_hline(yintercept = 0.1, color='blue')

## ----eval = FALSE-------------------------------------------------------------
#  dat <- data.frame(Y=Y, X=Xmis)
#  dat$Sex <- factor(rep(c('male', 'female'), times=n/2))
#  dat$Sex[sample(1:n, n*mis_rate)] <- NA
#  ilse1 <- ilse(Y~., data=dat, verbose = T)

## ----eval = FALSE-------------------------------------------------------------
#  s3 <- summary(ilse1, Nbt=40)
#  s3

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(10)
#  n <- 100
#  p <- 6
#  X <- MASS::mvrnorm(n, rep(0, p), cor.mat(p, rho=0.5))
#  beta0 <- rep(c(1,0), times=3)
#  Y <- -2+ X %*% beta0 + rnorm(n, sd=1)
#  message("The true regression coefficients are: ", paste0(beta0, '  '))

## ----eval = FALSE-------------------------------------------------------------
#  mis_rate <- 0.3
#  set.seed(1)
#  na_id <- sample(1:(n*p), n*p*mis_rate)
#  Xmis <- X
#  Xmis[na_id] <- NA

## ----eval = FALSE-------------------------------------------------------------
#  dat <- data.frame(Y=Y, X=Xmis)
#  ilse1 <- ilse(Y~., data=dat, verbose = T)
#  s3 <- summary(ilse1)
#  s3

## ----eval = FALSE-------------------------------------------------------------
#  lm1 <- lm(Y~Xmis)
#  s_cc <- summary.lm(lm1)
#  fimllm <- fimlreg(Y~Xmis)
#  s_fiml <- summary(fimllm)

## ----eval = FALSE-------------------------------------------------------------
#  library(ggthemes)
#  pMat <- cbind(CC=s_cc$coefficients[,4], ILSE=s3[,4], FIML=s_fiml[,4])
#  df1 <- data.frame(Pval= as.vector(pMat[-1,]),
#                      Method =factor(rep(c('CC', "ILSE", "FIML"),each=p)),
#                      covariate= factor(rep(paste0("X", 1:p), times=3)))
#  ggplot(data=df1, aes(x=covariate, y=Pval, fill=Method)) + geom_bar(position = "dodge", stat="identity",width = 0.5) + geom_hline(yintercept = 0.05, color='red') + scale_fill_economist()

## ----eval = FALSE-------------------------------------------------------------
#  
#  # generate data from linear model
#  set.seed(10)
#  n <- 100
#  p <- 6
#  X <- MASS::mvrnorm(n, rep(0, p), cor.mat(p, rho=0.5))
#  beta0 <- rep(c(1,-1), times=3)
#  Y <- -2+ X %*% beta0 + rnorm(n, sd=1)
#  
#  # generate missing values
#  mis_rate <- 0.8
#  set.seed(1)
#  na_id <- sample(1:(n*p), n*p*mis_rate)
#  Xmis <- X
#  Xmis[na_id] <- NA
#  # retain 4 complete cases.
#  Xmis[1:4,] <- X[1:4, ]
#  sum(complete.cases(Xmis))

## ----eval = FALSE-------------------------------------------------------------
#  lm1 <- lm(Y~Xmis)
#  summary.lm(lm1)

## ----eval = FALSE-------------------------------------------------------------
#  ilse2 <- ilse(Y~Xmis, verbose = T)
#  s2 <- summary(ilse2)
#  s2

## ----eval = FALSE-------------------------------------------------------------
#  n <- 1000
#  p <- 50
#  X <- MASS::mvrnorm(n, rep(0, p), cor.mat(p, rho=0.5))
#  beta0 <- rep(c(1,-1), length=p)
#  Y <- -2+ X %*% beta0 + rnorm(n, sd=1)
#  
#  mis_rate <- 0.3
#  set.seed(1)
#  na_id <- sample(1:(n*p), n*p*mis_rate)
#  Xmis <- X
#  Xmis[na_id] <- NA
#  
#  
#  Xmis[1:10,] <- X[1:10,]
#  lm1 <- lm(Y~Xmis)
#  lm1
#  system.time(ilse2 <- ilse(Y~Xmis, data=NULL, verbose=T))

## -----------------------------------------------------------------------------
sessionInfo()

