## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  library("ILSE")
#  data("nhanes")

## ----eval = FALSE-------------------------------------------------------------
#  ncomp <- sum(complete.cases(nhanes))
#  message("Number of complete cases is ", ncomp, '\n')
#  ilse2 <- ilse(age~., data=nhanes, verbose=T)
#  print(ilse2)

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(1)
#  s2 <- summary(ilse2, Nbt=20)
#  s2

## ----eval = FALSE-------------------------------------------------------------
#  lm1 <- lm(age~., data=nhanes)
#  s_cc <- summary.lm(lm1)
#  s_cc

## ----eval = FALSE-------------------------------------------------------------
#  fimllm <- fimlreg(age~., data=nhanes)
#  print(fimllm)

## ----eval = FALSE-------------------------------------------------------------
#  s_fiml <- summary(fimllm, Nbt=20)
#  s_fiml

## ----eval = FALSE-------------------------------------------------------------
#  library(ggplot2)
#  library(ggthemes)
#  pMat <- cbind(CC=s_cc$coefficients[,4], ILSE=s2[,4], FIML=s_fiml[,4])
#  df1 <- data.frame(Pval= as.vector(pMat[-1,]),
#                      Method =factor(rep(c('CC', "ILSE", "FIML"),each=3)),
#                      covariate= factor(rep(row.names(pMat[-1,]), times=3)))
#  ggplot(data=df1, aes(x=covariate, y=Pval, fill=Method)) + geom_bar(position = "dodge", stat="identity",width = 0.5) + geom_hline(yintercept = 0.05, color='red') + geom_hline(yintercept = 0.1, color='blue') +
#    scale_fill_economist()

## -----------------------------------------------------------------------------
sessionInfo()

