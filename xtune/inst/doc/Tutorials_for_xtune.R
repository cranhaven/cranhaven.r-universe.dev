## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ex1----------------------------------------------------------------------
library(xtune)
data("example")
X <- example$X; Y <- example$Y; Z <- example$Z
dim(X);dim(Z)

## ----dim----------------------------------------------------------------------
X[1:3,1:10]

## -----------------------------------------------------------------------------
Z[1:10,]

## ----fit1---------------------------------------------------------------------
fit.example1 <- xtune(X,Y,Z, family = "linear", c = 1)

## ----ex1uni-------------------------------------------------------------------
unique(fit.example1$penalty.vector)

## ----ex2_data-----------------------------------------------------------------
data(diet)
head(diet$DietItems)
head(diet$weightloss)

## ----ex2ex--------------------------------------------------------------------
head(diet$NuitritionFact)

## ----exfit--------------------------------------------------------------------
fit.diet = xtune(X = diet$DietItems,Y=diet$weightloss,Z = diet$NuitritionFact, family="binary", c = 0)

## ----indiv--------------------------------------------------------------------
fit.diet$penalty.vector

## ----ex3_data-----------------------------------------------------------------
data(gene)
gene$GeneExpression[1:3,1:5]
gene$PreviousStudy[1:5,]

## ----multiclass---------------------------------------------------------------
data("example.multiclass")
dim(example.multiclass$X); dim(example.multiclass$Y); dim(example.multiclass$Z)
head(example.multiclass$X)[,1:5]
head(example.multiclass$Y)
head(example.multiclass$Z)

## -----------------------------------------------------------------------------
fit.multiclass = xtune(X = example.multiclass$X,Y=example.multiclass$Y,Z = example.multiclass$Z, U = example.multiclass$U, family  = "multiclass", c = 0.5)

# check the tuning parameter
fit.multiclass$penalty.vector

## -----------------------------------------------------------------------------
pred.prob = predict_xtune(fit.multiclass,newX = cbind(example.multiclass$X, example.multiclass$U))
head(pred.prob)

## -----------------------------------------------------------------------------
pred.class <- predict_xtune(fit.multiclass,newX = cbind(example.multiclass$X, example.multiclass$U), type = "class")
head(pred.class)

## -----------------------------------------------------------------------------
misclassification(pred.class,true = example.multiclass$Y)

## ----sp1----------------------------------------------------------------------
fit.eb <- xtune(X,Y, family = "linear", c = 0.5)

## ----sp2----------------------------------------------------------------------

Z_iden = diag(ncol(diet$DietItems))
fit.diet.identity = xtune(diet$DietItems,diet$weightloss,Z_iden, family = "binary", c = 0.5)

## ----sp22---------------------------------------------------------------------
fit.diet.identity$penalty.vector

