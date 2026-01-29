## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, 
  include = !FALSE, 
  fig.width = 7, 
  fig.height = 5, 
  out.width = '0.39\\textwidth')
chinclude <- TRUE
library(graphpcor)

## ----graph1, include = chinclude----------------------------------------------
tree1 <- treepcor(p1 ~ c1 + c2 - c3)
tree1
summary(tree1)

## ----dim1---------------------------------------------------------------------
dim(tree1)

## ----visgraph1, include = chinclude-------------------------------------------
plot(tree1)

## ----qs1, include = chinclude-------------------------------------------------
prec(tree1)

## ----q, include = chinclude---------------------------------------------------
q1 <- prec(tree1, theta = 0)
q1

## ----v1, include = chinclude--------------------------------------------------
vcov(tree1) ## assume theta = 0 (\gamma_1 = 1)
vcov(tree1, theta = 0.5) # \gamma_1^2 = exp(2 * 0.5) = exp(1)
cov1a <- vcov(tree1, theta = 0) 
cov1a

## ----c1, include = chinclude--------------------------------------------------
c1 <- cov2cor(cov1a)
round(c1, 3)

## ----graph2-------------------------------------------------------------------
tree2 <- treepcor(
  p1 ~ p2 + c1 + c2, 
  p2 ~ c3 - c4)
dim(tree2)
tree2
summary(tree2)

## ----visgraph2----------------------------------------------------------------
plot(tree2)

## ----drop1--------------------------------------------------------------------
drop(tree2)

## ----q2-----------------------------------------------------------------------
q2 <- prec(tree2, theta = c(0, 0))
q2

## ----c2-----------------------------------------------------------------------
cov2 <- vcov(tree2, theta = c(0, 0))
cov2
c2 <- cov2cor(cov2)
round(c2, 3)

## ----graph2b------------------------------------------------------------------
tree2b <- treepcor(
  p1 ~ -p2 + c1 + c2, 
  p2 ~ -c3 + c4)
tree2b
summary(tree2b)

## ----prec2--------------------------------------------------------------------
q2b <- prec(tree2b, theta = c(0, 0))
q2b

## ----cov2b--------------------------------------------------------------------
all.equal(solve(q2)[1:4, 1:4], 
          solve(q2b)[1:4, 1:4])

