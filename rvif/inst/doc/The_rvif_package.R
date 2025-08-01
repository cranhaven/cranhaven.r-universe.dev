## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(rvif)

## -----------------------------------------------------------------------------
# cv_vif(x, tol = 1e-30)

## -----------------------------------------------------------------------------
head(Wissel, n=5)

## -----------------------------------------------------------------------------
x = Wissel[,-c(1,2)]
cv_vif(x)

## -----------------------------------------------------------------------------
attach(Wissel)
  reg_W = lm(D~C+I+CP)
detach(Wissel)
  
x = model.matrix(reg_W)
cv_vif(x)

## -----------------------------------------------------------------------------
set.seed(2025)

	obs = 100
	cte = rep(1, obs)
	x2 = rnorm(obs, 5, 0.01) # variable with very little variability
	x3 = rnorm(obs, 5, 10)
	x4 = x3 + rnorm(obs, 5, 1) # fourth variable related to the third
	x5 = rnorm(obs, -1, 30)
	x = cbind(cte, x2, x3, x4, x5) # the first column has to be the intercept
	
cv_vif(x)

## -----------------------------------------------------------------------------
head(soil, n=5)

	x = soil[,-16]
	
cv_vif(x)

## -----------------------------------------------------------------------------
x = soil[,-c(2,16)]
cv_vif(x)

## -----------------------------------------------------------------------------
y = soil[,16]
reg_S = lm(y~as.matrix(x))
cv_vif(model.matrix(reg_S))

## -----------------------------------------------------------------------------
cte = rep(1, length(y))
x = cbind(x, cte)
cv_vif(x)

## -----------------------------------------------------------------------------
cte = rep(1, obs)
set.seed(2025)
x2 = rnorm(obs, 3, 4)
	
  x = cbind(cte, x2)

cv_vif(x)

## -----------------------------------------------------------------------------
set.seed(2025)
x3 = rbinom(obs, 1, 0.5)
  
  x = cbind(cte, x2, x3)
  head(x, n=5)

cv_vif(x)

## -----------------------------------------------------------------------------
# cv_vif_plot(x, limit = 40)

## ----out.width="50%", fig.align='center'--------------------------------------
plot(-2:20, -2:20, type = "n", xlab="Coefficient of Variation", ylab="Variance Inflation Factor")
	abline(h=10, col="red", lwd=3, lty=2)
	abline(h=0, col="black", lwd=1)
	abline(v=0.1002506, col="red", lwd=3, lty=3)

text(-1.25, 2, "A", pos=3, col="blue")
text(-1.25, 12, "B", pos=3, col="blue")
text(10, 12, "C", pos=3, col="blue")
text(10, 2, "D", pos=3, col="blue")

## ----out.width="50%", fig.align='center'--------------------------------------
x = Wissel[,-c(1,2)]
cv_vif_plot(cv_vif(x))

## ----out.width="50%", fig.align='center'--------------------------------------
set.seed(2025)

    obs = 100
    cte = rep(1, obs)
    x2 = rnorm(obs, 5, 0.01) # variable with very little variability
    x3 = rnorm(obs, 5, 10)
    x4 = x3 + rnorm(obs, 5, 1) # fourth variable related to the third
    x5 = rnorm(obs, -1, 30)
    x = cbind(cte, x2, x3, x4, x5) # the first column has to be the intercept
    
cv_vif_plot(cv_vif(x))
cv_vif_plot(cv_vif(x), limit=0) # note how the 'limit' argument works

## -----------------------------------------------------------------------------
# rvifs(x, ul = TRUE, intercept = TRUE, tol = 1e-30)

## -----------------------------------------------------------------------------
set.seed(2025)

    obs = 100
    cte = rep(1, obs)
    x2 = rnorm(obs, 5, 0.01) # variable with very little variability
    x3 = rnorm(obs, 5, 10)
    x4 = x3 + rnorm(obs, 5, 1) # fourth variable related to the third
    x5 = rnorm(obs, -1, 30)
    x = cbind(cte, x2, x3, x4, x5) # the first column has to be the intercept
    
rvifs(x)

## -----------------------------------------------------------------------------
x = soil[,-16]
	rvifs(x, intercept=FALSE)
	rvifs(x[,-2], intercept=FALSE)

## -----------------------------------------------------------------------------
cte = rep(1, obs)
set.seed(2025)
x2 = rnorm(obs, 3, 4)
	
  x = cbind(cte, x2)

rvifs(x)

## -----------------------------------------------------------------------------
cte = rep(1, obs)
set.seed(2025)
x2 = rnorm(obs, 3, 0.04)
	
  x = cbind(cte, x2)

rvifs(x)

## -----------------------------------------------------------------------------
x2 = x2 - mean(x2)
	
  x = cbind(cte, x2)

rvifs(x)

## -----------------------------------------------------------------------------
rvifs(x, ul=FALSE)

## -----------------------------------------------------------------------------
# multicollinearity(y, x, alpha = 0.05)

## -----------------------------------------------------------------------------
summary(reg_W)

## -----------------------------------------------------------------------------
y = Wissel[,2]
x = Wissel[,3:6]
  
  multicollinearity(y, x)

## -----------------------------------------------------------------------------
data(KG)
attach(KG)
  reg_KG = lm(consumption~wage.income+non.farm.income+farm.income)
detach(KG)
  
summary(reg_KG)

## -----------------------------------------------------------------------------
head(KG, n=5)

  y = KG[,1]
  x = model.matrix(reg_KG)
  
multicollinearity(y, x)

## ----error = TRUE-------------------------------------------------------------
try({
head(SLM1, n=5)
attach(SLM1)
  reg_SLM1 = lm(y1~V)
detach(SLM1)

library(car)
vif(reg_SLM1)
})

## -----------------------------------------------------------------------------
y = SLM1[,1]
x = SLM1[,-1]

multicollinearity(y, x)

## -----------------------------------------------------------------------------
head(SLM2, n=5)

  y = SLM2[,1]
  x = SLM2[,-1]

multicollinearity(y, x)

## -----------------------------------------------------------------------------
multicollinearity(y, x, alpha=0.01)

## -----------------------------------------------------------------------------
y = soil[,16]
x = soil[,-16] 
intercept = rep(1, length(y))

x = cbind(intercept, x) # the design matrix has to have the intercept in the first column

  multicollinearity(y, x)
  
  multicollinearity(y, x[,-3]) # eliminating the problematic variable (SumCation)
  
names(x[,-3])  

