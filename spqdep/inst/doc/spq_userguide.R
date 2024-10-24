## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message = FALSE, collapse = TRUE, warning = FALSE------------------------
library(spdep)
library(spatialreg)
library(sf)
library(ggplot2)

## ----message = FALSE, collapse = TRUE-----------------------------------------
library(spqdep)
data("provinces_spain", package = "spqdep")
data("FastFood.sf", package = "spqdep")
data("Boots.sf", package = "spqdep")

## -----------------------------------------------------------------------------
set.seed(123)
N <- 100
cx <- runif(N)
cy <- runif(N)
coor <- cbind(cx,cy)
p <- c(1/6,3/6,2/6) # proportion of classes
rho = 0.5 # level of spatial structure
listw <- spdep::nb2listw(knn2nb(knearneigh(coor, k = 4)))
fx <- dgp.spq(list = listw, p = p, rho = rho)

## ----results='hide'-----------------------------------------------------------
ggplot(data.frame(fx = fx, cx = cx, cy = cy), aes(x = cx, y = cy, color = fx)) + 
    geom_point(size = 6) +
    theme_bw()

## -----------------------------------------------------------------------------
m = 3
r = 1
mh <- m.surround(x = cbind(cx,cy), m = m, r = r)
class(mh)

## ----print mh-----------------------------------------------------------------
print(mh)

## ----summary mh---------------------------------------------------------------
summary(mh)

## ----plot mh------------------------------------------------------------------
plot(mh, type = 1)

## ----plot mh prune------------------------------------------------------------
control <- list (dtmaxknn = 10)
mh.prune <- m.surround(x = coor, m = m, r = r, control = control)
plot(mh.prune)

## -----------------------------------------------------------------------------
q.test <- Q.test(fx = fx, coor = coor, m = 3, r = 1)

## -----------------------------------------------------------------------------
q.test.mc <- Q.test(fx = fx, coor = coor, m = 3, r = 1, distr = "mc")
summary(q.test.mc)

## -----------------------------------------------------------------------------
summary(q.test)

## -----------------------------------------------------------------------------
plot(q.test)

## ----warning = FALSE----------------------------------------------------------
# Case 3: With a sf object with isolated areas
sf_use_s2(FALSE)
provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
levels(provinces_spain$Mal2Fml) = c("men","woman")
f1 <- ~ Mal2Fml
q.test.sf <- Q.test(formula = f1, data = provinces_spain, m = 3, r = 1)

## -----------------------------------------------------------------------------
plot(q.test.sf)

## -----------------------------------------------------------------------------
p <- c(1/6,3/6,2/6)
rho = 0.5
QY1 <- dgp.spq(p = p, listw = listw, rho = rho)
rho = 0.8
QY2 <- dgp.spq(p = p, listw = listw, rho = rho)
dt = data.frame(QY1,QY2)
m = 3
r = 1
formula <- ~ QY1 + QY2
control <- list(dtmaxknn = 10)
qmap <- Q.map.test(formula = formula, data = dt, coor = coor, m = m, r = r, type ="combinations", control = control)

## -----------------------------------------------------------------------------
print(qmap[[1]])

## -----------------------------------------------------------------------------
plot(qmap, ci=.6)

## -----------------------------------------------------------------------------
listw <- knearneigh(coor, k = 3)
srq <- sp.runs.test(fx = fx, listw = listw)

## -----------------------------------------------------------------------------
print(srq)

## -----------------------------------------------------------------------------
plot(srq)

## -----------------------------------------------------------------------------
lsrq <- local.sp.runs.test(fx = fx, listw = listw, alternative = "less")

## -----------------------------------------------------------------------------
print(lsrq)

## -----------------------------------------------------------------------------
plot(lsrq, sig = 0.05)

## ----warning = FALSE----------------------------------------------------------
data("provinces_spain")
listw <- spdep::poly2nb(as(provinces_spain,"Spatial"), queen = FALSE)
provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
levels(provinces_spain$Mal2Fml) = c("men","woman")
plot(provinces_spain["Mal2Fml"])
formula <- ~ Mal2Fml
# Boots Version
lsrq <- local.sp.runs.test(formula = formula, data = provinces_spain, listw = listw, distr ="bootstrap", nsim = 199)
plot(lsrq, sf = provinces_spain, sig = 0.10)

## ----bernoulli-scan, warning = FALSE------------------------------------------
formula <- ~ Mal2Fml
scan.spain <- spqdep::scan.test(formula = formula, data = provinces_spain, 
                                case="men", nsim = 99, distr = "bernoulli")
print(scan.spain)

## ----flexible-scan, warning = FALSE, collapse = TRUE--------------------------
listw <- spdep::poly2nb(provinces_spain, queen = FALSE)
scan.spain <- spqdep::scan.test(formula = formula, data = provinces_spain, 
                                case="men", nsim = 99, windows = "flexible", 
                                listw = listw, nv = 6, distr = "bernoulli")
print(scan.spain)

## ----multinomial-scan---------------------------------------------------------
data(FastFood.sf)
formula <- ~ Type
scan.fastfood <- scan.test(formula = formula, data = FastFood.sf, nsim = 99, distr = "multinomial", windows = "elliptic", 
                           nv = 50)
print(scan.fastfood)

## -----------------------------------------------------------------------------
summary(scan.fastfood)

## ----warning = FALSE----------------------------------------------------------
plot(scan.spain, sf = provinces_spain)

## ----warning = FALSE----------------------------------------------------------
data(FastFood.sf)
# plot(scan.fastfood, sf = FastFood.sf)

## ----warning = FALSE, collapse=TRUE-------------------------------------------
coor <- st_coordinates(st_centroid(FastFood.sf))
listw <- spdep::knearneigh(coor, k = 4)
formula <- ~ Type
similarity <- similarity.test(formula = formula, data = FastFood.sf, listw = listw)
print(similarity)

## ----warning = FALSE----------------------------------------------------------
provinces_spain$Older <- cut(provinces_spain$Older, breaks = c(-Inf,19,22.5,Inf))
levels(provinces_spain$Older) = c("low","middle","high")
f1 <- ~ Older + Mal2Fml
jc1 <- jc.test(formula = f1, data = provinces_spain, distr = "asymptotic", alternative = "greater", zero.policy = TRUE)
summary(jc1)

## ----warning = FALSE----------------------------------------------------------
jc1 <- jc.test(formula = f1, data = provinces_spain, distr = "mc", alternative = "greater", zero.policy = TRUE)
summary(jc1)

## -----------------------------------------------------------------------------
data(Boots.sf)
listw <- spdep::poly2nb(as(Boots.sf,"Spatial"), queen = TRUE)
formula <- ~ BW
ljc <- local.jc.test(formula = formula, data = Boots.sf, case="B", listw = listw)

