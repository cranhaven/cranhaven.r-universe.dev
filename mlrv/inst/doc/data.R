## ----setup--------------------------------------------------------------------
library(mlrv)
library(foreach)
library(magrittr)

load("../data/hk_data.RData")
# data(hk_data)
colnames(hk_data) = c("SO2","NO2","Dust","Ozone","Temperature",
                      "Humidity","num_circu","num_respir","Hospital Admission",
                      "w1","w2","w3","w4","w5","w6")
n = nrow(hk_data)
t = (1:n)/n
hk = list()

hk$x = as.matrix(cbind(rep(1,n), scale(hk_data[,1:3])))
hk$y = hk_data$`Hospital Admission`

## -----------------------------------------------------------------------------
pvmatrix = matrix(nrow=2, ncol=4)
###inistialization
setting = list(B = 5000, gcv = 1, neighbour = 1)
setting$lb = floor(10/7*n^(4/15)) - setting$neighbour 
setting$ub = max(floor(25/7*n^(4/15))+ setting$neighbour,             
                  setting$lb+2*setting$neighbour+1)

## -----------------------------------------------------------------------------
setting$lrvmethod =0. 

i=1
# print(rule_of_thumb(y= hk$y, x = hk$x))
for(type in c("KPSS","RS","VS","KS")){
  setting$type = type
  print(type)
  result_reg = heter_covariate(list(y= hk$y, x = hk$x), setting, mvselect = -2)
  print(paste("p-value",result_reg))
  pvmatrix[1,i] = result_reg
  i = i + 1
}

## -----------------------------------------------------------------------------

setting$lrvmethod =1

i=1
for(type in c("KPSS","RS","VS","KS"))
{
  setting$type = type
  print(type)
  result_reg = heter_covariate(list(y= hk$y, x = hk$x), setting, mvselect = -2)
  print(paste("p-value",result_reg))
  pvmatrix[2,i] = result_reg
  i = i + 1
}




## -----------------------------------------------------------------------------
rownames(pvmatrix) = c("plug","diff")
colnames(pvmatrix) = c("KPSS","RS","VS","KS")
knitr::kable(pvmatrix,type="latex")
xtable::xtable(pvmatrix, digits = 3)

## -----------------------------------------------------------------------------
pvmatrix = matrix(nrow=2, ncol=4)
setting$lrvmethod = 0
i=1
for(type in c("KPSS","RS","VS","KS")){
  setting$type = type
  print(type)
  result_reg = heter_covariate(list(y= hk$y, x = hk$x),
                                             setting,
                                        mvselect = -2, shift = 1.2)
  print(paste("p-value",result_reg))
  pvmatrix[1,i] = result_reg
  i = i + 1
}

## -----------------------------------------------------------------------------
setting$lrvmethod =1
i=1
for(type in c("KPSS","RS","VS","KS"))
{
  setting$type = type
  print(type)
  result_reg = heter_covariate(list(y= hk$y, x = hk$x),
                                             setting,
                                        mvselect = -2, verbose_dist = TRUE, shift = 1.2)
  print(paste("p-value",result_reg))
  pvmatrix[2,i] = result_reg
  i = i + 1
}


## -----------------------------------------------------------------------------
rownames(pvmatrix) = c("plug","diff")
colnames(pvmatrix) = c("KPSS","RS","VS","KS")
knitr::kable(pvmatrix,type="latex")
xtable::xtable(pvmatrix, digits = 3)

## -----------------------------------------------------------------------------
pvmatrix = matrix(nrow=2, ncol=4)
setting$lrvmethod =0

i=1
for(type in c("KPSS","RS","VS","KS")){
  setting$type = type
  print(type)
  result_reg = heter_covariate(list(y= hk$y, x = hk$x),
                                             setting,
                                        mvselect = -2,  shift = 0.8)
  print(paste("p-value",result_reg))
  pvmatrix[1,i] = result_reg
  i = i + 1
}

## -----------------------------------------------------------------------------
setting$lrvmethod =1

i=1
for(type in c("KPSS","RS","VS","KS"))
{
  setting$type = type
  print(type)
  result_reg = heter_covariate(list(y= hk$y, x = hk$x),
                                             setting,
                                        mvselect = -2, verbose_dist = TRUE, shift = 0.8)
  print(paste("p-value",result_reg))
  pvmatrix[2,i] = result_reg
  i = i + 1
}

## -----------------------------------------------------------------------------
rownames(pvmatrix) = c("plug","diff")
colnames(pvmatrix) = c("KPSS","RS","VS","KS")
knitr::kable(pvmatrix,type="latex")
xtable::xtable(pvmatrix, digits = 3)

## -----------------------------------------------------------------------------
hk$x = as.matrix(cbind(rep(1,n), (hk_data[,1:3])))
hk$y = hk_data$`Hospital Admission`
setting$type = 0
setting$bw_set = c(0.1, 0.35)
setting$eta = 0.2
setting$lrvmethod = 1
setting$lb  = 10
setting$ub  = 15
hk1 = list()
hk1$x = hk$x[366:730,]
hk1$y = hk$y[366:730]
p1 <- heter_gradient(hk1, setting, mvselect = -2, verbose = T)
p1

