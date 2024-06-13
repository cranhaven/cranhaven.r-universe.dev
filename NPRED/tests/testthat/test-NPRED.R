skip('skip')

#devtools::install_github("zejiang-unsw/NPRED@v1.0.3")
library(NPRED)

#-----------------------------------------------------------------
#general test on PIC selction
data(data1)
NPRED::stepwise.PIC(data1[,1],data1[,-1])

###NPRED-Fortran
# results from previous bulid
# $cpy
# [1] 4 9 1
# 
# $cpyPIC
# [1] 0.6088356 0.3508629 0.2823850
# 
# $wt
# [1] 0.5646173 0.3204016 0.2411765
# 
# $lstwet
# Intercept         X1         X2         X3 
# 0.01084188 0.59125594 0.44045111 0.33017967

###NPRED-R
# $cpy
# [1] 4 9 1
# 
# $cpyPIC
# [1] 0.6173116 0.4261805 0.3981638
# 
# $wt
# [1] 0.5724777 0.3891802 0.3400597
# 
# $lstwet
# Intercept         X1         X2         X3 
# 0.01084188 0.59125594 0.44045111 0.3301796

#-----------------------------------------------------------------
data(data2)
NPRED::stepwise.PIC(data2[,1],data2[,-1])

###NPRED-Fortran
# results from previous bulid
# $cpy
# [1] 1 4
# 
# $cpyPIC
# [1] 0.6410743 0.4670626
# 
# $wt
# [1] 0.6293656 0.4307346
# 
# $lstwet
# Intercept          X1          X2 
# 0.003700531 0.621879435 0.394836477 

###NPRED-R
# $cpy
# [1] 1 4
# 
# $cpyPIC
# [1] 0.6513785 0.5045989
# 
# $wt
# [1] 0.6336731 0.4618859
# 
# $lstwet
# Intercept          X1          X2 
# 0.003700531 0.621879435 0.394836477 

#-----------------------------------------------------------------
data(data3)
NPRED::stepwise.PIC(data3[,1],data3[,-1])

###NPRED-Fortran
# results from previous bulid
# $cpy
# [1] 1
# 
# $cpyPIC
# [1] 0.909069
# 
# $wt
# [1] 1 (wrong) #correct 0.9095501
# 
# $lstwet
# Intercept           X 
# 0.002235072 0.915854066 (wrong)

###NPRED-R
# $cpy
# [1] 1
# 
# $cpyPIC
# [1] 0.9113155
# 
# $wt
# [1] 0.9113155
# 
# $lstwet
# Intercept          X 
# 0.02333108 0.91358821 

#-----------------------------------------------------------------
# set.seed(2020)
# AR1 model from paper with 9 dummy variables
data.ar1<-data.gen.ar1(500)
stepwise.PIC(data.ar1$x,data.ar1$dp)

# AR4 model from paper with total 9 dimensions
data.ar4<-data.gen.ar4(500)
stepwise.PIC(data.ar4$x,data.ar4$dp)

# AR9 model from paper with total 9 dimensions
data.ar9<-data.gen.ar9(500)
stepwise.PIC(data.ar9$x,data.ar9$dp)

#-----------------------------------------------------------------
#general test on knn with extrapolation - example 1
x <- ts(data3[,1])       # response
z <- ts(data3[,-1])      # possible predictors
zout <- ts(data.gen.ar1(500,ndim=15)$dp) #new input

xhat1=xhat2=x
# xhat1 <- NPRED::knn(x,z,zout,k=5,reg=T,extrap=F)
# xhat2 <- NPRED::knn(x,z,zout,k=5,reg=T,extrap=T)

for(i in 1:500) {
  
  xhat1[i] <- NPRED::knn(x[-i],z[-i,],z[i,],extrap=F)
  xhat2[i] <- NPRED::knn(x[-i],z[-i,],z[i,],extrap=T)
  
}

ts.plot(x,xhat1,xhat2,col=c("black","red","blue"),ylim=c(-10,10), lwd=c(1,1,2))

plot(xhat1,xhat2,xlim=c(-10,10),ylim=c(-10,10))
abline(coef = c(0,1),lwd=1)

