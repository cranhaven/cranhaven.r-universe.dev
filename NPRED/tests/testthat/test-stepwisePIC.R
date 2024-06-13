skip('skip')

library(NPRED)

#-------------------------------------------------------------------------------
#general test on PIC selction
data(data1)
t <- NPRED::stepwise.PIC(data1[,1],data1[,-1]);t
#NPRED::stepwise.PIC(data1[,1],data1[,-1], method=F)

# calc.PW(data1[,1],data1[,-1], t$cpy,t$cpyPIC)
# calc.PW(data1[,1],data1[,-1], t$cpy,t$cpyPIC, method = F)
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

data(data2)
t = NPRED::stepwise.PIC(data2[,1],data2[,-1]);t
#NPRED::stepwise.PIC(data2[,1],data2[,-1], method = F)

calc.PW(data2[,1],data2[,-1], t$cpy,t$cpyPIC)
calc.PW(data2[,1],data2[,-1], t$cpy,t$cpyPIC, method = F)
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

data(data3)
t = NPRED::stepwise.PIC(data3[,1],data3[,-1]);t
NPRED::stepwise.PIC(data3[,1],data3[,-1], method=F)

calc.PW(data3[,1],data3[,-1], t$cpy,t$cpyPIC)
calc.PW(data3[,1],data3[,-1], t$cpy,t$cpyPIC, method = F)
# results from previous bulid
# $cpy
# [1] 1
# 
# $cpyPIC
# [1] 0.909069
# 
# $wt
# [1] 1 (wrong)
# 
# $lstwet
# Intercept           X 
# 0.002235072 0.915854066 

#-------------------------------------------------------------------------------
# AR1 model from paper with 9 dummy variables
data.ar1<-data.gen.ar1(500)
t=stepwise.PIC(data.ar1$x,data.ar1$dp);t
#stepwise.PIC(data.ar1$x,data.ar1$dp, method = F)

calc.PW(data.ar1$x,data.ar1$dp, t$cpy,t$cpyPIC)
calc.PW(data.ar1$x,data.ar1$dp, t$cpy,t$cpyPIC, method = F)

# AR4 model from paper with total 9 dimensions
data.ar4<-data.gen.ar4(500)
t=stepwise.PIC(data.ar4$x,data.ar4$dp);t
stepwise.PIC(data.ar4$x,data.ar4$dp, method = F)

calc.PW(data.ar4$x,data.ar4$dp, t$cpy,t$cpyPIC)
calc.PW(data.ar4$x,data.ar4$dp, t$cpy,t$cpyPIC, method = F)

# AR9 model from paper with total 9 dimensions
data.ar9<-data.gen.ar9(500)
t=stepwise.PIC(data.ar9$x,data.ar9$dp);t
stepwise.PIC(data.ar9$x,data.ar9$dp, method=F)

calc.PW(data.ar9$x,data.ar9$dp, t$cpy,t$cpyPIC)
calc.PW(data.ar9$x,data.ar9$dp, t$cpy,t$cpyPIC, method = F)
