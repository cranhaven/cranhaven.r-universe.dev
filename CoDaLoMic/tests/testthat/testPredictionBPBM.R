NumSPBal=list(1,c(1,2))
DemSPBal=list(2,3)
MCMC.CHAINS=cbind(c(0.1,0.11),
                  c(0.2,0.21),
                  c(0.3,0.31),
                  c(-0.1,-0.11),
                  c(0.15,0.105),
                  c(0.44,0.41),
                  c(0.3,0.31),
                  c(0.201,0.221),
                  c(0.13,0.113) )
alpha=cbind(c(0.1,0.2,0.1),c(0.1,0.5,0.3))
K=3
esperanza=cbind(c(0.2,0.2,0.6))
Var=cbind(c(0.1,0.01,0.11))
VVmas=cbind(c(0,0,0))
VVmenos=cbind(c(0,0,0))
E=3
Tt=2
MatrizPBmodelo=cbind(c(1,0.3,0.2))


Varmas=matrix(0,E,2)
Varmenos=matrix(0,E,2)
muu=cbind(c( 0.1418154,-0.0189392, -0.2319346),  c( 0.16079625,  0.03747351, -0.18371388))
alphaa=exp(muu)

esperanzaa=matrix(0,E,2)
Varr=matrix(0,E,2)
for (t in 1:2){

esperanzaa[,t]=alphaa[,t]/(sum(alphaa[,t]))

for (j in 1:E) {
Varr[j,t]=((alphaa[j,t])*( sum(alphaa[,t])-alphaa[j,t]))/(( sum(alphaa[,t])+1)*( sum(alphaa[,t]))^(2))
}


  Varmas[,t]=esperanzaa[,t]+2*sqrt(Varr[,t])
  Varmenos[,t]=esperanzaa[,t]-2*sqrt(Varr[,t])


}
finE=cbind(esperanza,c(0.1111,0.5555556,0.3333333) ,apply(esperanzaa,1,mean))
finV=cbind(Var, c(0.05198181,0.1299545,0.1169591),apply(Varr,1,mean))
finA=cbind(alpha,apply(alphaa,1,mean) )

eT=c(0.1111,0.5555556,0.3333333)
VT=c(0.05198181,0.1299545,0.1169591)
Vtmas=eT+2*sqrt(VT)
Vtmenos=eT-2*sqrt(VT)

finVmas=cbind(VVmas, Vtmas,apply(Varmas,1,mean))
finVmenos=cbind(VVmenos, Vtmenos,apply(Varmenos,1,mean))
names(finVmas)<-NULL
names(finVmenos)<-NULL
colnames(finVmas)<-NULL
colnames(finVmenos)<-NULL

return.pred<-list(finE,finV,finA,finVmas,finVmenos)
names(return.pred)<-c("ExpectedValue.All", "VarianceValue.All","DirichlerParam.All", "ExpVarmas","ExpVarmenos")
dimnames(return.pred$ExpVarmas)<-NULL
dimnames(return.pred$ExpVarmenos)<-NULL

test_that("PredictionBPBM", {
  expect_equal(PredictionBPBM(NumSPBal,DemSPBal,MCMC.CHAINS, alpha,K,esperanza,Var,E,Tt,MatrizPBmodelo ),return.pred,tolerance=1e-5)
  })

