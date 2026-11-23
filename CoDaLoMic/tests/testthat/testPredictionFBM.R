#context("PredictionFBM")

Tt=2
E=3
tau=5
EspecieMaxima=3
K=3
parms11=c(0.1,0.2,0.3,0.4,0.5,0.6,tau)
alpha=cbind(c(1.726793,1.892901,1.380306),
            c(1,1,3))

Expected=cbind(c(alpha[1,1]/tau, alpha[2,1]/tau, alpha[3,1]/tau  ),
               c(alpha[1,2]/tau,alpha[2,2]/tau,alpha[3,2]/tau))

Variance=cbind(c(0.03768101, 0.03920954, 0.03330857 ),
               c( 0.03683242,0.02784883, 0.0413761 ))
Expected.final=Expected[,-2]
Variance.final=Variance[,-2]

epred=cbind(Expected.final,c(0.2,0.2,0.6),c(0.3062336,0.2138298,0.4799366))
varpred=cbind(Variance.final,c(0.02666667,0.02666667,0.040000),c(0.0354091,0.02801777,0.04159958))
alphaperd=cbind(alpha, c(1.531168,1.069149,2.399683))
varmas=epred+2*sqrt(varpred)


colnames(epred)<-NULL
colnames(varpred)<-NULL
colnames(alphaperd)<-NULL


list.FFe<-list(epred,varpred,alphaperd)
names(list.FFe)<-c("ExpectedValue.All", "VarianceValue.All","DirichlerParam.All")


test_that("PredictionFBM", {
  expect_equal(PredictionFBM(parms11,EspecieMaxima, alpha,K,Expected.final,Variance.final,E,Tt ),  list.FFe,tolerance=1e-5)

})
