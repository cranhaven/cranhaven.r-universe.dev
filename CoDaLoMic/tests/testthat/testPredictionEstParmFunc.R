#context("PredictionEstParmFunc")


pam.ini=rbind(c(0.1,0.2,0.3),c(0.4,0.5,0.6))

paramEstimadosFinal=c(5, as.vector(pam.ini))

EspecieMaxima=3

alpha=cbind(c(2,2,3),c(1,1,3))

K=3

esperanza=cbind(c(0.2,0.3,0.5))

Var=cbind(c(0.2,0.3,0.5))

E=3

Tt=2

alphaFinal=cbind(c(2,2,3),c(1,1,3),c(1.615934,2.397673,0.986393))
EsperanzaFinal=cbind(c(0.2,0.3,0.5),c(0.2,0.2,0.6),c(0.3231868,0.4795346,0.1972786) )
VarFinal= cbind(c(0.2,0.3,0.5),c(0.02666667,0.02666667,0.040000),c(0.03645618,0.04159686,0.02639329)   )
return.pred<-list(EsperanzaFinal,VarFinal,alphaFinal)
names(return.pred)<-c("ExpectedValue.All", "VarianceValue.All","DirichlerParam.All")

test_that("PredictionFBM", {
  expect_equal(PredictionEstParmFunc(paramEstimadosFinal,EspecieMaxima, alpha,K,esperanza,Var,E,Tt ),  return.pred,tolerance=1e-4)

})


