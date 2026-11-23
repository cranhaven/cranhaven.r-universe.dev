#context("ExpectedValue_EstParmFunc")

especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
tau1=0.4
parms1= cbind(c(0.1,0.2),c(-0.2,0.1),c(0.3,0.2))
parms11=c(tau1,as.vector( parms1))

alpha=cbind(c(0.1295173,0.1454412,0.1250415),
            c(0.09377089,0.1593625,0.1468666))

Expected=cbind(c(alpha[1,1]/tau1, alpha[2,1]/tau1, alpha[3,1]/tau1  ),
               c(alpha[1,2]/tau1,alpha[2,2]/tau1,alpha[3,2]/tau1))

Variance=cbind(c(0.1563937, 0.1652828,0.1534876),
                      c(0.1281936,0.1711991,0.165968))

Expected.final=Expected[,-2]
Variance.final=Variance[,-2]

list.F<-list(alpha, Expected.final,Variance.final)
names(list.F)<-c("Dirichlet.Param", "Expected.Value","Variance.Value")


test_that("EstParmFunc", {
  expect_equal(ExpectedValue_EstParmFunc(parms11,especie1),  list.F,tolerance=1e-5)

})
