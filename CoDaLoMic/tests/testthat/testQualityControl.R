#context("QualityControl")

names1=c("Bact1","Bact2")
Tt=3
esp1=cbind(c(0.9,0.1),c(0.6,0.4),c(0.3,0.7))
esp1modi=esp1[,-1]
esperanza=cbind(c(0.7,0.3),c(0.2,0.8))
E=2

matrizfin=cbind(c(0.1 ,0.1, 0.1  ),
                c(0.5555556, 0.5555556,0.5555556),
                c(0.02, 0.02,0.02),
                c(0.25,0.1964286,0.2232143))
colnames(matrizfin)<-c("RMSD", "Nash Sutchiffe Coefficient", "RSS","MAPE")
rownames(matrizfin)<-c(names1,"Mean")

test_that("QualityControl", {
  expect_equal(QualityControl(esp1modi,esperanza,names1), matrizfin,tolerance=1e-5)

})
