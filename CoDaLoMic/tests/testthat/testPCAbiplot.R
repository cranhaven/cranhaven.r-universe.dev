#Only the size of an array is checked to see that the function works
set.seed(123)
especie=t(gtools::rdirichlet(10,c(1,3,1,2,4)))
names=c("Bact1","Bact2","Bact3","Bact4","Bact5")
tau1=0.4
parms1= cbind(c(0.1,0.2,0.4,0.6),c(-0.2,0.1,0.1,0.3),c(0.3,0.2,0.3,0.5))
paramEstimadosFinal=c(as.vector( t(parms1)),tau1)


a=PCAbiplot(paramEstimadosFinal,names,5)

test_that("PCAbiplot", {
  expect_equal(dim(a$PCA$rotation),c(3,3))
})
