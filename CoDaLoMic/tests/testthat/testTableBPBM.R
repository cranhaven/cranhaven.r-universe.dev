
NumSPBal=list(c(3,4),3,2)
DemSPBal=list(1,5,4)
PerVar=c(41.37487, 21.08270, 19.16870)
MatrizPBmodelo=rbind(c(1.00000000,  1.0000000,  1.0000000,  1.0000000 , 1.0000000),
                     c(1.84449081, -1.3569851, -0.1388348, -0.5269079, -1.3288684),
                     c(0.27531685,  0.4741394, -1.9045554, -0.3581268, -0.4768543),
                     c( 0.07782991,  0.3492473, -0.7138882,  1.4332828, -0.7203295))
names=c("Bact1", "Bact2","Bact3","Bact4","Bact5")
Estimated.Param=c(0.5, 0.3, -0.2,0.1)
E=5

tat=TableBPBM(NumSPBal,DemSPBal,PerVar,MatrizPBmodelo,Estimated.Param,BB=0.55,names, E)
cat(tat, sep="\n")

test_that("TableBPBM", {
  expect_type(tat, "character")
})




