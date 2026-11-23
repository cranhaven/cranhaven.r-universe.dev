v=c(0.1,0.1,0.2,0.3,0.3)
Num2<-list(3,c(3,5),1,c(3,5,4))
Dem2<-list(5,4,2,c(1,2))
MatrizPBmodelo=rbind(c(1,1,1,1),
                     c(-0.3,0.2,0.5,0.6),
                     c(-0.4,0.3,0.5,0.6),
                     c(0.5,0.3,0.2,0.7),
                      c(-0.2,0.9,0.2,0.1)   )

vector.fin=rep(0,5)
vector.fin[1]=1
vector.fin[2]=PBalancePredi(Num2[[1]],Dem2[[1]],v)
vector.fin[3]=PBalancePredi(Num2[[2]],Dem2[[2]],v)
vector.fin[4]=PBalancePredi(Num2[[3]],Dem2[[3]],v)
vector.fin[5]=PBalancePredi(Num2[[4]],Dem2[[4]],v)


test_that("FVectorPBmodeloPredi", {
  expect_equal(FVectorPBmodeloPredi(Num2,Dem2,v,MatrizPBmodelo),  vector.fin,tolerance=1e-5)

})
