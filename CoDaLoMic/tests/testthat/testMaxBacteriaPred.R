#context("MaxBacteriaPred")


names2=c("Bact1","Bact2","Bact3","Bact4","Bact5")
set.seed(314)
esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))
e2=5

esp22=matrix(0,5,6)
esp22[3,]=esp2[5,]
esp22[5,]=esp2[3,]
esp22[1,]=esp2[1,]
esp22[2,]=esp2[2,]
esp22[4,]=esp2[4,]


names3=c("Bact1","Bact2","Bact3","Bact4","Bact5")
set.seed(314)
esp3=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
e3=5
t3=6

esp33=matrix(0,5,6)
esp33[3,]=esp3[5,]
esp33[5,]=esp3[3,]
esp33[1,]=esp3[1,]
esp33[2,]=esp3[2,]
esp33[4,]=esp3[4,]


test_that("MaxBacteriaPred", {
   expect_equal(MaxBacteriaPred(names2,esp2[,-c(4,5,6)],e2,3,Pred=4, 6,esp2, "Max"), list( especie=esp22[,-c(4,5,6)], especiemodi=esp22[,-c(1,4,5,6)], nombres=c("Bact1","Bact2","Bact5","Bact4","Bact3"), EE=3,EspecieMaxima=5, especie.All=esp22,especiemodi.All=esp22[,-1]))
  expect_equal(MaxBacteriaPred(names3,esp3[,-c(4,5,6)],e3,3,Pred=4, 6,esp3,"Min"), list( especie=esp33[,-c(4,5,6)], especiemodi=esp33[,-c(1,4,5,6)], nombres=c("Bact1","Bact2","Bact5","Bact4","Bact3"), EE=3,EspecieMaxima=5, especie.All=esp33,especiemodi.All=esp33[,-1]))
  expect_equal(MaxBacteriaPred(names3,esp3[,-c(4,5,6)],e3,3,Pred=4, 6,esp3,3), list( especie=esp33[,-c(4,5,6)], especiemodi=esp33[,-c(1,4,5,6)], nombres=c("Bact1","Bact2","Bact5","Bact4","Bact3"), EE=3,EspecieMaxima=5, especie.All=esp33,especiemodi.All=esp33[,-1]))

  })
