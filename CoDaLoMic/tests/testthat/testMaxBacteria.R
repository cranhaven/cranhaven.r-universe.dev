#context("MaxBacteria")

names1=c("Bact1","Bact2","Bact3")
set.seed(314)
esp1=t(gtools::rdirichlet(n=4, c(1,3,1)))
e1=3
t1=4

names2=c("Bact1","Bact2","Bact3","Bact4","Bact5")
set.seed(314)
esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))
e2=5
t2=6


names3=c("Bact1","Bact2","Bact3","Bact4","Bact5")
set.seed(314)
esp3=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
e3=5
t3=6


esp11=matrix(0,3,4)
esp11[2,]=esp1[3,]
esp11[3,]=esp1[2,]
esp11[1,]=esp1[1,]


esp22=matrix(0,5,6)
esp22[3,]=esp2[5,]
esp22[5,]=esp2[3,]
esp22[1,]=esp2[1,]
esp22[2,]=esp2[2,]
esp22[4,]=esp2[4,]

esp33=matrix(0,5,6)
esp33[3,]=esp3[5,]
esp33[5,]=esp3[3,]
esp33[1,]=esp3[1,]
esp33[2,]=esp3[2,]
esp33[4,]=esp3[4,]

test_that("MaxBacteria", {
  expect_equal(MaxBacteria(names1,esp1,e1,t1,"Max"), list( especie=esp11, especiemodi=esp11[,-1], nombres=c("Bact1","Bact3","Bact2"), EE=2,EspecieMaxima=3))
  expect_equal(MaxBacteria(names2,esp2,e2,t2,"Max"), list( especie=esp22, especiemodi=esp22[,-1], nombres=c("Bact1","Bact2","Bact5","Bact4","Bact3"), EE=3,EspecieMaxima=5))
  expect_equal(MaxBacteria(names3,esp3,e3,t3,"Min"), list( especie=esp33, especiemodi=esp33[,-1], nombres=c("Bact1","Bact2","Bact5","Bact4","Bact3"), EE=3,EspecieMaxima=5))
  expect_equal(MaxBacteria(names3,esp3,e3,t3,3), list( especie=esp33, especiemodi=esp33[,-1], nombres=c("Bact1","Bact2","Bact5","Bact4","Bact3"), EE=3,EspecieMaxima=5))
})
