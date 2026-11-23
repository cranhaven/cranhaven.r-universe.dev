

Num=c(1,2)
Dem=c(3,4)
DatosEsperanzas=c(0.1,0.3,0.4,0.2)



fin=sqrt(4/4)*log(((0.1*0.3)^(1/2))/((0.4*0.2)^(1/2)))

test_that("PBalancePredi", {
  expect_equal(PBalancePredi(Num,Dem,DatosEsperanzas), fin,tolerance=1e-5)
})
