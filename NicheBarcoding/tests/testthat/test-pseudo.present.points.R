# Test Start ###
test_that("check whether the simulation recordes are outputted as required",{
  data(en.vir)
  data<-data.frame(species=rep("Acosmeryx anceus",3),
                   Lon=c(145.380,145.270,135.461),
                   Lat=c(-16.4800,-5.2500,-16.0810))

  expect_equal(nrow(pseudo.present.points(data,10,2,1,en.vir=en.vir)),10)
})

test_that("test the expect error throwed when matching bad format",{
  data(en.vir)
  data<-data.frame(species=rep("Acosmeryx anceus",3),
                   Lon=c(145.380,145.270,135.461))

  expect_error(pseudo.present.points(data,10,2,1,en.vir=en.vir))
})

test_that("test when outputNum is lower than nrow(data)",{
  data(en.vir)
  data<-data.frame(species=rep("Acosmeryx anceus",3),
                   Lon=c(145.380,145.380,145.380),
                   Lat=c(-16.4800,-16.4800,-16.4800))

  expect_equal(pseudo.present.points(data,2,2,1,en.vir=en.vir),data)
})

test_that("test whether the coordinates are abnormal",{
  data(en.vir)
  data<-data.frame(species=rep("Acosmeryx anceus",3),
                   Lon=c(0,0,0),Lat=c(0,0,0))

  expect_error(pseudo.present.points(data,10,2,1,en.vir=en.vir,map=F),
               "must be re-confirmed!")
})
