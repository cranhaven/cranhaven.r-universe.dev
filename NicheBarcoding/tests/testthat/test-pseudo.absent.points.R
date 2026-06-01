# Test Start ###
test_that("check whether the simulation recordes are outputted as required",{
  data(en.vir)
  data<-data.frame(species=rep("Acosmeryx anceus",3),
                   Lon=c(145.380,145.270,135.461),
                   Lat=c(-16.4800,-5.2500,-16.0810))
  absent.points<-pseudo.absent.points(data,en.vir=en.vir,outputNum=100)

  expect_equal(nrow(absent.points$lonlat),100)
  expect_equal(nrow(absent.points$envir),100)
})

test_that("test the expect error throwed when matching bad format",{
  data(en.vir)
  data<-data.frame(species=rep("Acosmeryx anceus",3),
                   Lon=c(145.380,145.270,135.461))

  expect_error(pseudo.absent.points(data,en.vir=en.vir,outputNum=100))
})
