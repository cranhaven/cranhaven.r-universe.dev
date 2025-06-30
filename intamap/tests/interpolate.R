options(error = recover)
set.seed(15331)
library(intamap)
library(automap)
library(gstat)
library(psgp)
#loadMeuse()

sessionInfo()


crs = CRS("epsg:28992")
data("meuse")
coordinates(meuse) <- ~x+y
proj4string(meuse) <- crs
data("meuse.grid")
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
proj4string(meuse.grid) <- crs

meuse$value = log(meuse$zinc)
meuse.grid = meuse.grid[sample(1:dim(meuse.grid)[1], 100),]
output = interpolate(meuse, meuse.grid, list(mean=T, variance=T, nsim = 5), methodName = "automap")
summary(t(output$outputTable), digits = 4)

output = interpolate(meuse, meuse.grid,
    optList = list(idpRange = seq(0.1, 2.9, 0.5), nfold = 3), 
    outputWhat = list(mean=TRUE), methodName = "idw")
summary(t(output$outputTable), digits = 4)


output = interpolate(meuse, meuse.grid, list(mean=T, variance=T),methodName = "transGaussian")
summary(t(output$outputTable), digits = 4)


set.seed(15331)
data(meuse)
meuse = meuse[sample(dim(meuse)[1],30),]
meuse$value=meuse$zinc
coordinates(meuse) = ~x+y
mgrid = spsample(meuse,10,"regular")
gridded(mgrid) = TRUE
output1 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5),
                     methodName = "copula")

output2 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5),
                     methodName = "copula",optList = list(methodParameters = output1$methodParameters))

output3 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5),
                     methodName = "automap",optList = list(model = c("Exp", "Sph")), cv = TRUE)

output4 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5),
                      methodName = "psgp", cv = TRUE)

output5 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5), cv = TRUE, methodName = "automap")
output6 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5), optList = list(variogramModel = output5$variogramModel),
                      cv = TRUE)
output6$variogramModel$range[2] = 1000
output7 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5), 
                      cv = TRUE, optList = list(variogramModel = output6$variogramModel))
output8 = interpolate(meuse, mgrid, list(mean=T, variance=T, excprob = 1000,quantile = 0.5), 
                      cv = TRUE, optList = list(nclus = 4), methodName = "automap")

all.equal(output5$predictions, output6$predictions) # Should be the same
all.equal(output5$predictions, output8$predictions) # Should be the same
all.equal(output5$predictions, output7$predictions)  # Should be different


summary(t(output$outputTable), digits = 4)

output2$outputTable - output1$outputTable

summary(output3$predictions, digits = 4)
summary(output4$predictions, digits = 4)

