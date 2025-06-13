t <- uvbData[,1]
y <- uvbData[,1:3]
modelFile <- system.file("extdata","BIOMD0000000545_url.xml", package = "seeds")

# generate an odeModel object
uvb <- importSBML(modelFile, times = t, meas = y)
