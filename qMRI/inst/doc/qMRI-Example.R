### R code from vignette source 'qMRI-Example.Rnw'

###################################################
### code chunk number 1: 0
###################################################
options(digits=3, warn=0)


###################################################
### code chunk number 2: 1
###################################################
dataDir <- system.file("extdata", package = "qMRI")


###################################################
### code chunk number 3: 2
###################################################
t1Names <- paste0("t1w_", 1:8, ".nii.gz")
mtNames <- paste0("mtw_", 1:6, ".nii.gz")
pdNames <- paste0("pdw_", 1:8, ".nii.gz")
t1Files <- file.path(dataDir, t1Names)
mtFiles <- file.path(dataDir, mtNames)
pdFiles <- file.path(dataDir, pdNames)
B1File <- file.path(dataDir, "B1map.nii.gz")
maskFile <- file.path(dataDir, "mask.nii.gz")


###################################################
### code chunk number 4: 3
###################################################
TE <- c(2.3, 4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4,
        2.3, 4.6, 6.9, 9.2, 11.5, 13.8,
        2.3, 4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4)
TR <- rep(25, 22)
FA <- c(rep(21, 8), rep(6, 6), rep(6, 8))


###################################################
### code chunk number 5: 4
###################################################
library(qMRI)
mpm <- readMPMData(t1Files, pdFiles, mtFiles,
                   maskFile,
                   TR = TR, TE = TE, FA = FA,
                   verbose = FALSE)


###################################################
### code chunk number 6: 5
###################################################
setCores(2)
modelMPM <- estimateESTATICS(mpm,
                             method = "NLR",
                             verbose = FALSE)


###################################################
### code chunk number 7: 7
###################################################
# evaluate this here to be able to reduce the mask afterwards
setCores(2, reprt=FALSE)
modelMPMQLsp1 <- smoothESTATICS(modelMPM,
                                mpmData = extract(mpm, "ddata"),
                                kstar = 16,
                                alpha = 0.004,
                                patchsize = 1,
                                verbose = FALSE)


###################################################
### code chunk number 8: 7a
###################################################
# reduce mask to save time
mask <- extract(mpm,"mask")
mask[,c(1:10,12:21),] <- FALSE
mpm <- qMRI:::setMPMmask(mpm, mask)


###################################################
### code chunk number 9: 6
###################################################
sigma <- array(50, mpm$sdim)
modelMPMQL <- estimateESTATICS(mpm,
                               method = "QL",
                               sigma = sigma,
                               L = 1,
                               verbose = FALSE)


###################################################
### code chunk number 10: qMRI-Example.Rnw:237-240 (eval = FALSE)
###################################################
## ddata <- extract(mpm,"ddata")
## mask <- extract(mpm,"mask")
## if(require(dti)) sigma <- awslsigmc(ddata[1,,,],16,mask)$sigma


###################################################
### code chunk number 11: 7b (eval = FALSE)
###################################################
## # use setCores(ncores) to enable openMP parallelization
## setCores(2,reprt=FALSE)
## modelMPMQLsp1 <- smoothESTATICS(modelMPMQL,
##                                 mpmData = extract(mpm, "ddata"),
##                                 kstar = 16,
##                                 alpha = 0.004,
##                                 patchsize = 1,
##                                 verbose = FALSE)


###################################################
### code chunk number 12: 8
###################################################
library(adimpro)
rimage.options(zquantiles = c(.01, .99), ylab = "z")
par(mfrow = c(2, 4),
    mar = c(3, 3, 3, 1), mgp = c(2, 1, 0))
pnames <- c("T1", "MT", "PD", "R2star")
for (i in 1:4) {
  modelCoeff <- extract(modelMPMQL,"modelCoeff")
  rimage(modelCoeff[i, , 11, ])
  title(pnames[i])
}
for (i in 1:4) {
  modelCoeff <- extract(modelMPMQLsp1,"modelCoeff")
  rimage(modelCoeff[i, , 11, ])
  title(paste("smoothed", pnames[i]))
}


###################################################
### code chunk number 13: 9a
###################################################
mpmsp1 <- mpm
ddata <- extract(modelMPMQLsp1,"smoothedData")
dim(ddata) <- c(dim(ddata)[1],prod(dim(ddata)[-1]))
mpmsp1$ddata <- ddata[,mpm$mask]


###################################################
### code chunk number 14: 9
###################################################
modelMPM2 <- estimateESTATICS(mpmsp1,
                                method = "NLR",
                                L = 1,
                                verbose = FALSE)


###################################################
### code chunk number 15: 10
###################################################
qMRIMaps <- calculateQI(modelMPM,
                        b1File = B1File,
                        TR2 = 3.4)
qMRIQLMaps <- calculateQI(modelMPMQL,
                          b1File = B1File,
                          TR2 = 3.4)
qMRIQLSmoothedp1Maps <- calculateQI(modelMPMQLsp1,
                                    b1File = B1File,
                                    TR2 = 3.4)
qMRIMaps2 <- calculateQI(modelMPM2,
                           b1File = B1File,
                           TR2 = 3.4)


###################################################
### code chunk number 16: 11
###################################################
library(oro.nifti)
zlim <- matrix(c(0, 0, 0, 3000,
                 1.5, 35, 2, 10000),
               4, 2)
R1 <- readNIfTI(file.path(dataDir, "R1map.nii.gz"))
R2star <- readNIfTI(file.path(dataDir, "R2starmap.nii.gz"))
MT <- readNIfTI(file.path(dataDir, "MTmap.nii.gz"))
PD <- readNIfTI(file.path(dataDir, "PDmap.nii.gz"))
rimage.options(ylab = "z")
par(mfrow = c(4, 4),
    mar = c(3, 3, 3, 1), mgp = c(2, 1, 0))
nmaps <- c("R1", "R2star", "MT", "PD")
rimage(R1[, 11, ], zlim = zlim[1, ],
       main = paste("true", nmaps[1]))
rimage(R2star[, 11, ], zlim = zlim[2, ],
       main = paste("true", nmaps[2]))
rimage(MT[, 11, ], zlim = zlim[3, ],
       main = paste("true", nmaps[3]),
       col = colMT)
rimage(PD[, 11, ], zlim = zlim[4, ],
       main = paste("true", nmaps[4]))
qmap1 <- extract(qMRIQLMaps, nmaps)
for (i in 1:4) rimage(qmap1[[i]][, 11, ], zlim = zlim[i, ],
                      main = paste("Estimated", nmaps[i]),
                      col = if(i==3) colMT else grey(0:225/255))
qmap2 <- extract(qMRIQLSmoothedp1Maps, nmaps)
for (i in 1:4) rimage(qmap2[[i]][, 11, ], zlim = zlim[i, ],
                      main = paste("Smoothed", nmaps[i]),
                      col = if(i==3) colMT else grey(0:225/255))
qmap3 <- extract(qMRIMaps2, nmaps)
for (i in 1:4) rimage(qmap3[[i]][, 11, ], zlim = zlim[i, ],
                      main = paste("Smoothed data", nmaps[i]),
                      col = if(i==3) colMT else grey(0:225/255))


###################################################
### code chunk number 17: 12
###################################################
qmap0 <- extract(qMRIMaps,nmaps)
mask <- extract(mpm,"mask")
cat("\n",
    "Bias of NLR estimates\n",
      "R1", mean((qmap0$R1-R1)[mask]),
      "R2star", mean((qmap0$R2star-R2star)[mask]),
      "MT", mean((qmap0$MT-MT)[mask]),
      "PD", mean((qmap0$PD-PD)[mask]), "\n",
    "Bias of  QL estimates\n",
      "R1", mean((qmap1$R1-R1)[mask]),
      "R2star", mean((qmap1$R2star-R2star)[mask]),
      "MT", mean((qmap1$MT-MT)[mask]),
      "PD", mean((qmap1$PD-PD)[mask]), "\n")


###################################################
### code chunk number 18: 13
###################################################
cat("\n",
    "Root mean squared error of NLR estimate\n",
      "R1", sqrt(mean((qmap0$R1-R1)[mask]^2)),
      "R2star", sqrt(mean((qmap0$R2star-R2star)[mask]^2)),
      "MT", sqrt(mean((qmap0$MT-MT)[mask]^2)),
      "PD", sqrt(mean((qmap0$PD-PD)[mask]^2)), "\n",
    "Root mean squared error of  QL estimate\n",
      "R1", sqrt(mean((qmap1$R1-R1)[mask]^2)),
      "R2star", sqrt(mean((qmap1$R2star-R2star)[mask]^2)),
      "MT", sqrt(mean((qmap1$MT-MT)[mask]^2)),
      "PD", sqrt(mean((qmap1$PD-PD)[mask]^2)),"\n",
    "Root mean squared error of smoothed QL estimate\n",
      "R1", sqrt(mean((qmap2$R1-R1)[mask]^2)),
      "R2star", sqrt(mean((qmap2$R2star-R2star)[mask]^2)),
      "MT", sqrt(mean((qmap2$MT-MT)[mask]^2)),
      "PD", sqrt(mean((qmap2$PD-PD)[mask]^2)),"\n",
    "Root mean squared error of estimate from smoothed data \n",
      "R1", sqrt(mean((qmap3$R1-R1)[mask]^2)),
      "R2star", sqrt(mean((qmap3$R2star-R2star)[mask]^2)),
      "MT", sqrt(mean((qmap3$MT-MT)[mask]^2)),
      "PD", sqrt(mean((qmap3$PD-PD)[mask]^2)),"\n")


###################################################
### code chunk number 19: 14
###################################################
cat("Mean R1", mean(R1[mask]), "Mean R2star",
    mean(R2star[mask]), "Mean MT", mean(MT[mask]),
    "Mean PD", mean(PD[mask]),"\n")


