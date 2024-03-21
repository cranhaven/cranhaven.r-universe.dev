library(qMRI)
dataDir <- system.file("extdata",package="qMRI")
#
#  set file names for T1w, MTw and PDw images
#
t1Names <- paste0("t1w_",1:8,".nii.gz")
mtNames <- paste0("mtw_",1:6,".nii.gz")
pdNames <- paste0("pdw_",1:8,".nii.gz")
t1Files <- file.path(dataDir, t1Names)
mtFiles <- file.path(dataDir, mtNames)
pdFiles <- file.path(dataDir, pdNames)
#
#  file names of mask and B1 field map
#
B1File <- file.path(dataDir, "B1map.nii.gz")
maskFile <- file.path(dataDir, "mask.nii.gz")
#
#  Acquisition parameters (TE, TR, Flip Angle) for T1w, MTw and PDw images
#
TE <- c(2.3, 4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4,
        2.3, 4.6, 6.9, 9.2, 11.5, 13.8,
        2.3, 4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4)
TR <- rep(25, 22)
FA <- c(rep(21, 8), rep(6, 6), rep(6, 8))
#
#   read MPM example data
#
library(qMRI)
mpm <- readMPMData(t1Files, pdFiles, mtFiles,
                   maskFile, TR = TR, TE = TE,
                   FA = FA, verbose = FALSE)
#
#  Estimate Parameters in the ESTATICS model
#
modelMPM <- estimateESTATICS(mpm, method = "NLR", verbose=FALSE)
#
#  smooth maps of ESTATICS Parameters
#
setCores(2, reprt = FALSE)
modelMPMsp1 <- smoothESTATICS(modelMPM,
                              kstar = 16,
                              alpha = 0.004,
                              patchsize=1,
                              verbose = FALSE)
#
#  Compute quantitative maps (R1, R2star, PD, MT)
#
qMRIMaps <- calculateQI(modelMPM,
                        b1File = B1File,
                        TR2 = 3.4)
qMRISmoothedp1Maps <- calculateQI(modelMPMsp1,
                                    b1File = B1File,
                                    TR2 = 3.4)
#
#   some statistics on differences between results
#
qm <- extract(qMRIMaps,c("R1","R2star","MT","PD"))
qms <- extract(qMRISmoothedp1Maps,c("R1","R2star","MT","PD"))
mask <- extract(mpm,"mask")
cat("mean of estimated quantitative maps\n",
    mean(qm$R1[mask]), mean(qm$R2star[mask]), mean(qm$MT[mask]), mean(qm$PD[mask]),"\n",
    "mean of smoothed quantitative maps\n",
    mean(qms$R1[mask]), mean(qms$R2star[mask]), mean(qms$MT[mask]), mean(qms$PD[mask]),"\n",
    "Root mean squared difference between estimated and smoothed quantitative maps\n",
    sqrt(mean((qm$R1-qms$R1)[mask]^2)), sqrt(mean((qm$R2star-qms$R2star)[mask]^2)),
    sqrt(mean((qm$MT-qms$MT)[mask]^2)), sqrt(mean((qm$PD-qms$PD)[mask]^2)),"\n")
# set mask to y==11 only yo save time
# reduce mask to save time, need also to adapt storage of data
mask <- extract(mpm,"mask")
mask[,c(1:10,12:21),] <- FALSE
mpm <- qMRI:::setMPMmask(mpm, mask)
# Alternatively using Quasi-Likelihood
sigma <- 50
modelMPMQL <- estimateESTATICS(mpm, method = "QL",
                sigma = array(sigma, mpm$sdim), L = 1, verbose=FALSE)
qMRIMapsQL <- calculateQI(modelMPMQL,
                b1File = B1File,
                TR2 = 3.4)
mask <- extract(mpm,"mask")
qmQL <- extract(qMRIMapsQL,c("R1","R2star","MT","PD"))
cat("mean of estimated quantitative maps using QL\n",
    mean(qmQL$R1[mask]), mean(qmQL$R2star[mask]), mean(qmQL$MT[mask]), mean(qmQL$PD[mask]),"\n")
