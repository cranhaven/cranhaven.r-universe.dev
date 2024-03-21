### R code from vignette source 'IRMRI-Example.Rnw'

###################################################
### code chunk number 1: 0
###################################################
options(digits=3)


###################################################
### code chunk number 2: 0b
###################################################
dataDir0 <- system.file("extdataIR", package = "qMRI")
dataDir <- tempdir("IRdata")
library(oro.nifti)


###################################################
### code chunk number 3: 1
###################################################
library(qMRI)
segm <- readNIfTI(file.path(dataDir0,"Brainweb_segm"))
Sf <- 900
Rf <- 0.000285
Sgm <- 400
Rgm <- 0.00075
fgm <- .15
Swm <- 370
Rwm <- 0.0011
fwm <- .05
InvTimes <- c(100, 200, 400, 600, 800, 1200, 1600, 2000, 2500, 3000, 
              3500, 4000, 4500, 5000, 6000, Inf)
InvTimes0 <- c(100, 200, 400, 600, 800, 1200, 1600, 2000, 2500, 3000, 
              3500, 4000, 4500, 5000, 6000, 15000)


###################################################
### code chunk number 4: 2
###################################################
x <- seq(100,12000,10)
fintCSF <- qMRI:::IRhomogen(c(Sf,Rf),InvTimes0)
fintGM <- qMRI:::IRmix2(c(fgm,Rgm,Sgm),InvTimes0,Sf,Rf)
fintWM <- qMRI:::IRmix2(c(fwm,Rwm,Swm),InvTimes0,Sf,Rf)
plot(InvTimes0,fintCSF,xlab="InvTime",ylab="Intensity")
points(InvTimes0,fintGM,col=2)
points(InvTimes0,fintWM,col=3)
lines(x,qMRI:::IRhomogen(c(Sf,Rf),x))
lines(x,qMRI:::IRmix2(c(fgm,Rgm,Sgm),x,Sf,Rf),col=2)
lines(x,qMRI:::IRmix2(c(fwm,Rwm,Swm),x,Sf,Rf),col=3)


###################################################
### code chunk number 5: 3
###################################################
sigma <- 40
nTimes <- length(InvTimes0)
nCSF <- sum(segm==1)
nGM <- sum(segm==2)
nWM <- sum(segm==3)
IRdata <- array(0,c(nTimes,prod(dim(segm))))
IRdata[,segm==1] <- sqrt(rnorm(nTimes*nCSF,fintCSF,sigma)^2+
                         rnorm(nTimes*nCSF,0,sigma)^2)
IRdata[,segm==2] <- sqrt(rnorm(nTimes*nGM,fintGM,sigma)^2+
                         rnorm(nTimes*nGM,0,sigma)^2)
IRdata[,segm==3] <- sqrt(rnorm(nTimes*nWM,fintWM,sigma)^2+
                         rnorm(nTimes*nWM,0,sigma)^2)
dim(IRdata) <- c(nTimes,dim(segm))
for(i in 1:9) writeNIfTI(as.nifti(IRdata[i,,,]), 
                         file.path(dataDir,paste0("IR0",i)))
for(i in 10:nTimes) writeNIfTI(as.nifti(IRdata[i,,,]), 
                         file.path(dataDir,paste0("IR",i)))


###################################################
### code chunk number 6: 4
###################################################
library(qMRI)
t1Files <- list.files(dataDir,"*.nii.gz",full.names=TRUE)
segmFile <- file.path(dataDir0,"Brainweb_segm")
IRdata <- readIRData(t1Files, InvTimes0, segmFile, sigma=sigma,
                     L=1, segmCodes=c("CSF","GM","WM"))


###################################################
### code chunk number 7: 5
###################################################
setCores(2) # parallel mode using 2 threads
IRfluid <- estimateIRfluid(IRdata, method="NLR", verbose=FALSE)
cat("Estimated parameters Sf:", IRfluid$Sf, 
                        " Rf:", IRfluid$Rf, "\n")


###################################################
### code chunk number 8: 6
###################################################
IRmix <- estimateIRsolid(IRfluid, verbose=FALSE)


###################################################
### code chunk number 9: 7
###################################################
sIRmix <- smoothIRSolid(IRmix, alpha=1e-4, verbose=FALSE)


###################################################
### code chunk number 10: 8
###################################################
sIRmix <- estimateIRsolidfixed(sIRmix, verbose=FALSE)


###################################################
### code chunk number 11: 9
###################################################
par(mfrow=c(1,4),mar=c(3,3,3,.5),mgp=c(2,1,0))
library(adimpro)
rimage(segm[,,2])
title("Segmentation")
rimage(sIRmix$Sx[,,2],zlim=c(250,500))
title("solid intensity map")
rimage(sIRmix$Rx[,,2],zlim=c(0,.0015))
title("solid relaxation rate map")
rimage(sIRmix$fx[,,2],zlim=c(0,.4))
title("fluid proportion map")


###################################################
### code chunk number 12: 10 (eval = FALSE)
###################################################
## sIRmix <- estimateIR(IRdata, method="QL")


