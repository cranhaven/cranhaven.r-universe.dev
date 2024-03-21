simulateMPM <- function(QImap, nT1, nPD=0, nMT=0, TR, TE, FA, TR2=0, sigma=1, seed=NULL){
   rrician <- function(n, eta, sigma){
      z1 <- rnorm(n, eta, sigma)
      z2 <- rnorm(n, 0, sigma)
      sqrt(z1^2 + z2^2)
   }
   model <- QImap$model
   R2star <- QImap$R2star
   if(model>0){
      R1 <- QImap$R1/1000
      PD <- QImap$PD
      b1Map <- QImap$b1Map
   }
   if(model==2) {
      delta <- QImap$MT*(1-.4*b1Map)/60*b1Map^2
   }
   if(model<2) nMT <- 0
   if(model<1) nPD <- 0
   nvoxel <- length(R2star)
   nFiles <- nT1+nPD+nMT
   if(length(TR)!=nFiles) stop("Incorrect number of TR values")
   if(length(TE)!=nFiles) stop("Incorrect number of TE values")
   if(length(FA)!=nFiles) stop("Incorrect number of FA values")
   eta <- matrix(0,nFiles,nvoxel)
   for( i in 1:nFiles){
      eR2s <- exp(-R2star*TE[i])
      if(model==0){
         eta[i,] <- QImap$ST1*eR2s
      } else if(model==1){
         alpha <- b1Map*FA/180*pi
         sa <- sin(alpha)
         ca <- cos(alpha)
         eR1 <- exp(-R1*TR[i])
         eta[i,] <- PD*sa*(1-eR1)/(1-ca*eR1)*eR2s
      } else if(model==2){
        alpha <- b1Map*FA/180*pi
        sa <- sin(alpha)
        ca <- cos(alpha)
        eR1 <- exp(-R1*TR[i])
        eR1d <- exp(-R1*TR2) - eR1
        eta[i,] <- PD*sa*(1-eR1-delta*eR1d)/(1-ca*(1-delta)*eR1)*eR2s
      }
   }
   ddata <- matrix(rrician(nFiles*nvoxel,eta,sigma), nFiles, nvoxel)
   obj <- list(ddata = ddata,
              sdim = QImap$sdim,
              nFiles = nFiles,
              t1Files = paste0("SimulatedT1",1:nT1),
              pdFiles = if(nPD>0) paste0("SimulatedPD",1:nPD) else NULL,
              mtFiles = if(nMT>0) paste0("SimulatedMT",1:nMT) else NULL,
              model = QImap$model,
              maskFile = QImap$maskFile,
              mask = QImap$mask,
              TR = TR,
              TE = TE,
              FA = FA)
  class(obj) <- "MPMData"
  invisible(obj)
}
