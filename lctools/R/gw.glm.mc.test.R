gw.glm.light <- function(formula, family, dframe, bw, kernel, coords)
{  
  Obs <- nrow(dframe)
  
  #cat("\nNumber of Observations:", Obs)
  
  if(kernel == 'adaptive')
  { 
    Ne <- bw
    #cat("\nKernel: Adaptive\nNeightbours:", Ne)
  } 
  else 
  { 
    if(kernel == 'fixed')
    {
      #cat("\nKernel: Fixed\nBandwidth:", bw)
    }
  }
  
  Gl.Model <- eval(substitute(glm(formula, family=family, data = dframe)))
  
  RNames <- names(Gl.Model$coefficients)

  ModelVarNo <- length(Gl.Model$coefficients)
  
  DistanceT <- dist(coords)
  Dij <- as.matrix(DistanceT)
  
  GGLM_LEst <- as.data.frame(setNames(replicate(ModelVarNo,numeric(0), simplify = F), RNames[1:ModelVarNo]))

  for(m in 1:Obs){
    #Get the data
    DNeighbour <- Dij[,m]
    DataSet <- data.frame(dframe, DNeighbour=DNeighbour)
    
    #Sort by distance
    DataSetSorted <- DataSet[order(DataSet$DNeighbour),]
    
    if(kernel == 'adaptive')
    { 
      #Keep Nearest Neighbours
      SubSet <- DataSetSorted[1:Ne,]
      Kernel_H <- max(SubSet$DNeighbour)
    } 
    else 
    { 
      if(kernel == 'fixed')
      {
        SubSet <- subset(DataSetSorted, DNeighbour <= bw)
        Kernel_H <- bw
      }
    }
    
    #Bi-square weights
    Wts<-(1-(SubSet$DNeighbour/Kernel_H)^2)^2
    
    #Calculate WGLM
    Lcl.Model<-eval(substitute(glm(formula, family=family, data = SubSet, weights=Wts)))
    
    #Store in table
    GGLM_LEst[m,]<-Lcl.Model$coefficients
  }

  return(GGLM_LEst)
}

#-----------Monte Carlo Test --------------------------
gw.glm.mc.test <- function (Nsim = 19, formula, family, dframe, bw, kernel, coords) 
{
  
   Obs <- nrow(dframe)
   
   gw.glm.observed <- eval(substitute(gw.glm.light(formula, family, dframe, bw, kernel, coords)))
     
   var.lpest.glm.obs <- diag(var(gw.glm.observed))

     
   # print("Observed")
   # print(round(var.lpest.glm.obs,5)) 
     
   params<-length(var.lpest.glm.obs)
     
   l.pest.SIM <- matrix(data = NA, nrow = Nsim, ncol = params+1)
   l.pest.SIM.c<- matrix(data = 0, nrow = Nsim, ncol = params+1)

   
   for (i in 1:Nsim) {
     x = runif(Obs, min = 1, max = Obs * (Nsim + 1))
     tx <- round(x/(Nsim + 1))
     CoordsTmp <- data.frame(coords, RandomID = tx)
     CoordsSorted <- CoordsTmp[order(CoordsTmp$RandomID),]
     CoordsNew <- CoordsSorted[,1:2]
     
     gw.glm.SIM <- try(eval(substitute(gw.glm.light(formula, family, dframe, bw, kernel, CoordsNew))))
     
     if(inherits( gw.glm.SIM, "try-error"))
     {
       #on error repeat this iteration using
       #NAs
       i <- i-1
       cat("\nAn error occurred, iteration to be repeated:", i)
       next
       
       
     }
     
     l.pest.SIM[i, 1] <- i
     l.pest.SIM.c[i, 1] <- i
     
     for (j in 1:params) {
       
       l.pest.SIM[i, j+1] <- var(gw.glm.SIM[,j])
       
       if (l.pest.SIM[i, j+1] >= var.lpest.glm.obs[[j]]) {l.pest.SIM.c[i, j+1] <- 1}
     }
     
     # for (j in (params+1):(2*params)) {
     #   
     #   l.pest.SIM[i, j+1] <- var(gw.glm.SIM[,j-params])
     #   
     #   if (l.pest.SIM[i, j+1] >= var.lpest.glm.obs[[j-params]]) {l.pest.SIM.c[i, j+1] <- 1}
     # }
   }
   
   C.test <- colSums(l.pest.SIM.c)
   
   for (j in 1:(params)) {
     if ((Nsim - C.test[[j+1]]) < C.test[[j+1]]) {
       C.test[[j+1]] = Nsim - C.test[[j+1]]
     }
   }
   
   pseudo.p = (1 + C.test)/(Nsim + 1)
   
   return(list(var.lpest.obs = var.lpest.glm.obs, var.SIM = l.pest.SIM, var.SIM.c = l.pest.SIM.c, pseudo.p = pseudo.p[2:(params+1)]))
  
}

