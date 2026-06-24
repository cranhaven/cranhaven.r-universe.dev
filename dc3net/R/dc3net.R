## This package allows inferring disease networks by employing C3NET algorithm.
## Copyright (C) January 2015 Gokmen Altay <altayscience@gmail.com>
## This program is a free software for only academic useage but not for commercial useage; you can redistribute it and/or
## modify it under the terms of the GNU GENERAL PUBLIC LICENSE
## either version 3 of the License, or any later version.
##
## This program is distributed WITHOUT ANY WARRANTY; 
## You can get a copy of the GNU GENERAL PUBLIC LICENSE
## from
## http://www.gnu.org/licenses/gpl.html


dc3net <- function(dataT=c(), dataC=c(), probes=c(), genes=c(), method="cutoff", methodValue=0, itNum=1, rankDif=100, percentDif=0.6, rankdCom=10, percentCom=0.85, probFiltered=FALSE, visualization=1){
  
  dataMatrix <- FALSE
  
  cat("DC3NET: Process started...\n")
  # Check the existence of dataT and dataC matrices.
  if (length(dataT)==0 || length(dataC)==0) { return(print("Please enter two data or mutual information matrices.")) } 
  
  # Check NAs in the data
  x<-0
  x <- length(which(is.na(dataT)))
  if(x!=0)   return(cat("DC3NET: Test data or MIM has NAs, thus stopped executing. Remove NAs from the entry."))  
  y<-0
  y <- length(which(is.na(dataC)))
  if(y!=0) return(cat("DC3NET: Control data or MIM has NAs, thus stopped executing. Remove NAs from the entry."))
  
  if (length(genes)==0) {
    return(cat("DC3NET: Required parameter \"gene\" is missing. Please enter a gene vector."))
  }
  
  if (length(probes)==0) {
    return(cat("DC3NET: Required parameter \"probes\" is missing. Please enter a probe vector."))
  }
  
  cat("DC3NET: Method:", method, "| Method Value:",  methodValue, "| Iteration:", itNum, "| RankDif:", rankDif, "| PercentDif:",  percentDif, "| RankdCom:", rankdCom, "| PercentCom:", percentCom, "\n")
  
  
  # Check the type of the data (If not already MIM, then compute from the data)
  if(nrow(dataT) != ncol(dataT))
  { 
    # then this is a data matrix. Compute MIM
    
    dataMatrix <- TRUE
    
    if (method=="justp") {
      
      justp <- sigtestp(dataT, methodValue, itNum)
      dataT <- justp$mim
      Ic <- justp$I0
      mimT <- dataT
      
    } else if (method=="holm" || method=="hochberg" || method=="hommel" || method=="bonferroni" || method=="BH" || method=="BY") {  
        
      mtc <- sigtestMTC(dataT, methodValue, itNum, methodsig=as.character(method))
      dataT <- mtc$mim
      dataTtemp <- mtc$Inew
      
      dataTtemp.na <- dataTtemp
      dataTtemp.na[dataTtemp==0] <- NA
      Ic <- dataTtemp.na[which.min(dataTtemp.na)]
      mimT <- dataT
          
    } else {
      
      dataT <- copula(dataT) 
      dataT <- makemim(dataT)
      mimT <- dataT
      
    }
    
  }
  if(nrow(dataC) != ncol(dataC))
  {
    # then this is a data matrix. Compute MIM
    
    if (method=="justp") {
      
      justp <- sigtestp(dataC, methodValue, itNum)
      dataC <- justp$mim
      mimC <- dataC
      
    } else if (method=="holm" || method=="hochberg" || method=="hommel" || method=="bonferroni" || method=="BH" || method=="BY") {  
      
      mtc <- sigtestMTC(dataC, methodValue, itNum, methodsig=as.character(method))
      dataC <- mtc$mim
      dataCtemp <- mtc$Inew
      
      dataCtemp.na <- dataCtemp
      dataCtemp.na[dataCtemp==0] <- NA
      mimC <- dataC
      
    } else {
      
      dataC <- copula(dataC) 
      dataC <- makemim(dataC)
      mimC <- dataC
      
    }

  }

  # Select method for the first step.
  if (method=="cutoff") {

    if(methodValue== 0) Ic <- mean(dataT[upper.tri(dataT)])
    else Ic <- methodValue  
    
  } else if (method=="rank") {

    TopSigProbs <- methodValue
    Ic <- methodValue
    
  } else if (method=="justp" || method=="holm" || method=="hochberg" || method=="hommel" || method=="bonferroni" || method=="BH" || method=="BY") {

    if (dataMatrix!=TRUE) {
      return(cat("DC3NET: Justp and MTC methods (homl, hochberg, hommel, bonferroni, BH, BY) can not be used with mutual information matrix"))  
    }
    
  }  else {
    
    return(cat("DC3NET: There is a problem with the parameters. Please check them."))
    
  } 

  # Assign zero to the probes of the same gene.
  if(probFiltered==FALSE)
  { dataT <- probSameFilt(dataT, genes)
    dataC <- probSameFilt(dataC, genes)
  } 

  # Find max partner for each gene in tumor matrix.
  maxT <- maxofMIM(dataT)  

  # Order the list of interactions regarding MI values.
  maxTranked <- orderList(maxT)  
  ss <- which.min(abs(maxTranked[,1]))
  
  cat("DC3NET: Ic:", Ic, "\n")
  if (method!="rank") {
    TopSigProbs <- which.min(abs(maxTranked[,1] - Ic))
  } 
  
  # Extend the table of ranked MI values with gene names and probe pairs and then save it to the output folder.
  maxTranked <- cbind(genes[maxTranked[,2]],genes[maxTranked[,3]], maxTranked, probes[maxTranked[,2]], probes[maxTranked[,3]])
  rownames(maxTranked) <- c(1:nrow(maxTranked))
  colnames(maxTranked) <- c("Gene1","Gene2","MIval","i","j", "prob1", "prob2")
  maxTranked <- as.matrix(maxTranked)
  #save(maxTranked,maxT, file="output/maxTandRanked.rda")  
  rm(maxT)
  
    
  # Find max partner for each gene in control matrix.
  maxC <- maxofMIM(dataC) 
    
  # Finds the rank of test list in the control MI matrix to see any rank difference.
  RankedC <- rankList(maxTranked, dataC)
    
  ind<- as.integer(maxTranked[,4])
  ind2<- as.integer(maxTranked[,5])
    
  lng <- length(ind)
  mi<-c()
  for(i in 1:lng) mi <- append(mi,dataC[ind[i],ind2[i]])
    
  Nvals <- cbind(mi,maxC[ind,1],maxC[ind2,1], RankedC)
  colnames(Nvals) <-c("TestMIinCont","maxMIiCont","maxMIjCont","RANKiCont","RANKjCont")
  
  # Start of Differential Network of T & C
  
  AdjL <- c()
  AdjL <- dc3netdif(as.matrix(maxTranked[1:TopSigProbs,]), Nvals, rankdif=rankDif, percent=percentDif)
  
  DifNet <- uniqNet(AdjL)
  rm(AdjL)
  
  if (!is.null(DifNet)) {
    rownames(DifNet) <- c(1:nrow(DifNet))
  }
  
  cat("DC3NET: The dimension of Differential Network is:", dim(DifNet)[1], "\n")
  
  
  # End of Differential Network of T & C
  
  # Start of Common Network of T & C
  
  AdjL <- c()
  AdjL <- dc3netcom(as.matrix(maxTranked[1:TopSigProbs,]), Nvals, rankdif=rankdCom, percent=percentCom)
  
  CommonNet <- uniqNet(AdjL)
  rm(AdjL)
  
  rownames(CommonNet) <- c(1:nrow(CommonNet))
  cat("DC3NET: The dimension of Common Network is:", dim(CommonNet)[1],"\n")
  cat("DC3NET: [Process completed]")
  # print(c("cutoffs used are:",TopSigProbs,  rankDif, percentDif, rankdCom, percentCom))
  # End of Common Network of T & C
  
  if (visualization==1) {
    
    g <- DifNet[1:nrow(DifNet),1:2]
    g <- graph.data.frame(g, directed = FALSE)
    e <- get.edgelist(g)
    ge <- graph.edgelist(e,directed=FALSE)
    geadj <- get.adjacency(ge)
    gegr <- graph.adjacency(geadj, mode="undirected", diag=FALSE)
    V(ge)$label <- V(ge)$name
    rdp <- RedPort('MyPort')
    calld(rdp)
    addGraph( rdp, ge, layout.random(ge) )
    updateGraph(rdp)
  } else if (visualization==2) {
    
    g <- CommonNet[1:nrow(CommonNet),1:2]
    g <- graph.data.frame(g, directed = FALSE)
    e <- get.edgelist(g)
    ge <- graph.edgelist(e,directed=FALSE)
    geadj <- get.adjacency(ge)
    gegr <- graph.adjacency(geadj, mode="undirected", diag=FALSE)
    V(ge)$label <- V(ge)$name
    rdp <- RedPort('MyPort')
    calld(rdp)
    addGraph( rdp, ge, layout.random(ge) )
    updateGraph(rdp)
  }

  res <- new.env()  
  assign("DifNet", DifNet, envir=res)
  assign("CommonNet", CommonNet, envir=res)
  
  res
  
}

