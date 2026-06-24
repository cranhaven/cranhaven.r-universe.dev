dc3netdif <- function(netTemp, vals, rankdif, percent)
{
  # netTemp is test net list, e.g. tumor network.
  # vals variable is statistics in the other (control) condition

  numlinks <- nrow(netTemp)
  
  AdjL <- c()
  
  for(i in 1: numlinks){
    
    if( as.numeric(vals[i,4]) > rankdif & as.numeric(vals[i,1]) < (as.numeric(vals[i,2])*percent) & as.numeric(vals[i,5]) > rankdif & as.numeric(vals[i,1]) < (as.numeric(vals[i,3])*percent)  )  
    {
      
      temp <- append(netTemp[i,],i)
      
      temp <- append(temp,(as.numeric(vals[i,1])/as.numeric(vals[i,2])))
      
      temp <- append(temp, as.numeric(vals[i,4]))     
      
      temp <- append(temp,(as.numeric(vals[i,1])/as.numeric(vals[i,3])))
      
      temp <- append(temp, as.numeric(vals[i,5]))   
      
      AdjL <- rbind(AdjL,temp)		
    }
  }
  
  if(!is.null(AdjL)) {	
    colnames(AdjL)[8] <- "ContIndx"
    colnames(AdjL)[9] <- "ContrlMIrateG1"
    colnames(AdjL)[10] <- "ContrlRankG1"
    colnames(AdjL)[11] <- "ContrlMIrateG2"
    colnames(AdjL)[12] <- "ContrlRankG2"
    
    rownames(AdjL) <- c(1:nrow(AdjL))

  } else cat("DC3NET: No differential interactions found. Try less stringent cutoffs.\n")
  
  return(AdjL)
}
