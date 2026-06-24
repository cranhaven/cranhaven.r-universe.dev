uniqNet<- function(tops=c())
{
  if(is.null(nrow(tops))) cat("DC3NET: No list of interactions entered as matrix.\n")  
  else{
    tops <-as.matrix(tops)
    tmp <-c()
    nProbs <- nrow(tops)
    
    numCol <- ncol(tops)
    i<-1
    while(length(tops)> numCol)
    {
      x<-which((((tops[,1] %in% tops[i,1]) & (tops[,2] %in% tops[i,2]))  | ((tops[,2] %in% tops[i,1]) & (tops[,1] %in% tops[i,2]))))
      
      tmp <- rbind(tmp, tops[x[1],])
      rownames(tmp)[nrow(tmp)] <- rownames(tops)[x[1]]
      
      tops<-tops[-x,]
      
    }
    if(length(tops)== numCol) {
      tmp<-rbind(tmp,tops)
      rownames(tmp)[nrow(tmp)] <- rownames(tmp)[nrow(tmp)-1 ]
    }

    return(tmp)	
  } 
} 
