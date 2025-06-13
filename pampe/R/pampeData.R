pampeData <-
  function(data, start=1, frequency=1, timevar=NA, idvar=NA, yvar=NA){
      
      #Reshape to wide
      if(!is.na(timevar)&!is.na(idvar)){
        
        #If no y varname is included retrieve it
        #Keep all (only) var
        if (is.na(yvar)&ncol(data)==3){
          drop <- NULL
          varname <- colnames(data)[-which(colnames(data) %in% c(timevar, idvar))]
        }
        
        #If y varname included keep only that column
        if (!is.na(yvar)&ncol(data)!=3){
          drop <- colnames(data)[-which(colnames(data) %in% c(timevar, idvar, yvar))]
          varname <- yvar
        }  
        
        if (is.na(yvar)&ncol(data)!=3){
          stop("yvar undefined")
        }
        
        #Notice the switched id and time indices
        data <- reshape(data, timevar=idvar, idvar=timevar, drop=drop, direction="wide")
        rownames(data) <- data[,1]
        data <- data[,-1]
        #Rename cols
        colnames(data) <- substring(colnames(data), first=(nchar(varname)+2))

      }
      
      ##TS format
      data <- ts(data=data, start=start, frequency=frequency)
      rownames(data) <- time(data)
      result <- data
      return(result)
    
  }

