
.EnvBIBIT <- new.env()


.GetEnvBIBIT <- function(x){
  if(!exists(x,envir=.EnvBIBIT,inherits=FALSE)){
    return(NULL)
  }
  else{
    return(get(x=x,envir=.EnvBIBIT,inherits=FALSE))
  }
  
}

.AssignEnvBIBIT <- function(x,value){
  assign(x=x,value=value,envir=.EnvBIBIT)
}



# Function which checks which one of the candidates are viable to be added as extra columns
check_candidates <- function(data,included,candidates,noise){
  
  if(length(candidates)>0){
    
    noise <- ifelse(((noise<1)&(noise>0)),ceiling(noise*(length(included)+1)),noise)
    
    noise_in_rows <- ncol(data[,included,drop=FALSE])-rowSums(data[,included,drop=FALSE])
    
    rows_noise_allowed <- which((noise - noise_in_rows)>0)
    
    compatible_candidates <- sapply(candidates,FUN=function(x){
      
      zero_rows <- which(data[,x]==0)
      
      if(all(zero_rows %in% rows_noise_allowed)){
        return(TRUE)
      }else{
        return(FALSE)
      }
    })
    return(candidates[compatible_candidates])
  }else{
    return(candidates)
  }
  

}


# Function to extend the columns of the bibit bicluster RECURSIVELY
BC_column_extension_recursive <- function(result,data,noise,extend_limitcol=1,extend_mincol=1){
  
  # Check parameters & prepare list objects
  if(extend_limitcol<=0){stop("Unallowed extend_limitcol")}
  if(extend_mincol<1){stop("Unallowed extend_mincol")}
  extend_mincol <- as.integer(extend_mincol)
  
  time_extend <- round(proc.time()['elapsed']/60,2)
  
  BC.extended_list <- rowxnumber_list <-  numberxcol_list <- vector("list",result@Number)
  
  pb <- txtProgressBar(min=1,max=result@Number,initial=1,style=3)
 
  # Apply extension for each BC
  for(i.BC in 1:result@Number){
    
    
    # Progress print
    setTxtProgressBar(pb,i.BC)
    # progress_dots(i=i.BC,nBC=result@Number)
    # cat("BC",i.BC,"\n")
    #######
    
    
    if(extend_limitcol<1){
      extend_limitcol2 <- ceiling(extend_limitcol*sum(result@RowxNumber[,i.BC]))
    }else{
      extend_limitcol2 <- min(extend_limitcol,sum(result@RowxNumber[,i.BC]))
    }

    .AssignEnvBIBIT(x="extensions",value=list())
    
    included_temp <- which(result@NumberxCol[i.BC,])
    candidates_temp <- which(!result@NumberxCol[i.BC,])
    
    # Before going into recursion which checks candidates, already delete candidates which have a full-zero column when extending this BC
    # + delete candidates which do not have enough 1's (depending on extend_limitcol parameter)
    candidates_temp <- candidates_temp[(which(colSums(data[result@RowxNumber[,i.BC],candidates_temp,drop=FALSE])>=extend_limitcol2))]
    
    # Apply recursive procedure
    temp <- extension_recursive(data=data[result@RowxNumber[,i.BC],],included=included_temp,candidates=candidates_temp,noise=noise,startlength = length(included_temp))
    
    # Have we found extensions?
    extensions <- .GetEnvBIBIT("extensions")
    
    # If we found extensions, check if enough columns were added and add each extension to a biclust result
    if(length(extensions)>0){
      number_columns <- unlist(lapply(extensions,FUN=function(x){x$number_columns}))
      max_extracol <- max(number_columns)
      
      if(max_extracol>=extend_mincol){
        selected_ext <- which(number_columns==max_extracol)
        

        numberxcol <- matrix(rep(result@NumberxCol[i.BC,],length(selected_ext)+1),nrow=length(selected_ext)+1,byrow=TRUE)
        
        for(i.ext in 1:length(selected_ext)){numberxcol[i.ext+1,extensions[[selected_ext[i.ext]]]$included] <- TRUE}
        
        rowxnumber <- matrix(rep(result@RowxNumber[,i.BC],length(selected_ext)+1),ncol=length(selected_ext)+1)
        
        names_temp <- c(paste0("BC",i.BC),paste0("BC",i.BC,"_Ext",1:length(selected_ext)))
        rownames(numberxcol) <- names_temp
        colnames(rowxnumber) <- names_temp
        
        rowxnumber_list[[i.BC]] <- rowxnumber
        numberxcol_list[[i.BC]] <- numberxcol
        
        BC.extended_list[[i.BC]] <- data.frame(BC_Original=i.BC,Number_Extended=length(selected_ext),Number_AddedColumns=max_extracol)
        
      }else{
        rowxnumber_list[[i.BC]] <- result@RowxNumber[,i.BC,drop=FALSE]
        colnames(rowxnumber_list[[i.BC]]) <- paste0("BC",i.BC)
        numberxcol_list[[i.BC]] <- result@NumberxCol[i.BC,,drop=FALSE]
        rownames(numberxcol_list[[i.BC]]) <- paste0("BC",i.BC)
        BC.extended_list[[i.BC]] <- NULL
      }
      
    }else{
      rowxnumber_list[[i.BC]] <- result@RowxNumber[,i.BC,drop=FALSE]
      colnames(rowxnumber_list[[i.BC]]) <- paste0("BC",i.BC)
      numberxcol_list[[i.BC]] <- result@NumberxCol[i.BC,,drop=FALSE]
      rownames(numberxcol_list[[i.BC]]) <- paste0("BC",i.BC)
      BC.extended_list[[i.BC]] <- NULL
    }
    
    
  }
  
  close(pb)
  
  # Combine all extensions from all BC's into a single Biclust result
  
  #make a object with which BC's were extended, how many resulted BC's (when equal adding length), how many EXTRA columns
  BC.extended <- do.call(rbind,BC.extended_list)
  
  # do a do call rbind/cbind on lists to make final result
  RowxNumber <- do.call(cbind,rowxnumber_list)
  NumberxCol <- do.call(rbind,numberxcol_list)
  
  
  time_extend <- round(proc.time()['elapsed']/60-time_extend,2)
  
  info_temp <- result@info
  info_temp$Time_Min$extend <- time_extend
  info_temp$Time_Min$full <- info_temp$Time_Min$full + time_extend
  info_temp$BC.Extended <- BC.extended
  
  OUT <- new("Biclust",Parameters=result@Parameters,RowxNumber=RowxNumber,NumberxCol=NumberxCol,Number=ncol(RowxNumber),info=info_temp)
  
  return(OUT)

}

# Function for recursive column extension algorithm
extension_recursive <- function(data,included,candidates,noise,startlength){
  
  # Filter for viable candidates
  candidates <- check_candidates(data=data,included=included,candidates,noise)
    
  STOP <- FALSE
  
  # A stopping procedure which prevents recursion from continuing if we encounter an already saved combination (e.g. if something starts with 2,1 and we already did 1,2 then we can stop)
  if(length(candidates)>0){
    extensions_list <- .GetEnvBIBIT("extensions")
    
    # Check if we already have had this combination of added columns up till now, if so, we can stop, it will only result in the same BC
    if(length(extensions_list)>0 & (length(included)>(startlength+1))){
      added_length <- length(included)-startlength

      comb_list <- lapply(extensions_list,FUN=function(x){
        if(length(x$included)>=added_length){
          return(sort(x$included[1:added_length]))
        }
      })

      added_columns <- included[-c(1:startlength)]

      if(sum(unlist(
        lapply(comb_list,FUN=function(x){
        if(!is.null(x)){
          return(all(sort(added_columns)==x))
          }else{return(FALSE)}
        })
      ))>0){
        STOP <- TRUE
        # cat("stopped\n")
        }
    }
    
    if(!STOP){
      for(i.candidates in candidates){
        
        included_new <- included
        included_new[length(included_new)+1] <- i.candidates
        candidates_new <- candidates[!candidates==i.candidates]
        
        temp <- extension_recursive(data=data,included=included_new,candidates=candidates_new,noise=noise,startlength=startlength)
        
      }
    }

    
  }else if(length(included)>startlength){
    
    
    extensions_list <- .GetEnvBIBIT("extensions")
    extensions_list[[length(extensions_list)+1]] <- list(
      number_columns=(length(included)-startlength),
      included=included[-c(1:startlength)]
    )
    .AssignEnvBIBIT(x="extensions",value=extensions_list)
    
  }
  
}




# Function to extend the columns of the bibit bicluster NAIVELY (follow order of most 1`s columns)
BC_column_extension <- function(result,data,noise,extend_mincol=1,extend_limitcol=1){
  
  # Check parameters & prepare list objects
  time_extend <- round(proc.time()['elapsed']/60,2)
  
  if(extend_limitcol<=0){stop("Unallowed extend_limitcol")}
  if(extend_mincol<1){stop("Unallowed extend_mincol")}
  extend_mincol <- as.integer(extend_mincol)
  
  BC.extended_list <- rowxnumber_list <-  numberxcol_list <- vector("list",result@Number)
  
  # Apply extension for each BC
  for(i.BC in 1:result@Number){
    included_columns <- result@NumberxCol[i.BC,]
    
    colsums_temp <- colSums(data[result@RowxNumber[,i.BC],])
    column_candidates <- order(colsums_temp,decreasing=TRUE)
    
    if(extend_limitcol<1){
      extend_limitcol2 <- ceiling(extend_limitcol*sum(result@RowxNumber[,i.BC]))
    }else{
      extend_limitcol2 <- min(extend_limitcol,sum(result@RowxNumber[,i.BC]))
    }
    
    column_candidates <- column_candidates[which(colsums_temp[column_candidates]>=extend_limitcol2)]
    

    GO <- TRUE
    i.candidate <- 1
    
    
    while(GO & (i.candidate<=length(column_candidates))){
      
      if(!included_columns[column_candidates[i.candidate]]){
        
        included_columns_temp <- included_columns
        included_columns_temp[column_candidates[i.candidate]] <- TRUE
        
        zeros_allowed <- ifelse(((noise<1)&(noise>0)),ceiling(noise*sum(included_columns_temp)),noise)
        
        zeros_in_rows <- apply(data[result@RowxNumber[,i.BC],included_columns_temp],MARGIN=1,FUN=function(x){sum(x==0)})
        
        if(all(zeros_in_rows<=zeros_allowed)){
          
          included_columns <- included_columns_temp
          i.candidate <- i.candidate+1
          
          
        }else{
          GO <- FALSE
        }
      }else{
        i.candidate <- i.candidate+1
      }
      
    }
    
    n_addedcolumns <- sum(included_columns)-sum(result@NumberxCol[i.BC,])
    
    if(n_addedcolumns>=extend_mincol){
      
      rowxnumber <- matrix(rep(result@RowxNumber[,i.BC],2),ncol=2)
      numberxcol <- matrix(FALSE,nrow=2,ncol=ncol(data))
      numberxcol[1,] <- result@NumberxCol[i.BC,]
      numberxcol[2,included_columns] <- TRUE
      names_temp <- c(paste0("BC",i.BC),paste0("BC",i.BC,"_Ext1"))
      colnames(rowxnumber) <- names_temp
      rownames(numberxcol) <- names_temp
      
      rowxnumber_list[[i.BC]] <- rowxnumber
      numberxcol_list[[i.BC]] <- numberxcol
      
      BC.extended_list[[i.BC]] <- data.frame(BC_Original=i.BC,Number_Extended=1,Number_AddedColumns=n_addedcolumns)
      
      
    }else{
      
      rowxnumber_list[[i.BC]] <- result@RowxNumber[,i.BC,drop=FALSE]
      colnames(rowxnumber_list[[i.BC]]) <- paste0("BC",i.BC)
      numberxcol_list[[i.BC]] <- result@NumberxCol[i.BC,,drop=FALSE]
      rownames(numberxcol_list[[i.BC]]) <- paste0("BC",i.BC)
      BC.extended_list[[i.BC]] <- NULL
    }
    
    

  }
  
  # Recombine all BC extensions into a single Biclust result
  BC.extended <- do.call(rbind,BC.extended_list)
  
  RowxNumber <- do.call(cbind,rowxnumber_list)
  NumberxCol <- do.call(rbind,numberxcol_list)
  
  
  time_extend <- round(proc.time()['elapsed']/60-time_extend,2)
  
  info_temp <- result@info
  info_temp$Time_Min$extend <- time_extend
  info_temp$Time_Min$full <- info_temp$Time_Min$full + time_extend
  info_temp$BC.Extended <- BC.extended
  
  OUT <- new("Biclust",Parameters=result@Parameters,RowxNumber=RowxNumber,NumberxCol=NumberxCol,Number=ncol(RowxNumber),info=info_temp)
  
  return(OUT)
}




extension_procedure <- function(result2,data,extend_noise,extend_mincol,extend_limitcol,extend_columns,extend_contained){
  
  if(extend_columns!="none" & (result2@Number>0)){
    Number_result_noEXT <- result2@Number
    cat("\nExtending Columns using:",toupper(extend_columns))
    if(extend_columns=="naive"){
      cat("...")
      result2 <- BC_column_extension(result=result2,data=data,noise=extend_noise,extend_mincol = extend_mincol,extend_limitcol = extend_limitcol)
    }else if(extend_columns=="recursive"){
      cat("\n")
      result2 <- BC_column_extension_recursive(result=result2,data=data,noise=extend_noise,extend_mincol = extend_mincol,extend_limitcol = extend_limitcol)
      
    }
    cat("DONE\n")
    
    
    # Delete duplicate BC's (While putting the original BC's first so they do not get deleted) #
    n_added_BC <- result2@Number-Number_result_noEXT
    
    cat("Total BC after extending:",result2@Number,paste0("(Extra BC's: ",n_added_BC,")"),"\n\n")
    
    if(n_added_BC>0){
      
      indices_original <- which(!grepl("_Ext",colnames(result2@RowxNumber)))
      
      cat("Checking for Duplicate Biclusters... ")
      # In order to quickly delete duplicates, BC row and column memberships are encoded to 16bit words first
      nrow_data <- nrow(data)
      ncol_data <- ncol(data)
      
      nblocksrow <- ceiling(nrow_data/16)
      nblockscol <- ceiling(ncol_data/16)
      
      
      decBC_mat <- matrix(NA,nrow=result2@Number,ncol=nblocksrow+nblockscol,dimnames=list(colnames(result2@RowxNumber),NULL))
      temp <- 1:nrow_data
      rowchunks <- split(temp,ceiling(seq_along(temp)/16))
      temp <- 1:ncol_data
      colchuncks <- split(temp,ceiling(seq_along(temp)/16))
      
      for(i.decBC in 1:result2@Number){
        
        for(i.rowblock in 1:nblocksrow){
          decBC_mat[i.decBC,i.rowblock] <- strtoi(paste0(result2@RowxNumber[rowchunks[[i.rowblock]],i.decBC]+0,collapse=""),2)
        }
        
        for(i.colblock in 1:nblockscol){
          matindex <- i.colblock+nblocksrow
          decBC_mat[i.decBC,matindex] <- strtoi(paste0(result2@NumberxCol[i.decBC,colchuncks[[i.colblock]]]+0,collapse=""),2)
        }
      }
      
      # Change order to original BC's appear first
      order_temp <- 1:nrow(decBC_mat)
      order_temp <- c(order_temp[indices_original],order_temp[-indices_original])
      decBC_mat <- decBC_mat[order_temp,]
      
      dup_temp <- duplicated(decBC_mat,MARGIN=1)
      temp_index <- which(dup_temp)
      
      if(length(temp_index)>0){
        dup_names <- rownames(decBC_mat)[temp_index]
        decBC_mat <- decBC_mat[-temp_index,] 
        
        dup_index <- sapply(dup_names,FUN=function(x){which(x==colnames(result2@RowxNumber))})
        
        result2@RowxNumber <- result2@RowxNumber[,-dup_index,drop=FALSE]
        result2@NumberxCol <- result2@NumberxCol[-dup_index,,drop=FALSE]
        result2@Number <- nrow(result2@NumberxCol)
      }
      
      cat("DONE\n")
      cat("Number of duplicate BC's deleted:",sum(dup_temp),"\n")
      
      
      # If required, check if biclusters are contained within each other (excluding original biclusters) #
      if(extend_contained & n_added_BC>1){
        
        cat("\nChecking for contained Biclusters... \n")
        
        
        # Exclude original biclusters from this procedure
        indices_original <- which(!grepl("_Ext",rownames(decBC_mat)))
        
        decBC_mat <- decBC_mat[-indices_original,,drop=FALSE]
        
        contained_vector <- rep(NA,nrow(decBC_mat))
        # note: go through all, but skip if current i.decBC or j.decBC is already in contained_vector
        
        pb <- txtProgressBar(min=1,max=(nrow(decBC_mat)-1),initial=1,style=3)
        
        for(i.decBC in 1:(nrow(decBC_mat)-1)){
          
          ## Progress dots
          # progress_dots(i=i.decBC,nBC=nrow(decBC_mat)-1)
          setTxtProgressBar(pb,i.decBC)
          #
          
          if(!(i.decBC%in%contained_vector)){
            for(j.decBC in (i.decBC+1):(nrow(decBC_mat))){
              if(!(j.decBC%in%contained_vector)){
                current_comp <- c(i.decBC,j.decBC)
                contained <- BCcontained(decBC_mat[i.decBC,],decBC_mat[j.decBC,])
                if(!is.null(contained)){
                  contained_vector[current_comp[contained]] <- current_comp[contained]
                }
              } 
            }
          }
          
          # cat(length(contained_vector[!is.na(contained_vector)]),"\n")
        }
        close(pb)
        
        
        contained_vector <- contained_vector[!is.na(contained_vector)]
        
        if(length(contained_vector)>0){
          contained_names <- rownames(decBC_mat)[contained_vector]
          contained_index <- sapply(contained_names,FUN=function(x){which(x==colnames(result2@RowxNumber))})
          
          result2@RowxNumber <- result2@RowxNumber[,-contained_index]
          result2@NumberxCol <- result2@NumberxCol[-contained_index,]
          result2@Number <- nrow(result2@NumberxCol)
        }
        
        cat("DONE\n")
        cat("Number of contained within BC's deleted:",length(contained_vector),"\n\n")
        
      }
      
      
      cat("Final Total of Biclusters:",result2@Number,"\n\n")
      
      # Fix BC naming in rowxnumber and numberxcol (so Ext starts with 1 again and goes further +1)
      indices_original <- which(!grepl("_Ext",colnames(result2@RowxNumber)))
      
      names_list <- vector("list",length(indices_original))
      
      if(length(names_list)>1){
        for(i in 1:(length(indices_original)-1)){
          names_list[[i]] <- colnames(result2@RowxNumber)[indices_original[i]:(indices_original[i+1]-1)]
        }
      }
      names_list[[length(indices_original)]] <- colnames(result2@RowxNumber)[indices_original[length(indices_original)]:length(colnames(result2@RowxNumber))]
      
      
      newnames <- unlist(lapply(names_list,FUN=function(x){
        if(length(x)>1){x[2:length(x)] <- paste0(x[1],"_Ext",1:(length(x)-1))}
        return(x)
      }))
      
      colnames(result2@RowxNumber) <- newnames
      rownames(result2@NumberxCol) <- newnames
      
      # Adapt BC.Extended info + make NULL object if no extensions
      BC.Extended <- result2@info$BC.Extended
      if(!is.null(BC.Extended)){
        for(i in 1:nrow(BC.Extended)){
          BC.Extended$Number_Extended[i] <- length(names_list[[BC.Extended$BC_Original[i]]])-1
        }
        result2@info$BC.Extended <- BC.Extended[BC.Extended$Number_Extended>0,]
      }
      
    }
    
  }
  return(result2)
}



# BC_column_extension_pattern <- function(result,data,noise){
#   
#   
#   BC_extended <- rep(FALSE,result@Number)
#   
#   for(i.BC in 1:result@Number){
#     included_columns <- result@NumberxCol[i.BC,]
#     
#     column_candidates <- order(colSums(data[result@RowxNumber[,i.BC],]),decreasing=TRUE)
#     
#     GO <- TRUE
#     i.candidate <- 1
#     
#     
#     while(GO & (i.candidate<=length(column_candidates))){
#       
#       if(!included_columns[column_candidates[i.candidate]]){
#         
#         included_columns_temp <- included_columns
#         included_columns_temp[column_candidates[i.candidate]] <- TRUE
#         
#         zeros_allowed <- ifelse(((noise<1)&(noise>0)),ceiling(noise*sum(included_columns_temp)),noise)
#         
#         zeros_in_rows_withoutpattern <- apply(data[result@RowxNumber[,i.BC],included_columns_temp],MARGIN=1,FUN=function(x){sum(x==0)})[-c(1,2)]
#         
#         if(all(zeros_in_rows_withoutpattern<=zeros_allowed)){
#           
#           included_columns <- included_columns_temp
#           i.candidate <- i.candidate+1
#           
#           
#         }else{
#           GO <- FALSE
#         }
#       }else{
#         i.candidate <- i.candidate+1
#       }
#       
#     }
#     
#     if(sum(included_columns)>sum(result@NumberxCol[i.BC,])){BC_extended[i.BC] <- TRUE}
#     result@NumberxCol[i.BC,] <- included_columns
#     
#   }
#   
#   if(sum(BC_extended)>=1){
#     result@RowxNumber <- result@RowxNumber[,BC_extended,drop=FALSE]
#     result@NumberxCol <- result@NumberxCol[BC_extended,,drop=FALSE]
#     result@Number <- sum(BC_extended)
#     result@info <- list()
#   }else{
#     nrow.data <- nrow(result@RowxNumber)
#     ncol.data <- ncol(result@NumberxCol)
#     
#     result <- new("Biclust",Parameters=result@Parameters,
#                   RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
#                   NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
#                   Number=0,
#                   info=list()) 
#   }
#   
#   
#   return(result)
# }