

#' Finding Maximum Size Biclusters
#' 
#' Simple function which scans a \code{Biclust} result and returns which biclusters have maximum row, column or size (row*column).
#' 
#' @author Ewoud De Troyer
#' 
#' @export
#' @param result A \code{Biclust} result. (e.g. The return object from \code{bibit} or \code{bibit2})
#' @param top The number of top row/col/size dimension which are searched for. (e.g. default \code{top=1} gives only the maximum)
#' 
#' @return A list containing:
#' \itemize{
#' \item \code{$row}: A matrix containing in the columns the Biclusters which had maximum rows, and in the rows the Row Dimension, Column Dimension and Size.
#' \item \code{$column}: A matrix containing in the columns the Biclusters which had maximum columns, and in the rows the Row Dimension, Column Dimension and Size.
#' \item \code{$size}: A matrix containing in the columns the Biclusters which had maximum size, and in the rows the Row Dimension, Column Dimension and Size.
#' }
#' 
#' @examples 
#' \dontrun{
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#' result <- bibit(data,minr=2,minc=2)
#' 
#' MaxBC(result)
#' 
#' }
MaxBC <- function(result,top=1){
  if(class(result)!="Biclust" & class(result)!="iBBiG"){stop("result needs to be of class 'Biclust'")}
  
  rowsum <- colSums(result@RowxNumber)
  colsum <- rowSums(result@NumberxCol)
  sizesum <- rowsum*colsum
  
  top.col <- sort(unique(colsum),decreasing=TRUE)[1:top]
  top.row <- sort(unique(rowsum),decreasing=TRUE)[1:top]
  top.size <- sort(unique(sizesum),decreasing=TRUE)[1:top]
  
  
  for(i in 1:top){
    
    ind.colmax <- which(top.col[i]==colsum)
    ind.rowmax <- which(top.row[i]==rowsum)
    ind.sizemax <- which(top.size[i]==sizesum)
    
    if(i==1){
      
      row <- rbind(RowDim=rowsum[ind.rowmax],ColDim=colsum[ind.rowmax],SizeDim=sizesum[ind.rowmax])
      colnames(row) <- paste0("BC",ind.rowmax)
      
      column <- rbind(RowDim=rowsum[ind.colmax],ColDim=colsum[ind.colmax],SizeDim=sizesum[ind.colmax])
      colnames(column) <- paste0("BC",ind.colmax)
      
      size <- rbind(RowDim=rowsum[ind.sizemax],ColDim=colsum[ind.sizemax],SizeDim=sizesum[ind.sizemax])
      colnames(size) <- paste0("BC",ind.sizemax)
      
    }else{
      if(length(ind.rowmax)>0){
        row.temp <- rbind(RowDim = rowsum[ind.rowmax], ColDim = colsum[ind.rowmax], 
                          SizeDim = sizesum[ind.rowmax])
        colnames(row.temp) <- paste0("BC", ind.rowmax)
        row <- cbind(row, row.temp)
      }
      
      if(length(ind.colmax)>0){
        column.temp <- rbind(RowDim = rowsum[ind.colmax], 
                             ColDim = colsum[ind.colmax], SizeDim = sizesum[ind.colmax])
        colnames(column.temp) <- paste0("BC", ind.colmax)
        column <- cbind(column, column.temp)
      }
      
      if(length(ind.sizemax)>0){
        size.temp <- rbind(RowDim = rowsum[ind.sizemax], 
                           ColDim = colsum[ind.sizemax], SizeDim = sizesum[ind.sizemax])
        colnames(size.temp) <- paste0("BC", ind.sizemax)
        size <- cbind(size, size.temp)
      }
    }
    
  }
  
  
  
  return(list(row=row,column=column,size=size))
}






#' Transform R matrix object to BiBit input files.
#'
#' Transform the R matrix object to 1 \code{.arff} for the data and 2 \code{.csv} files for the row and column names. These are the 3 files required for the original BiBit Java algorithm
#' The path of these 3 files can then be used in the \code{arff_row_col} parameter of the \code{bibit} function.
#'
#' @author Ewoud De Troyer
#'
#' @export
#' @param matrix The binary input matrix.
#' @param name Basename for the 3 input files.
#' @param path Directory path where to write the 3 input files to.
#'
#' @return 3 input files for BiBit:
#' \itemize{
#' \item One \code{.arff} file containing the data.
#' \item One \code{.csv} file for the row names. The file contains 1 column of names without quotation.
#' \item One \code{.csv} file for the column names. The file contains 1 column of names without quotation.
#' }
#'
#' @examples
#' \dontrun{
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#'
#' make_arff_row_col(matrix=data,name="data",path="")
#'
#' result <- bibit(data,minr=5,minc=5,
#'                 arff_row_col=c("data_arff.arff","data_rownames.csv","data_colnames.csv"))
#' }
make_arff_row_col <- function(matrix,name="data",path=""){
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.vector(matrix),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  
  # Check if rownames & colnames contain ; or ,  -> should be deleted and give warnings it was deleted
  rowdot <- grepl(",",rownames(matrix))
  if(sum(rowdot)>0){
    rownames(matrix) <- gsub(",","",rownames(matrix))
    warning(paste0("Row names ",paste0(which(rowdot),collapse = ",")," contained a ',' which was deleted."),call.=FALSE)
  }
  rowsc <- grepl(";",rownames(matrix))
  if(sum(rowsc)>0){
    rownames(matrix) <- gsub(";","",rownames(matrix))
    warning(paste0("Row names ",paste0(which(rowsc),collapse = ",")," contained a ';' which was deleted."),call.=FALSE)
  }
  coldot <- grepl(",",colnames(matrix))
  if(sum(coldot)>0){
    colnames(matrix) <- gsub(",","",colnames(matrix))
    warning(paste0("Column names ",paste0(which(coldot),collapse = ",")," contained a ',' which was deleted."),call.=FALSE)
  }
  colsc <- grepl(";",colnames(matrix))
  if(sum(colsc)>0){
    colnames(matrix) <- gsub(";","",colnames(matrix))
    warning(paste0("Column names ",paste0(which(colsc),collapse = ",")," contained a ';' which was deleted."),call.=FALSE)
  }
  
  # No duplicate row names allowed!
  if(sum(table(rownames(matrix))>1)){stop("No duplicate row names allowed!")}
  
  write.arff(t(matrix),file=paste0(getwd(),"/",path,"/",name,"_arff.arff"))
  write.table(matrix(rownames(matrix),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=paste0(getwd(),"/",path,"/",name,"_rownames.csv"))
  write.table(matrix(colnames(matrix),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=paste0(getwd(),"/",path,"/",name,"_colnames.csv"))
  
}





rows_in_BC <- function(bicresult,rows){
  
  if(class(bicresult)!="Biclust"){stop("bicresult is not a Biclust class object",call.=FALSE)}
  
  BC.boolean <- sapply(1:bicresult@Number,FUN=function(x){
    return(all(rows%in%which(bicresult@RowxNumber[,x])))
  })
  
  BC.sel <- which(BC.boolean)
  
  rowdim <- colSums(bicresult@RowxNumber[,BC.sel,drop=FALSE])
  coldim <- rowSums(bicresult@NumberxCol[BC.sel,,drop=FALSE])
  sizedim <- rowdim*coldim
  
  out <- matrix(c(rowdim,coldim,sizedim),byrow=TRUE,nrow=3,ncol=length(BC.sel),dimnames=list(c("RowDim","ColDim","SizeDim"),paste0("BC",BC.sel)))
  out <- out[,order(sizedim,decreasing=TRUE)]
  
  return(out)
}







rows_full1_in_BC <- function(matrix,bicresult,rows){
  
  if(class(bicresult)!="Biclust"){stop("bicresult is not a Biclust class object",call.=FALSE)}
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  
  
  BC.boolean <- sapply(1:bicresult@Number,FUN=function(x){
    
    if(all(rows%in%which(bicresult@RowxNumber[,x]))){
      
      submat <- matrix[rows,bicresult@NumberxCol[x,]]
      return(all(submat==1))
      
    }else{
      return(FALSE)
    }
  })
  
  BC.sel <- which(BC.boolean)
  
  rowdim <- colSums(bicresult@RowxNumber[,BC.sel,drop=FALSE])
  coldim <- rowSums(bicresult@NumberxCol[BC.sel,,drop=FALSE])
  sizedim <- rowdim*coldim
  
  out <- matrix(c(rowdim,coldim,sizedim),byrow=TRUE,nrow=3,ncol=length(BC.sel),dimnames=list(c("RowDim","ColDim","SizeDim"),paste0("BC",BC.sel)))
  out <- out[,order(sizedim,decreasing=TRUE)]
  
  return(out)
}


fitness_score <- function(BC,alpha=1){
  if(!identical(as.numeric(as.vector(BC)),as.numeric(as.logical(BC)))){stop("BC is not a binary matrix!",call.=FALSE)}
  
  W_ik <- apply(BC,MARGIN=1,FUN=sum)
  
  p_i <- W_ik/ncol(BC)
  
  H_i <- sapply(p_i,FUN=function(x){
    if(x==0 | x==1){
      return(0)
    }else{
      return(-x*log(x,base=2)-(1-x)*log(1-x,base=2))
    }
  })
  
  S_i <- rep(0,length(H_i))
  S_i[p_i>0.5] <- W_ik[p_i>0.5]*(1-H_i[p_i>0.5])^alpha
  
  return(list(S_i=S_i,score=sum(S_i),score_idea=sum(S_i)/(nrow(BC)*ncol(BC))))
}



GOF <- function(matrix,bicresult,alpha=1,verbose=FALSE){
  if(class(bicresult)!="Biclust"){stop("bicresult is not a Biclust class object",call.=FALSE)}
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}

  if(alpha<0 | alpha>1){stop("alpha should be between 0 and 1")}
  
  outputlist_S <- vector("list",bicresult@Number)
  names(outputlist_S) <- paste0("BC",1:bicresult@Number)

  outputdf <- matrix(NA,nrow=bicresult@Number,ncol=2,dimnames=list(names(outputlist_S),c("score","score_idea")))
  
    
  for(i in 1:length(outputlist_S)){
    BC <- matrix[bicresult@RowxNumber[,i],bicresult@NumberxCol[i,]]
    temp <- fitness_score(BC,alpha=alpha)
    outputlist_S[[i]] <- temp$S_i
    outputdf[i,] <- c(temp$score,temp$score_idea)
  }
  
  output <- list(fitness=as.data.frame(outputdf),S_i=outputlist_S)
  
  class(output) <- "GOFBC"
  
  # Do a summary print
  if(verbose){summary(output)}
  
  return(output)
}


summary.GOFBC <- function(object,...){
  
  score <- object$fitness$score
  score_idea <- object$fitness$score_idea
  names(score) <- names(score_idea) <- rownames(object$fitness)
  
  selnumber <- ifelse(length(score)>=5,5,length(score))
  
  cat("Top 5 Fitness Scores:\n")
  cat("---------------------\n")
  print(sort(score,decreasing=TRUE)[1:selnumber])
  cat("\n")
  cat("Top 5 Experimental Fitness Scores:\n")
  cat("----------------------------------\n")
  print(sort(score_idea,decreasing=TRUE)[1:selnumber])
  
}


#' @export
print.bibit3 <- function(x,...){
  
  
  for(i in 1:(length(x)-1)){
    cat("\n")
    cat(toupper(names(x)[i]),"\n")
    cat(paste0(rep("-",nchar(toupper(names(x)[i]))),collapse=""),"\n\n")
    
    patterns <- c("FullPattern","SubPattern","Extended")
    
    for(j in 1:length(patterns)){
      cat(paste0(toupper(patterns[j]),":"),"\n")
      
      object <- x[[i]][[patterns[j]]]
      
      n<-object@Number
      
      if(n>1)
      {
        cat("\nNumber of Clusters found: ",object@Number, "\n")
        cat("\nCluster sizes:\n")
        
        rowcolsizes<-rbind(colSums(object@RowxNumber[,1:n]),rowSums(object@NumberxCol[1:n,]))
        rownames(rowcolsizes)<-c("Number of Rows:","Number of Columns:")
        colnames(rowcolsizes)<-paste("BC", 1:n)
        #print.default(format(rowcolsizes, print.gap = 2, quote = FALSE))
        print(rowcolsizes)
      }
      else
      {
        if(n==1) cat("\nThere was one cluster found with\n ",sum(object@RowxNumber[,1]), "Rows and ", sum(object@NumberxCol), "columns\n")
        if(n==0) cat("\nThere was no cluster found\n")
      }
      cat("\n\n")
      
      
    }
  }
  
}


#' @export
summary.bibit3 <- function(object,...){
  print(object)
}



#' @title Extract BC from \code{bibit3} result and add pattern
#' 
#' @description Function which will print the BC matrix and add 2 duplicate articial pattern rows on top. The function allows you to see the BC and the pattern the BC was guided towards to.
#' @author Ewoud De Troyer
#' 
#' @export
#' @param result Result produced by \code{\link{bibit3}}
#' @param matrix The binary input matrix.
#' @param pattern Vector containing either the number or name of which patterns the BC results should be extracted.
#' @param type Vector for which BC results should be printed.
#' \itemize{
#' \item Full Pattern (\code{"full"})
#' \item Sub Pattern (\code{"sub"})
#' \item Extended (\code{"ext"})
#' }
#' @param BC Vector of BC indices which should be printed, conditioned on \code{pattern} and \code{type}.
#' @return Prints queried biclusters.
#' @examples
#' \dontrun{ 
#' set.seed(1)
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' colsel <- sample(1:ncol(data),ncol(data))
#' data <- data[sample(1:nrow(data),nrow(data)),colsel]
#' 
#' pattern_matrix <- matrix(0,nrow=3,ncol=100)
#' pattern_matrix[1,1:7] <- 1
#' pattern_matrix[2,11:15] <- 1
#' pattern_matrix[3,13:20] <- 1
#' 
#' pattern_matrix <- pattern_matrix[,colsel]
#' 
#' 
#' out <- bibit3(matrix=data,minr=2,minc=2,noise=0.1,pattern_matrix=pattern_matrix,
#'               subpattern=TRUE,extend_columns=TRUE,pattern_combinations=TRUE)
#' out  # OR print(out) OR summary(out)
#' 
#' 
#' bibit3_patternBC(result=out,matrix=data,pattern=c(1),type=c("full","sub","ext"),BC=c(1,2))
#' }
bibit3_patternBC <- function(result,matrix,pattern=c(1),type=c("full","sub","ext"),BC=c(1)){
  
  if(class(result)!="bibit3"){stop("result is not a `bibit3' S3 object")}
  
  # Check if matrix is binary (DISCRETIZED NOT YET IMPLEMENTED!)
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  
  # Checking other input
  nPatterns <- nrow(result$pattern_matrix)
  
  if(class(pattern)=="character"){
    if(sum(!(pattern %in% names(result)))>0){stop("One of the patterns is not in the result object")}
    pattern <- sapply(pattern,FUN=function(x){which(x==names(result))})
  }
  if(class(pattern)!="numeric"){stop("pattern should be numeric vector")}
  if(sum(pattern>nPatterns)>0){stop("One of the patterns is not in the result object")}
  
  if(sum(!(type %in% c("full","sub","ext")))>0){stop("type contains wrong input")}
  type <- sapply(type,FUN=function(x){switch(x,full="FullPattern",sub="SubPattern",ext="Extended")})
  names(type) <- NULL
  
  if(!(class(BC)%in%c("integer","numeric"))){stop("BC should be numeric vector")}
  
  # Printing
  for(i.pattern in pattern){
    for(i.type in type){
      for(i.BC in BC){
        
        if(i.BC<=result[[i.pattern]][[i.type]]@Number){
          cat(paste0(toupper(names(result)[i.pattern])," - ",i.type," - BC ",i.BC))
          
          extra_rows <- matrix(rep(result$pattern_matrix[i.pattern,],2),nrow=2,byrow=TRUE,dimnames=list(paste0(names(result)[i.pattern],c("_Art1","_Art2")),colnames(matrix)))
          
          BCprint <- matrix[result[[i.pattern]][[i.type]]@RowxNumber[,i.BC],result[[i.pattern]][[i.type]]@NumberxCol[i.BC,]]
          cat("\n\n")
          print(rbind(extra_rows[,result[[i.pattern]][[i.type]]@NumberxCol[i.BC,]],BCprint))
          cat("\n\n")
        }
      }
    }
  }
}





jaccard_bc <- function(result,BC1,BC2){
  
  combine.m <- result1.m <- result2.m <- matrix(0,nrow=dim(result@RowxNumber)[1],ncol=dim(result@NumberxCol)[2])
  result1.m[result@RowxNumber[,BC1],result@NumberxCol[BC1,]] <- 1
  combine.m[result@RowxNumber[,BC1],result@NumberxCol[BC1,]] <- 1
  
  result2.m[result@RowxNumber[,BC2],result@NumberxCol[BC2,]] <- 1
  combine.m[result@RowxNumber[,BC2],result@NumberxCol[BC2,]] <- 1
  
  m1 <- sum(result1.m)
  m2 <- sum(result2.m)
  m12 <- sum(combine.m)
  
  JI <- (m1+m2-(m12))/(m12)
  
  return(JI)
}

same_bc <- function(result,BC1,BC2){
  if((all((result@RowxNumber[,BC1]-result@RowxNumber[,BC2])==0))&(all((result@NumberxCol[BC1,]-result@NumberxCol[BC2,])==0))){return(TRUE)}else{FALSE}
}



# Small function which checks if 1 BC is contained in another. The input should be a bit word representation of the row and column booleans of both BC's
BCcontained <- function(BC1word,BC2word){
  intersectword <- apply(rbind(BC1word,BC2word),MARGIN=2,FUN=function(x){bitwAnd(x[1],x[2])})
  if(all(intersectword==BC1word)){return(1)}
  if(all(intersectword==BC2word)){return(2)}
}


progress_dots <- function(i,nBC){
  if(nBC<=200){
    if(i%%40==0 | i==nBC){
      cat(".",i,"\n")
    }else{
      cat(".")
    }
  }else{
    linenumber <- ceiling(nBC/5)
    numberperdot <- round(linenumber/40,digits=0)
    if(i%%linenumber==0 | i==nBC){
      cat(".",i,"\n")
    }else{
      if(i%%numberperdot==0){cat(".")}
    }
  }
}


biclust_correctdim <- function(result,matrix){
  if(class(result)=="Biclust"){
    if((nrow(matrix)!=nrow(result@RowxNumber))|(ncol(matrix)!=ncol(result@NumberxCol))){stop("result and matrix have incompatible dimenions",call.=FALSE)}
  }else if(class(result)=="BiBitWorkflow"){
    if((nrow(matrix)!=nrow(result$Biclust@RowxNumber))|(ncol(matrix)!=ncol(result$Biclust@NumberxCol))){stop("result and matrix have incompatible dimenions",call.=FALSE)}
  }
}




#' @title Summary Method for Biclust Class
#' @description Summary Method for Biclust Class
#' @param object Biclust S4 Object
#' @export
setMethod("summary", "Biclust",
          function(object)
          {
            cat("\nAn object of class",class(object),"\n\n")
            cat("call:", deparse(object@Parameters$Call,0.75*getOption("width")),
                sep="\n\t")
            n<-object@Number
            
            if(n>1)
            {
              cat("\nNumber of Clusters found: ",object@Number, "\n")
              cat("\nCluster sizes:\n")
              
              rowcolsizes<-rbind(colSums(object@RowxNumber[,1:n]),rowSums(object@NumberxCol[1:n,]))
              rownames(rowcolsizes)<-c("Number of Rows:","Number of Columns:")
              colnames(rowcolsizes)<-paste("BC", 1:n)
              #print.default(format(rowcolsizes, print.gap = 2, quote = FALSE))
              print(rowcolsizes)
            }
            else
            {
              if(n==1) cat("\nThere was one cluster found with\n ",sum(object@RowxNumber[,1]), "Rows and ", sum(object@NumberxCol), "columns")
              if(n==0) cat("\nThere was no cluster found")
            }
            cat("\n\n")
            
            
          })



