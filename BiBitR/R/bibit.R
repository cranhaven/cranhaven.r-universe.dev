




## IMPORTS ##

#' @importFrom foreign write.arff read.arff
#' @import biclust
#' @importFrom methods new setGeneric setMethod
#' @importFrom utils read.table write.table combn txtProgressBar setTxtProgressBar capture.output
#' @importFrom viridis viridis
#' @importFrom cluster agnes clusGap maxSE
#' @importFrom dendextend color_branches
#' @importFrom stats hclust as.hclust cutree as.dendrogram as.dist fisher.test heatmap lm
#' @importFrom lattice levelplot
#' @importFrom grDevices dev.new dev.off pdf png
#' @importFrom graphics abline barplot image legend par plot points text
#' @importFrom randomcoloR distinctColorPalette
NULL





#' @title The BiBit Algorithm
#' 
#' @description A R-wrapper which directly calls the original Java code for the BiBit algorithm (\url{http://eps.upo.es/bigs/BiBit.html}) and transforms it to the output format of the \code{Biclust} R package.
#' 
#' @details This function uses the original Java code directly (with the intended input and output). Because the Java code was not refactored, the \code{rJava} package could not be used.
#' The \code{bibit} function does the following:
#' \enumerate{
#' \item Convert R matrix to a \code{.arff} output file.
#' \item Use the \code{.arff} file as input for the Java code which is called by \code{system()}.
#' \item The outputted \code{.txt} file from the Java BiBit algorithm is read in and transformed to a \code{Biclust} object.
#' }
#' Because of this, there is a chance of \emph{overhead} when applying the algorithm on large datasets. Make sure your machine has enough RAM available when applying to big data.
#' 
#' @author Ewoud De Troyer
#' 
#' @references Domingo S. Rodriguez-Baena, Antonia J. Perez-Pulido and Jesus S. Aguilar-Ruiz (2011), "A biclustering algorithm for extracting bit-patterns from binary datasets", \emph{Bioinformatics}
#' 
#' @export
#' @param matrix The binary input matrix.
#' @param minr The minimum number of rows of the Biclusters.
#' @param minc The minimum number of columns of the Biclusters.
#' @param arff_row_col If you want to circumvent the internal R function to convert the matrix to \code{.arff} format, provide the pathname of this file here. Additionally, two \code{.csv} files should be provided containing 1 column of row and column names. These two files should not contain a header or quotes around the names, simply 1 column with the names.\cr 
#' (\emph{Example}: \code{arff_row_col=c("...\\\\data\\\\matrix.arff","...\\\\data\\\\rownames.csv","...\\\\data\\\\colnames.csv")})\cr
#' \emph{Note:} These files can be generated with the \code{\link{make_arff_row_col}} function.
#' \cr \strong{Warning:} Should you use the \code{write.arff} function from the \code{foreign} package, remember to transpose the matrix first.
#' @param output_path If as output, the original txt output of the Java code is desired, provide the outputh path here (without extension). In this case the \code{bibit} function will skip the transformation to a Biclust class object and simply return \code{NULL}.\cr 
#' (\emph{Example}: \code{output_path="...\\\\out\\\\bibitresult"})
#' \cr
#' (\emph{Description Output}: The following information about every bicluster generated will be printed in the output file: number of rows, number of columns, name of rows and name of columns.
#' @return A Biclust S4 Class object.
#' 
#' @examples 
#' \dontrun{
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#' result <- bibit(data,minr=5,minc=5)
#' result
#' MaxBC(result)
#' }
bibit <- function(matrix=NULL,minr=2,minc=2,arff_row_col=NULL,output_path=NULL){
  
  pm <- match.call()
  
  
  if(is.null(arff_row_col)){
    time_arff <- round(proc.time()['elapsed']/60,2)
    
    # Check if matrix is binary (DISCRETIZED NOT YET IMPLEMENTED!)
    if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
    if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
    
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
    
    # Transform data into arff format
    cat("Transform matrix into arff format...")
    
    bibitdata_path <- tempfile("bibitdata",fileext=".arff")
    bibitrows_path <- tempfile("bibitrows",fileext=".csv")
    bibitcols_path <- tempfile("bibitcols",fileext=".csv")
    
    write.arff(t(matrix),file=bibitdata_path)
    write.table(matrix(rownames(matrix),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=bibitrows_path)
    write.table(matrix(colnames(matrix),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=bibitcols_path)
    
    cat("DONE\n")
    cat("\n")
    
    time_arff <- round(proc.time()['elapsed']/60-time_arff,2)
    
    
  }else{
    time_arff <- 0
    
    if(length(arff_row_col)!=3){stop("arff_row_col should contain 3 elements",call.=FALSE)}
    bibitdata_path <- arff_row_col[1]
    bibitrows_path <- arff_row_col[2]
    bibitcols_path <- arff_row_col[3]
    
  }

  if(is.null(output_path)){
    bibitoutput_path <- tempfile("bibitoutput",fileext = "")
  }else{
    bibitoutput_path <- output_path
  }

  
  time_bibit <- proc.time()['elapsed']/60
  

  javaloc <- paste0(find.package("BiBitR")[1],"/java/BiBit.jar")
  # javaloc <- gsub("/","\\\\",javaloc)
  
  # BiBit.jar location needs to be standardized for package location! # .libPaths()
  # command <- paste("java -jar -Xmx1000M",javaloc,bibitdata_path,"1",minr,minc,bibitoutput_path,bibitrows_path,bibitcols_path,1)
  command <- paste("java -jar -Xmx1000M",paste0("\"",javaloc,"\""),paste0("\"",bibitdata_path,"\""),"1",minr,minc,paste0("\"",bibitoutput_path,"\""),paste0("\"",bibitrows_path,"\""),paste0("\"",bibitcols_path,"\""),1)
  
  system(command)
  
  time_bibit <- round(proc.time()['elapsed']/60-time_bibit,2)
  
  
  if(is.null(output_path)){
    cat("\n")
    cat("Transforming into biclust output...")
    
    time_biclust <- round(proc.time()['elapsed']/60,2)
    result <- bibit2biclust(data=matrix,resultpath=paste0(bibitoutput_path,"_1.txt"),arff_row_col = arff_row_col)
    cat("DONE\n")
    time_biclust <- round(proc.time()['elapsed']/60-time_biclust,2)
    

    if(!is.null(result)){
      result2 <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                     RowxNumber=result$RowxNumber,
                     NumberxCol=result$NumberxCol,
                     Number=result$Number,
                     info=list(Time_Min=list(arff=time_arff,bibit=time_bibit,biclust=time_biclust,full=time_arff+time_bibit+time_biclust)))
      
    }else{
      
      if(!is.null(arff_row_col)){
        rownames.data <- as.character(read.table(arff_row_col[2],header=FALSE)[,1])
        colnames.data <- as.character(read.table(arff_row_col[3],header=FALSE)[,1])
        nrow.data <- length(rownames.data)
        ncol.data <- length(colnames.data)
      }else{
        nrow.data <- nrow(matrix)
        ncol.data <- ncol(matrix)
      }
      
      
      result2 <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                     RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                     NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                     Number=0,
                     info=list(Time_Min=list(arff=time_arff,bibit=time_bibit,biclust=time_biclust,full=time_arff+time_bibit+time_biclust)))
    }

    return(result2)
    
  }else{
    return(NULL)
  }

}



bibit2biclust <- function(data,resultpath,arff_row_col){
  result <- read.table(resultpath,header=TRUE,sep=";")
  
  if(is.null(arff_row_col)){
    rownames.data <- rownames(data)
    colnames.data <- colnames(data)
    nrow.data <- nrow(data)
    ncol.data <- ncol(data)
  }else{
    rownames.data <- as.character(read.table(arff_row_col[2],header=FALSE)[,1])
    colnames.data <- as.character(read.table(arff_row_col[3],header=FALSE)[,1])
    nrow.data <- length(rownames.data)
    ncol.data <- length(colnames.data)
  }
  
  
  if(dim(result)[1]>0){
    
    result$Rows <- as.character(result$Rows)
    result$Columns <- as.character(result$Columns)
    
    Number <- nrow(result)
    
    rowlist <- strsplit(result$Rows,",")
    # for(i in 1:length(rowlist)){
    #   rowlist[[i]] <- rowlist[[i]][1:result$NumOfRows[i]]
    # }
    
    collist <- strsplit(result$Columns,", ")
    # for(i in 1:length(collist)){
    #   collist[[i]] <- collist[[i]][1:result$NumOfColumns[i]]
    # }
    
    # Let's add a quick to avoid problems...
    if(!identical(result$NumOfRows,unlist(lapply(rowlist,FUN=length)))){warning("Issue reading row names...")}
    if(!identical(result$NumOfColumns,unlist(lapply(collist,FUN=length)))){warning("Issue reading column names...")}
    
    
    rowlist_index <- lapply(rowlist,FUN=function(x){rownames.data %in%  x})
    collist_index <- lapply(collist,FUN=function(x){colnames.data %in%  x})
    
    RowxNumber <- matrix(unlist(rowlist_index),byrow=FALSE,nrow=nrow.data,ncol=Number)
    NumberxCol <- matrix(unlist(collist_index),byrow=TRUE,nrow=Number,ncol=ncol.data)
    
    # again quick BC dimension check 
    if(!identical(result$NumOfRows,as.integer(colSums(RowxNumber)))){warning("Issue row BC dimension")}
    if(!identical(result$NumOfColumns,as.integer(rowSums(NumberxCol)))){warning("Issue column BC dimension")}
    
    
    
    # Temporart list output, needs to be changed to biclust object
    return(list(Parameters=list(),Number=Number,RowxNumber=RowxNumber,NumberxCol=NumberxCol,info=list()))
    
  }else{
    return(NULL)
  }
}






#' @title The BiBit Algorithm with Noise Allowance
#' 
#' @description Same function as \code{\link{bibit}} with an additional new noise parameter which allows 0's in the discovered biclusters (See Details for more info).
#' 
#' @section Details - General: 
#' \code{bibit2} follows the same steps as described in the Details section of \code{\link{bibit}}.\cr
#' Following the general steps of the BiBit algorithm, the allowance for noise in the biclusters is inserted in the original algorithm as such:
#' \enumerate{
#' \item Binary data is encoded in bit words.
#' \item Take a pair of rows as your starting point.
#' \item Find the maximal overlap of 1's between these two rows and save this as a pattern/motif. You now have a bicluster of 2 rows and N columns in which N is the number of 1's in the motif.
#' \item Check all remaining rows if they match this motif, \emph{however} allow a specific amount of 0's in this matching as defined by the \code{noise} parameter. Those rows that match completely or those within the allowed noise range are added to bicluster.
#' \item Go back to \emph{Step 2} and repeat for all possible row pairs.
#' }
#' \emph{Note:} Biclusters are only saved if they satisfy the \code{minr} and \code{minc} parameter settings and if the bicluster is not already contained completely within another bicluster.\cr
#' \cr
#' What you will end up with are biclusters not only consisting out of 1's, but biclusters in which 2 rows (the starting pair) are all 1's and in which the other rows could contain 0's (= noise).\cr
#' \cr
#' \emph{Note:} Because of the extra checks involved in the noise allowance, using noise might increase the computation time a little bit.
#' 
#' 
#' @section Details - Column Extension:
#' An optional procedure which can be applied \emph{after} applying the BiBit algorithm (with noise) is called \emph{Column Extension}. 
#' The procedure will add extra columns to a BiBit bicluster, keeping into account the allowed \code{extend_noise} level in each row.
#' The primary goal is to, after applying BiBit with noise, to also try and add some noise to the 2 initial `perfect` rows.
#' Other parameters like \code{extend_mincol} and \code{extend_limitcol} can also further restrict which extensions should be discovered.
#' \cr This procedure can be done either \emph{naively} (fast) or \emph{recursively} (more slow and thorough) with the \code{extend_columns} parameter.
#' 
#' \describe{
#' \item{\code{"naive"}}{Subsetting on the bicluster rows, the column candidates are ordered based on the most 1's in a column. Afterwards, in this order, each column is sequentially checked and added when the resulted BC is still within row noise levels.
#' \cr This has 2 major consequences:
#' \itemize{
#' \item{If 2 columns are identical, the first in the dataset is added, while the second isn't (depending on the noise level allowed per row).}
#' \item{If 2 non-identical columns are viable to be added (correct row noise), the column with the most 1's is added. Afterwards the second column might not be viable anymore.}
#' }
#' Note that using this method will always result in a maximum of 1 extended bicluster per original bicluster.
#' }
#' \item{\code{"recursive"}}{
#' Conditioning the group of candidates for the allowed row noise level, each possible/allowed combination of adding columns to the bicluster is checked. Only the resulted biclusters with the highest number of extra columns are saved.
#' Of course this could result in multiple extensions for 1 bicluster if there are multiple `maximum added columns` results.
#' 
#' }
#' }
#' \emph{Note:} These procedures are followed by a fast check if the extensions resulted in any duplicate biclusters. If so, these are deleted from the final result.
#' 
#' 
#' 
#' @author Ewoud De Troyer
#' 
#' @references Domingo S. Rodriguez-Baena, Antonia J. Perez-Pulido and Jesus S. Aguilar-Ruiz (2011), "A biclustering algorithm for extracting bit-patterns from binary datasets", \emph{Bioinformatics}
#' 
#' @export
#' @param matrix The binary input matrix.
#' @param minr The minimum number of rows of the Biclusters.
#' @param minc The minimum number of columns of the Biclusters.
#' @param noise Noise parameter which determines the amount of zero's allowed in the bicluster (i.e. in the extra added rows to the starting row pair).
#' \itemize{
#' \item \code{noise=0}: No noise allowed. This gives the same result as using the \code{\link{bibit}} function. (default)
#' \item \code{0<noise<1}: The \code{noise} parameter will be a noise percentage. The number of allowed 0's in a (extra) row in the bicluster will depend on the column size of the bicluster. 
#' More specifically \code{zeros_allowed = ceiling(noise * columnsize)}. For example for \code{noise=0.10} and a bicluster column size of \code{5}, the number of allowed 0's would be \code{1}.
#' \item \code{noise>=1}: The \code{noise} parameter will be the number of allowed 0's in a (extra) row in the bicluster independent from the column size of the bicluster. In this noise option, the noise parameter should be an integer.
#' }
#' 
#' @param arff_row_col If you want to circumvent the internal R function to convert the matrix to \code{.arff} format, provide the pathname of this file here. Additionally, two \code{.csv} files should be provided containing 1 column of row and column names. These two files should not contain a header or quotes around the names, simply 1 column with the names.\cr 
#' (\emph{Example}: \code{arff_row_col=c("...\\\\data\\\\matrix.arff","...\\\\data\\\\rownames.csv","...\\\\data\\\\colnames.csv")})\cr
#' \emph{Note:} These files can be generated with the \code{\link{make_arff_row_col}} function.
#' \cr \strong{Warning:} Should you use the \code{write.arff} function from the \code{foreign} package, remember to transpose the matrix first.
#' @param output_path If as output, the original txt output of the Java code is desired, provide the outputh path here (without extension). In this case the \code{bibit} function will skip the transformation to a Biclust class object and simply return \code{NULL}.\cr 
#' (\emph{Example}: \code{output_path="...\\\\out\\\\bibitresult"})
#' \cr
#' (\emph{Description Output}: The following information about every bicluster generated will be printed in the output file: number of rows, number of columns, name of rows and name of columns.
#' @param extend_columns \emph{Column Extension Parameter}\cr Can be one of the following: \code{"none"}, \code{"naive"}, \code{"recursive"} which will apply either a naive or recursive column extension procedure. (See Details Section for more information.)
#' \cr Based on the extension, additional biclusters will be created in the Biclust object which can be seen in the column and row names of the \code{RowxNumber} and \code{NumberxCol} slots (\code{"_Ext"} suffix).
#' \cr The \code{info} slot will also contain some additional information. Inside this slot, \code{BC.Extended} contains info on which original biclusters were extended, how many columns were added, and in how many extra extended biclusters this resulted.
#' \cr \cr \strong{Warning:} Using a percentage-based \code{extend_noise} (or \code{noise} by default) in combination with the recursive procedure will result in a large amount of biclusters and increase the computation time a lot. Depending on the data when using recursive in combination with a noise percentage, it is advised to keep it reasonable small (e.g. 10\%). Another remedy is to sufficiently increase the \code{extend_limitcol} either as a percentage or integer to limit the candidates of columns.
#' @param extend_mincol \emph{Column Extension Parameter}\cr A minimum number of columns that a bicluster should be able to be extended with before saving the result. (Default=1)
#' @param extend_limitcol \emph{Column Extension Parameter}\cr The number (\code{extend_limitcol>=1}) or percentage (\code{0<extend_limitcol<1}) of 1's that a column (subsetted on the BC rows) should at least contain for it to be a candidate to be added to the bicluster as an extension. (Default=1) (Increase this parameter if the recursive extension takes too long. Limiting the pool of candidates will decrease computation time, but restrict the results more.)
#' 
#' @param extend_noise \emph{Column Extension Parameter}\cr The maximum allowed noise (in each row) when extending the columns of the bicluster. Can take the same as the \code{noise} parameter. By default this is the same value as \code{noise}.
#' @param extend_contained \emph{Column Extension Parameter}\cr Logical value if extended results should be checked if they contain each other (and deleted if this is the case). Default = \code{FALSE}. This can be a lengthy procedure for a large amount of biclusters (>1000).
#' 
#' 
#' @return A Biclust S4 Class object.
#' 
#' @examples 
#' \dontrun{
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#' 
#' result1 <- bibit2(data,minr=5,minc=5,noise=0.2)
#' result1
#' MaxBC(result1,top=1)
#' 
#' result2 <- bibit2(data,minr=5,minc=5,noise=3)
#' result2
#' MaxBC(result2,top=2)
#' }
bibit2 <- function(matrix=NULL,minr=2,minc=2,noise=0,arff_row_col=NULL,output_path=NULL,
                   extend_columns="none",extend_mincol=1,extend_limitcol=1,extend_noise=noise,extend_contained=FALSE
                   ){
  
  pm <- match.call()
  
  # Various parameter checks
  if(noise<0){stop("noise parameter can not be negative",call.=FALSE)}
  if(noise>=1){noise <- as.integer(noise)}
  
  if(extend_noise<0){stop("extend_noise parameter can not be negative",call.=FALSE)}
  if(extend_noise>=1){extend_noise <- as.integer(extend_noise)}
  if(extend_noise<noise){stop("extend_noise can't be lower than noise",call.=FALSE)}
  if(length(extend_columns)!=1){stop("extend_columns needs 1 input",call.=FALSE)}
  if(!(extend_columns)%in%c("none","naive","recursive")){stop("extend_columns should be \"none\", \"naive\" or \"recursive\"",call.=FALSE)}
  if(extend_limitcol<=0){stop("extend_limitcol should be larger than 0",call.=FALSE)}
  if(extend_mincol<1){stop("extend_mincol should be larger than or equal to 1",call.=FALSE)}
  

  
  if(is.null(arff_row_col)){
    time_arff <- round(proc.time()['elapsed']/60,2)
    
    # Check if matrix is binary (DISCRETIZED NOT YET IMPLEMENTED!)
    if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
    if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
    
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
    
    # Transform data into arff format
    cat("Transform matrix into arff format...")
    
    bibitdata_path <- tempfile("bibitdata",fileext=".arff")
    bibitrows_path <- tempfile("bibitrows",fileext=".csv")
    bibitcols_path <- tempfile("bibitcols",fileext=".csv")
    
    write.arff(t(matrix),file=bibitdata_path)
    write.table(matrix(rownames(matrix),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=bibitrows_path)
    write.table(matrix(colnames(matrix),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=bibitcols_path)
    
    cat("DONE\n")
    cat("\n")
    
    time_arff <- round(proc.time()['elapsed']/60-time_arff,2)
    
    
  }else{
    time_arff <- 0
    
    if(length(arff_row_col)!=3){stop("arff_row_col should contain 3 elements",call.=FALSE)}
    bibitdata_path <- arff_row_col[1]
    bibitrows_path <- arff_row_col[2]
    bibitcols_path <- arff_row_col[3]
    
  }
  
  if(is.null(output_path)){
    bibitoutput_path <- tempfile("bibitoutput",fileext = "")
  }else{
    bibitoutput_path <- output_path
  }
  
  
  time_bibit <- proc.time()['elapsed']/60
  

  javaloc <- paste0(find.package("BiBitR")[1],"/java/BiBit2.jar")
  # javaloc <- gsub("/","\\\\",javaloc)
  
  # BiBit.jar location needs to be standardized for package location! # .libPaths()
  command <- paste("java -jar -Xmx1000M",paste0("\"",javaloc,"\""),paste0("\"",bibitdata_path,"\""),"1",minr,minc,paste0("\"",bibitoutput_path,"\""),paste0("\"",bibitrows_path,"\""),paste0("\"",bibitcols_path,"\""),1,paste0(" ",noise))
  # cat(command,"\n")
  
  ## APPLY JAVA ALGORITHM OF BIBIT ##
  system(command)
  
  time_bibit <- round(proc.time()['elapsed']/60-time_bibit,2)
  
  ## TRANSFROM OUTPUT TO BICLUST RESULT ##
  if(is.null(output_path)){
    cat("\n")
    cat("Transforming into biclust output... ")
    
    time_biclust <- round(proc.time()['elapsed']/60,2)
    result <- bibit2biclust(data=matrix,resultpath=paste0(bibitoutput_path,"_1.txt"),arff_row_col = arff_row_col)
    cat("DONE\n")
    time_biclust <- round(proc.time()['elapsed']/60-time_biclust,2)
    
    
    if(!is.null(result)){
      result2 <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                     RowxNumber=result$RowxNumber,
                     NumberxCol=result$NumberxCol,
                     Number=result$Number,
                     info=list(Time_Min=list(arff=time_arff,bibit=time_bibit,biclust=time_biclust,full=time_arff+time_bibit+time_biclust)))
      
      ## COLUMN EXTENSION PROCEDURE ##
      if(!is.null(arff_row_col) & extend_columns!="none"){
        matrix <- read.arff(arff_row_col[1])
        rownames.data <- as.character(read.table(arff_row_col[2],header=FALSE)[,1])
        colnames.data <- as.character(read.table(arff_row_col[3],header=FALSE)[,1])
        if(length(rownames.data)!=nrow(matrix)){
          matrix <- t(matrix)
        }
        rownames(matrix) <- rownames.data
        colnames(matrix) <- colnames.data
        
      }
      result2 <- extension_procedure(result2=result2,data=matrix,extend_noise=extend_noise,extend_mincol=extend_mincol,extend_limitcol=extend_limitcol,extend_columns=extend_columns,extend_contained)
        
      
      
    }else{
      
      if(!is.null(arff_row_col)){
        rownames.data <- as.character(read.table(arff_row_col[2],header=FALSE)[,1])
        colnames.data <- as.character(read.table(arff_row_col[3],header=FALSE)[,1])
        nrow.data <- length(rownames.data)
        ncol.data <- length(colnames.data)
      }else{
        nrow.data <- nrow(matrix)
        ncol.data <- ncol(matrix)
      }
      
      
      result2 <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                     RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                     NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                     Number=0,
                     info=list(Time_Min=list(arff=time_arff,bibit=time_bibit,biclust=time_biclust,full=time_arff+time_bibit+time_biclust)))
    }
    
    return(result2)
    
  }else{
    return(NULL)
  }
  
}




#' @title The BiBit Algorithm with Noise Allowance guided by Provided Patterns.
#' 
#' @description Same function as \code{\link{bibit2}} but only aims to discover biclusters containing the (sub) pattern of provided patterns or their combinations.
#' @details The goal of the \code{\link{bibit3}} function is to provide one or multiple patterns in order to only find those biclusters exhibiting those patterns.
#' Multiple patterns can be given in matrix format, \code{pattern_matrix}, and their pairwise combinations can automatically be added to this matrix by setting \code{pattern_combinations=TRUE}.
#' All discovered biclusters are still subject to the provided \code{noise} level.
#' 
#' Three types of Biclusters can be discovered:
#' \describe{
#' \item{\emph{Full Pattern: }}{Bicluster which overlaps completely (within allowed noise levels) with the provided pattern. The column size of this bicluster is always equal to the number of 1's in the pattern.}
#' \item{\emph{Sub Pattern: }}{Biclusters which overlap with a part of the provided pattern within allowed noise levels. Will only be given if \code{subpattern=TRUE} (default). Setting this option to \code{FALSE} decreases computation time.}
#' \item{\emph{Extended: }}{Using the resulting biclusters from the full and sub patterns, other columns will be attempted to be added to the biclusters while keeping the noise as low as possible (the number of rows in the BC stays constant). 
#' This can be done either with \code{extend_columns} equal to \code{"naive"} or \code{"recursive"}. More info on the difference can be found in the Details Section of \code{\link{bibit2}}.
#' \cr Naturally the articially added pattern rows will not be taken into account with the noise levels as they are 0 in each other column.
#' \cr The question which is attempted to be answered here is \emph{`Do the rows, which overlap partly or fully with the given pattern, have other similarities outside the given pattern?`}
#' }
#' } 
#' 
#' \emph{How?}
#' \cr The BiBit algorithm is applied to a data matrix that contains 2 identical artificial rows at the top which contain the given pattern. 
#' The default algorithm is then slightly altered to only start from this articial row pair (=Full Pattern) or from 1 artificial row and 1 other row (=Sub Pattern).
#' 
#' \emph{Note 1 - Large Data:}
#' \cr The \code{arff_row_col} can still be provided in case of large data matrices, but the \code{.arff} file should already contain the pattern of interest in the first two rows. Consequently not more than 1 pattern at a time can be investigated with a single call of \code{bibit3}.
#' 
#' \emph{Note 2 - Viewing Results:}
#' \cr A \code{print} and \code{summary} method has been implemented for the output object of \code{bibit3}. It gives an overview of the amount of discovered biclusters and their dimensions
#' \cr Additionally, the \code{\link{bibit3_patternBC}} function can extract a Bicluster and add the artificial pattern rows to investigate the results.
#' 
#' @author Ewoud De Troyer
#' 
#' @references Domingo S. Rodriguez-Baena, Antonia J. Perez-Pulido and Jesus S. Aguilar-Ruiz (2011), "A biclustering algorithm for extracting bit-patterns from binary datasets", \emph{Bioinformatics}
#' 
#' @export
#' @param matrix The binary input matrix.
#' @param minr The minimum number of rows of the Biclusters. (Note that in contrast to \code{\link{bibit}} and \code{\link{bibit2}}, this can be be set to 1 since we are looking for additional rows to the provided pattern.)
#' @param minc The minimum number of columns of the Biclusters.
#' @param noise Noise parameter which determines the amount of zero's allowed in the bicluster (i.e. in the extra added rows to the starting row pair).
#' \itemize{
#' \item \code{noise=0}: No noise allowed. This gives the same result as using the \code{\link{bibit}} function. (default)
#' \item \code{0<noise<1}: The \code{noise} parameter will be a noise percentage. The number of allowed 0's in a (extra) row in the bicluster will depend on the column size of the bicluster. 
#' More specifically \code{zeros_allowed = ceiling(noise * columnsize)}. For example for \code{noise=0.10} and a bicluster column size of \code{5}, the number of allowed 0's would be \code{1}.
#' \item \code{noise>=1}: The \code{noise} parameter will be the number of allowed 0's in a (extra) row in the bicluster independent from the column size of the bicluster. In this noise option, the noise parameter should be an integer.
#' }
#' @param pattern_matrix Matrix (Number of Patterns x Number of Data Columns) containing the patterns of interest.
#' @param subpattern Boolean value if sub patterns are of interest as well (default=TRUE).
#' @param pattern_combinations Boolean value if the pairwise combinations of patterns (the intersecting 1's) should also used as starting points (default=FALSE).
#' @param arff_row_col Same argument as in \code{\link{bibit}} and \code{\link{bibit2}}. However you can only provide 1 pattern by using this option. For \code{bibit3} to work, the pattern has to be added 2 times on top of the matrix (= identical first 2 rows).
#' @param extend_columns \emph{Column Extension Parameter}\cr Can be one of the following: \code{"none"}, \code{"naive"}, \code{"recursive"} which will apply either a naive or recursive column extension procedure. (See Details Section for more information.)
#' \cr Based on the extension, additional biclusters will be created in the Biclust object which can be seen in the column and row names of the \code{RowxNumber} and \code{NumberxCol} slots (\code{"_Ext"} suffix).
#' \cr The \code{info} slot will also contain some additional information. Inside this slot, \code{BC.Extended} contains info on which original biclusters were extended, how many columns were added, and in how many extra extended biclusters this resulted.
#' \cr \cr \strong{Warning:} Using a percentage-based \code{extend_noise} (or \code{noise} by default) in combination with the recursive procedure will result in a large amount of biclusters and increase the computation time a lot. Depending on the data when using recursive in combination with a noise percentage, it is advised to keep it reasonable small (e.g. 10\%). Another remedy is to sufficiently increase the \code{extend_limitcol} either as a percentage or integer to limit the candidates of columns.
#' @param extend_mincol \emph{Column Extension Parameter}\cr A minimum number of columns that a bicluster should be able to be extended with before saving the result. (Default=1)
#' @param extend_limitcol \emph{Column Extension Parameter}\cr The number (\code{extend_limitcol>=1}) or percentage (\code{0<extend_limitcol<1}) of 1's that a column (subsetted on the BC rows) should at least contain for it to be a candidate to be added to the bicluster as an extension. (Default=1) (Increase this parameter if the recursive extension takes too long. Limiting the pool of candidates will decrease computation time, but restrict the results more.)
#' 
#' @param extend_noise \emph{Column Extension Parameter}\cr The maximum allowed noise (in each row) when extending the columns of the bicluster. Can take the same as the \code{noise} parameter. By default this is the same value as \code{noise}.
#' @param extend_contained \emph{Column Extension Parameter}\cr Logical value if extended results should be checked if they contain each other (and deleted if this is the case). Default = \code{FALSE}. This can be a lengthy procedure for a large amount of biclusters (>1000).
#' 
#' @return A S3 list object, \code{"bibit3"} in which each element (apart from the last one) corresponds with a provided pattern or combination thereof. \cr
#' Each element is a list containing:
#' \describe{
#' \item{\code{Number}: }{Number of Initially found BC's by applying BiBit with the provided pattern.} 
#' \item{\code{Number_Extended}: }{Number of additional discovered BC's by extending the columns.}
#' \item{\code{FullPattern}: }{Biclust S4 Class Object containing the Bicluster with the Full Pattern.}
#' \item{\code{SubPattern}: }{Biclust S4 Class Object containing the Biclusters showing parts of the pattern.}
#' \item{\code{Extended}: }{Biclust S4 Class Object containing the additional Biclusters after extending the biclusters (column wise) of the full and sub patterns}
#' \item{\code{info}: }{Contains \code{Time_Min} element which includes the elapsed time of parts and the full analysis.}
#' }
#' The last element in the list is a matrix containing all the investigated patterns.
#' 
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
bibit3 <- function(matrix=NULL,minr=1,minc=2,noise=0,pattern_matrix=NULL,subpattern=TRUE,pattern_combinations=FALSE,arff_row_col=NULL,
                   extend_columns="none",extend_mincol=1,extend_limitcol=1,extend_noise=noise,extend_contained=FALSE){
  
  pm <- match.call()
  minr <- minr + 2
  
  ###
  # Legacy compatibility for GUI
  if(is.logical(extend_columns)){
    extend_columns <- ifelse(extend_columns,"naive","none")
  }
  
  if(noise<0){stop("noise parameter can not be negative",call.=FALSE)}
  if(noise>=1){noise <- as.integer(noise)}
  
  ## Extend parameters
  if(extend_noise<0){stop("extend_noise parameter can not be negative",call.=FALSE)}
  if(extend_noise>=1){extend_noise <- as.integer(extend_noise)}
  if(extend_noise<noise){stop("extend_noise can't be lower than noise",call.=FALSE)}
  if(length(extend_columns)!=1){stop("extend_columns needs 1 input",call.=FALSE)}
  if(!(extend_columns)%in%c("none","naive","recursive")){stop("extend_columns should be \"none\", \"naive\" or \"recursive\"",call.=FALSE)}
  if(extend_limitcol<=0){stop("extend_limitcol should be larger than 0",call.=FALSE)}
  if(extend_mincol<1){stop("extend_mincol should be larger than or equal to 1",call.=FALSE)}
  
  ###
  
  
  if(is.null(arff_row_col)){
    
    # Check if matrix is binary (DISCRETIZED NOT YET IMPLEMENTED!)
    if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
    if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
    
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
    
    # Check pattern matrix
    if(is.null(pattern_matrix)){stop("pattern_matrix needs to be provided",call.=FALSE)}
    if(class(pattern_matrix)!="matrix"){stop("pattern_matrix parameter should contain a matrix object",call.=FALSE)}
    if(!identical(as.numeric(as.vector(pattern_matrix)),as.numeric(as.logical(pattern_matrix)))){stop("pattern_matrix is not a binary matrix!",call.=FALSE)}
    if(is.null(rownames(pattern_matrix))){rownames(pattern_matrix) <- paste0("Pattern",1:nrow(pattern_matrix))}
    if(ncol(pattern_matrix)!=ncol(matrix)){stop("matrix and pattern_matrix have a different number of columns",call.=FALSE)}
    
    
    # If combinations required, add to pattern!
    if(pattern_combinations & nrow(pattern_matrix)>1){
      cat("Computing pattern combinations...")
      comb_temp <- combn(1:nrow(pattern_matrix),2)
      comb_matrix <- matrix(NA,nrow=ncol(comb_temp),ncol=ncol(pattern_matrix),dimnames=list(paste0("comb",1:ncol(comb_temp))))
      
      for(i.comb in 1:ncol(comb_temp)){
        comb_matrix[i.comb,] <- ((pattern_matrix[comb_temp[1,i.comb],]+pattern_matrix[comb_temp[2,i.comb],])==2)+0
        rownames(comb_matrix)[i.comb] <- paste0(rownames(pattern_matrix)[comb_temp[1,i.comb]],"_",rownames(pattern_matrix)[comb_temp[2,i.comb]])
      }
      
      pattern_matrix <- rbind(pattern_matrix,comb_matrix)
      
      cat("DONE\n\n")
    }
    
    # Delete zero-rows
    zero_rows <- which(rowSums(pattern_matrix)==0)
    if(length(zero_rows)>0){
      pattern_matrix <- pattern_matrix[-zero_rows,,drop=FALSE]
    }
    
    
    nPatterns <- nrow(pattern_matrix)
    if(nPatterns==0){stop("No viable patterns in pattern_matrix, all zero values.")}
    
  }else{
    time_arff <- 0
    
    if(length(arff_row_col)!=3){stop("arff_row_col should contain 3 elements",call.=FALSE)}
    bibitdata_path <- arff_row_col[1]
    bibitrows_path <- arff_row_col[2]
    bibitcols_path <- arff_row_col[3]
    
    pattern_matrix <- matrix(NA,nrow=1,ncol=1,dimnames=list("arff_Pattern"))
    nPatterns <- 1
  }
  
  #############################################
  ## PREPARE BASIC ARFF FILE & READ IN LINES ##
  #############################################
  
  cat("Transform matrix into arff format...")
  
  bibitbasic_path <- tempfile("bibitbasic",fileext=".arff")
  write.arff(t(matrix),file=bibitbasic_path)
  basic_file <- file(bibitbasic_path)
  basic_lines <- readLines(basic_file)
  close(basic_file)
  
  number_white <- nrow(matrix)+2
  
  cat("DONE\n\n")
  
  ######################################
  ## START FOR LOOP OVER ALL PATTERNS ##
  ######################################
  FINAL_RESULT <- vector("list",nPatterns)
  names(FINAL_RESULT) <- rownames(pattern_matrix)
  
  
  for(i.pattern in 1:nPatterns){
    
    if(i.pattern>1){cat("\n=============================================================================\n\n")}
    cat(toupper(rownames(pattern_matrix)[i.pattern]),"\n\n")
    
    if(is.null(arff_row_col)){
      time_arff <- round(proc.time()['elapsed']/60,2)
      
      # Add patterns to matrix
      matrix_with_pattern <- rbind(matrix(rep(pattern_matrix[i.pattern,],2),nrow=2,byrow=TRUE,dimnames = list(paste0(rownames(pattern_matrix)[i.pattern],"_Art",c(1,2)))),matrix)
      
      # Transform data into arff format
      cat("Changing arff file...",rownames(pattern_matrix)[i.pattern],"...")
      
      bibitdata_path <- tempfile("bibitdata",fileext=".arff")
      bibitrows_path <- tempfile("bibitrows",fileext=".csv")
      bibitcols_path <- tempfile("bibitcols",fileext=".csv")
      
      
      new_lines_meta <- basic_lines[1:number_white]
      new_lines_data <- basic_lines[(number_white+1):length(basic_lines)]
      
      pattern <- apply(cbind(matrix_with_pattern[1,],matrix_with_pattern[2,]),MARGIN=1,FUN=paste0,collapse=",")
      new_rownames <- rownames(matrix_with_pattern)[c(1,2)]
      
      meta1 <- new_lines_meta[1]
      new_lines_meta <- new_lines_meta[-1]
      new_lines_meta <- c(meta1,paste0("@attribute ",new_rownames," numeric"),new_lines_meta)
      new_lines_data <- apply(cbind(pattern,new_lines_data),MARGIN=1,FUN=paste0,collapse=",")
      
      new_file <- file(bibitdata_path)
      writeLines(c(new_lines_meta,new_lines_data),new_file)
      close(new_file)
      
      write.table(matrix(rownames(matrix_with_pattern),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=bibitrows_path)
      write.table(matrix(colnames(matrix_with_pattern),ncol=1),quote=FALSE,row.names=FALSE,col.names=FALSE,file=bibitcols_path)
      
      cat("DONE\n")
      cat("\n")
      
      time_arff <- round(proc.time()['elapsed']/60-time_arff,2)
      
    }else{
      matrix_with_pattern <- NULL
      if(extend_columns!="none"){
        
        matrix_with_pattern <- read.arff(bibitdata_path)
        rownames.data <- as.character(read.table(bibitrows_path,header=FALSE)[,1])
        colnames.data <- as.character(read.table(bibitcols_path,header=FALSE)[,1])
        if(length(rownames.data)!=nrow(matrix_with_pattern)){
          matrix_with_pattern <- t(matrix_with_pattern)
        }
        rownames(matrix_with_pattern) <- rownames.data
        colnames(matrix_with_pattern) <- colnames.data
        
      }
    }
    
    # Apply BiBit Algorithm
    
    cat("Initiate BiBit for",rownames(pattern_matrix)[i.pattern],"...\n")
    cat("\n")
    
    bibitoutput_path <- tempfile("bibitoutput",fileext = "")
    
    
    time_bibit <- proc.time()['elapsed']/60
    
    
    javaloc <- paste0(find.package("BiBitR")[1],"/java/BiBit3.jar")
    # javaloc <- paste0(getwd(),"/inst/java/BiBit3.jar")
    
    subpat <- ifelse(subpattern,1,0)
    
    # BiBit.jar location needs to be standardized for package location! # .libPaths()
    command <- paste("java -jar -Xmx1000M",paste0("\"",javaloc,"\""),paste0("\"",bibitdata_path,"\""),"1",minr,minc,paste0("\"",bibitoutput_path,"\""),paste0("\"",bibitrows_path,"\""),paste0("\"",bibitcols_path,"\""),1,paste0(" ",noise),paste0(" ",subpat))
    # cat(command,"\n")
    system(command)
    
    time_bibit <- round(proc.time()['elapsed']/60-time_bibit,2)
    
    
    cat("\n")
    cat("Transforming into biclust output...")
    
    time_biclust <- round(proc.time()['elapsed']/60,2)
    result <- bibit2biclust(data=matrix_with_pattern,resultpath=paste0(bibitoutput_path,"_1.txt"),arff_row_col = arff_row_col)
    cat("DONE\n")
    time_biclust <- round(proc.time()['elapsed']/60-time_biclust,2)
    
    
    # Small prep
    if(!is.null(arff_row_col)){
      rownames.data <- as.character(read.table(arff_row_col[2],header=FALSE)[,1])
      colnames.data <- as.character(read.table(arff_row_col[3],header=FALSE)[,1])
      nrow.data <- length(rownames.data)
      ncol.data <- length(colnames.data)
    }else{
      nrow.data <- nrow(matrix_with_pattern)
      ncol.data <- ncol(matrix_with_pattern)
    }
    
    
    
    # Look for and label the Biclusters (Full Pattern (zero or not)/Sub Pattern)
    
    
    if(!is.null(result)){
      result2 <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                     RowxNumber=result$RowxNumber,
                     NumberxCol=result$NumberxCol,
                     Number=result$Number,
                     info=list(Time_Min=list(arff=time_arff,bibit=time_bibit,biclust=time_biclust,full=time_arff+time_bibit+time_biclust)))
      
      
      
      FullPattern <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                         RowxNumber=result2@RowxNumber[,1,drop=FALSE],
                         NumberxCol=result2@NumberxCol[1,,drop=FALSE],
                         Number=1,
                         info=list())
      
      
      if(subpattern & result2@Number>1){
        SubPattern <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                          RowxNumber=result2@RowxNumber[,2:result2@Number,drop=FALSE],
                          NumberxCol=result2@NumberxCol[2:result2@Number,,drop=FALSE],
                          Number=result2@Number-1,
                          info=list())
      }else{
        SubPattern <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                          RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                          NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                          Number=0,
                          info=list())
      }
      
      
      if(extend_columns!="none"){
        
        # Reduce matrix and result only for Extended part (Artificial rows may not influence extension procedure)
        ########################
        ########################
        result2_temp <- result2
        
        result2_temp@RowxNumber <- result2_temp@RowxNumber[-c(1,2),,drop=FALSE]
        
        if(result2_temp@Number>0){
          deleteBC_index <- which(colSums(result2_temp@RowxNumber)==0)
          
          if(length(deleteBC_index)>0){
            if(length(deleteBC_index)==result2_temp@Number){
              result2_temp <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                                  RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                                  NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                                  Number=0,
                                  info=list())
            }else{
              result2_temp@RowxNumber <- result2_temp@RowxNumber[,-deleteBC_index]
              result2_temp@NumberxCol <- result2_temp@NumberxCol[-deleteBC_index,]
              result2_temp@Number <- ncol(result2_temp@RowxNumber)
            }
          }
        }
        ########################
        ########################
        
        # Use extension_procedure, delete original BC's, check if there were extensions...
        # check for BC.Extender, if NULL, then make similar object below, otherwise delete parts
        
        Extended <- extension_procedure(result2=result2_temp,data=matrix_with_pattern[-c(1,2),],extend_noise=extend_noise,extend_mincol=extend_mincol,extend_limitcol=extend_limitcol,extend_columns=extend_columns,extend_contained=extend_contained)
        
        if(!is.null(Extended@info$BC.Extended)){
          
          original_index <- which(!grepl("_Ext",colnames(Extended@RowxNumber)))
          Extended@Number <- Extended@Number - length(original_index)
          Extended@RowxNumber <- Extended@RowxNumber[,-original_index,drop=FALSE]
          Extended@NumberxCol <- Extended@NumberxCol[-original_index,,drop=FALSE]
          
          time_extend <- Extended@info$Time_Min$extend
          Extended@info$Time_Min <- NULL
          Number_Extended <- Extended@Number
          
        }else{
          Extended <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                          RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                          NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                          Number=0,
                          info=list())  
          time_extend <- 0
          Number_Extended <- 0
        }
        
        
        # TO DO: take time extend from result + TO DO: add and check parameters + add documentaiton
        
      }else{
        Extended <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                        RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                        NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                        Number=0,
                        info=list())  
        time_extend <- 0
        Number_Extended <- 0
      }
      
      time_final <- list(arff=result2@info$Time_Min$arff,bibit= result2@info$Time_Min$bibit,biclust=result2@info$Time_Min$biclust,extend=time_extend,full=result2@info$Time_Min$full+time_extend)
      
      FINAL_RESULT[[i.pattern]] <- list(Number=result2@Number,Number_Extended=Number_Extended,FullPattern=FullPattern,SubPattern=SubPattern,Extended=Extended,info=list(Time_Min=time_final))
      
      
    }else{
      
      
      result2 <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                     RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                     NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                     Number=0,
                     info=list())
      
      FINAL_RESULT[[i.pattern]] <- list(Number=0,Number_Extended=0,FullPattern=result2,SubPattern=result2,Extended=result2,info=list(Time_Min=list(arff=time_arff,bibit=time_bibit,biclust=time_biclust,extend=0,full=time_arff+time_bibit+time_biclust)))
      
    }
    
  }
  
  # DELETE ARTIFICIAL ROWS FROM BC RESULTS , if no other rows remain, go to empty result
  
  for(i.list in 1:length(FINAL_RESULT)){
    for(j.list in c("FullPattern","SubPattern")){
      
      FINAL_RESULT[[i.list]][[j.list]]@RowxNumber <- FINAL_RESULT[[i.list]][[j.list]]@RowxNumber[-c(1,2),,drop=FALSE]
      
      if(FINAL_RESULT[[i.list]][[j.list]]@Number>0){
        deleteBC_index <- which(colSums(FINAL_RESULT[[i.list]][[j.list]]@RowxNumber)==0)
        
        if(length(deleteBC_index)>0){
          if(length(deleteBC_index)==FINAL_RESULT[[i.list]][[j.list]]@Number){
            FINAL_RESULT[[i.list]][[j.list]] <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                                                    RowxNumber=matrix(FALSE,nrow=nrow.data,ncol=1),
                                                    NumberxCol=matrix(FALSE,nrow=1,ncol=ncol.data),
                                                    Number=0,
                                                    info=list())
          }else{
            FINAL_RESULT[[i.list]][[j.list]]@RowxNumber <- FINAL_RESULT[[i.list]][[j.list]]@RowxNumber[,-deleteBC_index]
            FINAL_RESULT[[i.list]][[j.list]]@NumberxCol <- FINAL_RESULT[[i.list]][[j.list]]@NumberxCol[-deleteBC_index,]
            FINAL_RESULT[[i.list]][[j.list]]@Number <- ncol(FINAL_RESULT[[i.list]][[j.list]]@RowxNumber)
          }
        }
      }
    }
  }
  
  # END RESULT
  FINAL_RESULT$pattern_matrix <- pattern_matrix
  class(FINAL_RESULT) <- "bibit3"
  return(FINAL_RESULT)
}









#' @title Column Extension Procedure
#' 
#' @description Function which accepts result from \code{\link{bibit}}, \code{\link{bibit2}} or \code{\link{bibit3}} and will (re-)apply the column extension procedure. This means if the result already contained extended biclusters that these will be deleted.
#' 
#' @section Details - Column Extension:
#' An optional procedure which can be applied \emph{after} applying the BiBit algorithm (with noise) is called \emph{Column Extension}. 
#' The procedure will add extra columns to a BiBit bicluster, keeping into account the allowed \code{extend_noise} level in each row.
#' The primary goal is to, after applying BiBit with noise, to also try and add some noise to the 2 initial `perfect` rows.
#' Other parameters like \code{extend_mincol} and \code{extend_limitcol} can also further restrict which extensions should be discovered.
#' \cr This procedure can be done either \emph{naively} (fast) or \emph{recursively} (more slow and thorough) with the \code{extend_columns} parameter.
#' 
#' \describe{
#' \item{\code{"naive"}}{Subsetting on the bicluster rows, the column candidates are ordered based on the most 1's in a column. Afterwards, in this order, each column is sequentially checked and added when the resulted BC is still within row noise levels.
#' \cr This has 2 major consequences:
#' \itemize{
#' \item{If 2 columns are identical, the first in the dataset is added, while the second isn't (depending on the noise level allowed per row).}
#' \item{If 2 non-identical columns are viable to be added (correct row noise), the column with the most 1's is added. Afterwards the second column might not be viable anymore.}
#' }
#' Note that using this method will always result in a maximum of 1 extended bicluster per original bicluster.
#' }
#' \item{\code{"recursive"}}{
#' Conditioning the group of candidates for the allowed row noise level, each possible/allowed combination of adding columns to the bicluster is checked. Only the resulted biclusters with the highest number of extra columns are saved.
#' Of course this could result in multiple extensions for 1 bicluster if there are multiple `maximum added columns` results.
#' 
#' }
#' }
#' \emph{Note:} These procedures are followed by a fast check if the extensions resulted in any duplicate biclusters. If so, these are deleted from the final result.
#' 
#' @export
#' @param result Result from \code{\link{bibit}}, \code{\link{bibit2}} or \code{\link{bibit3}}.
#' @param matrix The binary input matrix.
#' @param arff_row_col The same file directories (with the same limitations) as given in \code{\link{bibit}}, \code{\link{bibit2}} or \code{\link{bibit3}}.
#' @param BC A numeric/integer vector of BC's which should be extended. Different behaviour for the 3 types of input results:
#' \describe{
#' \item{\code{bibit}}{\code{BC} directly takes the corresponding biclusters from the result and extends them. (e.g. \code{BC=c(1,10)} is then remapped to \code{c("BC1","BC1_Ext1","BC2","BC2_Ext1") in the new output})}
#' \item{\code{bibit2}}{\code{BC} corresponds with the original non-extended biclusters from the \code{\link{bibit2}} result. These original biclusters are selected and extended. (e.g. \code{BC=c(1,10)} selects biclusters \code{c("BC1","BC10")} which are then remapped to \code{c("BC1","BC1_Ext1","BC2","BC2_Ext1") in the new output})} 
#' \item{\code{bibit3}}{\code{BC} corresponds with the biclusters when combining the FULLPATTERN and SUBPATTERN result together. For example choosing \code{BC=1} would only select the 1 FULLPATTERN bicluster for each pattern and try to extend it. (e.g. \code{BC=c(1,10)} selects biclusters 1 and 10 from the combined fullpattern and subpattern result (meaning the full pattern BC and the 9th subpattern BC) which are then remapped to \code{c("BC1","BC1_Ext1","BC2","BC2_Ext1") in the new output}) }
#' }
#' 
#' @param extend_columns \emph{Column Extension Parameter}\cr Can be one of the following: \code{"naive"} or \code{"recursive"} which will apply either a naive or recursive column extension procedure. (See Details Section for more information.)
#' \cr Based on the extension, additional biclusters will be created in the Biclust object which can be seen in the column and row names of the \code{RowxNumber} and \code{NumberxCol} slots (\code{"_Ext"} suffix).
#' \cr The \code{info} slot will also contain some additional information. Inside this slot, \code{BC.Extended} contains info on which original biclusters were extended, how many columns were added, and in how many extra extended biclusters this resulted.
#' \cr \cr \strong{Warning:} Using a percentage-based \code{extend_noise} in combination with the recursive procedure will result in a large amount of biclusters and increase the computation time a lot. Depending on the data when using recursive in combination with a noise percentage, it is advised to keep it reasonable small (e.g. 10\%). Another remedy is to sufficiently increase the \code{extend_limitcol} either as a percentage or integer to limit the candidates of columns.
#' @param extend_mincol \emph{Column Extension Parameter}\cr A minimum number of columns that a bicluster should be able to be extended with before saving the result. (Default=1)
#' @param extend_limitcol \emph{Column Extension Parameter}\cr The number (\code{extend_limitcol>=1}) or percentage (\code{0<extend_limitcol<1}) of 1's that a column (subsetted on the BC rows) should at least contain for it to be a candidate to be added to the bicluster as an extension. (Default=1) (Increase this parameter if the recursive extension takes too long. Limiting the pool of candidates will decrease computation time, but restrict the results more.)
#' 
#' @param extend_noise \emph{Column Extension Parameter}\cr The maximum allowed noise (in each row) when extending the columns of the bicluster. Can take the same as the \code{noise} parameter.
#' @param extend_contained \emph{Column Extension Parameter}\cr Logical value if extended results should be checked if they contain each other (and deleted if this is the case). Default = \code{FALSE}. This can be a lengthy procedure for a large amount of biclusters (>1000).
#'
#' @author Ewoud De Troyer
#' @return A Biclust S4 Class object or bibit3 S3 list Class object
#' 
#' @examples 
#' \dontrun{
#' 
#' set.seed(1)
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#' 
#' result <- bibit2(data,minr=5,minc=5,noise=0.1,extend_columns = "recursive",
#'               extend_mincol=1,extend_limitcol=1)
#' result
#' result2 <- bibit_columnextension(result=out,matrix=data,arff_row_col=NULL,BC=c(1,10),
#'                               extend_columns="recursive",extend_mincol=1,
#'                               extend_limitcol=1,extend_noise=2,extend_contained=FALSE)
#' result2
#' }
bibit_columnextension <- function(result,matrix,arff_row_col=NULL,BC=NULL,
                                  extend_columns="naive",extend_mincol=1,extend_limitcol=1,extend_noise=1,extend_contained=FALSE){
  
  # explain what BC does (note: name will change to order in new result!)
  
  
  # Check if result comes from bibit/bibit2/bibit3
  if(!(class(result)=="Biclust" | class(result)=="bibit3")){
    
    if(class(result)=="Biclust"){
      if(result@Parameters$Method!="BiBit"){stop("result is Biclust class but does come from bibit")}
    }else{
      stop("result does not come from bibit/bibit2 or bibit3 function")
    }
  }
  
  pm <- match.call()
  
  # Check parameters
  if(!is.null(BC)){
    if(!(class(BC) %in% c("numeric","integer"))){stop("BC should be a numeric or integer vector",call.=FALSE)}
    BC <- as.integer(BC)
  }
  if(extend_noise<0){stop("extend_noise parameter can not be negative",call.=FALSE)}
  if(extend_noise>=1){extend_noise <- as.integer(extend_noise)}
  if(length(extend_columns)!=1){stop("extend_columns needs 1 input",call.=FALSE)}
  if(!(extend_columns)%in%c("naive","recursive")){stop("extend_columns should be \"naive\" or \"recursive\"",call.=FALSE)}
  if(extend_limitcol<=0){stop("extend_limitcol should be larger than 0",call.=FALSE)}
  if(extend_mincol<1){stop("extend_mincol should be larger than or equal to 1",call.=FALSE)}
  
  
  # Check matrix
  if(is.null(arff_row_col)){

    # Check if matrix is binary (DISCRETIZED NOT YET IMPLEMENTED!)
    if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
    if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
    
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
    

  }else{
    # Make matrix if arffpath is given
    
    time_arff <- 0
    
    if(length(arff_row_col)!=3){stop("arff_row_col should contain 3 elements",call.=FALSE)}

    matrix <- read.arff(arff_row_col[1])
    rownames.data <- as.character(read.table(arff_row_col[2],header=FALSE)[,1])
    colnames.data <- as.character(read.table(arff_row_col[3],header=FALSE)[,1])
    if(length(rownames.data)!=nrow(matrix)){
      matrix <- t(matrix)
    }
    rownames(matrix) <- rownames.data
    colnames(matrix) <- colnames.data
    
  }
  
  
  # Depending on result (bibit/bibit2/bibit3)
  # Check if there is extended result, if so delete it and reapply extension
  
  if(class(result)=="Biclust"){
    
    if(!is.null(result@info$BC.Extended)){
      ext_index <- which(grepl("_Ext",colnames(result@RowxNumber)))
      result@Number <- result@Number-length(ext_index)
      result@RowxNumber <- result@RowxNumber[,-ext_index,drop=FALSE]
      result@NumberxCol <- result@NumberxCol[-ext_index,,drop=FALSE]
      result@info$BC.Extended <- NULL
    }else{
      colnames(result@RowxNumber) <- paste0("BC",1:result@Number)  
      rownames(result@NumberxCol) <- paste0("BC",1:result@Number)  
    }
    
    if(!is.null(result@info$Time_Min$extend)){
      result@info$Time_Min$full <- result@info$Time_Min$full - result@info$Time_Min$extend
      result@info$Time_Min$extend <- NULL
    }
    result@Parameters$Call <- pm
    
    
    if(!is.null(BC)){
      BC_names <- paste0("BC",BC)
      BC_index <- sapply(BC_names,FUN=function(x){which(x==colnames(result@RowxNumber))})
      result@Number <- length(BC_index)
      result@RowxNumber <- result@RowxNumber[,BC_index,drop=FALSE]
      result@NumberxCol <- result@NumberxCol[BC_index,,drop=FALSE]
      
      cat("\nChosen specific BC's:\n")
      for(i.BC in 1:length(BC)){
        cat(BC_names[i.BC],"mapped to",paste0("BC",i.BC),"\n")
      }
      cat("\n")
    }
    cat("Number of Original BC's:",result@Number)
    result <- extension_procedure(result2=result,data=matrix,extend_noise=extend_noise,extend_mincol=extend_mincol,extend_limitcol=extend_limitcol,extend_columns=extend_columns,extend_contained=extend_contained)
    return(result)
    
    
  }else if(class(result)=="bibit3"){
    
    for(i.pattern in 1:(length(result)-1)){
      cat(paste0("\n",names(result)[i.pattern]))
      cat("\n----------\n")
      
      result2 <- result[[i.pattern]]
      
      if(!is.null(result2$info$Time_Min$extend)){
        result2$info$Time_Min$full <- result2$info$Time_Min$full - result2$info$Time_Min$extend
        result2$info$Time_Min$extend <- 0
      }
      
      # Combine Fullpattern, subpattern result, then apply extension and overwrite previous Extended result
      result_temp <- new("Biclust",Parameters=list(Call=pm,method="BiBit"),
                         RowxNumber=cbind(result2$FullPattern@RowxNumber,result2$SubPattern@RowxNumber),
                         NumberxCol=rbind(result2$FullPattern@NumberxCol,result2$SubPattern@NumberxCol),
                         Number=result2$Number,
                         info=list(Time_Min=list(
                           arff=result2$info$Time_Min$arff,
                           bibit=result2$info$Time_Min$bibit,
                           biclust=result2$info$Time_Min$biclust,
                           full=result2$info$Time_Min$full
                         ))
                         )
      
      if(!is.null(BC)){
        result_temp@Number <- length(BC)
        
        result_temp@RowxNumber <- result_temp@RowxNumber[,BC,drop=FALSE]
        result_temp@NumberxCol <- result_temp@NumberxCol[BC,,drop=FALSE]
        
        cat("\nChosen specific BC's:\n")
        for(i.BC in 1:length(BC)){
          cat(paste0("BC",BC[i.BC]),"mapped to",paste0("BC",i.BC),"\n")
        }
        cat("\n")
      }
      cat("Number of Original BC's:",result_temp@Number)
      
      
      result2$Extended <- extension_procedure(result2=result_temp,data=matrix,extend_noise=extend_noise,extend_mincol=extend_mincol,extend_limitcol=extend_limitcol,extend_columns=extend_columns,extend_contained=extend_contained)
      
      
      if(!is.null(result2$Extended@info$BC.Extended)){
        # Don't forget to change Number_Extended, change time in result2, delete time in result2$Extended and delete original BC's! 
        ext_index <- which(grepl("_Ext",colnames(result2$Extended@RowxNumber)))
        result2$Extended@Number <- length(ext_index)
        result2$Extended@RowxNumber <- result2$Extended@RowxNumber[,ext_index,drop=FALSE]
        result2$Extended@NumberxCol <- result2$Extended@NumberxCol[ext_index,,drop=FALSE]
        result2$Number_Extended <- result2$Extended@Number
        
        result2$info$Time_Min$extend <- result2$Extended@info$Time_Min$extend
        result2$info$Time_Min$full <- result2$Extended@info$Time_Min$full
        result2$Extended@info$Time_Min <- NULL
      }else{
        result2$Number_Extended <- 0
        
        result2$Extended <- new("Biclust",Parameters=list(Call=pm,Method="BiBit"),
                        RowxNumber=matrix(FALSE,nrow=nrow(matrix),ncol=1),
                        NumberxCol=matrix(FALSE,nrow=1,ncol=ncol(matrix)),
                        Number=0,
                        info=list()) 
      }
      

      
      result[[i.pattern]] <- result2
    }
    return(result) 
  }
  
  
  
}



