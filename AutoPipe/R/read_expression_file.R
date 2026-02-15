#' Input Expression File 
#'
#' This function is used to upload a table into R for further use in the AutoPipe
#'
#'
#' @usage read_expression_file(file, format = "csv", sep=";",gene_name="SYMBOL", Trans=FALSE)
#' 
#' @param file The path of the expression table
#' @param format The format of the table "csv" or "txt"
#' @param sep The seperator of the input table
#' @param gene_name Genes are given in "SYMBOL" or "ENTREZID"
#' @param Trans Need Matrix Transpose TRUE or FALSE
#' @return A data.frame with a gene expression matrix
#'
#' @export read_expression_file

read_expression_file=function(file, format = "csv", sep=";",gene_name="SYMBOL", Trans=FALSE){
  
  print(paste("----- Start Import --------"))
  data_out=if(format=="csv"){
    
    data_out=utils::read.csv(file,row.names=1,header=T,sep = sep )
    if(Trans==T){data_out=t(data_out)}
    data_out=if(gene_name=="SYMBOL"){
      new=clusterProfiler::bitr(rownames(data_out), fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
      rownames(new)=new$ENTREZID
      new=new[!duplicated(new$ENTREZID), ]
      data_out=data_out[new$SYMBOL, ]
      rownames(data_out)=new$ENTREZID
      return(data_out)
    }
    
    return(data_out)
  }
  data_out=if(format=="txt"){
    data_out=utils::read.table(file,row.names=1,header=T)
    if(Trans==T){data_out=(t(data_out))}
    
    data_out=if(gene_name=="SYMBOL"){
      new=clusterProfiler::bitr(rownames(data_out), fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
      #Remove Duplicated genes
      new=new[!duplicated(new$ENTREZID), ]
      
      rownames(new)=new$ENTREZID
      
      data_out=data_out[new$SYMBOL, ]
      rownames(data_out)=new$ENTREZID
      return(data_out)
    }
    return(data_out)
  }
  
  
  print(paste("-- Data contained", nrow(data_out), "Genes ------------- Data contained", ncol(data_out), "Samples -----"))
  
  return(data_out)
}


