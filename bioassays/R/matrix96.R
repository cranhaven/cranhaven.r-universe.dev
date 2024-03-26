#'@name matrix96
#'
#'@aliases matrix96
#'
#' @title Formatting Long Dataframe in to a Matrix Layout of Multi well Plate
#'
#' @description This function format a long dataframe (with col and row columns) in to a multiwell plate matrix layout.
#'
#' @usage matrix96 (dataframe, column, rm = "FALSE")
#'
#'@param dataframe dataframe to be formatted
#'
#'@param column name of column (as a string in "") that need be converted as a matrix
#'
#'@param rm If rm = "TRUE" then -ve and NA are assigned as 0
#'
#'@details The 'dataframe' to be formatted should have a 'col' and 'row' columns representing the column and rowname of the corresponding multiwell plate.
#'
#'@return A matrix data with row and column names corresponding to multiwell plate
#'
#'@author A.A Palakkan
#'
#'@examples
#'## loading data
#'data(rawdata96, metafile96, metafile384)
#'rawdata<- data2plateformat(rawdata96, platetype = 96)
#'rawdata<- plate2df(rawdata)
#'
#'## eg:1 rawdata to matrix format (column: value)
#'matrix96(rawdata,"value")
#'
#'## eg:2 metafile96 to matrix format (column: id)
#'matrix96(metafile96,"id")
#'
#'## eg:3 metafile384 to matrix format (column: cell)
#'matrix96(metafile384,"cell")
#'
#'@keywords manip
#'
#' @export
#'

# function to view the dataframe as a matrix of appropriate multiwell format
#df=dataframe to be formatted, col =column to be viewed in 96 well format, rm= assing -ve and NA as 0 if "true"


matrix96 <- function(dataframe,column,rm="FALSE"){

if(is.numeric(column)){data<-matrix(dataframe[,column],ncol=length(unique(dataframe[,2])), byrow = TRUE)}

if(!is.numeric(column)){
  name<- as.character(column)
  col<- which(colnames(dataframe)==name)
  data<-matrix(dataframe[,col],ncol=length(unique(dataframe[,2])), byrow = TRUE)
}

rownames(data)<-unique(dataframe$row)
colnames(data)<-unique(dataframe$col)

if(rm==TRUE){
  data[is.na(data)] <- 0
  data[(data<0)]<-0
}

return(data)

}
