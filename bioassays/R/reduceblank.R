#'@name reduceblank
#'
#'@aliases reduceblank
#'
#'@title Reduce Blank Values
#'
#'@description This function can reduce 'blank' value from readings. Can handle separate blanks for separate groups in the dataframe.
#'
#'@usage reduceblank(dataframe, x_vector, blank_vector, y)
#'
#'@param  dataframe Data in the form of dataframe
#'@param x_vector A character vector of groups/entries for which the blank has to be reduced.
#'@param blank_vector A character vector of blank names whose value has to be reduced.
#'@param y Name of the column (column should be numeric in nature) whose values has be reduced.
#'
#'@details This function will reduce the first blank vector element from first x_vector element and so on.
#'
#'@return A dataframe with a new column 'blankminus' (result of the blankminus function) added to the right.
#'
#'@author A.A Palakkan
#'
#'@examples
#'## loading data
#'data(metafile384, rawdata384)
#'rawdata<-plate2df(data2plateformat(rawdata384,platetype = 384))
#'data_DF2<- dplyr::inner_join(rawdata,metafile384,by=c("row","col","position"))
#'
#'## eg:1 reduce blanks of data_DF2.
#'# reduce seperate blanks (mean of blank wells) for drug1, drug2, drug3 and drug4.
#'#blanks are blank1, blank2, blank3 and blank4 respectively for different drug.
#'
#'data_blk<-reduceblank(data_DF2,
#'          x_vector=c("drug1","drug2","drug3","drug4"),
#'          blank_vector = c("blank1","blank2","blank3","blank4"),
#'          "value")
#'
#'@keywords manip
#'
#'@importFrom magrittr %>%
#'@importFrom dplyr filter select
#'@export
#'
#'

reduceblank<-function(dataframe,x_vector,blank_vector,y){

# Initial settings#
rowname <- NULL
zzz <- data.frame(stringsAsFactors= FALSE) ## An empty dataframe to dump result
blkdf<-data.frame(stringsAsFactors= FALSE)
dataframe[,y]<-as.numeric(dataframe[,y])


# if x_vector == "All"#
if(identical(x_vector,"All")){
  dataframe$rowname<-as.numeric(rownames(dataframe))

  for (a in seq_along(blank_vector)) {
    ycolumn<-names(dataframe)[which(dataframe == (blank_vector[a]), arr.ind=TRUE)[, "col"]][1]
    blank<-dataframe %>% dplyr::filter (get(ycolumn) == (blank_vector[a]))
    blkdf<-rbind(blkdf,blank)
  }

  meanblank<-mean(blkdf[,y])
  dataframe$blankminus<-dataframe[,y]-meanblank
  dataframe<-dataframe[order(as.numeric(rownames(dataframe))),,drop=FALSE]
  dataframe<-dplyr::select(dataframe,-rowname)
  return(dataframe)
}


# if x_vector =! "All"#

else{

  for (a in seq_along(x_vector)) {

    xcolumn<-names(dataframe)[which(dataframe == (x_vector[a]), arr.ind=TRUE)[, "col"]][1]
    ycolumn<-names(dataframe)[which(dataframe == (blank_vector[a]), arr.ind=TRUE)[, "col"]][1]

    dataframe$rowname<-as.numeric(rownames(dataframe))
    fdf<-dataframe %>% dplyr::filter (get(xcolumn) == (x_vector[a]))
    blank<-fdf %>% dplyr::filter (get(ycolumn) == (blank_vector[a]))
    meanblank<-mean(blank[,y])

    fdf$blankminus<-fdf[,y]-meanblank
    rownames(fdf)<-fdf$rowname
    zzz<-rbind(zzz,fdf)
  }

  zzz<-zzz[order(as.numeric(rownames(zzz))),,drop=FALSE]
  zzz<-dplyr::select(zzz,-rowname)
  return(zzz)
}

}

