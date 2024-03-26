#'@name dfsummary
#'
#'@aliases dfsummary
#'
#'@title Summarize a Dataframe After Grouping Samples
#'
#'@description This function summarize the dataframe (based on a column). It has additional controls to group samples and to omit variables not needed.
#'
#'@usage dfsummary(dataframe, y, grp_vector, rm_vector, nickname, rm="FALSE", param)
#'
#'@param dataframe data in dataframe format
#'
#'@param y column name whose values has to be summarized (column elements need to be numeric
#'
#'@param grp_vector a character vector of column names whose order indicate the order of grouping.
#'
#'@param rm_vector a character vector of items that need to be omitted before summarizing.
#'
#'@param nickname label name for the entries in output dataframe.
#'
#'@param rm rm = "FALSE" if outliers not to be removed, rm = "TRUE" If outliers to be removed.
#'
#'@param param a vector of parameters for more stringent outlier removal. param has to be entered in the format c(strict, cutoff, n). For details please refer \code{\link{rmodd_summary}}
#'
#'@details This function first remove 'rm_vector' elements from the 'dataframe'. Samples are grouped (each level of a 'grp_vector' element as separate group) and sorted (based on 'grp_vector' elements order). column 'y' is then summarized for each group (please refer \code{\link{rmodd_summary}}: for details.
#'
#'@return A dataframe. First columns are named as grp_vector elements. Followed by a 'label' column (element is 'nickname').This 'label' column will be useful when analyzing multiple plates. Summary statistics of 'y' appear as columns: N (number of samples/group), Mean (average/group), SD (standard deviation/group) and  CV (percentage cv/group)
#'
#'@author A.A Palakkan
#'
#'@examples
#'## loading data
#'data(metafile384, rawdata384)
#'
#'rawdata<-plate2df(data2plateformat(rawdata384,platetype = 384))
#'data_DF2<- dplyr::inner_join(rawdata,metafile384,by=c("row","col","position"))
#'
#'## eg:1 summarising the 'value' after grouping samples and omitting blanks.
#'# grouping order cell, compound, concentration and type.
#'
#'result2 <- dfsummary(data_DF2,y = "value",
#'           grp_vector = c("cell","compound","concentration","type"),
#'           rm_vector = c("blank1","blank2","blank3","blank4"),
#'           nickname = "384well",
#'           rm = "FALSE",param = c(strict="FALSE",cutoff=40,n=12))
#'
#'
#'@keywords arith
#'
#'@importFrom magrittr %>%
#'@importFrom dplyr filter select group_by
#'@export




dfsummary<-function(dataframe,y,grp_vector,rm_vector,nickname,rm="FALSE",param){

position <- NULL
zzz <- data.frame(stringsAsFactors= FALSE) ## An empty dataframe to dump result

## Removing rm_vector ####
filterdata<-dataframe
if("row" %in% colnames(filterdata)){filterdata<-dplyr::select(filterdata, -row)}
if("col" %in% colnames(filterdata)){filterdata<-dplyr::select(filterdata, -col)}
if("position" %in% colnames(filterdata)){filterdata<-dplyr::select(filterdata, -position)}

for (b in seq_along(rm_vector)) {
  ycolumn<-names(filterdata)[which(filterdata == (rm_vector[b]), arr.ind=TRUE)[, "col"]][1]
  if(is.na(ycolumn)){
    }else{filterdata <-filterdata %>% dplyr::filter (get(ycolumn) != (rm_vector[b]))}
}
rownames(filterdata)<-c(seq_along(rownames(filterdata)))

### order of filtering###
var<-c(grp_vector,y)
var<-var[!(duplicated(var))]

##catagorizing data columns #####
filterdata<- filterdata %>% dplyr::group_by(!!(as.name(var[1]))) %>% dplyr::select(var)

n<-ncol(filterdata)
for (q in seq(1,n,1)) {
  if (q==n){
    filterdata[, q] <- filterdata[,q]<-vapply(filterdata[,q],
                                              function(x) as.numeric(x),
                                              numeric(nrow(filterdata)))}
  else{
    filterdata[, q] <- vapply(filterdata[,q],
                              function(x) as.character(x),
                              character(nrow(filterdata)))}
}

colnames(filterdata)<-var
filterdata<-as.data.frame(filterdata)

###

runs<-c(seq(1,ncol(filterdata)-1,1))
exp<-""
ord<-""
cyc<-list("NA")

# grid layout of filter strategy ###
for (j in seq_along(runs)) {
  assign(paste0("F",runs[j]),unique(filterdata[,j]))
  cyc<-c(cyc,list(eval(print(as.symbol(paste0("F",j))))))
  exp<-paste0(exp,",",colnames(filterdata)[j],"=","F",j)
  ord<-paste0(ord, "," ,"grid$",colnames(filterdata)[j])
}

exp<-sub(",","",exp)
ord<-sub(",","",ord)
cyc<-cyc[-1]

grid<- eval(parse(text = paste("expand.grid", "(",exp, ")")))
grid<-eval(parse(text= paste("grid","[","order","(",ord,")",",","]")))
grid<-as.data.frame(grid)
rownames(grid)<-c(seq_along(rownames(grid)))
grid[] <- lapply(grid, as.character)

## params for strict summerizing #
df1<-data.frame()
vectorfilters<-colnames(grid)
strict<-as.character(param["strict"])
cutoff<-as.integer(param["cutoff"])
n<- as.integer(param["n"])

#### Filtering and summerizing ###
for (gr in seq_along(rownames(grid))) {
  df1<-filterdata
  vector2<-grid[gr,]

  for (i in seq_along(vector2)) {
    df1<-df1 %>% dplyr::filter (get(vectorfilters[i])==as.character(vector2[i]))
  }

  vv<-df1[,y]
  if(rm=="FALSE"){cal<- rmodd_summary(vv, rm = "FALSE")}
  if(rm=="TRUE"){ cal<- rmodd_summary(vv, rm = "TRUE", strict, cutoff,n) }

  calculations<-data.frame(label=nickname,N=cal[3],
                           Mean=round(cal[1],3),
                           SD=round(cal[4],3),
                           CV=round(abs(cal[5]),2))

  rownames(calculations)<-gr

  calculations<-cbind(grid[gr,],calculations)
  zzz = rbind(zzz,calculations)
}

return (zzz)

}

