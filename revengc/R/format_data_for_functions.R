library(stringr)

#The "column.marginal" function makes a dataframe of the column titles and column marginals
column.marginal<-function(contingencytable){
  # user can read in csv based with header = TRUE or header = FALSE
  # this function will regonize that and act accordingly to get the correct column names
  if (any(na.omit(colnames(contingencytable))=="V1")) {cnames=contingencytable[1,]} else {cnames=colnames(contingencytable)}
  # remove commas from column name
  cnames<-str_replace_all(as.matrix(cnames),",","")
  if(is.na(cnames[1])==FALSE){cnames[1]<-NA}
  cnames[cnames==" NA"] <- NA
  cnames[cnames=="NA "] <- NA
  cnames[cnames==""] <- NA
  # remove na values
  cnames<-na.omit(cnames)
  # get column marginals
  csum<-contingencytable[nrow(contingencytable),]
  # remove na values
  csum[which(csum=="NA")]<-NA
  csum<-na.omit(as.numeric(csum))
  # remove comma from column marginals
  csum<-str_replace_all(as.matrix(csum),",","")
  csum[csum==""] <- NA
  csum<-na.omit(as.numeric(csum))
  if (length(csum)!=length(cnames)) {csum<-csum[-length(csum)]}
  coltable<-data.frame(cnames,csum)
  names(coltable)<-c("Column Category", "Marginal Frequencies")
  return(coltable)
} # end of column_marginal function



#The "row.marginal" function makes a dataframe of the row titles and row marginals
row.marginal<-function (contingencytable){
  #get row censored names
  rnames<-contingencytable[,1]
  rnames[rnames==""] <- NA
  #remove commas from row names
  rnames<-str_replace_all(rnames,",","")
  rnames[rnames==" NA"] <- NA
  rnames[rnames=="NA "] <- NA
  rnames[rnames==""] <- NA
  #remove na values
  rnames<-na.omit(rnames)
  #get row marginals
  rsum<-contingencytable[,ncol(contingencytable)]
  #remove commas from row marginals
  rsum<-str_replace_all(rsum,",","")
  rsum<-str_replace_all(rsum," ","")
  rsum[which(rsum=="NA")]<-NA
  rsum<-na.omit(as.numeric(rsum))
  #removing total of row total if present
  if (length(rsum)!=length(rnames)) {rsum<-rsum[-length(rsum)]}
  rowtable<-data.frame(rnames,rsum)
  names(rowtable)<-c("Row Category", "Marginal Frequencies")
  return(rowtable)
} #end of row_marginal function