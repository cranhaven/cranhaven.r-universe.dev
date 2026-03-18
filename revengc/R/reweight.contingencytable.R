library(stringr)
library(dplyr)
reweight.contingencytable<-function(observed.table, estimated.table){

  
  Xlowerbound = min(as.numeric(row.names(estimated.table)))
  Xupperbound = max(as.numeric(row.names(estimated.table)))
  Ylowerbound = min(as.numeric(colnames(estimated.table)))
  Yupperbound = max(as.numeric(colnames(estimated.table)))
  
# ################################################################################## start functions that help format data first
  
#The findtypeofcensoring_univariatetable function regonizes the symbols and divides a frequency table up into the 6 censored cases
findtypeofcensoring_univariatetable<-function (univariatefreqtable){
  
  #making a dataframe from the category case (column one)
  cases<-data.frame(univariatefreqtable)
  cases<-cases[1]
  #relabeled column with censoring as Censoring_Symbol
  names(cases) <- c("Censoring_Symbol")
  
  # changed lowercase up uppercase if users input "l, le, i, g, ge"
  cases$Censoring_Symbol<-toupper(as.matrix(cases$Censoring_Symbol))
  
  #will list censoring type in column called Type_of_Censoring
  cases$Type_of_Censoring <- ""
  
  #find what numbers go with censoring and put it in censor_number column
  cases$Censor_Number<-str_replace_all(cases$Censoring_Symbol, "[-,<,<=,>,>=,+,\\,]", " ")
  cases$Censor_Number<-str_replace_all(cases$Censor_Number, "[L, LE, I, G, GE]", " ")
  cases$Censor_Number<-str_replace_all(cases$Censor_Number, "[l, le, i, g, ge]", " ")
  
  # remove numbers from censoring_symbol column
  cases$Censoring_Symbol<-gsub('[0-9]+', '', cases$Censoring_Symbol)
  cases$Censoring_Symbol<-gsub('\\.', '', cases$Censoring_Symbol)
  
  #named the symbols allowed in model
  L1 <- c("<")
  L1a <- c("L")
  L2 <- c("<=")
  L2a <- c("LE")
  I <- c("-")
  Ia <- c("I")
  G1 <- c(">")
  G1a <- c("G")
  G2 <- c(">=")
  G2a<-c("[+]")
  G2b<-c("GE")
  
  # remove any spaces found in symbol column
  cases$Censoring_Symbol<-gsub('\\s+', '', cases$Censoring_Symbol)
  
  #look to see if these symbols are in the provided table
  leftcensorcase1<-grep("^<$", cases$Censoring_Symbol)
  leftcensorcase1a<-grep("^L$", cases$Censoring_Symbol)
  
  leftcensorcase2<-grep("^<=$", cases$Censoring_Symbol)
  leftcensorcase2a<-grep("^LE$", cases$Censoring_Symbol)
  
  intervalcensor<-grep("^[-]$", cases$Censoring_Symbol)
  intervalcensora<-grep("^I$", cases$Censoring_Symbol)
  
  rightcensorcase1<-grep("^>$", cases$Censoring_Symbol)
  rightcensorcase1a<-grep("^G$", cases$Censoring_Symbol)
  
  rightcensorcase2<-grep("^>=$", cases$Censoring_Symbol)
  rightcensorcase2a<-grep("^[+]$", cases$Censoring_Symbol)
  rightcensorcase2b<-grep("^GE$", cases$Censoring_Symbol)
  
  
  #if the cases were there then this puts an L1, L2, I, G1, G2, or U in column called Type_of_Censoring
  if (length(leftcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, L1)),"Type_of_Censoring"] <- "L1"}
  if (length(leftcensorcase1a)>0){cases[which (str_detect(cases$Censoring_Symbol, L1a)),"Type_of_Censoring"] <- "L1a"}
  
  if (length(leftcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, L2)),"Type_of_Censoring"] <- "L2"}
  if (length(leftcensorcase2a)>0){cases[which (str_detect(cases$Censoring_Symbol, L2a)),"Type_of_Censoring"] <- "L2a"}
  
  if (length(intervalcensor)>0){cases[which (str_detect(cases$Censoring_Symbol, I)),"Type_of_Censoring"] <- "I"}
  if (length(intervalcensora)>0){cases[which (str_detect(cases$Censoring_Symbol, Ia)),"Type_of_Censoring"] <- "Ia"}
  
  if (length(rightcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, G1)),"Type_of_Censoring"] <- "G1"}
  if (length(rightcensorcase1a)>0){cases[which (str_detect(cases$Censoring_Symbol, G1a)),"Type_of_Censoring"] <- "G1a"}
  
  if (length(rightcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, G2)),"Type_of_Censoring"] <- "G2"}
  if (length(rightcensorcase2a)>0){cases[which (str_detect(cases$Censoring_Symbol, G2a)),"Type_of_Censoring"] <- "G2a"}
  if (length(rightcensorcase2b)>0){cases[which (str_detect(cases$Censoring_Symbol, G2b)),"Type_of_Censoring"] <- "G2b"}
  
  
  #make all the different labels of censoring consistent
  cases$Type_of_Censoring[cases$Type_of_Censoring =="L1a"]<-c("L1")
  cases$Type_of_Censoring[cases$Type_of_Censoring =="L2a"]<-c("L2")
  cases$Type_of_Censoring[cases$Type_of_Censoring =="Ia"]<-c("I")
  cases$Type_of_Censoring[cases$Type_of_Censoring =="G1a"]<-c("G1")
  cases$Type_of_Censoring[cases$Type_of_Censoring =="G2a"]<-c("G2")
  cases$Type_of_Censoring[cases$Type_of_Censoring =="G2b"]<-c("G2")
  
  #listing the no censoring cases as "U"
  cases$Type_of_Censoring[cases$Type_of_Censoring==""]<-"U"
  
  #put in errors to let users know that there can't be duplicates of Greater than or Less than
  if(length(grep("G1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 greater than (> or G) category')
  if(length(grep("G2", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 greater than or equal to (>= or GE) category')
  if(length(grep("L2", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than or equal to (<= or LE) category')
  if(length(grep("L1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than (< or L) category')
  if(length(grep("L1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than (< or L) category')
  if(length(grep("L1", cases$Type_of_Censoring))==1 && length(grep("L2", cases$Type_of_Censoring))==1) stop ('Censored table can not  have both a less than (< or L) category and a less than or equal to category (<= or LE)')
  if(length(grep("G1", cases$Type_of_Censoring))==1 && length(grep("G2", cases$Type_of_Censoring))==1) stop ('Censored table can not have both a greater than (< or G) category and a greater than or equal to category (<= or GE)')
  
  return (cases)} #end findtypeofcensoring_univariatetable function

#####
#####

#The fixdata_univariatecase function regonizes the symbols and divides a frequency table up into the 6 censored cases
#This is a formatting data function
#This uses the findtypeofcensoring_univariatetable function

fixdata_univariatecase<-function(univariatefreqtable) {
  
  # conduct findtypeofcensoring_univariatetable function on data
  cases<-findtypeofcensoring_univariatetable(data.frame(univariatefreqtable))
  
  #gives a row number for each of the 6 censoring types
  lo1<-as.numeric(which(cases$Type_of_Censoring=='L1'))
  lo2<-as.numeric(which(cases$Type_of_Censoring=='L2'))
  inte<-as.numeric(which(cases$Type_of_Censoring=='I'))
  gre1<-as.numeric(which(cases$Type_of_Censoring=='G1'))
  gre2<-as.numeric(which(cases$Type_of_Censoring=='G2'))
  ex<-as.numeric(which(cases$Type_of_Censoring=='U'))
  
  #finding what number corresponds to the freqency value based off type of censoring
  #put as.numeric and as.character to help with format
  #R was changing decimals to whole numbers without putting as.numeric and as.character
  countl1<-as.numeric(as.character(univariatefreqtable[lo1,2]))
  countl2<-as.numeric(as.character(univariatefreqtable[lo2,2]))
  counti<-as.numeric(as.character(univariatefreqtable[inte,2]))
  countg1<-as.numeric(as.character(univariatefreqtable[gre1,2]))
  countg2<-as.numeric(as.character(univariatefreqtable[gre2,2]))
  counte<-as.numeric(as.character(univariatefreqtable[ex,2]))
  
  #finding what number (in the Censor_Number) corresponds to the censor type
  #values listed in cases$Censor_Number[] are factors
  #intervalnumber will be as.vector because will have to run strsplit function on this value later
  lowernumber1<-as.numeric(as.character(cases$Censor_Number[lo1]))
  lowernumber2<-as.numeric(as.character(cases$Censor_Number[lo2]))
  intervalnumber<-as.vector(cases$Censor_Number[inte])
  greaternumber1<-as.numeric(as.character(cases$Censor_Number[gre1]))
  greaternumber2<-as.numeric(as.character(cases$Censor_Number[gre2]))
  exactnumber<-as.numeric(as.character(cases$Censor_Number[ex]))
  
  #combining the category number (without its symbol) and the freqency number that corresponds to that category number
  lower1<-t(c(lowernumber1,countl1))
  lower2<-t(c(lowernumber2,countl2))
  greater1<-t(c(greaternumber1,countg1))
  greater2<-t(c(greaternumber2,countg2))
  #unlike the left and right censor categories there could be multiple of the no censored category
  exact<-unname(rbind(exactnumber,counte))
  
  #spliting the interval(s)
  if (length(intervalnumber)>0){
    spl<-na.omit(as.numeric(unlist(strsplit(intervalnumber,' ', fixed=FALSE))))
    interval<-matrix(spl,length(intervalnumber),2,byrow=TRUE)
    interval<-unname(cbind(interval,counti))
  } else {interval=NULL}
  
  # testing to see if intervals are closed..
  if (length(interval)!=0){
    intervalclosed<-as.vector(interval[,1:2]) 
    if (any(duplicated(intervalclosed))==TRUE) stop (paste(intervalclosed[which(duplicated(intervalclosed)==TRUE)],
                                                           'is repeated in different - or I categories and this is not allowed (i.e. need closed intervals)'))
  }
  
  
  #return values for later use
  final<-list(leftcensored1=lower1,leftcensored2=lower2, nocensored=exact,
              rightcensored1=greater1,
              rightcensored2=greater2, intervalcensored=interval
              # lowerbound = lowerbound,
              # upperbound=upperbound
  )
  
  #replacing any negative numbers with 0
  #there might be negative numbers, but these negative values will cause error in the likelihood function
  final<-rapply(final,function(x) ifelse(x<0,0,x), how = "replace")
  
  # add errors
  if(length(final$leftcensored2)!= 0 && length(final$intervalcensored) != 0 &&
     final$leftcensored2[1,1] == final$intervalcensored[1,1]) stop ('Censored table can not have the same number in both the <= or LE category and the - or I category (i.e. need closed intervals)')
  
  if(length(final$rightcensored2)!= 0 && length(final$intervalcensored) != 0 &&
     final$rightcensored2[1,1] == final$intervalcensored[nrow(final$intervalcensored),2]) stop ('Censored table can not have the same number in both the >= or GE or + category and the - or I category (i.e. need closed intervals)')
  
  return(final)
} #end fixdata_univariatecase function


# ################################################################################## end functions that help format data first


# looking at provided table
# griding the row and col names 
rnames<-findtypeofcensoring_univariatetable(row.marginal(observed.table))$Type_of_Censoring
cnames<-findtypeofcensoring_univariatetable(column.marginal(observed.table))$Type_of_Censoring
# tn = table names
tn_expaned<-expand.grid(rnames, cnames)

# removing marginals to get to inside of table
# br = bottom row, tr = top row, rc = right column, and lc = left column
brgone<-observed.table[-nrow(observed.table),]
# for removing the top row we have to also accounte for if user read in csv with header=TRUE or header=FALSE
trgone=unname(brgone)
rcgone<-trgone[, -ncol(trgone)]
lcgone<-rcgone[,-1]
Inside<-matrix(as.matrix(lcgone), dim(data.frame(lcgone))*dim(data.frame(lcgone))[2], 1)
#r emoving commas from inside of table
inside<-str_replace_all(Inside,",","")

suminside<-sum(as.numeric(inside))

# make probabilities 
if (suminside != 1) {
  inside<-as.numeric(inside)/sum(as.numeric(inside))
  }
  

rowsum<-sum(row.marginal(observed.table)$`Marginal Frequencies`)
colsum<-sum(column.marginal(observed.table)$`Marginal Frequencies`)


if(isFALSE(all.equal(rowsum,colsum)) ) stop ('Margins are not equal.')
if(isFALSE(all.equal(rowsum,suminside))) stop ('Row margins are not equal to inside of table.')
if(isFALSE(all.equal(colsum,suminside))) stop ('Column margins are not equal to inside of table.')

# as.numeric(inside)/sum(as.numeric(inside))
# put inside table with corresponding rows and cols
tn_expaned$repvar<-inside
names(tn_expaned)<-c("row", "col", "inside")
tn_expaned$rowcol<-paste(tn_expaned$row, tn_expaned$col, sep = "_")
# final inside of table with censoring symbols
tn_expaned<-tn_expaned[3:4]

# find numbers for each name
rnumb<-findtypeofcensoring_univariatetable(row.marginal(observed.table))$Censor_Number
cnumb<-findtypeofcensoring_univariatetable(column.marginal(observed.table))$Censor_Number
# remove spaces from the numbers
rnumb<-gsub(" ", "", rnumb) 
cnumb<-gsub(" ", "", cnumb) 
# expand grid for numbers just like did for censored symbols
rowandcolnumb<-expand.grid(rnumb, cnumb)

# final inside of table with censoring symbols and numbers
tn_expaned<-cbind(tn_expaned, "Numbers" = paste(rowandcolnumb$Var1, rowandcolnumb$Var2, sep = "_"))

# create merge column for later..
tn_expaned<-cbind(tn_expaned, "Censoring_Numbers" = paste(tn_expaned$rowcol, tn_expaned$Numbers, sep = "_"))[-c(2,3)]

# run function to know cases of the provided table
repx<-fixdata_univariatecase(row.marginal(observed.table))
repy<-fixdata_univariatecase(column.marginal(observed.table))

# uncensored table names expaned grid
tn_expand_new<-expand.grid(Xlowerbound:Xupperbound, Ylowerbound:Yupperbound)

# function to give censoring symbols/pasted numbers to tn_expand_new
##########
censoringtypeforunobserved.table<-function (tn_expand_new,repx,repy ){
  
  # get rid of count values in repx
  if(length(repx$leftcensored1)!=0) {repx$leftcensored1<-repx$leftcensored1[1,1]} 
  if(length(repx$leftcensored2)!=0) {repx$leftcensored2<-repx$leftcensored2[1,1]}
  if(length(repx$intervalcensored)!=0) {repx$intervalcensored<-repx$intervalcensored[,c(1,2)]}
  if(length(repx$rightcensored1)!=0) {repx$rightcensored1<-repx$rightcensored1[1,1]}
  if(length(repx$rightcensored2)!=0) {repx$rightcensored2<-repx$rightcensored2[1,1]}
  if(length(repx$nocensored)!=0) {repx$nocensored<-repx$nocensored[1,]}
  
  # get rid of count values in repy
  if(length(repy$leftcensored1)!=0) {repy$leftcensored1<-repy$leftcensored1[1,1]} 
  if(length(repy$leftcensored2)!=0) {repy$leftcensored2<-repy$leftcensored2[1,1]}
  if(length(repy$intervalcensored)!=0) {repy$intervalcensored<-repy$intervalcensored[,c(1,2)]}
  if(length(repy$rightcensored1)!=0) {repy$rightcensored1<-repy$rightcensored1[1,1]}
  if(length(repy$rightcensored2)!=0) {repy$rightcensored2<-repy$rightcensored2[1,1]}
  if(length(repy$nocensored)!=0) {repy$nocensored<-repy$nocensored[1,]}
  
  
  # function for to find what uncensored number falls between inteval values
  findinterval<-function (x, y){
    if (is.null(nrow(y))) {y=paste0(y[1], y[2])} else {
      result = NULL
      for (i in 1:nrow(y)){
        if(x>=y[i,1] && x <=y[i,2]) {result[i]<-c("yes")} else {result[i]<-c("no")}  
      }
      y = y[which(result=="yes"),]
      y = paste0(y[1], y[2])
    } # end else statement
    return(y)
  } 
  
  ### going through the 36 cases.. 
  # case where row x has L1    
  if (length(repx$leftcensored1) != 0){
    
    if (length(repy$leftcensored1)!=0){
      if(tn_expand_new$Var1<repx$leftcensored1 && tn_expand_new$Var2<repy$leftcensored1) {
        tn_expand_new$rowcol<-c("L1_L1")
        tn_expand_new$Numbers<-paste(repx$leftcensored1, repy$leftcensored1, sep = "_")
      } }
    
    if (length(repy$leftcensored2)!=0){
      if(tn_expand_new$Var1<repx$leftcensored1 && tn_expand_new$Var2<=repy$leftcensored2) {
        tn_expand_new$rowcol<-c("L1_L2")
        tn_expand_new$Numbers<-paste(repx$leftcensored1, repy$leftcensored2, sep = "_")
      } }
    
    if (length(repy$intervalcensored)!=0){
      if(tn_expand_new$Var1<repx$leftcensored1 && tn_expand_new$Var2>=min(repy$intervalcensored) && tn_expand_new$Var2<=max(repy$intervalcensored)) {
        tn_expand_new$rowcol<-c("L1_I")
        tn_expand_new$Numbers<-paste(repx$leftcensored1, findinterval(tn_expand_new$Var2, repy$intervalcensored), sep = "_")
      } }
    
    if (length(repy$rightcensored1)!=0){
      if(tn_expand_new$Var1<repx$rightcensored1 && tn_expand_new$Var2>repy$rightcensored1) {
        tn_expand_new$rowcol<-c("L1_G1")
        tn_expand_new$Numbers<-paste(repx$leftcensored1, repy$rightcensored1, sep = "_")
      } }
    
    if (length(repy$rightcensored2)!=0){
      if(tn_expand_new$Var1<repx$leftcensored1 && tn_expand_new$Var2>=repy$rightcensored2) {
        tn_expand_new$rowcol<-c("L1_G2")
        tn_expand_new$Numbers<-paste(repx$leftcensored1, repy$rightcensored2, sep = "_")
      } }
    
    if (length(repy$nocensored)!=0){
      if(tn_expand_new$Var1<repx$leftcensored1 && any(tn_expand_new$Var2==repy$nocensored)) {
        tn_expand_new$rowcol<-c("L1_U")
        matchingvalue<-which(tn_expand_new$Var2==repy$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$leftcensored1, repy$nocensored[matchingvalue], sep = "_")
      } }      
  } # end case where row x has L1
  
  #############################
  # case where row x has L2    
  if (length(repx$leftcensored2) != 0){
    
    if (length(repy$leftcensored1)!=0){
      if(tn_expand_new$Var1<=repx$leftcensored2 && tn_expand_new$Var2<repy$leftcensored1) {
        tn_expand_new$rowcol<-c("L2_L1")
        tn_expand_new$Numbers<-paste(repx$leftcensored2, repy$leftcensored1, sep = "_")
      } }
    
    if (length(repy$leftcensored2)!=0){
      if(tn_expand_new$Var1<=repx$leftcensored2 && tn_expand_new$Var2<=repy$leftcensored2) {
        tn_expand_new$rowcol<-c("L2_L2")
        tn_expand_new$Numbers<-paste(repx$leftcensored2, repy$leftcensored2, sep = "_")
      } }
    
    if (length(repy$intervalcensored)!=0){
      if(tn_expand_new$Var1<=repx$leftcensored2 && tn_expand_new$Var2>=min(repy$intervalcensored) && tn_expand_new$Var2<=max(repy$intervalcensored)) {
        tn_expand_new$rowcol<-c("L2_I")
        tn_expand_new$Numbers<-paste(repx$leftcensored2, findinterval(tn_expand_new$Var2, repy$intervalcensored), sep = "_")
      } }
    
    if (length(repy$rightcensored1)!=0){
      if(tn_expand_new$Var1<=repx$rightcensored1 && tn_expand_new$Var2>repy$rightcensored1) {
        tn_expand_new$rowcol<-c("L2_G1")
        tn_expand_new$Numbers<-paste(repx$leftcensored2, repy$rightcensored1, sep = "_")
      } }
    
    if (length(repy$rightcensored2)!=0){
      if(tn_expand_new$Var1<=repx$leftcensored2 && tn_expand_new$Var2>=repy$rightcensored2) {
        tn_expand_new$rowcol<-c("L2_G2")
        tn_expand_new$Numbers<-paste(repx$leftcensored2, repy$rightcensored2, sep = "_")
      } }
    
    if (length(repy$nocensored)!=0){
      if(tn_expand_new$Var1<=repx$leftcensored2 && any(tn_expand_new$Var2==repy$nocensored)) {
        tn_expand_new$rowcol<-c("L2_U")
        matchingvalue<-which(tn_expand_new$Var2==repy$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$leftcensored2, repy$nocensored[matchingvalue], sep = "_")
      } }      
  } # end case where row x has L2
  #############################
  # case where row x has G1    
  if (length(repx$rightcensored1) != 0){
    
    if (length(repy$leftcensored1)!=0){
      if(tn_expand_new$Var1>repx$rightcensored1 && tn_expand_new$Var2<repy$leftcensored1) {
        tn_expand_new$rowcol<-c("G1_L1")
        tn_expand_new$Numbers<-paste(repx$rightcensored1, repy$leftcensored1, sep = "_")
      } }
    
    if (length(repy$leftcensored2)!=0){
      if(tn_expand_new$Var1>repx$rightcensored1 && tn_expand_new$Var2<=repy$leftcensored2) {
        tn_expand_new$rowcol<-c("G1_L2")
        tn_expand_new$Numbers<-paste(repx$rightcensored1, repy$leftcensored2, sep = "_")
      } }
    
    if (length(repy$intervalcensored)!=0){
      if(tn_expand_new$Var1>repx$rightcensored1 && tn_expand_new$Var2>=min(repy$intervalcensored) && tn_expand_new$Var2<=max(repy$intervalcensored)) {
        tn_expand_new$rowcol<-c("G1_I")
        tn_expand_new$Numbers<-paste(repx$rightcensored1, findinterval(tn_expand_new$Var2, repy$intervalcensored), sep = "_")
      } }
    
    if (length(repy$rightcensored1)!=0){
      if(tn_expand_new$Var1>repx$rightcensored1 && tn_expand_new$Var2>repy$rightcensored1) {
        tn_expand_new$rowcol<-c("G1_G1")
        tn_expand_new$Numbers<-paste(repx$rightcensored1, repy$rightcensored1, sep = "_")
      } }
    
    if (length(repy$rightcensored2)!=0){
      if(tn_expand_new$Var1>repx$rightcensored1 && tn_expand_new$Var2>=repy$rightcensored2) {
        tn_expand_new$rowcol<-c("G1_G2")
        tn_expand_new$Numbers<-paste(repx$rightcensored1, repy$rightcensored2, sep = "_")
      } }
    
    if (length(repy$nocensored)!=0){
      if(tn_expand_new$Var1>repx$rightcensored1 && any(tn_expand_new$Var2==repy$nocensored)) {
        tn_expand_new$rowcol<-c("G1_U")
        matchingvalue<-which(tn_expand_new$Var2==repy$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$rightcensored1, repy$nocensored[matchingvalue], sep = "_")
      } }      
  } # end case where row x has G1
  
  #############################
  # case where row x has G2    
  if (length(repx$rightcensored2) != 0){
    
    if (length(repy$leftcensored1)!=0){
      if(tn_expand_new$Var1>=repx$rightcensored2 && tn_expand_new$Var2<repy$leftcensored1) {
        tn_expand_new$rowcol<-c("G2_L1")
        tn_expand_new$Numbers<-paste(repx$rightcensored2, repy$leftcensored1, sep = "_")
      } }
    
    if (length(repy$leftcensored2)!=0){
      if(tn_expand_new$Var1>=repx$rightcensored2 && tn_expand_new$Var2<=repy$leftcensored2) {
        tn_expand_new$rowcol<-c("G2_L2")
        tn_expand_new$Numbers<-paste(repx$rightcensored2, repy$leftcensored2, sep = "_")
      } }
    
    if (length(repy$intervalcensored)!=0){
      if(tn_expand_new$Var1>=repx$rightcensored2 && tn_expand_new$Var2>=min(repy$intervalcensored) && tn_expand_new$Var2<=max(repy$intervalcensored)) {
        tn_expand_new$rowcol<-c("G2_I")
        tn_expand_new$Numbers<-paste(repx$rightcensored2, findinterval(tn_expand_new$Var2, repy$intervalcensored), sep = "_")
      } }
    
    if (length(repy$rightcensored1)!=0){
      if(tn_expand_new$Var1>=repx$rightcensored2 && tn_expand_new$Var2>repy$rightcensored1) {
        tn_expand_new$rowcol<-c("G2_G1")
        tn_expand_new$Numbers<-paste(repx$rightcensored2, repy$rightcensored1, sep = "_")
      } }
    
    if (length(repy$rightcensored2)!=0){
      if(tn_expand_new$Var1>=repx$rightcensored2 && tn_expand_new$Var2>=repy$rightcensored2) {
        tn_expand_new$rowcol<-c("G2_G2")
        tn_expand_new$Numbers<-paste(repx$rightcensored2, repy$rightcensored2, sep = "_")
      } }
    
    if (length(repy$nocensored)!=0){
      if(tn_expand_new$Var1>=repx$rightcensored2 && any(tn_expand_new$Var2==repy$nocensored)) {
        tn_expand_new$rowcol<-c("G2_U")
        matchingvalue<-which(tn_expand_new$Var2==repy$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$rightcensored2, repy$nocensored[matchingvalue], sep = "_")
      } }      
  } # end case where row x has G2
  #############################
  
  # case where row x has U    
  if (length(repx$nocensored) != 0){
    
    if (length(repy$leftcensored1)!=0){
      if(any(tn_expand_new$Var1==repx$nocensored) && tn_expand_new$Var2<repy$leftcensored1) {
        tn_expand_new$rowcol<-c("U_L1")
        matchingvalue<-which(tn_expand_new$Var1==repx$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$nocensored[matchingvalue], repy$leftcensored1, sep = "_")
      } }
    
    if (length(repy$leftcensored2)!=0){
      if(any(tn_expand_new$Var1==repx$nocensored) && tn_expand_new$Var2<=repy$leftcensored2) {
        tn_expand_new$rowcol<-c("U_L2")
        matchingvalue<-which(tn_expand_new$Var1==repx$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$nocensored[matchingvalue], repy$leftcensored2, sep = "_")
      } }
    
    if (length(repy$intervalcensored)!=0){
      if(any(tn_expand_new$Var1==repx$nocensored) && tn_expand_new$Var2>=min(repy$intervalcensored) && tn_expand_new$Var2<=max(repy$intervalcensored)) {
        tn_expand_new$rowcol<-c("U_I")
        matchingvalue<-which(tn_expand_new$Var1==repx$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$nocensored[matchingvalue], findinterval(tn_expand_new$Var2, repy$intervalcensored), sep = "_")
      } }
    
    if (length(repy$rightcensored1)!=0){
      if(any(tn_expand_new$Var1==repx$nocensored) && tn_expand_new$Var2>repy$rightcensored1) {
        tn_expand_new$rowcol<-c("U_G1")
        matchingvalue<-which(tn_expand_new$Var1==repx$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$nocensored[matchingvalue], repy$rightcensored1, sep = "_")
      } }
    
    if (length(repy$rightcensored2)!=0){
      if(any(tn_expand_new$Var1==repx$nocensored) && tn_expand_new$Var2>=repy$rightcensored2) {
        tn_expand_new$rowcol<-c("U_G2")
        matchingvalue<-which(tn_expand_new$Var1==repx$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$nocensored[matchingvalue], repy$rightcensored2, sep = "_")
      } }
    
    if (length(repy$nocensored)!=0){
      if(any(tn_expand_new$Var1==repx$nocensored) && any(tn_expand_new$Var2==repy$nocensored)) {
        tn_expand_new$rowcol<-c("U_U")
        matchingvalue1<-which(tn_expand_new$Var1==repx$nocensored, arr.ind=TRUE)
        matchingvalue2<-which(tn_expand_new$Var2==repy$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(repx$nocensored[matchingvalue1], repy$nocensored[matchingvalue2], sep = "_")
      } }         
  } # end case where row x has U  
  #############################
  
  # case where row x has I    
  if (length(repx$intervalcensored) != 0){
    
    if (length(repy$leftcensored1)!=0){
      if(tn_expand_new$Var1>=min(repx$intervalcensored) && tn_expand_new$Var1<=max(repx$intervalcensored) && tn_expand_new$Var2<repy$leftcensored1) {
        tn_expand_new$rowcol<-c("I_L1")
        tn_expand_new$Numbers<-paste(findinterval(tn_expand_new$Var1, repx$intervalcensored), repy$leftcensored1, sep = "_")
      } }
    
    if (length(repy$leftcensored2)!=0){
      if(tn_expand_new$Var1>=min(repx$intervalcensored) && tn_expand_new$Var1<=max(repx$intervalcensored) && tn_expand_new$Var2<=repy$leftcensored2) {
        tn_expand_new$rowcol<-c("I_L2")
        tn_expand_new$Numbers<-paste(findinterval(tn_expand_new$Var1, repx$intervalcensored), repy$leftcensored2, sep = "_")
      } }
    
    if (length(repy$intervalcensored)!=0){
      if(tn_expand_new$Var1>=min(repx$intervalcensored) && tn_expand_new$Var1<=max(repx$intervalcensored) && tn_expand_new$Var2>=min(repy$intervalcensored) && tn_expand_new$Var2<=max(repy$intervalcensored)) {
        tn_expand_new$rowcol<-c("I_I")
        tn_expand_new$Numbers<-paste(findinterval(tn_expand_new$Var1, repx$intervalcensored), findinterval(tn_expand_new$Var2, repy$intervalcensored), sep = "_")
      } }
    
    if (length(repy$rightcensored1)!=0){
      if(tn_expand_new$Var1>=min(repx$intervalcensored) && tn_expand_new$Var1<=max(repx$intervalcensored) && tn_expand_new$Var2>repy$rightcensored1) {
        tn_expand_new$rowcol<-c("I_G1")
        tn_expand_new$Numbers<-paste(findinterval(tn_expand_new$Var1, repx$intervalcensored), repy$rightcensored1, sep = "_")
      } }
    
    if (length(repy$rightcensored2)!=0){
      if(tn_expand_new$Var1>=min(repx$intervalcensored) && tn_expand_new$Var1<=max(repx$intervalcensored) && tn_expand_new$Var2>=repy$rightcensored2) {
        tn_expand_new$rowcol<-c("I_G2")
        tn_expand_new$Numbers<-paste(findinterval(tn_expand_new$Var1, repx$intervalcensored), repy$rightcensored2, sep = "_")
      } }
    
    if (length(repy$nocensored)!=0){
      if(tn_expand_new$Var1>=min(repx$intervalcensored) && tn_expand_new$Var1<=max(repx$intervalcensored)  && any(tn_expand_new$Var2==repy$nocensored)) {
        tn_expand_new$rowcol<-c("I_U")
        matchingvalue<-which(tn_expand_new$Var2==repy$nocensored, arr.ind=TRUE)
        tn_expand_new$Numbers<-paste(findinterval(tn_expand_new$Var1, repx$intervalcensored), repy$nocensored[matchingvalue], sep = "_")
      } }         
    
  } # end case where row x has I    
  
  ### end 36 cases.. 
  # checking if user didn't censor correctly
  if(any(tn_expand_new$rowcol=="") ==TRUE) stop('Check Censoring.')
  
  # make merge column
  tn_expand_new$Censoring_Numbers<-paste(tn_expand_new$rowcol, tn_expand_new$Numbers, sep = "_")
  tn_expand_new<-tn_expand_new[, c("Var1", "Var2", "Censoring_Numbers")]
  # return main function 
  return(tn_expand_new)}
# end function to give censoring symbols/pasted numbers to tn_expand_new
##########


# run function over all tn_expand_new
final= NULL
for (i in 1:nrow(tn_expand_new)){
  final[[i]]<-censoringtypeforunobserved.table(tn_expand_new=tn_expand_new[i,],repx=repx, repy = repy)
}

# dataframe with censoring symbols and numbers for all uncensored variables
final<-data.frame(matrix(unlist(final), nrow(tn_expand_new), 3, byrow =TRUE))
names(final)<-c("row", "col", "Censoring_Numbers")

# so inner_join doesn't give error
tn_expaned$Censoring_Numbers<-as.character(tn_expaned$Censoring_Numbers)
final$Censoring_Numbers<-as.character(final$Censoring_Numbers)

# merge dataframe with censoring symbols and numbers for all uncensored variables (final) with tn_expanded so can get inside of table values
insidefortable<-inner_join(final, tn_expaned, by= c("Censoring_Numbers"))

# need to replace row and col values that start at 1:length of rows/cols to fill in table properly 
rowreplace<-data.frame("row" = Xlowerbound:Xupperbound, "Wantr" = 1:(length(Xlowerbound:Xupperbound)))
colreplace<-data.frame("col" = Ylowerbound:Yupperbound, "Wantc" = 1:(length(Ylowerbound:Yupperbound)))

# mergining the new values to old 
# inner_join has to have same class
insidefortable$row<-as.numeric(as.character(insidefortable$row))
rowreplacetable<-inner_join(insidefortable, rowreplace, by = c("row"))

rowreplacetable$col<-as.numeric(as.character(rowreplacetable$col))
colreplacetable<-inner_join(rowreplacetable, colreplace, by = c("col"))
# rename final join table 
insidefortable<-colreplacetable

# junk
# insidefortable<-insidefortable[,c("Censoring_Numbers","Wantr", "Wantc", "inside")]
# # making sure if duplicates of interior cells then fix it split by value to random 
# splitbytable<-data.frame("splitby" = 1:length(inside),"inside" =as.character(inside))
# splitbytable$inside<-as.character(splitbytable$inside)
# insidefortablefinal<-merge(splitbytable, insidefortable, by.x= c("inside"), by.y = ("inside"))

# split by nesoring number 
out <- split(insidefortable , insidefortable$Censoring_Numbers )


# FINAL X IS OUTPUTFROM MIPFP
# function that reweights it 
rw<-function(x, out, estimated.table){
uncensored_chunk<-estimated.table[unique(out[[x]]$Wantr), unique(out[[x]]$Wantc)]
sum_uncensored_chunk<-sum(uncensored_chunk) 
censoredvalue<-as.numeric(unique(out[[x]]$inside))

reweighted<-(uncensored_chunk*censoredvalue)/sum_uncensored_chunk
# replace 0/0 or nan with 0 
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
reweighted[is.nan.data.frame(reweighted)] = 0
rewerightedfinal<-data.frame(expand.grid(unique(out[[x]]$Wantr), unique(out[[x]]$Wantc)),unlist(as.list(reweighted)))
names(rewerightedfinal)<-c("Wantr", "Wantc", "inside")
# print(cbind(x, sum(rewerightedfinal$inside)))
return(rewerightedfinal)
}

recoutput<-NULL
for(i in 1:length(out)){
  recoutput[[i]]<-rw(i,out, estimated.table )
  # print(sum(recoutput[[i]]))
}


recoutput_rbind <- do.call(rbind, recoutput)
names(recoutput_rbind)<-c("Wantr", "Wantc", "inside")
# create empty final matrix
finalouput<-matrix(as.numeric(""), length(Xlowerbound:Xupperbound) , length(Ylowerbound:Yupperbound))

# fill seed
for (i in 1:nrow(recoutput_rbind)) {
  finalouput[recoutput_rbind[i,1], recoutput_rbind[i,2]]<-as.numeric(recoutput_rbind[i,3])
}

finalouput<-data.frame(finalouput)
names(finalouput)<-Xlowerbound:Xupperbound
colnames(finalouput)<-Ylowerbound:Yupperbound

return(finalouput)
}


