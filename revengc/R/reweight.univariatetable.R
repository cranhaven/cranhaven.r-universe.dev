reweight.univariatetable<-function(observed.table, estimated.table) {
  
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
# make probabilities if not already
observed.table<-data.frame(observed.table)
names(observed.table)<-c("cateogry", "value")
observed.table$value<-as.numeric(as.character(observed.table$value))

if (sum(as.numeric(observed.table$value)) !=1){
observed.table$value<-observed.table$value/sum(observed.table$value)
}

minnumber<-min(as.numeric(names(estimated.table)))
repx<-fixdata_univariatecase(observed.table)

# error for users first
nums<-findtypeofcensoring_univariatetable(observed.table)$Censor_Number
minnumberinobservedtable<-min(na.omit(as.numeric(unlist(strsplit(nums, split = " ")))))

if(minnumberinobservedtable<minnumber) stop("Min number is observed.tableis lower than min number in estimated.table")

### going through the cases.. 
# case L1    
if (length(repx$leftcensored1) != 0){
  uncensored_chunk<-estimated.table[1:(repx$leftcensored1[1,1]-minnumber)]
  sum_uncensored_chunk<-sum(uncensored_chunk) 
  censoredvalue<-as.numeric(repx$leftcensored1[1,2])
  reweighted<-(uncensored_chunk*censoredvalue)/sum_uncensored_chunk
  estimated.table[1:(repx$leftcensored1[1,1]-minnumber)]<-reweighted
} 

# case L2    
if (length(repx$leftcensored2) != 0){
  uncensored_chunk<-estimated.table[1:(repx$leftcensored2[1,1]-minnumber+1)]
  sum_uncensored_chunk<-sum(uncensored_chunk) 
  censoredvalue<-as.numeric(repx$leftcensored2[1,2])
  reweighted<-(uncensored_chunk*censoredvalue)/sum_uncensored_chunk
  estimated.table[1:(repx$leftcensored2[1,1]-minnumber+1)]<-reweighted
} 
  
  # case G1 
if (length(repx$rightcensored1) != 0){
  uncensored_chunk<-estimated.table[(repx$rightcensored1[1,1]-minnumber+2):length(estimated.table)]
  sum_uncensored_chunk<-sum(uncensored_chunk) 
  censoredvalue<-as.numeric(repx$rightcensored1[1,2])
  reweighted<-(uncensored_chunk*censoredvalue)/sum_uncensored_chunk
  estimated.table[(repx$rightcensored1[1,1]-minnumber+2):length(estimated.table)]<-reweighted
} 
  
  # case G2
  if (length(repx$rightcensored2) != 0){
    uncensored_chunk<-estimated.table[(repx$rightcensored2[1,1]-minnumber+1):length(estimated.table)]
    sum_uncensored_chunk<-sum(uncensored_chunk) 
    censoredvalue<-as.numeric(repx$rightcensored2[1,2])
    reweighted<-(uncensored_chunk*censoredvalue)/sum_uncensored_chunk
    estimated.table[(repx$rightcensored2[1,1]-minnumber+1):length(estimated.table)]<-reweighted
  } 
  
  
  # case I
  if (length(repx$intervalcensored) != 0){
    for ( i in 1:nrow(repx$intervalcensored)){
    uncensored_chunk<-estimated.table[(repx$intervalcensored[i,1]-minnumber+1):(repx$intervalcensored[i,2]-minnumber+1)]
    sum_uncensored_chunk<-sum(uncensored_chunk) 
    censoredvalue<-as.numeric(repx$intervalcensored[i,3])
    reweighted<-(uncensored_chunk*censoredvalue)/sum_uncensored_chunk
    estimated.table[(repx$intervalcensored[i,1]-minnumber+1):(repx$intervalcensored[i,2]-minnumber+1)]<-reweighted
    }
  }
  
  # case E
  if (length(repx$nocensored) != 0){
    for ( i in 1:ncol(repx$nocensored)){
    uncensored_chunk<-estimated.table[repx$nocensored[1,i]-minnumber+1]
    sum_uncensored_chunk<-sum(uncensored_chunk) 
    censoredvalue<-as.numeric(repx$nocensored[2,i])
    reweighted<-(uncensored_chunk*censoredvalue)/sum_uncensored_chunk
    estimated.table[repx$nocensored[1,i]-minnumber+1]<-reweighted
    } 
  }


  # return reweighted estimated table 
  return(estimated.table)
  
 } # end function   
