library(stringr)

# cnbinom.pars is a function that outputs mu and r from univariate censored table input 
# assume negative binomial 

cnbinom.pars<-function (censoredtable){
  
  
  # make probabilities if not already
  censoredtable<-data.frame(censoredtable)
  names(censoredtable)<-c("cateogry", "value")
  censoredtable$value<-as.numeric(as.character(censoredtable$value))
  
  if (sum(as.numeric(censoredtable$value)) !=1){
    censoredtable$value<-censoredtable$value/sum(censoredtable$value)
  }
  
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
  

  ##### end function that help format data first ###################
  
  
  #The "loglikelihood_univariatecase" gives the Log Likelihood for a simple frequency table

  loglikelihood_univariatecase<-function (fixdata_univariatecase_output,par = c(mu, r)){

    #the if statement checks to see if the particular case even exist
    #if a cased doesn't exist then that final value for that case equals 0
    #these if statements take the output from the fixdata_univariatecase function
    #the sum is taken because there could be more than 1 of a particular category (usually more multiple cases of interval and exact)
    #notice Log is taken after the ppois is computed (log.p=TRUE will NOT be the same mathematical value)
    #also sometimes the trunc function produces -INF, this function replaces that with a 0

    # maximize two paramters 
    mu = par[1]
    r= par[2]
    # reparameterize 
    p = r/(r+mu)
    
    #< censor case
    if (length(fixdata_univariatecase_output$leftcensored1)==0) {finall1=0} else {
          # minus one because it is not equal to that left censored number 
        finall1=log(pnbinom((fixdata_univariatecase_output$leftcensored1[1,1]-1), size = r, mu = mu)) *
                    fixdata_univariatecase_output$leftcensored1[1,2]
        finall1<-replace(finall1, is.infinite(finall1),0)
      finall1=sum(finall1)}

    #<= censor case
    if (length(fixdata_univariatecase_output$leftcensored2)==0) {finall2=0} else {
      finall2=log(pnbinom(fixdata_univariatecase_output$leftcensored2[1,1],size = r, mu = mu))*
                  fixdata_univariatecase_output$leftcensored2[1,2]
      finall2<-replace(finall2, is.infinite(finall2),0)
      finall2=sum(finall2)}

    #0 censor case
    #different format in for loop
    #here using [1,i], [2,i], and ncol NOT [i,1], [i,2], and nrow because how fixdata_univariatecase outputs this case
    if (length(fixdata_univariatecase_output$nocensored)==0) {finale=0} else {
      finale=NULL
      for (i in 1:ncol(fixdata_univariatecase_output$nocensored)){
        finale[i]=log(dnbinom(as.numeric(fixdata_univariatecase_output$nocensored[1,i]),size = r, mu = mu))*
                  as.numeric(fixdata_univariatecase_output$nocensored[2, i])}
      finale<-replace(finale, is.infinite(finale),0)
      finale=sum(finale)}

    #> censor case
    if (length(fixdata_univariatecase_output$rightcensored1)==0) {finalg1=0} else {
    # plus one because it is not equal to that right censored number 
      finalg1=log(pnbinom(fixdata_univariatecase_output$rightcensored1[1,1]+1,size = r, mu = mu, lower.tail=FALSE))*
              fixdata_univariatecase_output$rightcensored1[1,2]
      finalg1<-replace(finalg1, is.infinite(finalg1),0)
      finalg1=sum(finalg1)}

    #>= or + censor case
    if (length(fixdata_univariatecase_output$rightcensored2)==0) {finalg2=0} else {
      finalg2=NULL
      finalg2=log(pnbinom(fixdata_univariatecase_output$rightcensored2[1,1],size = r,mu = mu, lower.tail=FALSE))*
              fixdata_univariatecase_output$rightcensored2[1,2]
      finalg2<-replace(finalg2, is.infinite(finalg2),0)
      finalg2=sum(finalg2)}

    # - censor case
    if (sum(fixdata_univariatecase_output$intervalcensored)==0) {finali=0} else {
      finali=NULL
      for (i in 1:nrow(fixdata_univariatecase_output$intervalcensored)){
        #have a -1 for second part of interval  
        finali[i]=log(pnbinom(fixdata_univariatecase_output$intervalcensored[i,2],size =r, mu = mu)-
                          pnbinom(((fixdata_univariatecase_output$intervalcensored[i,1])-1),size =r, mu = mu))*
                          fixdata_univariatecase_output$intervalcensored[i,3]}
      finali<-replace(finali, is.infinite(finali),0)
      finali=sum(finali)}
    
    #sums all the functions' final result
    #multiplied by -1 because the optim function (used below) minimizes so this is easiest way to minimize
    final<-(finall1+finall2+finale+finalg1+finalg2+finali)*-1
    return(final)
  } #end of loglikelihood_univariatecase function

  #####
  #####

  # optimize mu and r in loglikelihood_univariatecase
  # hide warnings... 
  options(warn=-1)
  fixdata_univariatecase_output=fixdata_univariatecase(censoredtable)
  op<- optim(par=c(100,100),
             fn =loglikelihood_univariatecase, 
             fixdata_univariatecase_output=fixdata_univariatecase_output)
  final<-op$par
  mu = final[1]
  r = final[2]
  return(list("Average"= mu, "Dispersion"=r))
}

