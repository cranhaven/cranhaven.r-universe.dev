#' Correction of Metabolic Rate Measurements
#'
#' The function is used to correct metabolic rate measurements for background respiration. To this end, oxygen consumption is estimated as the slope of the linear regression of measured \eqn{O_{2}} concentration over time, and is extracted for background respiration test and for each measurement phase. The correction is based on subtraction of oxygen consumption obtained during background respiration test from oxygen consumption obtained during metabolic rate measurements.
#'
#' @usage
#' correct.meas(info.data, pre.data, post.data, meas.data,
#'              method = c("pre.test", "post.test", "average",
#'                         "linear", "exponential", "parallel"),
#'              empty.chamber = c("CH1", "CH2", "CH3", "CH4",
#'                                "CH5", "CH6", "CH7", "CH8"))
#'
#' @param info.data  a data frame obtained by using the function \code{\link{input.info}}
#' @param pre.data  a data frame obtained by using the function \code{\link{import.test}} for a blank test before actual metabolic rate measurements
#' @param post.data  a data frame obtained by using the function \code{\link{import.test}} for a blank test after actual metabolic rate measurements
#' @param meas.data  a data frame obtained by using the function \code{\link{import.meas}} for actual metabolic rate measurements
#' @param method  string: the name of the method used for background respiration correction:
#' \itemize{
#' \item  "pre.test" - subtracts oxygen consumption of pre.data from oxygen consumptions of meas.data
#' \item  "post.test" - subtracts oxygen consumption of post.data from oxygen consumptions of meas.data
#' \item  "average" - subtracts an averaged oxygen consumption of pre.data and\cr post.data from oxygen consumptions of meas.data
#' \item  "linear" - subtracts a vector of progressively changing microbial consumptions from oxygen consumptions of meas.data. The values of oxygen consumption are linearly predicted from two reference points: oxygen consumption of pre.data and oxygen consumption of post.data.
#' \item  "exponential" - subtracts a vector of progressively changing microbial consumptions from oxygen consumptions of meas.data. The values of oxygen consumption are exponentially predicted from two reference points: oxygen consumption of pre.data and oxygen consumption of post.data.
#' \item  "parallel" - subtracts oxygen consumption in an empty chamber from oxygen consumptions of meas.data for each chamber
#' }
#' @param empty.chamber  string: the name of an empty chamber used only for the method 'parallel'
#'
#' @importFrom chron chron dates times
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @return  The function returns a data frame containing data of metabolic rate measurements corrected for background respiration. The data frame is used in the functions \code{\link{QC.meas}}, \code{\link{QC.activity}},\cr \code{\link{extract.slope}} and \code{\link{QC.slope}}.
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first five lines of the code:
#' data(info)
#' data(pre)
#' data(post)
#' data(AMR.raw)
#' \dontrun{
#' data(SMR.raw)
#' SMR.clean <- correct.meas(info.data = info,
#'                           pre.data = pre,
#'                           meas.data = SMR.raw,
#'                           method = "pre.test")
#' }
#'
#' AMR.clean <- correct.meas(info.data = info,
#'                           post.data = post,
#'                           meas.data = AMR.raw,
#'                           method = "post.test")
#'
#' @references {Svendsen, M. B. S., Bushnell, P. G., & Steffensen, J. F. (2016). Design and setup of intermittent-flow respirometry system for aquatic organisms. Journal of Fish Biology, 88(1), 26-50.}
#'
#' @export

correct.meas <- function (info.data, pre.data, post.data, meas.data,
                        method = c("pre.test", "post.test", "average",
                                   "linear", "exponential", "parallel"),
                        empty.chamber = c("CH1", "CH2", "CH3", "CH4",
                                          "CH5", "CH6", "CH7", "CH8")){
  Date.Time <- Date <- Real.Time <- Time <- Phase <- Start.Meas <- End.Meas <- Temp.1 <- Ox.1 <- NULL
  Chamber.No <- Ind <- Mass <- Volume <- Init.O2 <- Temp <- O2 <- BR <- Temp.2 <- Ox.2 <- Temp.3 <- NULL
  Ox.3 <- Temp.4 <- Ox.4 <- Temp.5 <- Ox.5 <- Temp.6 <- Ox.6 <- Temp.7 <- Ox.7 <- Temp.8 <- Ox.8 <- NULL

  if (ncol(meas.data) == 10){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear or exponential")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-temp.CH1
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)

    rm(temp.df)
    rm(temp.CH1)
  }


  else if (ncol(meas.data) == 12){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1, Temp.2, Ox.2))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH2<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.2, Ox.2))
    row.names(temp.CH2)<-NULL
    names(temp.CH2)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH2$Chamber.No<-as.factor(rep("CH2", dim(temp.CH2)[1]))
    temp.CH2$Ind<-as.factor(rep(info.data$ID[2], dim(temp.CH2)[1]))
    temp.CH2$Mass<-rep(info.data$Mass[2], dim(temp.CH2)[1])
    temp.CH2$Volume <- rep(info.data$Volume[2], dim(temp.CH2)[1])

    x<-levels(temp.CH2$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH2, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH2<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }

    else if(method == "parallel"){

      if(empty.chamber == "CH1"){
        x <- as.numeric(temp.CH1$O2-temp.CH1$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH2"){
        x <- as.numeric(temp.CH2$O2-temp.CH2$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        rm(x)
      }

      else{
        print("Please, choose which chamber is empty: CH1 or CH2")
      }
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear, exponential or parallel")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-rbind(temp.CH1, temp.CH2)
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)

    rm(temp.df)
    rm(temp.CH1)
    rm(temp.CH2)
  }


  else if (ncol(meas.data) == 14){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1, Temp.2, Ox.2, Temp.3, Ox.3))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH2<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.2, Ox.2))
    row.names(temp.CH2)<-NULL
    names(temp.CH2)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH2$Chamber.No<-as.factor(rep("CH2", dim(temp.CH2)[1]))
    temp.CH2$Ind<-as.factor(rep(info.data$ID[2], dim(temp.CH2)[1]))
    temp.CH2$Mass<-rep(info.data$Mass[2], dim(temp.CH2)[1])
    temp.CH2$Volume <- rep(info.data$Volume[2], dim(temp.CH2)[1])

    x<-levels(temp.CH2$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH2, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH2<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH3<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.3, Ox.3))
    row.names(temp.CH3)<-NULL
    names(temp.CH3)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH3$Chamber.No<-as.factor(rep("CH3", dim(temp.CH3)[1]))
    temp.CH3$Ind<-as.factor(rep(info.data$ID[3], dim(temp.CH3)[1]))
    temp.CH3$Mass<-rep(info.data$Mass[3], dim(temp.CH3)[1])
    temp.CH3$Volume <- rep(info.data$Volume[3], dim(temp.CH3)[1])

    x<-levels(temp.CH3$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH3, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH3<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }

    else if(method == "parallel"){

      if(empty.chamber == "CH1"){
        x <- as.numeric(temp.CH1$O2-temp.CH1$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH2"){
        x <- as.numeric(temp.CH2$O2-temp.CH2$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH3"){
        x <- as.numeric(temp.CH3$O2-temp.CH3$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        rm(x)
      }

      else{
        print("Please, choose which chamber is empty: CH1, CH2 or CH3")
      }
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear, exponential or parallel")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-rbind(temp.CH1, temp.CH2, temp.CH3)
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)

    rm(temp.df)
    rm(temp.CH1)
    rm(temp.CH2)
    rm(temp.CH3)
  }


  else if (ncol(meas.data) == 16){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1, Temp.2, Ox.2, Temp.3, Ox.3, Temp.4, Ox.4))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH2<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.2, Ox.2))
    row.names(temp.CH2)<-NULL
    names(temp.CH2)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH2$Chamber.No<-as.factor(rep("CH2", dim(temp.CH2)[1]))
    temp.CH2$Ind<-as.factor(rep(info.data$ID[2], dim(temp.CH2)[1]))
    temp.CH2$Mass<-rep(info.data$Mass[2], dim(temp.CH2)[1])
    temp.CH2$Volume <- rep(info.data$Volume[2], dim(temp.CH2)[1])

    x<-levels(temp.CH2$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH2, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH2<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH3<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.3, Ox.3))
    row.names(temp.CH3)<-NULL
    names(temp.CH3)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH3$Chamber.No<-as.factor(rep("CH3", dim(temp.CH3)[1]))
    temp.CH3$Ind<-as.factor(rep(info.data$ID[3], dim(temp.CH3)[1]))
    temp.CH3$Mass<-rep(info.data$Mass[3], dim(temp.CH3)[1])
    temp.CH3$Volume <- rep(info.data$Volume[3], dim(temp.CH3)[1])

    x<-levels(temp.CH3$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH3, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH3<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH4<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.4, Ox.4))
    row.names(temp.CH4)<-NULL
    names(temp.CH4)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH4$Chamber.No<-as.factor(rep("CH4", dim(temp.CH4)[1]))
    temp.CH4$Ind<-as.factor(rep(info.data$ID[4], dim(temp.CH4)[1]))
    temp.CH4$Mass<-rep(info.data$Mass[4], dim(temp.CH4)[1])
    temp.CH4$Volume <- rep(info.data$Volume[4], dim(temp.CH4)[1])

    x<-levels(temp.CH4$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH4, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH4<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }

    else if(method == "parallel"){

      if(empty.chamber == "CH1"){
        x <- as.numeric(temp.CH1$O2-temp.CH1$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH2"){
        x <- as.numeric(temp.CH2$O2-temp.CH2$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH3"){
        x <- as.numeric(temp.CH3$O2-temp.CH3$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH4"){
        x <- as.numeric(temp.CH4$O2-temp.CH4$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        rm(x)
      }

      else{
        print("Please, choose which chamber is empty: CH1, CH2, CH3 or CH4")
      }
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear, exponential or parallel")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-rbind(temp.CH1, temp.CH2, temp.CH3, temp.CH4)
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)

    rm(temp.df)
    rm(temp.CH1)
    rm(temp.CH2)
    rm(temp.CH3)
    rm(temp.CH4)
  }


  else if (ncol(meas.data) == 18){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1, Temp.2, Ox.2, Temp.3, Ox.3, Temp.4, Ox.4,
                                        Temp.5, Ox.5))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH2<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.2, Ox.2))
    row.names(temp.CH2)<-NULL
    names(temp.CH2)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH2$Chamber.No<-as.factor(rep("CH2", dim(temp.CH2)[1]))
    temp.CH2$Ind<-as.factor(rep(info.data$ID[2], dim(temp.CH2)[1]))
    temp.CH2$Mass<-rep(info.data$Mass[2], dim(temp.CH2)[1])
    temp.CH2$Volume <- rep(info.data$Volume[2], dim(temp.CH2)[1])

    x<-levels(temp.CH2$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH2, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH2<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH3<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.3, Ox.3))
    row.names(temp.CH3)<-NULL
    names(temp.CH3)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH3$Chamber.No<-as.factor(rep("CH3", dim(temp.CH3)[1]))
    temp.CH3$Ind<-as.factor(rep(info.data$ID[3], dim(temp.CH3)[1]))
    temp.CH3$Mass<-rep(info.data$Mass[3], dim(temp.CH3)[1])
    temp.CH3$Volume <- rep(info.data$Volume[3], dim(temp.CH3)[1])

    x<-levels(temp.CH3$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH3, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH3<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH4<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.4, Ox.4))
    row.names(temp.CH4)<-NULL
    names(temp.CH4)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH4$Chamber.No<-as.factor(rep("CH4", dim(temp.CH4)[1]))
    temp.CH4$Ind<-as.factor(rep(info.data$ID[4], dim(temp.CH4)[1]))
    temp.CH4$Mass<-rep(info.data$Mass[4], dim(temp.CH4)[1])
    temp.CH4$Volume <- rep(info.data$Volume[4], dim(temp.CH4)[1])

    x<-levels(temp.CH4$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH4, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH4<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH5<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.5, Ox.5))
    row.names(temp.CH5)<-NULL
    names(temp.CH5)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH5$Chamber.No<-as.factor(rep("CH5", dim(temp.CH5)[1]))
    temp.CH5$Ind<-as.factor(rep(info.data$ID[5], dim(temp.CH5)[1]))
    temp.CH5$Mass<-rep(info.data$Mass[5], dim(temp.CH5)[1])
    temp.CH5$Volume <- rep(info.data$Volume[5], dim(temp.CH5)[1])

    x<-levels(temp.CH5$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH5, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH5<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }

    else if(method == "parallel"){

      if(empty.chamber == "CH1"){
        x <- as.numeric(temp.CH1$O2-temp.CH1$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH2"){
        x <- as.numeric(temp.CH2$O2-temp.CH2$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH3"){
        x <- as.numeric(temp.CH3$O2-temp.CH3$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH4"){
        x <- as.numeric(temp.CH4$O2-temp.CH4$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH5"){
        x <- as.numeric(temp.CH5$O2-temp.CH5$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        rm(x)
      }

      else{
        print("Please, choose which chamber is empty: CH1, CH2, CH3, CH4 or CH5")
      }
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear, exponential or parallel")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-rbind(temp.CH1, temp.CH2, temp.CH3, temp.CH4, temp.CH5)
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)

    rm(temp.df)
    rm(temp.CH1)
    rm(temp.CH2)
    rm(temp.CH3)
    rm(temp.CH4)
    rm(temp.CH5)
  }


  else if (ncol(meas.data) == 20){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1, Temp.2, Ox.2, Temp.3, Ox.3, Temp.4, Ox.4,
                                        Temp.5, Ox.5, Temp.6, Ox.6))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH2<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.2, Ox.2))
    row.names(temp.CH2)<-NULL
    names(temp.CH2)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH2$Chamber.No<-as.factor(rep("CH2", dim(temp.CH2)[1]))
    temp.CH2$Ind<-as.factor(rep(info.data$ID[2], dim(temp.CH2)[1]))
    temp.CH2$Mass<-rep(info.data$Mass[2], dim(temp.CH2)[1])
    temp.CH2$Volume <- rep(info.data$Volume[2], dim(temp.CH2)[1])

    x<-levels(temp.CH2$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH2, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH2<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH3<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.3, Ox.3))
    row.names(temp.CH3)<-NULL
    names(temp.CH3)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH3$Chamber.No<-as.factor(rep("CH3", dim(temp.CH3)[1]))
    temp.CH3$Ind<-as.factor(rep(info.data$ID[3], dim(temp.CH3)[1]))
    temp.CH3$Mass<-rep(info.data$Mass[3], dim(temp.CH3)[1])
    temp.CH3$Volume <- rep(info.data$Volume[3], dim(temp.CH3)[1])

    x<-levels(temp.CH3$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH3, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH3<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH4<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.4, Ox.4))
    row.names(temp.CH4)<-NULL
    names(temp.CH4)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH4$Chamber.No<-as.factor(rep("CH4", dim(temp.CH4)[1]))
    temp.CH4$Ind<-as.factor(rep(info.data$ID[4], dim(temp.CH4)[1]))
    temp.CH4$Mass<-rep(info.data$Mass[4], dim(temp.CH4)[1])
    temp.CH4$Volume <- rep(info.data$Volume[4], dim(temp.CH4)[1])

    x<-levels(temp.CH4$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH4, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH4<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH5<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.5, Ox.5))
    row.names(temp.CH5)<-NULL
    names(temp.CH5)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH5$Chamber.No<-as.factor(rep("CH5", dim(temp.CH5)[1]))
    temp.CH5$Ind<-as.factor(rep(info.data$ID[5], dim(temp.CH5)[1]))
    temp.CH5$Mass<-rep(info.data$Mass[5], dim(temp.CH5)[1])
    temp.CH5$Volume <- rep(info.data$Volume[5], dim(temp.CH5)[1])

    x<-levels(temp.CH5$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH5, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH5<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH6<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.6, Ox.6))
    row.names(temp.CH6)<-NULL
    names(temp.CH6)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH6$Chamber.No<-as.factor(rep("CH6", dim(temp.CH6)[1]))
    temp.CH6$Ind<-as.factor(rep(info.data$ID[6], dim(temp.CH6)[1]))
    temp.CH6$Mass<-rep(info.data$Mass[6], dim(temp.CH6)[1])
    temp.CH6$Volume <- rep(info.data$Volume[6], dim(temp.CH6)[1])

    x<-levels(temp.CH6$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH6, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH6<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH6[temp.CH6$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH6$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH6[temp.CH6$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH6$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }

    else if(method == "parallel"){

      if(empty.chamber == "CH1"){
        x <- as.numeric(temp.CH1$O2-temp.CH1$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH2"){
        x <- as.numeric(temp.CH2$O2-temp.CH2$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH3"){
        x <- as.numeric(temp.CH3$O2-temp.CH3$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH4"){
        x <- as.numeric(temp.CH4$O2-temp.CH4$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH5"){
        x <- as.numeric(temp.CH5$O2-temp.CH5$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH6"){
        x <- as.numeric(temp.CH6$O2-temp.CH6$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        rm(x)
      }

      else{
        print("Please, choose which chamber is empty: CH1, CH2, CH3, CH4, CH5 or CH6")
      }
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear, exponential or parallel")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-rbind(temp.CH1, temp.CH2, temp.CH3, temp.CH4, temp.CH5, temp.CH6)
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)

    rm(temp.df)
    rm(temp.CH1)
    rm(temp.CH2)
    rm(temp.CH3)
    rm(temp.CH4)
    rm(temp.CH5)
    rm(temp.CH6)
  }


  else if (ncol(meas.data) == 22){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1, Temp.2, Ox.2, Temp.3, Ox.3, Temp.4, Ox.4,
                                        Temp.5, Ox.5, Temp.6, Ox.6, Temp.7, Ox.7))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH2<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.2, Ox.2))
    row.names(temp.CH2)<-NULL
    names(temp.CH2)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH2$Chamber.No<-as.factor(rep("CH2", dim(temp.CH2)[1]))
    temp.CH2$Ind<-as.factor(rep(info.data$ID[2], dim(temp.CH2)[1]))
    temp.CH2$Mass<-rep(info.data$Mass[2], dim(temp.CH2)[1])
    temp.CH2$Volume <- rep(info.data$Volume[2], dim(temp.CH2)[1])

    x<-levels(temp.CH2$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH2, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH2<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH3<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.3, Ox.3))
    row.names(temp.CH3)<-NULL
    names(temp.CH3)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH3$Chamber.No<-as.factor(rep("CH3", dim(temp.CH3)[1]))
    temp.CH3$Ind<-as.factor(rep(info.data$ID[3], dim(temp.CH3)[1]))
    temp.CH3$Mass<-rep(info.data$Mass[3], dim(temp.CH3)[1])
    temp.CH3$Volume <- rep(info.data$Volume[3], dim(temp.CH3)[1])

    x<-levels(temp.CH3$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH3, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH3<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH4<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.4, Ox.4))
    row.names(temp.CH4)<-NULL
    names(temp.CH4)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH4$Chamber.No<-as.factor(rep("CH4", dim(temp.CH4)[1]))
    temp.CH4$Ind<-as.factor(rep(info.data$ID[4], dim(temp.CH4)[1]))
    temp.CH4$Mass<-rep(info.data$Mass[4], dim(temp.CH4)[1])
    temp.CH4$Volume <- rep(info.data$Volume[4], dim(temp.CH4)[1])

    x<-levels(temp.CH4$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH4, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH4<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH5<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.5, Ox.5))
    row.names(temp.CH5)<-NULL
    names(temp.CH5)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH5$Chamber.No<-as.factor(rep("CH5", dim(temp.CH5)[1]))
    temp.CH5$Ind<-as.factor(rep(info.data$ID[5], dim(temp.CH5)[1]))
    temp.CH5$Mass<-rep(info.data$Mass[5], dim(temp.CH5)[1])
    temp.CH5$Volume <- rep(info.data$Volume[5], dim(temp.CH5)[1])

    x<-levels(temp.CH5$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH5, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH5<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH6<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.6, Ox.6))
    row.names(temp.CH6)<-NULL
    names(temp.CH6)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH6$Chamber.No<-as.factor(rep("CH6", dim(temp.CH6)[1]))
    temp.CH6$Ind<-as.factor(rep(info.data$ID[6], dim(temp.CH6)[1]))
    temp.CH6$Mass<-rep(info.data$Mass[6], dim(temp.CH6)[1])
    temp.CH6$Volume <- rep(info.data$Volume[6], dim(temp.CH6)[1])

    x<-levels(temp.CH6$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH6, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH6<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH7<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.7, Ox.7))
    row.names(temp.CH7)<-NULL
    names(temp.CH7)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH7$Chamber.No<-as.factor(rep("CH7", dim(temp.CH7)[1]))
    temp.CH7$Ind<-as.factor(rep(info.data$ID[7], dim(temp.CH7)[1]))
    temp.CH7$Mass<-rep(info.data$Mass[7], dim(temp.CH7)[1])
    temp.CH7$Volume <- rep(info.data$Volume[7], dim(temp.CH7)[1])

    x<-levels(temp.CH7$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH7, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH7<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH7"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH7, type="response", se.fit=F))
      any(x>0)
      temp.CH7$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH7"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH7, type="response", se.fit=F))
      any(x>0)
      temp.CH7$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH7"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH7, type="response", se.fit=F))
      any(x>0)
      temp.CH7$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH6[temp.CH6$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH6$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH7"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH7"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH7[temp.CH7$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH7$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH6[temp.CH6$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH6$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH7"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH7"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH7[temp.CH7$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH7$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }

    else if(method == "parallel"){

      if(empty.chamber == "CH1"){
        x <- as.numeric(temp.CH1$O2-temp.CH1$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH2"){
        x <- as.numeric(temp.CH2$O2-temp.CH2$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH3"){
        x <- as.numeric(temp.CH3$O2-temp.CH3$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH4"){
        x <- as.numeric(temp.CH4$O2-temp.CH4$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH5"){
        x <- as.numeric(temp.CH5$O2-temp.CH5$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH6"){
        x <- as.numeric(temp.CH6$O2-temp.CH6$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH7"){
        x <- as.numeric(temp.CH7$O2-temp.CH7$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        rm(x)
      }

      else{
        print("Please, choose which chamber is empty: CH1, CH2, CH3, CH4, CH5, CH6 or CH7")
      }
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear, exponential or parallel")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-rbind(temp.CH1, temp.CH2, temp.CH3, temp.CH4, temp.CH5, temp.CH6, temp.CH7)
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)

    rm(temp.df)
    rm(temp.CH1)
    rm(temp.CH2)
    rm(temp.CH3)
    rm(temp.CH4)
    rm(temp.CH5)
    rm(temp.CH6)
    rm(temp.CH7)
  }


  else if (ncol(meas.data) == 24){
    temp.df <- meas.data
    M.total<-temp.df$Total.Phases[1]
    temp.df<-subset(meas.data, select=c(Date.Time, Date, Real.Time, Time, Phase, Start.Meas, End.Meas, Temp.1, Ox.1, Temp.2, Ox.2, Temp.3, Ox.3, Temp.4, Ox.4,
                                      Temp.5, Ox.5, Temp.6, Ox.6, Temp.7, Ox.7, Temp.8, Ox.8))
    row.names(temp.df)<-NULL

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Compling the Chamber-Specific Data
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH1<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.1, Ox.1))
    row.names(temp.CH1)<-NULL
    names(temp.CH1)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH1$Chamber.No<-as.factor(rep("CH1", dim(temp.CH1)[1]))
    temp.CH1$Ind<-as.factor(rep(info.data$ID[1], dim(temp.CH1)[1]))
    temp.CH1$Mass<-rep(info.data$Mass[1], dim(temp.CH1)[1])
    temp.CH1$Volume <- rep(info.data$Volume[1], dim(temp.CH1)[1])

    # rather than have you manually run the code for each unique measurement period, let's automate the process with a 'for' loop
    x<-levels(temp.CH1$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH1, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH1<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH2<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.2, Ox.2))
    row.names(temp.CH2)<-NULL
    names(temp.CH2)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH2$Chamber.No<-as.factor(rep("CH2", dim(temp.CH2)[1]))
    temp.CH2$Ind<-as.factor(rep(info.data$ID[2], dim(temp.CH2)[1]))
    temp.CH2$Mass<-rep(info.data$Mass[2], dim(temp.CH2)[1])
    temp.CH2$Volume <- rep(info.data$Volume[2], dim(temp.CH2)[1])

    x<-levels(temp.CH2$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH2, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH2<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH3<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.3, Ox.3))
    row.names(temp.CH3)<-NULL
    names(temp.CH3)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH3$Chamber.No<-as.factor(rep("CH3", dim(temp.CH3)[1]))
    temp.CH3$Ind<-as.factor(rep(info.data$ID[3], dim(temp.CH3)[1]))
    temp.CH3$Mass<-rep(info.data$Mass[3], dim(temp.CH3)[1])
    temp.CH3$Volume <- rep(info.data$Volume[3], dim(temp.CH3)[1])

    x<-levels(temp.CH3$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH3, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH3<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH4<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.4, Ox.4))
    row.names(temp.CH4)<-NULL
    names(temp.CH4)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH4$Chamber.No<-as.factor(rep("CH4", dim(temp.CH4)[1]))
    temp.CH4$Ind<-as.factor(rep(info.data$ID[4], dim(temp.CH4)[1]))
    temp.CH4$Mass<-rep(info.data$Mass[4], dim(temp.CH4)[1])
    temp.CH4$Volume <- rep(info.data$Volume[4], dim(temp.CH4)[1])

    x<-levels(temp.CH4$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH4, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH4<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH5<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.5, Ox.5))
    row.names(temp.CH5)<-NULL
    names(temp.CH5)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH5$Chamber.No<-as.factor(rep("CH5", dim(temp.CH5)[1]))
    temp.CH5$Ind<-as.factor(rep(info.data$ID[5], dim(temp.CH5)[1]))
    temp.CH5$Mass<-rep(info.data$Mass[5], dim(temp.CH5)[1])
    temp.CH5$Volume <- rep(info.data$Volume[5], dim(temp.CH5)[1])

    x<-levels(temp.CH5$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH5, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH5<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH6<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.6, Ox.6))
    row.names(temp.CH6)<-NULL
    names(temp.CH6)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH6$Chamber.No<-as.factor(rep("CH6", dim(temp.CH6)[1]))
    temp.CH6$Ind<-as.factor(rep(info.data$ID[6], dim(temp.CH6)[1]))
    temp.CH6$Mass<-rep(info.data$Mass[6], dim(temp.CH6)[1])
    temp.CH6$Volume <- rep(info.data$Volume[6], dim(temp.CH6)[1])

    x<-levels(temp.CH6$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH6, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH6<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH7<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.7, Ox.7))
    row.names(temp.CH7)<-NULL
    names(temp.CH7)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH7$Chamber.No<-as.factor(rep("CH7", dim(temp.CH7)[1]))
    temp.CH7$Ind<-as.factor(rep(info.data$ID[7], dim(temp.CH7)[1]))
    temp.CH7$Mass<-rep(info.data$Mass[7], dim(temp.CH7)[1])
    temp.CH7$Volume <- rep(info.data$Volume[7], dim(temp.CH7)[1])

    x<-levels(temp.CH7$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH7, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH7<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    temp.CH8<-subset(temp.df, select=c(Date.Time:End.Meas, Temp.8, Ox.8))
    row.names(temp.CH8)<-NULL
    names(temp.CH8)<-c(names(temp.df)[1:7], "Temp", "O2")
    temp.CH8$Chamber.No<-as.factor(rep("CH8", dim(temp.CH8)[1]))
    temp.CH8$Ind<-as.factor(rep(info.data$ID[8], dim(temp.CH8)[1]))
    temp.CH8$Mass<-rep(info.data$Mass[8], dim(temp.CH8)[1])
    temp.CH8$Volume <- rep(info.data$Volume[8], dim(temp.CH8)[1])

    x<-levels(temp.CH8$Phase)
    temp2.df<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                         Temp=numeric(), O2=numeric(), Chamber.No=factor(), Ind=factor(), Mass=numeric(), Volume=numeric(), Init.O2=numeric())
    for(i in 1:length(x))
    {	x.df<-subset(temp.CH8, Phase==x[i])
    x.init<-rep(x.df$O2[1], dim(x.df)[1])
    x.df$Init.O2<-x.init
    temp2.df<-rbind(temp2.df, x.df) }

    temp.CH8<-temp2.df
    rm(x)
    rm(temp2.df)
    rm(i)
    rm(x.df)
    rm(x.init)
    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    # Correcting Data & Compiling Final Dataset
    #--------------------------------------------------------------------------------------------------------------------------------------------------#

    if(method == "pre.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH7"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH7, type="response", se.fit=F))
      any(x>0)
      temp.CH7$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH8"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH8, type="response", se.fit=F))
      any(x>0)
      temp.CH8$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "post.test"){
      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH7"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH7, type="response", se.fit=F))
      any(x>0)
      temp.CH7$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH8"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH8, type="response", se.fit=F))
      any(x>0)
      temp.CH8$BR<-x
      rm(x)
      rm(temp.lm)
    }

    else if(method == "average"){
      prepost.data<-pre.data
      prepost.data$delta.O2 <- (pre.data$delta.O2 + post.data$delta.O2)/2

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH1"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH1, type="response", se.fit=F))
      any(x>0)
      temp.CH1$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH2"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH2, type="response", se.fit=F))
      any(x>0)
      temp.CH2$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH3"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH3, type="response", se.fit=F))
      any(x>0)
      temp.CH3$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH4"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH4, type="response", se.fit=F))
      any(x>0)
      temp.CH4$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH5"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH5, type="response", se.fit=F))
      any(x>0)
      temp.CH5$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH6"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH6, type="response", se.fit=F))
      any(x>0)
      temp.CH6$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH7"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH7, type="response", se.fit=F))
      any(x>0)
      temp.CH7$BR<-x
      rm(x)
      rm(temp.lm)

      temp.lm<-lm(delta.O2~Time, data=subset(prepost.data, Chamber.No=="CH8"))
      temp.lm$coefficients[1] <- 0
      x<-as.vector(predict.lm(temp.lm, temp.CH8, type="response", se.fit=F))
      any(x>0)
      temp.CH8$BR<-x
      rm(x)
      rm(temp.lm)
      rm(prepost.data)
    }

    else if(method == "linear"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH6[temp.CH6$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH6$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH7"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH7"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH7[temp.CH7$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH7$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }
      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH8"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH8"))
        pro <- (1-i/(M.total+1))*temp.lm1$coefficients[2] + i/(M.total+1)*temp.lm2$coefficients[2]
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH8[temp.CH8$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH8$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro"))
      rm(list = c("M.total", "M.phase"))
    }

    else if(method == "exponential"){
      M.phase <- levels(temp.df$Phase)

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH1"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH1"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH1[temp.CH1$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH1$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH2"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH2"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH2[temp.CH2$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH2$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH3"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH3"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH3[temp.CH3$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH3$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH4"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH4"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH4[temp.CH4$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH4$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH5"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH5"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH5[temp.CH5$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH5$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH6"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH6"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH6[temp.CH6$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH6$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))

      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH7"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH7"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH7[temp.CH7$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH7$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))


      b<-NULL
      for (i in M.phase){
        a <- as.numeric(substr(i, 2, 3))
        b <-append(b, a)
      }

      y<-NULL
      for (i in b){
        temp.lm1<-lm(delta.O2~Time, data=subset(pre.data, Chamber.No=="CH8"))
        temp.lm2<-lm(delta.O2~Time, data=subset(post.data, Chamber.No=="CH8"))
        exp.coef<-sign(temp.lm2$coefficients[2]/temp.lm1$coefficients[2]) * abs(temp.lm2$coefficients[2]/temp.lm1$coefficients[2])^(1/(M.total+1))
        pro = temp.lm1$coefficients[2] * exp.coef^i
        temp.lm1$coefficients[1] <- 0
        temp.lm1$coefficients[2] <- pro
        lm.M <- assign(paste("lm.M", i, sep=""),temp.lm1)
        x<-as.vector(predict.lm(lm.M, temp.CH8[temp.CH8$Phase==paste("M", i, sep=""),], type="response", se.fit=F))
        y<-append(y, x)
      }
      any(y>0)
      temp.CH8$BR<-y
      rm(list = ls(pattern = "lm.M"))
      rm(list = c("temp.lm1", "temp.lm2", "a", "b", "i", "x", "y", "pro", "exp.coef"))
    }


    else if(method == "parallel"){

      if(empty.chamber == "CH1"){
        x <- as.numeric(temp.CH1$O2-temp.CH1$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH2"){
        x <- as.numeric(temp.CH2$O2-temp.CH2$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH3"){
        x <- as.numeric(temp.CH3$O2-temp.CH3$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH4"){
        x <- as.numeric(temp.CH4$O2-temp.CH4$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH5"){
        x <- as.numeric(temp.CH5$O2-temp.CH5$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH6"){
        x <- as.numeric(temp.CH6$O2-temp.CH6$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH7"){
        x <- as.numeric(temp.CH7$O2-temp.CH7$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else if(empty.chamber == "CH8"){
        x <- as.numeric(temp.CH8$O2-temp.CH8$Init.O2)
        temp.CH1$BR <- x
        temp.CH2$BR <- x
        temp.CH3$BR <- x
        temp.CH4$BR <- x
        temp.CH5$BR <- x
        temp.CH6$BR <- x
        temp.CH7$BR <- x
        temp.CH8$BR <- x
        rm(x)
      }

      else{
        print("Please, choose which chamber is empty: CH1, CH2, CH3, CH4, CH5, CH6, CH7 or CH8")
      }
    }

    else{
      print("Please, choose the method: pre.test, post.test, average, linear, exponential or parallel")
    }

    #--------------------------------------------------------------------------------------------------------------------------------------------------#
    MR.data.all<-rbind(temp.CH1, temp.CH2, temp.CH3, temp.CH4, temp.CH5, temp.CH6, temp.CH7, temp.CH8)
    MR.data.all<-subset(MR.data.all, select=c(Date.Time:End.Meas, Chamber.No, Ind, Mass, Volume, Init.O2, Temp, O2, BR))
    MR.data.all$O2.correct<-MR.data.all$O2 - MR.data.all$BR

    rm(temp.df)
    rm(temp.CH1)
    rm(temp.CH2)
    rm(temp.CH3)
    rm(temp.CH4)
    rm(temp.CH5)
    rm(temp.CH6)
    rm(temp.CH7)
    rm(temp.CH8)

    MR.data.all$DO.unit <- info.data$DO.unit[1]
    return(MR.data.all)
  }
  else{
  }
}
