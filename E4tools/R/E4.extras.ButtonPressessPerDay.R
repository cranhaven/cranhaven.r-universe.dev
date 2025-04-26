#' EDA Extra Processing: Get number of button presses per participant, per day from the combined "button_pressess.RDS" file
#'
#' This function allows you extract button pressess per participant, per day. It will output a data frame (not an RDS file) that you can use for analyses. You must first extract button pressess using the E4_EDA_Process.part2.ExtractButtonPresses() function.
#' @param rdslocation.buttonpress location of folder where button press output is stored (the file is called "button_presses.RDS"). This should end in / .
#' @param ImputeNAs This will create NAs for any days between the first and last day of study data for each participant. If no data = no presses (which is likely the case, use the "ImputeZeros" option to make them zeros instead).
#' @param ImputeZeros Do you want to make the NAs for days without data zeros instead of NA?
#' @return Dataframe with a three columns: ID, date, number of button pressess.
#' @export
#' @examples
#' Presses_Per_Day<-
#'   E4.extras.ButtonPressessPerDay(rdslocation.buttonpress=
#'                                    paste(system.file(package="E4tools"),
#'                                    "/extdata/output/presses/",sep=""),
#'                                              ImputeNAs=TRUE,ImputeZeros=TRUE)
#'Presses_Per_Day



E4.extras.ButtonPressessPerDay<-function(rdslocation.buttonpress,ImputeNAs=FALSE,ImputeZeros=FALSE){
  press_summary<-readRDS(paste(rdslocation.buttonpress,"button_presses.RDS",sep=""))

  press_summary$press<-1
  press_summary$date<-anytime::anydate(press_summary$ts)

 press_day<-stats::aggregate(press~ID+date,data=press_summary,FUN="sum")

  if(ImputeNAs==TRUE){
  IDs<-as.numeric(as.character(as.vector(levels(as.factor(press_day$ID)))))
  press_day_w_NAs<-NULL
  for(ID in IDs) {
    press_day_single_P<-press_day[press_day$ID==ID,]
    dates<-(seq(from=anytime::anydate(min(press_day_single_P$date)),to=anytime::anydate(max(press_day_single_P$date)),by=1))
    xx<-as.data.frame(cbind(anytime::anydate(dates),1,ID))
    names(xx)<-c("date","NoPress","ID")
    xx$date<-anytime::anydate(xx$date)
    press_day_single_P<-merge(press_day_single_P,xx,by=c("ID","date"),all.y=TRUE)
    press_day_single_P$NoPress<-NULL
    press_day_w_NAs<-rbind(press_day_single_P,press_day_w_NAs)
  }
  }

 ##second if statement is to address times where there are no NAs
 if(ImputeZeros==TRUE){if(nrow(press_day_w_NAs[is.na(press_day_w_NAs$press),])>0){press_day_w_NAs[is.na(press_day_w_NAs$press),]$press<-0}}

 if(ImputeNAs==TRUE){return(press_day_w_NAs)}
 if(ImputeNAs==FALSE){return(press_day)}



}






