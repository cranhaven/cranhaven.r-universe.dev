#' Diagnostics: Plot Temperature data and button presses
#'
#' This will allow you to see all binned temperature data for a participant, along with which band they were wearing and when they pressed the event marker. One PDF file is made per participant. You must run E4.extras.BinEDA() first to prepare for this step.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.binnedtemp folder location where raw temperature data are stored
#' @param rdslocation.buttonpress location of folder where button press output is stored (from EDA part 2)
#' @param Plot_E4s Do you want a line at the bottom of the plot showing which E4 the participant was wearing?
#' @param plotlocation.temp folder where you want PDF outputs to go.
#' @param TempType Do you want Farenheit (TempType=F) or Celcius (TempType=C, default). If you did not elect to include Farenheit in the first temperature step, this step will calcuate it for you.
#' @keywords diagnostics plots
#' @export
#' @examples
#' \dontrun{E4.Diagnostics.EDAplot(participant_list=c(1001:1004),
#' rdslocation.buttonpress="~/study/data/tags/",
#' rdslocation.binnedtemp="~/study/data/Binned_EDA/",
#' plotlocation.EDA="~/study/data/EDAplots/")}
#'
#'
#'
#'
E4.Diagnostics.tempplot<-function(participant_list,rdslocation.binnedtemp,rdslocation.buttonpress,plotlocation.temp,Plot_E4s=TRUE,TempType="C"){
  ts_time<-TEMP_C<-TEMP_F<-E4_serial<-NULL #fixes to avoid CRAN note

  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(rdslocation.binnedtemp=="helper"){rdslocation.binnedtemp<-get("rdslocation.binnedtemp",envir=E4tools.env)}
  if(rdslocation.buttonpress=="helper"){rdslocation.buttonpress<-get("rdslocation.buttonpress",envir=E4tools.env)}
  if(plotlocation.temp=="helper"){plotlocation.temp<-get("plotlocation.temp",envir=E4tools.env)}

  ##Open button press file (since that only needs to be done once per set b/c all participants' data are in one file)
  Buttons<-readRDS(paste(rdslocation.buttonpress,"button_presses.rds",sep=""))

   ###open data
  for(NUMB in participant_list){
    if(file.exists(paste(rdslocation.binnedtemp,NUMB,"_binnedEDA.rds",sep=""))==FALSE){
      message(paste("No data for ",NUMB,". Was EDA part 1 or EDA binning not run or did it fail for this participant? Going on to next participant.",sep=""))
      next
    }
    PlotData<-readRDS(paste(rdslocation.binnedtemp,NUMB,"_binnedEDA.rds",sep=""))

    if(nrow(PlotData)>10){

    message(paste("Starting participant",NUMB))

      ##If TempType=F, make sure F is there, if not calculate it
      if(TempType=="F" & length(PlotData$TEMP_F)==0){PlotData$TEMP_F<-(PlotData$TEMP_C*(9/5))+32}


###make variable that gives date (for facetting)

if(PlotData$ts[1]>10000000000){PlotData$ts_date<-anytime::anydate(PlotData$ts/1000)}
if(PlotData$ts[1]<10000000000){PlotData$ts_date<-anytime::anydate(PlotData$ts)}





PlotData$ts_time<-data.table::as.ITime(PlotData$ts/1000)
XX<-as.POSIXlt(PlotData$ts/1000,origin="1970-01-01")


## Make time stamp that has the same date and correct time (so facet lines up correctly) -- thie "date" is only used to work around ggplot's requirements
PlotData$ts_time<-as.POSIXct(as.character(paste("2019-01-01 ",chron::times(format(XX, "%H:%M:%S"))," EST",sep="")))


###button pressess####

Plot_Buttons<-Buttons[Buttons$ID==NUMB,]

if(nrow(Plot_Buttons)>0){
###check to see what format button presses are in (second vs. milisecond)
if(Plot_Buttons$ts[1]>10000000000){Plot_Buttons$Press_Time<-anytime::anytime(Plot_Buttons$ts/1000)}
if(Plot_Buttons$ts[1]<10000000000){Plot_Buttons$Press_Time<-anytime::anytime(Plot_Buttons$ts)}

Plot_Buttons$ts_time<-as.POSIXct(as.character(paste("2019-01-01 ",chron::times(format(Plot_Buttons$Press_Time, "%H:%M:%S"))," EST",sep="")))

###make variable that gives date (for facetting)

if(Plot_Buttons$ts[1]>10000000000){Plot_Buttons$ts_date<-anytime::anydate(Plot_Buttons$ts/1000)}
if(Plot_Buttons$ts[1]<10000000000){Plot_Buttons$ts_date<-anytime::anydate(Plot_Buttons$ts)}
}

if(nrow(Plot_Buttons)==0){message(paste("NOTE: No button press data for participant number ",NUMB,". The plot will show temperature data only for this participant."))}



##make labels
BinSize<-round((PlotData$ts[5]-PlotData$ts[4])/(60*1000),0)

##make plot####

if(TempType=="C"){
  PlotOut<-ggplot2::ggplot()+
    ggplot2::geom_path(ggplot2::aes(x=ts_time,y=TEMP_C,group=1),data=PlotData)+
    ggplot2::facet_wrap(~ts_date)+
    ggplot2::scale_x_time(labels = scales::time_format("%H:%M",tz = "America/New_York"),breaks=seq(as.POSIXct("2019-01-01 00:00:00 EST"),as.POSIXct("2019-01-01 24:00:00 EST"),"6 hours"))+
    ggplot2::labs(x="Time of Day",y="Binned Temperature (Degrees C)",title=paste("All data for participant ID ",NUMB,sep=""),subtitle=(paste("(",BinSize," minute bins)",sep="")))
}

if(TempType=="F"){
  PlotOut<-ggplot2::ggplot()+
    ggplot2::geom_path(ggplot2::aes(x=ts_time,y=TEMP_F,group=1),data=PlotData)+
    ggplot2::facet_wrap(~ts_date)+
    ggplot2::scale_x_time(labels = scales::time_format("%H:%M",tz = "America/New_York"),breaks=seq(as.POSIXct("2019-01-01 00:00:00 EST"),as.POSIXct("2019-01-01 24:00:00 EST"),"6 hours"))+
    ggplot2::labs(x="Time of Day",y="Binned Temperature (Degrees F)",title=paste("All data for participant ID ",NUMB,sep=""),subtitle=(paste("(",BinSize," minute bins)",sep="")))
}


## add button pressess
if(nrow(Plot_Buttons)>0){PlotOut<-PlotOut+ggplot2::geom_vline(ggplot2::aes(xintercept=ts_time),data=Plot_Buttons)}

if(Plot_E4s==TRUE){PlotOut<-PlotOut+ggplot2::geom_line(ggplot2::aes(x=ts_time,y=0.1,group=E4_serial,linetype=E4_serial),data=PlotData)}



### Save File
if(!dir.exists(plotlocation.temp)==TRUE){dir.create(plotlocation.temp,recursive=TRUE)}
ggplot2::ggsave(filename=paste(plotlocation.temp,"TEMPplot_",NUMB,".pdf",sep=""),plot=PlotOut,width=11,height=8.5,units="in")
    }
    if(nrow(PlotData)<10){message(paste("No temperature data for ",NUMB,", going to next participant.",sep=""))}
  }
}


