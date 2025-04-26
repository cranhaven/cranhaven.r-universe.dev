#' Diagnostics: Plot EDA data and button presses
#'
#' This will allow you to see all binned EDA data for a participant, along with which band they were wearing and when they pressed the event marker. One PDF file is made per participant. You must run E4.extras.BinEDA() first to prepare for this step.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001").
#' @param rdslocation.binnedEDA folder location where binned EDA is stored (from E4.extras.BinEDA function).
#' @param rdslocation.buttonpress location of folder where button press output is stored (from part extract raw EDA part 2). Set to FALSE if you do not want to plot the button presses.
#' @param plotlocation.EDA Folder where you want to store the PDF plots. Set this to FALSE if you do not want to save the PDF output. You should only set to false if you are displaying the plot instead, and thus should also set display_plot to TRUE.
#' @param RejectFlagCount What percent of samples in the bin must be bad for the entire bin to be marked bad? Default is 48, which is 10 percent of samples in a 2-minute bin.
#' @param Plot_E4s Do you want a line at the bottom of the plot showing which E4 the participant was wearing?
#' @param display_plot Do you want the plot to be displayed on screen in addition to saving the PDF file? Defaults to false. This is most useful if you are only looking at one participant's data.
#' @keywords diagnostics plots
#' @export
#' @examples
#' E4.Diagnostics.EDAplot(participant_list=c(1001),
#'                        rdslocation.buttonpress=FALSE,
#'                        rdslocation.binnedEDA=paste(system.file(package="E4tools"),
#'                        "/extdata/plots/",sep=""),
#'                        plotlocation.EDA=FALSE,display_plot=TRUE)


E4.Diagnostics.EDAplot<-function(participant_list,rdslocation.binnedEDA,rdslocation.buttonpress,plotlocation.EDA,RejectFlagCount=48,Plot_E4s=TRUE,display_plot=FALSE){

  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(rdslocation.binnedEDA=="helper"){rdslocation.binnedEDA<-get("rdslocation.binnedEDA",envir=E4tools.env)}
  if(rdslocation.buttonpress=="helper"){rdslocation.buttonpress<-get("rdslocation.buttonpress",envir=E4tools.env)}
  if(plotlocation.EDA=="helper"){plotlocation.EDA<-get("plotlocation.EDA",envir=E4tools.env)}

  ##Open button press file (since that only needs to be done once per set b/c all participants' data are in one file)
  if(rdslocation.buttonpress!=FALSE){Buttons<-readRDS(paste(rdslocation.buttonpress,"button_presses.rds",sep=""))}


   ###open data
  for(NUMB in participant_list){
    if(file.exists(paste(rdslocation.binnedEDA,NUMB,"_binnedEDA.rds",sep=""))==FALSE){
      message(paste("No data for ",NUMB,". Was EDA part 1 or EDA binning not run or did it fail for this participant? Going on to next participant.",sep=""))
      next
    }
    PlotData<-readRDS(paste(rdslocation.binnedEDA,NUMB,"_binnedEDA.rds",sep=""))

    if(nrow(PlotData)>10){

    message(paste("Starting participant",NUMB))

##Open Data



###make variable that gives date (for facetting)

if(PlotData$ts[1]>10000000000){PlotData$ts_date<-anytime::anydate(PlotData$ts/1000)}
if(PlotData$ts[1]<10000000000){PlotData$ts_date<-anytime::anydate(PlotData$ts)}





PlotData$ts_time<-data.table::as.ITime(PlotData$ts/1000)
XX<-as.POSIXlt(PlotData$ts/1000,origin="1970-01-01")


## Make time stamp that has the same date and correct time (so facet lines up correctly) -- thie "date" is only used to work around ggplot's requirements
PlotData$ts_time<-as.POSIXct(as.character(paste("2019-01-01 ",chron::times(format(XX, "%H:%M:%S"))," EST",sep="")))


###button pressess####
if(rdslocation.buttonpress!=FALSE){
Plot_Buttons<-Buttons[Buttons$ID==NUMB,]

if(nrow(Plot_Buttons)>0){
###check to see what format button presses are in (second vs. milisecond)
if(Plot_Buttons$ts[1]>10000000000){Plot_Buttons$Press_Time<-anytime::anytime(Plot_Buttons$ts/1000)}
if(Plot_Buttons$ts[1]<10000000000){Plot_Buttons$Press_Time<-anytime::anytime(Plot_Buttons$ts)}

Plot_Buttons$ts_time<-as.POSIXct(as.character(paste("2019-01-01 ",chron::times(format(Plot_Buttons$Press_Time, "%H:%M:%S"))," EST",sep="")))}

###make variable that gives date (for facetting)

if(Plot_Buttons$ts[1]>10000000000){Plot_Buttons$ts_date<-anytime::anydate(Plot_Buttons$ts/1000)}
if(Plot_Buttons$ts[1]<10000000000){Plot_Buttons$ts_date<-anytime::anydate(Plot_Buttons$ts)}


if(nrow(Plot_Buttons)==0){message(paste("NOTE: No button press data for participant number ",NUMB,". The plot will show EDA data only."))}
}

###make reject % variables
PlotData$EDA_reject_CAT<-"GOOD"
if(max(PlotData$EDA_reject)>=RejectFlagCount){PlotData[PlotData$EDA_reject>=RejectFlagCount,]$EDA_reject_CAT<-"BAD"}



##make labels
BadLabel<-paste(round((stats::ftable(PlotData$EDA_reject_CAT)[1]/sum(stats::ftable(PlotData$EDA_reject_CAT))*100),2),"% of bins",sep="")
GoodLabel<-paste(round((stats::ftable(PlotData$EDA_reject_CAT)[2]/sum(stats::ftable(PlotData$EDA_reject_CAT))*100),2),"% of bins",sep="")
BinSize<-round((PlotData$ts[5]-PlotData$ts[4])/(60*1000),0)

##make plot####
ts_time<-EDA_HighLowPass<-EDA_reject_CAT<-ts_date<-TEMP_C<-TEMP_F<-E4_serial<-NULL #fix to avoid CRAN note

PlotOut<-ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=ts_time,y=EDA_HighLowPass,color=as.factor(EDA_reject_CAT),group=1),data=PlotData)+
  ggplot2::facet_wrap(~ts_date)+
  ggplot2::scale_x_time(labels = scales::time_format("%H:%M",tz = "America/New_York"),breaks=seq(as.POSIXct("2019-01-01 00:00:00 EST"),as.POSIXct("2019-01-01 24:00:00 EST"),"6 hours"))+
  ggplot2::labs(x="Time of Day",y="Binned EDA \n(w/high + low pass filter)",title=paste("All data for participant ID ",NUMB,sep=""),subtitle=(paste("(",BinSize," minute bins)",sep="")))

##next two if statements handle issues with the data quality legend when all data are good.
if(max(PlotData$EDA_reject)>=RejectFlagCount){ PlotOut<-PlotOut+ggplot2::scale_color_manual(values=c("red", "blue"),name="Data Quality",
                               labels = c(paste("Bad\n",BadLabel,sep=""),paste("Good\n",GoodLabel,sep="")))}

if(max(PlotData$EDA_reject)<RejectFlagCount){PlotOut<-PlotOut+ggplot2::scale_color_manual(values=c("blue"),name="Data Quality",
                                                                                         labels = c(paste("Good\n 100% of bins",sep="")))}

## add button pressess
if(rdslocation.buttonpress!=FALSE){if(nrow(Plot_Buttons)>0){PlotOut<-PlotOut+ggplot2::geom_vline(ggplot2::aes(xintercept=ts_time),data=Plot_Buttons)}}

if(Plot_E4s==TRUE){PlotOut<-PlotOut+ggplot2::geom_line(ggplot2::aes(x=ts_time,y=0.1,group=E4_serial,linetype=E4_serial),data=PlotData)}

## display plot
if(display_plot==TRUE){return(PlotOut)}

### Save File

if(plotlocation.EDA!=FALSE){
if(!dir.exists(plotlocation.EDA)==TRUE){dir.create(plotlocation.EDA,recursive=TRUE)}
ggplot2::ggsave(filename=paste(plotlocation.EDA,"EDAplot_",NUMB,".pdf",sep=""),plot=PlotOut,width=11,height=8.5,units="in")}


    }
    if(nrow(PlotData)<10){message(paste("No EDA data for ",NUMB,", going to next participant.",sep=""))}
  }
}


