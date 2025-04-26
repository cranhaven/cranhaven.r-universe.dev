#' EDA Processing Part 2: Extract button presses
#'
#' This function allows you extract button presses and remove presses that are within a certain number of minutes before the end of a session or that are too close to another button press. If the participant has not pressed the button at all, it will give you a warning and continue with the other participants.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param rdslocation.buttonpress folder location where you want the RDS output to go (make sure that it ends in /). The file will be named "button_presses.RDS"
#' @param summarylocation location of folder where summaries from part 1 were saved (make sure that it ends in /)
#' @param cutoff.ends how close (in minutes) to the ends of a file do you want to cut off button presses (because they could be accidental e.g., when turning the band off). Default is 0, which will not remove button presses at all.
#' @param cutoff.overlap if you want to remove button presses within X number of minutes, enter that value here. Default is 0, which will not remove button presses at all.
#' @keywords EDA
#' @export
#' @examples
#' E4_EDA_Process.part2.ExtractButtonPresses(participant_list=c(1001:1002),
#'                                           ziplocation=paste(system.file(package="E4tools"),
#'                                           "/extdata/E4_demo_data/",sep=""),
#'                                           rdslocation.buttonpress=paste(tempdir(),
#'                                           "/extdata/output/presses/",sep=""),
#'                                           summarylocation=paste(system.file(package="E4tools"),
#'                                           "/extdata/output/summaries/",sep=""),
#'                                           cutoff.ends=2, cutoff.overlap=20)
#'
#'
#'
#'




E4_EDA_Process.part2.ExtractButtonPresses<-function(participant_list,ziplocation,rdslocation.buttonpress,summarylocation,cutoff.ends=0,cutoff.overlap=0){
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
TAG3<-NULL
## for file helper function

if(ziplocation=="helper"){ziplocation<-get("ziplocation",envir=E4tools.env)}
if(rdslocation.buttonpress=="helper"){rdslocation.buttonpress<-get("rdslocation.buttonpress",envir=E4tools.env)}
if(summarylocation=="helper"){summarylocation<-get("summarylocation",envir=E4tools.env)}

  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))

    #load summary file

    part1summary<-utils::read.csv(paste(summarylocation,NUMB,"_summary.csv",sep=""))

    #get path to participant folder
    zipDIR<-paste(ziplocation,NUMB,sep="")

    # get list of all zip files in the folder (one zip file per session)
    zipfiles <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)
    #ZIPS_for_list <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)


    TAG1<-NULL
    for (ZIPS in zipfiles) {

      CURR_ZIP<-paste(ziplocation,NUMB,"/",ZIPS,sep="")
      if(file.size(CURR_ZIP)>6400){
      if(file.size(utils::unzip(CURR_ZIP, unzip = "internal",
                         exdir=tempdir(),files="tags.csv"))>0){

        TAG<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=tempdir(),
                            files="tags.csv"),sep=",",header=FALSE)
      TAG1<-rbind(TAG1,TAG)}

    }}



    if(is.null(TAG1)==TRUE) {message(paste("No button pressess for",NUMB,"moving to next P"))
      next}
    names(TAG1)<-"Press_TS"

    ###remove presses within XX minutes of the end of a file (XX = minutes as defined in cutoff.ends)
   if(cutoff.ends>0){
    for (iENDS in part1summary$EndTime) {
      TAG1<-TAG1[!(TAG1<iENDS & TAG1>(iENDS-(cutoff.ends*60)))]
    }
    }



    ###remove presses within XX minutes of the end of a file (XX = minutes as defined in cutoff.ends)
    if(cutoff.ends>0){
      for (iENDS in part1summary$EndTime) {
        TAG1<-TAG1[!(TAG1<iENDS & TAG1>(iENDS-(cutoff.ends*60)))]
      }
    }



    TAG1<-as.data.frame(TAG1);names(TAG1)<-"Press_TS"

    ###remove button presses within XX minutes of one already happening (XX = minutes as defined in cutoff.overlap)

    if(cutoff.overlap>0){
      TAG1a<-as.data.frame(sort(TAG1$Press_TS))
      names(TAG1a)<-"Press_TS"
      TAG1a<-suppressMessages(DataCombine::slide(TAG1a,Var="Press_TS",NewVar="Press_TS_Lag",slideBy=-1))
      TAG1a$TimeBetween<-TAG1a$Press_TS-TAG1a$Press_TS_Lag
      TAGS1b<-TAG1a[TAG1a$TimeBetween>(cutoff.overlap*60),]$Press_TS
      TAG1<-c(TAG1a$Press_TS[1],TAGS1b[-1]) #workaround for NA on first value that didn't have a lag

    }

    TAG1<-as.data.frame(TAG1);names(TAG1)<-"Press_TS"




    TAG2<-cbind(NUMB,TAG1)
    TAG3<-rbind(TAG3,TAG2)




  }


names(TAG3)<-c("ID","ts")

##add ts in ms
TAG3$ts_ms<-TAG3$ts*1000

### save button press file

  if(!dir.exists(rdslocation.buttonpress)==TRUE){dir.create(rdslocation.buttonpress,recursive=TRUE)}
  saveRDS(TAG3,file=paste(rdslocation.buttonpress,"button_presses.RDS",sep=""))

}








