#' Match EDA data to button pressess
#'
#' This function allows you to extract the data that are within X minutes before and/or after a button press. If there are no button pressess for a participant, it will issue a warning and continue with the next participant.
#' Inputs: (1) List of participant numbers, (2) location of individual EDA files from step 1, (3) location of button presses from step 2.
#' Outputs: (1) RDS file with EDA data before and/or after button presses (and control data), for each participant and combined.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.MatchedEDA folder location where you want the RDS outputs to go (make sure that it ends in /). The combined data file will go into this directory. Individual participants' data will go into a subdirectory in this folder called "individual_participants"
#' @param rdslocation.EDA folder where rds files for individual Ps' EDA data are stored (from part 1)
#' @param rdslocation.buttonpress location of folder where button press output is stored (from part 2)
#' @param min.before how many minutes before a button press do you want EDA data? Enter 0 if you do not want ANY data before (i.e., you're using only data post-press)
#' @param min.after how many minutes after a button press do you want EDA data? Enter 0 if you do not want ANY data after (i.e., you're using only data pre-press)
#' @param control add in control cases, defaults to T (default is to specify controls from exactly 24 hours prior to the press, provided there was not a press then too)
#' @keywords EDA
#' @export
#' @examples
#' E4_EDA_Process.part3.MatchPressesToEDA(participant_list=c(1001),
#'                                       rdslocation.buttonpress=paste(system.file(package="E4tools"),
#'                                       "/extdata/output/presses/",sep=""),
#'                                       rdslocation.MatchedEDA=paste(tempdir(),
#'                                       "/extdata/output/matched_EDA/",sep=""),
#'                                       rdslocation.EDA=paste(system.file(package="E4tools"),
#'                                       "/extdata/output/raw_EDA/",sep=""),
#'                                       min.before=20,min.after=20,control=TRUE)


E4_EDA_Process.part3.MatchPressesToEDA<-function(participant_list,rdslocation.MatchedEDA,rdslocation.EDA,rdslocation.buttonpress,min.before,min.after,control=TRUE){

  TAG3<-NULL;EDA_press_OUT1<-NULL;RDS_COMB1<-NULL

## for file helper function
if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
if(rdslocation.MatchedEDA=="helper"){rdslocation.MatchedEDA<-get("rdslocation.MatchedEDA",envir=E4tools.env)}
if(rdslocation.buttonpress=="helper"){rdslocation.buttonpress<-get("rdslocation.buttonpress",envir=E4tools.env)}
if(rdslocation.EDA=="helper"){rdslocation.EDA<-get("rdslocation.EDA",envir=E4tools.env)}
  press_summary<-readRDS(paste(rdslocation.buttonpress,"button_presses.RDS",sep=""))

###create directory structure
if(!dir.exists(rdslocation.MatchedEDA)==TRUE){dir.create(rdslocation.MatchedEDA,recursive = T)}
individual_directory<-paste(rdslocation.MatchedEDA,"individual_participants/",sep="")
if(!dir.exists(individual_directory)==TRUE){dir.create(individual_directory,recursive = T)}


#`%dopar%` <- foreach::`%dopar%`
#doParallel::registerDoParallel(parallel::detectCores()[1]-1) ##detects cores and then registeres n-1 cores (so one core is left over)
### for progress bar
#doSNOW::registerDoSNOW(parallel::makeCluster(parallel::detectCores()[1]-1))
#pb <- utils::txtProgressBar(max = length(participant_list), style = 3)
#progress <- function(n) utils::setTxtProgressBar(pb, n)
#foreach::foreach(NUMB=participant_list,.options.snow = list(progress = progress)) %dopar% {

for (NUMB in participant_list){
    message(paste("Starting participant",NUMB))
    EDA_press_OUT1<-NULL
    EDA_press_OUT_CONTROL1<-NULL


    #read EDA data for the individual participant
    EDA_participant<-readRDS(paste(rdslocation.EDA,NUMB,"_EDA.rds",sep=""))

    #select only presses from that participant
    press_times<-press_summary[press_summary$ID==NUMB,]$ts
    press_times<-press_times*1000 #make into ms
    if(length(press_times)==0) {message(paste("No button pressess for",NUMB,"moving to next P"))
      next}
    #create a list of control times that are 24 hours before

    press_times_control<-press_times-86400000

    ##remove control times that overlap with existing press times

    to_remove<-NULL

    for(CONTROL in press_times_control) {
      for(PRESS in press_times) {
        if((CONTROL<PRESS+1200 & CONTROL>PRESS-1200)){
          to_remove<-c(to_remove,CONTROL)
        }else next

      }
      press_times_control<-press_times_control[!(press_times_control %in% to_remove)]
    }


    ### BEFORE PRESSESS
    if (min.before>0){
    for(CURR_PRESS in press_times){
      if(sum(EDA_participant$ts<CURR_PRESS & EDA_participant$ts>(CURR_PRESS-(min.before*60*1000)))>0){
    EDA_Before_Button_Press<-EDA_participant[(EDA_participant$ts<CURR_PRESS & EDA_participant$ts>(CURR_PRESS-(min.before*60*1000))),]
    EDA_press_OUT<-as.data.frame(
     cbind(EDA_Before_Button_Press$Participant,
           EDA_Before_Button_Press$E4_serial,
           CURR_PRESS,
           "BEFORE",
           "CASE",
           EDA_Before_Button_Press$ts,
           EDA_Before_Button_Press$EDA_raw,
           EDA_Before_Button_Press$EDA_filtered,
           EDA_Before_Button_Press$EDA_FeatureScaled,
           EDA_Before_Button_Press$EDA_FeatureScaled_Filtered))
    EDA_press_OUT1<-rbind(EDA_press_OUT1,EDA_press_OUT)
      }
    }






    }

    ### AFTER PRESSESS
    if (min.after>0){
      for(CURR_PRESS in press_times){
        if(sum(EDA_participant$ts>CURR_PRESS & EDA_participant$ts<(CURR_PRESS+(min.after*60*1000)))>0){
        EDA_After_Button_Press<-EDA_participant[(EDA_participant$ts>CURR_PRESS & EDA_participant$ts<(CURR_PRESS+(min.after*60*1000))),]
        EDA_press_OUT<-as.data.frame(
          cbind(EDA_After_Button_Press$Participant,
                EDA_After_Button_Press$E4_serial,
                CURR_PRESS,
                "AFTER",
                "CASE",
                EDA_After_Button_Press$ts,
                EDA_After_Button_Press$EDA_raw,
                EDA_After_Button_Press$EDA_filtered,
                EDA_After_Button_Press$EDA_FeatureScaled,
                EDA_After_Button_Press$EDA_FeatureScaled_Filtered))
        EDA_press_OUT1<-rbind(EDA_press_OUT1,EDA_press_OUT)
      }

      }

    }


    ### BEFORE PRESSESS - CONTROL
    if(control==TRUE){
      if(min.before>0){
      for(CURR_PRESS in press_times_control){
        if(sum(EDA_participant$ts<CURR_PRESS & EDA_participant$ts>(CURR_PRESS-(min.before*60*1000)))>0){
          EDA_Before_Button_Press_CONTROL<-EDA_participant[(EDA_participant$ts<CURR_PRESS & EDA_participant$ts>(CURR_PRESS-(min.before*60*1000))),]
          EDA_press_OUT_CONTROL<-as.data.frame(
            cbind(EDA_Before_Button_Press_CONTROL$Participant,
                  EDA_Before_Button_Press_CONTROL$E4_serial,
                  CURR_PRESS,
                  "BEFORE",
                  "CONTROL",
                  EDA_Before_Button_Press_CONTROL$ts,
                  EDA_Before_Button_Press_CONTROL$EDA_raw,
                  EDA_Before_Button_Press_CONTROL$EDA_filtered,
                  EDA_Before_Button_Press_CONTROL$EDA_FeatureScaled,
                  EDA_Before_Button_Press_CONTROL$EDA_FeatureScaled_Filtered))
          EDA_press_OUT1<-rbind(EDA_press_OUT1,EDA_press_OUT_CONTROL)
        }
      }
      }
      }

    ### AFTER PRESSESS- CONTROL
    if(control==TRUE){
      if (min.after>0){
      for(CURR_PRESS in press_times_control){
        if(sum(EDA_participant$ts>CURR_PRESS & EDA_participant$ts<(CURR_PRESS+(min.after*60*1000)))>0){
          EDA_After_Button_Press_Control<-EDA_participant[(EDA_participant$ts>CURR_PRESS & EDA_participant$ts<(CURR_PRESS+(min.after*60*1000))),]
          EDA_press_OUT_CONTROL<-as.data.frame(
            cbind(EDA_After_Button_Press_Control$Participant,
                  EDA_After_Button_Press_Control$E4_serial,
                  CURR_PRESS,
                  "AFTER",
                  "CONTROL",
                  EDA_After_Button_Press_Control$ts,
                  EDA_After_Button_Press_Control$EDA_raw,
                  EDA_After_Button_Press_Control$EDA_filtered,
                  EDA_After_Button_Press_Control$EDA_FeatureScaled,
                  EDA_After_Button_Press_Control$EDA_FeatureScaled_Filtered))
          EDA_press_OUT1<-rbind(EDA_press_OUT1,EDA_press_OUT_CONTROL)
        }

      }

    }}

    ###Save individual files
    if(!dir.exists(individual_directory)==T){dir.create(individual_directory,recursive=T)}
    names(EDA_press_OUT1)<-c("ID","E4_serial","PressTime","BeforeAfter","CaseControl","Data_TS","EDA_raw","EDA_filtered","EDA_FeatureScaled","EDA_FeatureScaled_Filtered")
    saveRDS(EDA_press_OUT1,file=paste(individual_directory,"EDA_presses_",NUMB,".RDS",sep=""))
    }

  ### on entire dataset

RDSfiles <- list.files(individual_directory, pattern="*.RDS", full.names=FALSE)
for(RDSLIST in RDSfiles) {

  CURR_ZIP<-paste(individual_directory,"/",RDSLIST,sep="")


  RDS_COMB<-readRDS(file=CURR_ZIP)
  RDS_COMB1<-rbind(RDS_COMB1,RDS_COMB)
}
if(!dir.exists(rdslocation.MatchedEDA)==T){dir.create(rdslocation.MatchedEDA,recursive=T)}
saveRDS(RDS_COMB1,file=paste(rdslocation.MatchedEDA,"EDA_presses_COMBINED",".RDS",sep=""))
#names(EDA_press_OUT1)<-c("ID","E4_serial","PressTime","BeforeAfter","Data_TS","EDA_raw","EDA_filtered","EDA_FeatureScaled")
  #saveRDS(EDA_press_OUT1,file=paste(rdslocation.MatchedEDA,"EDA_presses.RDS",sep=""))

}



