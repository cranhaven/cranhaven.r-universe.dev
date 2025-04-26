#' Acc Processing Part 1: Extract raw acceleromter data
#'
#' This allows you extract acceleromter data. It will output raw acceleromter data (x,y,z).
#' Inputs are: (1) List of participant numbers and (2) location where ZIP folders are stored. Outputs are: (1) one RDS file per participant with all data. A working example and vignette will be added later.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param rdslocation.acc folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @keywords acc
#' @export
#' @examples
#' E4.Acc_Process.part1.ExtractRawAcc(
#'   participant_list=c(1001),
#'   ziplocation=paste(system.file(package="E4tools"),
#'   "/extdata/E4_demo_data/",sep=""),
#'   rdslocation.acc=paste(tempdir(),"/extdata/output/raw_acc/",sep=""))



E4.Acc_Process.part1.ExtractRawAcc<-function(participant_list,ziplocation,rdslocation.acc){



  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(ziplocation=="helper"){ziplocation<-get("ziplocation",envir=E4tools.env)}
  if(rdslocation.acc=="helper"){rdslocation.acc<-get("rdslocation.acc",envir=E4tools.env)}


  for (NUMB in participant_list) {
  message(paste("Starting participant",NUMB))




  #get path to participant folder
  zipDIR<-paste(ziplocation,NUMB,sep="")

  # get list of all zip files in the folder (one zip file per session)
  zipfiles <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)


  ###
ACC<-NULL
  for (ZIPS in zipfiles) {
    CURR_ZIP<-paste(ziplocation,NUMB,"/",ZIPS,sep="")

    if(file.size(CURR_ZIP)>6400){
      if(file.size(utils::unzip(CURR_ZIP, unzip = "internal",
                                exdir=tempdir(),files="ACC.csv"))>500){
         ACC_single<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=tempdir(),files="ACC.csv"),sep=",",header=FALSE) ###extract ACC
        StartTime<-ACC_single[1,1] #get start time
        SamplingRate<-ACC_single[2,1] #get sampling rate (will always be 32hz, but adding here for future-proofing)
        ACC_single<-ACC_single[-c(1:2),] # remove first three rows (since they contained start time and sampling rate)

        ACC_single<-as.data.frame(ACC_single) ##dataframes are easier to work with
        E4_serial<-substring(ZIPS, regexpr("_", ZIPS) + 1)
        E4_serial<-substr(E4_serial,1,6)
        ACC_single$E4_serial<-E4_serial



        EndTime<-(StartTime+round((nrow(ACC_single)/SamplingRate),0)) # calculate end time by adding start time to number of rows / sampling rate [which = number of seconds]

        ##make start and end times in miliseconds

        ACC_single$ts<-seq(from=StartTime*1000,to=EndTime*1000,length.out=nrow(ACC_single)) ##multiplying by 1000 in order to put everything in miliseconds
        ACC<-rbind(ACC,ACC_single) ##merge


      }
    }
  }


names(ACC)<-c("acc_x","acc_y","acc_z","E4_serial","ts")
ACC$Participant<-NUMB
ACC<-ACC[c("Participant", "E4_serial", "ts","acc_x","acc_y","acc_z")]

if(!dir.exists(rdslocation.acc)==TRUE){dir.create(rdslocation.acc,recursive=TRUE)}
filename<-paste(rdslocation.acc,NUMB,"_acc.rds",sep="")
saveRDS(ACC,file=filename)
  }
}
