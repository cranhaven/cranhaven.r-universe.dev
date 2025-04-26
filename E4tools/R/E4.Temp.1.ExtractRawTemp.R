#' Temperature Processing Part 1: Extract raw temperature data
#'
#' Extract raw temperatuer data.
#' Inputs are: (1) List of participant numbers and (2) location where ZIP folders are stored. Outputs are: (1) one RDS file per participant with all data. A working example and vignette will be added later.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param rdslocation.temp folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @param IncludeFarenheit do you want to include a column with temperature in Farenheit also? Defaults to true. Celcius, which is recorded by the E4, will always be included.
#' @keywords TEMP
#' @export
#' @examples
#' \dontrun{E4.Temp.part1.extract_raw_temp(participant_list=c(1001:1002),
#' ziplocation="~/documents/study/data/",
#' rdslocation.temp="~/documents/study/data/TEMP/")}



E4.Temp.part1.extract_raw_temp<-function(participant_list,ziplocation,rdslocation.temp,IncludeFarenheit=TRUE){
  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(ziplocation=="helper"){ziplocation<-get("ziplocation",envir=E4tools.env)}
  if(rdslocation.temp=="helper"){rdslocation.temp<-get("rdslocation.temp",envir=E4tools.env)}

   for (NUMB in participant_list) {
  message(paste("Starting participant",NUMB))

  #get path to participant folder
  zipDIR<-paste(ziplocation,NUMB,sep="")

  # get list of all zip files in the folder (one zip file per session)
  zipfiles <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)

if(length(zipfiles)==0){message(paste("No data for ",NUMB,". Going on to next participant.",sep=""))
  next
  }

  ###
TEMP<-NULL
  for (ZIPS in zipfiles) {
    CURR_ZIP<-paste(ziplocation,NUMB,"/",ZIPS,sep="")

    if(file.size(CURR_ZIP)>6400){
      if(file.size(utils::unzip(CURR_ZIP, unzip = "internal",
                                exdir=tempdir(),files="TEMP.csv"))>500){

       TEMP_single<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=tempdir(),
                                                 files="TEMP.csv"),sep=",",header=FALSE) ###extract TEMP
        StartTime<-TEMP_single[1,1] #get start time
        SamplingRate<-TEMP_single[2,1] #get sampling rate (will always be 4hz, but adding here for future-proofing)
        TEMP_single<-TEMP_single[-c(1:2),] # remove first two rows (since they contained start time and sampling rate)

        TEMP_single<-as.data.frame(TEMP_single) ##dataframes are easier to work with
        E4_serial<-substring(ZIPS, regexpr("_", ZIPS) + 1)
        E4_serial<-substr(E4_serial,1,6)
        TEMP_single$E4_serial<-E4_serial



        EndTime<-(StartTime+round((nrow(TEMP_single)/SamplingRate),0)) # calculate end time by adding start time to number of rows / sampling rate [which = number of seconds]

        ##make start and end times in miliseconds

        TEMP_single$ts<-seq(from=StartTime*1000,to=EndTime*1000,length.out=nrow(TEMP_single)) ##multiplying by 1000 in order to put everything in miliseconds
        TEMP<-rbind(TEMP,TEMP_single) ##merge


      }
    }
  }


names(TEMP)<-c("TEMP_C","E4_serial","ts")
TEMP$Participant<-NUMB
TEMP<-TEMP[c("Participant", "E4_serial", "ts","TEMP_C")]

if(IncludeFarenheit==TRUE){TEMP$TEMP_F<-(TEMP$TEMP_C*(9/5))+32}

if(!dir.exists(rdslocation.temp)==TRUE){dir.create(rdslocation.temp,recursive=TRUE)}
filename<-paste(rdslocation.temp,NUMB,"_TEMP.rds",sep="")
saveRDS(TEMP,file=filename)
  }
  }
