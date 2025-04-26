#' GGIR Export
#'
#' This function will allow you to export a CSV file that is compatible with GGIR. It will create one CSV (not RDS like other parts of E4Tools) per participant.
#' The CSV file will contain a header compatiable with GGIR, the information in the header is:
#' Header includes:
#' 1. Participant ID, 2. Number of E4s used in the data file, 3. Time stamp type (unix, in miliseconds), 4. Time zone (using format that GGIR uses), 5. ACC sampling rate, 6. ACC dynamic range (in ±g),
#' 7. ACC resolution (in bits), 8. Temp sampling rate, 9. Temp units, 10. Temp range min, 11. Temp range max, 12. Temp resolution
#' The columns in the output file are: 1. Timestamp, 2. E4 Serial, 3. Raw ACC X in bits, 4. Raw ACC Y in bits, 5. Raw ACC Z in bits.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /). Enter ziplocation=ziplocation to use the prespecified folder structure from E4.Prep.FileHelper
#' @param csvlocation.GGIRout folder location where you want the CSV outputs to go (make sure that it ends in /). Enter csvlocation.GGIRout=csvlocation.GGIRout to use the prespecified folder structure from E4.Prep.FileHelper.
#' @param tz timezone where these data were collected (see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)
#' @keywords acc
#' @importFrom data.table data.table rbindlist setkey setDT :=
#' @export
#' @examples
#' \dontrun{E4.Acc_Process.Part1.ExtractRawAcc(participant_list=c(1001:1002),
#' ziplocation="~/documents/study/data/",
#' csvlocation.GGIRout="~/documents/study/data/acc/")}


E4.GGIR.Export<-function(participant_list,ziplocation,csvlocation.GGIRout,tz){

  ts<-E4serial<-NULL #fixes to avoid CRAN note

  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(ziplocation=="helper"){ziplocation<-get("ziplocation",envir=E4tools.env)}
  if(csvlocation.GGIRout=="helper"){csvlocation.GGIRout<-get("csvlocation.GGIRout",envir=E4tools.env)}


  for (NUMB in participant_list) {
  message(paste("Starting participant",NUMB))

  #get path to participant folder
  zipDIR<-paste(ziplocation,NUMB,sep="")

  # get list of all zip files in the folder (one zip file per session)
  zipfiles <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)


### CREATE ONE SINGLE FILE PER PARTICIPANT
  ACC_TEMP<-NULL
  for (ZIPS in zipfiles) {
    CURR_ZIP<-paste(ziplocation,NUMB,"/",ZIPS,sep="")

    if(file.size(CURR_ZIP)>6400){
      if(file.size(utils::unzip(CURR_ZIP, unzip = "internal",
                                exdir=tempdir(),files="ACC.csv"))>500){

       ACC_single<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=tempdir(),
                                                 files="ACC.csv"),sep=",",header=FALSE) ###extract ACC
        StartTime<-ACC_single[1,1] #get start time
        SamplingRate<-ACC_single[2,1] #get sampling rate (will always be 32hz, but adding here for future-proofing)
        ACC_single<-ACC_single[-c(1:2),] # remove first three rows (since they contained start time and sampling rate)

        ACC_single<-as.data.frame(ACC_single) ##dataframes are easier to work with
        E4_serial<-substring(ZIPS, regexpr("_", ZIPS) + 1)
        E4_serial<-substr(E4_serial,1,6)




        EndTime<-(StartTime+round((nrow(ACC_single)/SamplingRate),0)) # calculate end time by adding start time to number of rows / sampling rate [which = number of seconds]

        ##make start and end times in miliseconds

        ACC_single$ts<-seq(from=StartTime*1000,to=EndTime*1000,length.out=nrow(ACC_single)) ##multiplying by 1000 in order to put everything in miliseconds


        ### adding temperature
        TEMP_single<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=tempdir(),
                                                 files="TEMP.csv"),sep=",",header=FALSE) ###extract ACC
        StartTime_TEMP<-TEMP_single[1,1] #get start time
        SamplingRate_TEMP<-TEMP_single[2,1] #get sampling rate (will always be 4hz, but adding here for future-proofing)
        TEMP_single<-TEMP_single[-c(1:2),] # remove first two rows (since they contained start time and sampling rate)

        TEMP_single<-as.data.frame(TEMP_single) ##dataframes are easier to work with
        EndTime_TEMP<-(StartTime_TEMP+round((nrow(TEMP_single)/SamplingRate_TEMP),0)) # calculate end time by adding start time to number of rows / sampling rate [which = number of seconds]

        ##make start and end times in miliseconds

        TEMP_single$ts<-seq(from=StartTime_TEMP*1000,to=EndTime_TEMP*1000,length.out=nrow(TEMP_single)) ##multiplying by 1000 in order to put everything in miliseconds
        ACC_single_table<-data.table::as.data.table(ACC_single)
        TEMP_single_table<-data.table::as.data.table(TEMP_single)
        data.table::setkey(ACC_single_table, ts)
        data.table::setkey(TEMP_single_table, ts)
        ACC_TEMP_SINGLE<-TEMP_single_table[ACC_single_table, roll="nearest"]
        ACC_TEMP_SINGLE$serial<-E4_serial

        ACC_TEMP<-rbind(ACC_TEMP,ACC_TEMP_SINGLE) ##merge
      }
    }
  }


### OPERATIONS ON ENTIRE PARTICIPANT FILE ####

  ACC_TEMP$ts<-round(ACC_TEMP$ts/1000,0) #GGIR works better not in ms
  names(ACC_TEMP)<-c("temp","ts","acc_x","acc_y","acc_z","E4serial")


 data.table::setcolorder(ACC_TEMP, c("ts","E4serial","acc_x","acc_y","acc_z","temp"))


### make header

 #note device_serial_number will only show one right now
 ## N_subfiles refers to devices, not the number of sessions

 header_col1<-rbind("device_brand",
                    "recording_ID",
                    "device_serial_number",
                    "N_subfiles",
                    "timestamp_type",
                    "timezone_tzdata_format",
                    "acc_sample_rate",
                    "acc_unit",
                    "acc_dynrange_plusmin_g",
                    "acc_bit_resolution",
                    "temp_sample_rate",
                    "temp_units",
                    "temp_range_min",
                    "temp_range_max",
                    "temp_resolution",
                    "starttime",
                    "",
                    "",
                    "")



overall_start<-((anytime::anytime(min(as.numeric((ACC_TEMP$ts))),tz=tz)))

header_col2<-cbind(c("E4",
                      NUMB,
                      as.character(as.factor(ACC_TEMP$E4serial)[1]),
                      levels(as.factor(ACC_TEMP$E4serial)),
                      "unix_ms",
                      tz,
                      SamplingRate,
                      "bits",
                      2,
                      8,
                      SamplingRate_TEMP,
                      "celsius",
                      -40,
                      115,
                      0.2,
                     as.character(format(overall_start,usetz=TRUE)),
                      "",
                      "",
                      ""))



ACC_TEMP_header<-cbind(header_col1,header_col2,"","","")

#adding in column labels in the rihgt place (they'll pop up after the header)
 ACC_TEMP_header<-rbind(ACC_TEMP_header,c("timestamp","acc_x_bits","acc_y_bits","acc_z_bits","temp"))


 ACC_TEMP_header<-data.table::as.data.table(ACC_TEMP_header)



 ##removing serial column since we don't need it in the output (but do need it to generate the header)
 ACC_TEMP<-ACC_TEMP[,E4serial:=NULL]

 names(ACC_TEMP_header)<-names(ACC_TEMP) #to facilitate the merge

 ACC_TEMP<-rbind(ACC_TEMP_header,ACC_TEMP)


 ### merge header

if(!dir.exists(csvlocation.GGIRout)==TRUE){dir.create(csvlocation.GGIRout,recursive=TRUE)}
filename<-paste(csvlocation.GGIRout,NUMB,"_GGIR_out.csv",sep="")
utils::write.table(ACC_TEMP,file=filename,quote=TRUE,col.names=FALSE,sep=" ",na="",row.names=FALSE)
  }
}
