#' EDA Processing Part 1: Extract and filter EDA data
#'
#' This function allows you extract and filter EDA data. It will output raw data, filtered data (using user-specified high and low pass filters + a butterworth filter), and filtered + feature-scaled ([0,1]) data. It will also provide summary data at the participant and session level.
#' Inputs are: (1) List of participant numbers and (2) location where ZIP folders are stored. Outputs are: (1) one RDS file per participant with all data, (2) summary file that gives participant-level meta-data.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param rdslocation.EDA folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @param summarylocation folder location where you want participant level summaries to be saved.
#' @param EDA_low_cut This is a HIGH PASS filter. What EDA value (in microsiemens) should be used as the minimum cutoff (0 = cuts off samples that have 0us)
#' @param LowPctCutoff what percentage of samples in a five-second block must contain the low cutoff in order to exclude that block? (e.g., if .5, there must be at least 50 percent of the samples below the low-cut value to exclude the 5-sec block)
#' @param EDA_high_cut This is a LOW PASS filter. What EDA value (in microsiemens) should be used as the maximum cutoff (100 = cuts off samples above 100us)
#' @param HighPctCutoff what percentage of samples in a five-second block must contain the high cutoff in order to exclude that block?
#' @param KeepRejectFlag Do you want to keep the flag that shows which data the high and low pass filters rejected? If you want to run the diagnostic steps, you must keep this. Defaults to TRUE.
#' @param UseMultiCore Do you want to use more than one core for processing? Defaults to FALSE.
#' @keywords EDA
#' @importFrom foreach %dopar%
#' @export
#' @examples
#'E4_EDA_Process.part1.ExtractRawEDA(participant_list=c(1001:1003),
#' ziplocation=paste(system.file(package="E4tools"),"/extdata/E4_demo_data/",sep=""),
#' rdslocation.EDA=paste(tempdir(),"/extdata/output/raw_EDA/",sep=""),
#' summarylocation=paste(tempdir(),"/extdata/output/summaries/",sep=""),
#' EDA_low_cut=0.001,LowPctCutoff=.75,
#' EDA_high_cut=25,HighPctCutoff=.75)


E4_EDA_Process.part1.ExtractRawEDA<-function(participant_list,ziplocation,rdslocation.EDA,summarylocation,EDA_low_cut=0,LowPctCutoff=1,EDA_high_cut=1000,HighPctCutoff=1,
                                             KeepRejectFlag=TRUE,UseMultiCore=FALSE){


  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(ziplocation=="helper"){ziplocation<-get("ziplocation",envir=E4tools.env)}
  if(rdslocation.EDA=="helper"){rdslocation.EDA<-get("rdslocation.EDA",envir=E4tools.env)}
  if(summarylocation=="helper"){summarylocation<-get("summarylocation",envir=E4tools.env)}


  `%dopar%` <- foreach::`%dopar%`

##determine the number of cores

  if(UseMultiCore==TRUE){
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

  if (nzchar(chk) && chk == "TRUE") {
    # For Testing
    NumbCoresUse <- 2
  } else {
    NumbCoresUse<-parallel::detectCores()[1]-1 #get number of cores and sets it to n-1 (so one core is left over during processing)
    if(length(participant_list)<NumbCoresUse){NumbCoresUse==length(participant_list)} #if there are more cores than participants, change number of cores to number of participants, to avoid opening unused connections

  }
}

  if(UseMultiCore==FALSE){NumbCoresUse<-1}


  cl<-parallel::makeCluster(NumbCoresUse)
  doParallel::registerDoParallel(cl)



  ## for progress bar
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(max = length(participant_list), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)

NUMB<-NULL #fix to avoid CRAN note
  #for (NUMB in participant_list)
  foreach::foreach(NUMB=participant_list,.options.snow = list(progress = progress)) %dopar% {


    #get path to participant folder
    zipDIR<-paste(ziplocation,NUMB,sep="")

    # get list of all zip files in the folder (one zip file per session)
    zipfiles <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)




    EDA<-NULL;Individual_Ends<-NULL;EDA_raw<-NULL;Session_combined<-NULL
    for (ZIPS in zipfiles) {
      CURR_ZIP<-paste(ziplocation,NUMB,"/",ZIPS,sep="")

      if(file.size(CURR_ZIP)>6400){
        if(file.size(utils::unzip(CURR_ZIP, unzip = "internal",
                                  exdir=tempdir(),files="EDA.csv"))>500){

          EDA_single<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=tempdir(),
                                                   files="EDA.csv"),sep=",",header=FALSE) ###extract EDA
          StartTime<-EDA_single[1,1] #get start time
          SamplingRate<-EDA_single[2,1] #get sampling rate (will always be 4hz, but adding here for future-proofing)
          EDA_single<-EDA_single[-c(1:3),] # remove first three rows (since they contained start time, sampling rate, and a 0.00 SCL value)

          EDA_single<-as.data.frame(EDA_single) ##dataframes are easier to work with
          E4_serial<-substring(ZIPS, regexpr("_", ZIPS) + 1)
          E4_serial<-substr(E4_serial,1,6)
          EDA_single$E4_serial<-E4_serial



          EndTime<-(StartTime+round((nrow(EDA_single)/SamplingRate),0)) # calculate end time by adding start time to number of rows / sampling rate [which = number of seconds]

          ##make start and end times in miliseconds

          EDA_single$ts<-seq(from=StartTime*1000,to=EndTime*1000,length.out=nrow(EDA_single)) ##multiplying by 1000 in order to put everything in miliseconds
          EDA<-rbind(EDA,EDA_single) ##merge

          ##Create session-level summmaries
          Session_single<-(cbind((as.character(NUMB)),as.numeric(StartTime),as.numeric(EndTime),(as.character(E4_serial))))
          Session_combined<-as.data.frame(rbind(Session_combined,Session_single))
        }
      }
    }

    colnames(EDA)<-c("EDA_raw","E4_serial","ts")

    ##create session level metrics
    colnames(Session_combined)<-c("ID","StartTime","EndTime","E4Serial")
    Session_combined$EndTime<-as.numeric(as.character(Session_combined$EndTime))
    Session_combined$StartTime<-as.numeric(as.character(Session_combined$StartTime))
    Session_combined$SessionLength<-(Session_combined$EndTime-Session_combined$StartTime)/(60*60)


    ####Filtering
    ###remove times when band is prob not being worn

    EDA$FiveSecBin<-rep(seq(1,(nrow(EDA)/(5*4))),each=20,length.out=nrow(EDA))

    ### ID samples when EDA values are below cutoff


    EDA$TooLow<-0


    if (sum(EDA$EDA_raw<=EDA_low_cut)>0){EDA[EDA$EDA_raw<=EDA_low_cut,]$TooLow<-1}



    EDA$EDA_reject_toolow<-stats::ave(EDA$TooLow,EDA$FiveSecBin,FUN=function(x) sum(x))

    ### ID samples when EDA values are above cutoff
    EDA$TooHigh<-0

    if (sum(EDA$EDA_raw>=EDA_high_cut)>0){EDA[EDA$EDA_raw>=EDA_high_cut,]$TooHigh<-1}



    EDA$EDA_reject_toohigh<-stats::ave(EDA$TooHigh,EDA$FiveSecBin,FUN=function(x) sum(x))



    ### ID samples to exlucde that have a sufficent number of too high or too low values

    #if (sum(EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject)!=0){EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject<-1}

    EDA$EDA_reject<-0



    if(max(EDA$EDA_reject_toolow)>=(LowPctCutoff*20)){EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject<-1}
    if(max(EDA$EDA_reject_toohigh)>=(HighPctCutoff*20)){EDA[EDA$EDA_reject_toohigh>=(HighPctCutoff*20),]$EDA_reject<-1}

    #report how many samples were rejected
    message(paste(sum(EDA$EDA_reject)," samples rejected (",round((sum(EDA$EDA_reject)/nrow(EDA))*100,2),"% of all samples for this P)",sep=""))

    ### butterworth filter
    bf<-signal::butter(n=6,0.01)       # 1 Hz low-pass filter, 6th order
    EDA$EDA_filtered<-NA
    EDA[EDA$EDA_reject==0,]$EDA_filtered<- signal::filter(bf, EDA[EDA$EDA_reject==0,]$EDA_raw)


    #### scaling
    EDA$EDA_FeatureScaled<-BBmisc::normalize(EDA$EDA_raw,method="range",range=c(0,1)) ### do feature-scaling
    EDA$EDA_FeatureScaled_Filtered<-BBmisc::normalize(EDA$EDA_filtered,method="range",range=c(0,1)) ### do feature-scaling
    EDA$Participant<-NUMB

    ### eda high and low pass filtered only
    EDA$EDA_HighLowPass<-EDA$EDA_raw
    if(sum(EDA$EDA_reject)>0){EDA[EDA$EDA_reject==1,]$EDA_HighLowPass<-NA}



    ###merge EDA data into full participant dataset and save
    EDA_raw<-rbind(EDA_raw,EDA)

    ##remove extra columns
    EDA_raw$FiveSecBin<-NULL
    EDA_raw$TooLow<-NULL
    EDA_raw$EDA_reject_toolow<-NULL
    EDA_raw$TooHigh<-NULL


    #reorder
    EDA_raw<-EDA_raw[c("Participant", "E4_serial", "ts","EDA_raw","EDA_HighLowPass","EDA_FeatureScaled","EDA_filtered","EDA_FeatureScaled_Filtered","EDA_reject")]

    ###remove optional columsn
    if(KeepRejectFlag==FALSE){EDA_raw$EDA_reject<-NULL}




    if(!dir.exists(rdslocation.EDA)==TRUE){dir.create(rdslocation.EDA,recursive=TRUE)}
    filename<-paste(rdslocation.EDA,NUMB,"_EDA.rds",sep="")
    saveRDS(EDA_raw,file=filename)

    ###merge session summary data and save
    if(!dir.exists(summarylocation)==TRUE){dir.create(summarylocation,recursive=TRUE)}
    summaryfilename<-paste(summarylocation,NUMB,"_summary.csv",sep="")
    utils::write.csv(Session_combined,file=summaryfilename)

  }


  parallel::stopCluster(cl)
}
