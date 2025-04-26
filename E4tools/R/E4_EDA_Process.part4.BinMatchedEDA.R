#' Bin the EDA data matched to button presses
#'
#' This function allows you to bin the data that has been matched to the button pressess (from step 3).
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.MatchedEDA folder location of the combined EDA file from step 3. (The file is called EDA_presses_COMBINED.RDS)
#' @param rdslocation.BinnedMatchedEDA location of folder where you want the binned data to be stored
#' @param min.before how many minutes before a button press do you want EDA data? Enter 0 if you do not want ANY data before (i.e., you're using only data post-press). This should match what you entered in step 3!
#' @param min.after how many minutes after a button press do you want EDA data? Enter 0 if you do not want ANY data after (i.e., you're using only data pre-press). This should match what you entered in step 3!
#' @param control does this dataset include control cases? This should match what you did in step 3.
#' @keywords EDA
#' @export
#' @examples
#'E4_EDA_Process.part4.BinMatchedEDA(participant_list=c(1001:1002),
#'                                   rdslocation.MatchedEDA=paste(system.file(package="E4tools"),
#'                                   "/extdata/output/matched_EDA/",sep=""),
#'                                   rdslocation.BinnedMatchedEDA=
#'                                   paste(tempdir(),"/extdata/output/binned_matched_EDA/",sep=""),
#'                                   min.after = 20,min.before = 20,control=TRUE)


E4_EDA_Process.part4.BinMatchedEDA<-function(participant_list,rdslocation.MatchedEDA,rdslocation.BinnedMatchedEDA,min.after,min.before,control=FALSE){


  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(rdslocation.MatchedEDA=="helper"){rdslocation.MatchedEDA<-get("rdslocation.MatchedEDA",envir=E4tools.env)}
  if(rdslocation.BinnedMatchedEDA=="helper"){rdslocation.BinnedMatchedEDA<-get("rdslocation.BinnedMatchedEDA",envir=E4tools.env)}



  #MatchedEDA<-readRDS(paste(rdslocation.MatchedEDA,"EDA_presses_COMBINED.RDS",sep=""))
  #MatchedEDA<-data.table::as.data.table(MatchedEDA)
  EDA_Binned_Merged<-NULL
  EDA_Binned_Single<-NULL
  EDA_MERGED_PARTICIPANT<-NULL



  for(NUMB in participant_list) {
    message(paste("Starting participant number",NUMB))
    if(file.exists(paste(rdslocation.MatchedEDA,"individual_participants/EDA_presses_",NUMB,".RDS",sep=""))==FALSE) {message(paste("No button pressess for",NUMB,"moving to next P"))
      next}


    EDA_participant<-readRDS(paste(rdslocation.MatchedEDA,"individual_participants/EDA_presses_",NUMB,".RDS",sep=""))
    #EDA_participant<-data.table::as.data.table(EDA_participant)
    #read EDA data for the individual participant
    #EDA_participant<-MatchedEDA[MatchedEDA$ID==NUMB,]
    EDA_MERGED_PARTICIPANT<-NULL


###Bins for BEFORE - CASE
if(min.before>0){

  Before_After="BEFORE"
  TYPE="CASE"
    EDA_participant_BEFORE<-EDA_participant[(EDA_participant$BeforeAfter=="BEFORE" & EDA_participant$CaseControl=="CASE"),]
    for(PressTime in  levels(EDA_participant_BEFORE$PressTime)) {
      if(nrow(EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,])>0 & nrow(EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,])<5000){
        BINS_before<-EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,]

        BINS_before<-BINS_before[order(BINS_before$Data_TS),]


        BINS_before$bin<-rep(seq((min.before*-1),-1,by=2),each=480,length.out=nrow(BINS_before)) #create 2-minute bins

        EDA_Binned_Single_raw<-stats::aggregate(as.numeric(as.character(EDA_raw))~(bin),data=BINS_before,FUN="mean")
        EDA_Binned_Single_filtered<-stats::aggregate(as.numeric(as.character(EDA_filtered))~(bin),data=BINS_before,FUN="mean")
        EDA_Binned_Single_fscale<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled))~(bin),data=BINS_before,FUN="mean")
        EDA_Binned_Single_fscale_filtered<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled_Filtered))~(bin),data=BINS_before,FUN="mean")

        EDA_Binned_Single<-merge(EDA_Binned_Single_raw,EDA_Binned_Single_filtered,by="bin",all=TRUE)
        EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale,by="bin",all=TRUE)
        EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale_filtered,by="bin",all=TRUE)

        Press_Numb<-as.numeric(as.character(PressTime))
        EDA_Binned_Single<-cbind(NUMB,Press_Numb,Before_After,TYPE,EDA_Binned_Single)
        EDA_Binned_Merged<-rbind(EDA_Binned_Merged,EDA_Binned_Single)
        EDA_MERGED_PARTICIPANT<-rbind(EDA_MERGED_PARTICIPANT,EDA_Binned_Single)

        }
      }
}

###Bins for AFTER -- CASE
if(min.after>0){

  Before_After="AFTER"
  TYPE="CASE"
  EDA_participant_AFTER<-EDA_participant[(EDA_participant$BeforeAfter=="AFTER" & EDA_participant$CaseControl=="CASE"),]
    for(PressTime in  levels(EDA_participant_AFTER$PressTime)) {
      if(nrow(EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,])>0 & nrow(EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,])<5000){
        BINS_after<-EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,]

        BINS_after<-BINS_after[order(BINS_after$Data_TS),]


        BINS_after$bin<-rep(seq(2,min.after,by=2),each=480,length.out=nrow(BINS_after)) #create 10 2-minute bins

        EDA_Binned_Single_raw<-stats::aggregate(as.numeric(as.character(EDA_raw))~(bin),data=BINS_after,FUN="mean")
        EDA_Binned_Single_filtered<-stats::aggregate(as.numeric(as.character(EDA_filtered))~(bin),data=BINS_after,FUN="mean")
        EDA_Binned_Single_fscale<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled))~(bin),data=BINS_after,FUN="mean")
        EDA_Binned_Single_fscale_filtered<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled_Filtered))~(bin),data=BINS_after,FUN="mean")



        EDA_Binned_Single<-merge(EDA_Binned_Single_raw,EDA_Binned_Single_filtered,by="bin",all=TRUE)
        EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale,by="bin",all=TRUE)
        EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale_filtered,by="bin",all=TRUE)

        Press_Numb<-as.numeric(as.character(PressTime))
        EDA_Binned_Single<-cbind(NUMB,Press_Numb,Before_After,TYPE,EDA_Binned_Single)
        EDA_Binned_Merged<-rbind(EDA_Binned_Merged,EDA_Binned_Single)
        EDA_MERGED_PARTICIPANT<-rbind(EDA_MERGED_PARTICIPANT,EDA_Binned_Single)
        }
    }
}

###Bins for BEFORE - CONTROL
    if(control==TRUE){
    if(min.before>0){

      Before_After="BEFORE"
      TYPE="CONTROL"
      EDA_participant_BEFORE<-EDA_participant[(EDA_participant$BeforeAfter=="BEFORE" & EDA_participant$CaseControl=="CONTROL"),]
      for(PressTime in levels(EDA_participant_BEFORE$PressTime)) {
        if(nrow(EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,])>0 & nrow(EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,])<5000){
          BINS_before<-EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,]

          BINS_before<-BINS_before[order(BINS_before$Data_TS),]


          BINS_before$bin<-rep(seq((min.before*-1),-1,by=2),each=480,length.out=nrow(BINS_before)) #create 2-minute bins

          EDA_Binned_Single_raw<-stats::aggregate(as.numeric(as.character(EDA_raw))~(bin),data=BINS_before,FUN="mean")
          EDA_Binned_Single_filtered<-stats::aggregate(as.numeric(as.character(EDA_filtered))~(bin),data=BINS_before,FUN="mean")
          EDA_Binned_Single_fscale<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled))~(bin),data=BINS_before,FUN="mean")
          EDA_Binned_Single_fscale_filtered<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled_Filtered))~(bin),data=BINS_before,FUN="mean")

          EDA_Binned_Single<-merge(EDA_Binned_Single_raw,EDA_Binned_Single_filtered,by="bin",all=TRUE)
          EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale,by="bin",all=TRUE)
          EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale_filtered,by="bin",all=TRUE)

          Press_Numb<-as.numeric(as.character(PressTime))+86400
          EDA_Binned_Single<-cbind(NUMB,Press_Numb,Before_After,TYPE,EDA_Binned_Single)
          #names(EDA_Binned_Single)[2]<-c("PressTime")
          EDA_Binned_Merged<-rbind(EDA_Binned_Merged,EDA_Binned_Single)
          EDA_MERGED_PARTICIPANT<-rbind(EDA_MERGED_PARTICIPANT,EDA_Binned_Single)
          }
      }
    }
}
    ###Bins for AFTER -- CONTROL
   if(control==TRUE){
     if(min.after>0){

      Before_After="AFTER"
      TYPE="CONTROL"
      EDA_participant_AFTER<-EDA_participant[(EDA_participant$BeforeAfter=="AFTER" & EDA_participant$CaseControl=="CONTROL"),]
      for(PressTime in  levels(EDA_participant_AFTER$PressTime)) {
        if(nrow(EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,])>0 & nrow(EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,])<5000){
          BINS_after<-EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,]

          BINS_after<-BINS_after[order(BINS_after$Data_TS),]


          BINS_after$bin<-rep(seq(2,min.after,by=2),each=480,length.out=nrow(BINS_after)) #create 10 2-minute bins

          EDA_Binned_Single_raw<-stats::aggregate(as.numeric(as.character(EDA_raw))~(bin),data=BINS_after,FUN="mean")
          EDA_Binned_Single_filtered<-stats::aggregate(as.numeric(as.character(EDA_filtered))~(bin),data=BINS_after,FUN="mean")
          EDA_Binned_Single_fscale<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled))~(bin),data=BINS_after,FUN="mean")
          EDA_Binned_Single_fscale_filtered<-stats::aggregate(as.numeric(as.character(EDA_FeatureScaled_Filtered))~(bin),data=BINS_after,FUN="mean")

          EDA_Binned_Single<-merge(EDA_Binned_Single_raw,EDA_Binned_Single_filtered,by="bin",all=TRUE)
          EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale,by="bin",all=TRUE)
          EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale_filtered,by="bin",all=TRUE)

          Press_Numb<-as.numeric(as.character(PressTime))+86400
          EDA_Binned_Single<-cbind(NUMB,Press_Numb,Before_After,TYPE,EDA_Binned_Single)


          EDA_Binned_Merged<-rbind(EDA_Binned_Merged,EDA_Binned_Single)
          EDA_MERGED_PARTICIPANT<-rbind(EDA_MERGED_PARTICIPANT,EDA_Binned_Single)
          }
      }
    }


    }
### on individual P
    names(EDA_MERGED_PARTICIPANT)<-c("ID","PressTime","BeforeAfter","CaseControl","MinBeforeAfter","EDA_raw","EDA_filtered","EDA_FeatureScaled","EDA_Filtered_FeatureScaled")
    rdslocation.BinnedMatchedEDA.participant<-paste(rdslocation.BinnedMatchedEDA,"individual_participants/",sep="")
    if(!dir.exists(rdslocation.BinnedMatchedEDA.participant)==T){dir.create(rdslocation.BinnedMatchedEDA.participant,recursive = T)}
    saveRDS(EDA_MERGED_PARTICIPANT,file=paste(rdslocation.BinnedMatchedEDA.participant,"EDA_merged_binned_",NUMB,".RDS",sep=""))
    }





  ### on entire dataset
  names(EDA_Binned_Merged)<-c("ID","PressTime","BeforeAfter","CaseControl","MinBeforeAfter","EDA_raw","EDA_filtered","EDA_FeatureScaled","EDA_Filtered_FeatureScaled")
  if(!dir.exists(rdslocation.BinnedMatchedEDA)==T){dir.create(rdslocation.BinnedMatchedEDA,recursive = T)}
  saveRDS(EDA_Binned_Merged,file=paste(rdslocation.BinnedMatchedEDA,"EDA_merged_binned_ALL.RDS",sep=""))

}



