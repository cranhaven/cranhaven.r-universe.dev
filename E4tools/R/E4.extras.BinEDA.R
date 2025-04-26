#' Extras: Make EDA bins
#'
#' Put EDA data in bins of X minutes length
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.EDA folder location where raw EDA (from part 1) is saved.
#' @param rdslocation.binnedEDA folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @param BinLengthMin folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @param RejectFlag Did you include in step 1 the option to keep the flag that shows which data the high and low pass filters rejected (By default, these are included in step 1) AND do you want to include a summary in this dataset of how many samples in a bin were rejected? If you want to run the diagnostic steps, you must keep this. Defaults to TRUE.
#' @keywords EDA
#' @export
#' @examples
#' E4.extras.BinEDA(participant_list=c(1001),
#' rdslocation.EDA=paste(system.file(package="E4tools"),"/extdata/output/raw_EDA/",sep=""),
#' rdslocation.binnedEDA=paste(tempdir(),"/extdata/output/binned_EDA/",sep=""),
#' BinLengthMin=2,RejectFlag=TRUE)


E4.extras.BinEDA<-function(participant_list,rdslocation.EDA,rdslocation.binnedEDA,BinLengthMin,RejectFlag=TRUE){
  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(rdslocation.EDA=="helper"){rdslocation.EDA<-get("rdslocation.EDA",envir=E4tools.env)}
  if(rdslocation.binnedEDA=="helper"){rdslocation.binnedEDA<-get("rdslocation.binnedEDA",envir=E4tools.env)}


  ###open data
  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))


    if(file.exists(paste(rdslocation.EDA,NUMB,"_EDA.rds",sep=""))==FALSE){
      message(paste("No data for ",NUMB,". Was EDA part 1 not run or did it fail for this participant? Going on to next participant.",sep=""))
      next
      }

    BinData<-readRDS(paste(rdslocation.EDA,NUMB,"_EDA.rds",sep=""))


##calculate metrics for bin sizes
BinLengthSamples<-(4*60*BinLengthMin)
tot_bins<-round(nrow(BinData)/BinLengthSamples)

###make bin numbers
BinData$bin<-rep(1:tot_bins,each=BinLengthSamples,length.out=nrow(BinData))

### aggregate across bins
BinnedEDA<-stats::aggregate(cbind(EDA_raw,EDA_HighLowPass,EDA_FeatureScaled,EDA_filtered,EDA_FeatureScaled_Filtered)~(bin),data=BinData,FUN="mean")
BinnedTS<-stats::aggregate(ts~(bin),data=BinData,FUN="min")
if(RejectFlag==TRUE){BinnedReject<-stats::aggregate(EDA_reject~(bin),data=BinData,FUN="sum")}
BinnedSerial<-stats::aggregate(E4_serial~(bin),data=BinData,FUN="max")

##merge into one file
Binned_Merged<-merge(BinnedTS,BinnedEDA,by="bin")
if(RejectFlag==TRUE){Binned_Merged<-merge(Binned_Merged,BinnedReject,by="bin")}
Binned_Merged<-merge(Binned_Merged,BinnedSerial,by="bin")
Binned_Merged$NUMB<-NUMB

###save
if(!dir.exists(rdslocation.binnedEDA)==T){dir.create(rdslocation.binnedEDA,recursive=TRUE)}
filename<-paste(rdslocation.binnedEDA,NUMB,"_binnedEDA.rds",sep="")
saveRDS(Binned_Merged,file=filename)
}
}

