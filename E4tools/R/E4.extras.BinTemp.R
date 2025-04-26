#' Temperature part 2: Make temperature bins
#'
#' Put temperature data in bins of X minutes length
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.temp folder location where raw temperature (from part 1) is saved.
#' @param rdslocation.binnedtemp folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @param BinLengthMin folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @keywords acc
#' @export
#' @examples
#' \dontrun{E4.extras.BinEDA(participant_list=c(1001:1004),rdslocation.binnedtemp="~/study/data/EDA/",
#'rdslocation.binnedtemp="~/study/data/Binned_EDA/",
#'BinLengthMin=2,
#'RejectFlag=TRUE)}


E4.Temp.part2.bin_temp<-function(participant_list,rdslocation.temp,rdslocation.binnedtemp,BinLengthMin){
  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(rdslocation.temp=="helper"){rdslocation.temp<-get("rdslocation.temp",envir=E4tools.env)}
  if(rdslocation.binnedtemp=="helper"){rdslocation.binnedtemp<-get("rdslocation.binnedtemp",envir=E4tools.env)}


###open data
  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))


    if(file.exists(paste(rdslocation.temp,NUMB,"_TEMP.rds",sep=""))==FALSE){
      message(paste("No data for ",NUMB,". Was EDA part 1 not run or did it fail for this participant? Going on to next participant.",sep=""))
      next
      }

    BinData<-readRDS(paste(rdslocation.temp,NUMB,"_TEMP.rds",sep=""))


##calculate metrics for bin sizes
BinLengthSamples<-(4*60*BinLengthMin)
tot_bins<-round(nrow(BinData)/BinLengthSamples)

###make bin numbers
BinData$bin<-rep(1:tot_bins,each=BinLengthSamples,length.out=nrow(BinData))



### aggregate across bins
if(length(BinData$TEMP_F)>0){BinnedTemp<-stats::aggregate(cbind(TEMP_C,TEMP_F)~(bin),data=BinData,FUN="mean")}
if(length(BinData$TEMP_F)==0){BinnedTemp<-stats::aggregate(cbind(TEMP_C)~(bin),data=BinData,FUN="mean")}

BinnedTS<-stats::aggregate(ts~(bin),data=BinData,FUN="min")

BinnedSerial<-stats::aggregate(E4_serial~(bin),data=BinData,FUN="max")

##merge into one file
Binned_Merged<-merge(BinnedTS,BinnedTemp,by="bin")
Binned_Merged<-merge(Binned_Merged,BinnedSerial,by="bin")
Binned_Merged$NUMB<-NUMB
if(length(BinData$TEMP_F)>0){Binned_Merged<-Binned_Merged[c("NUMB","E4_serial","ts","TEMP_C","TEMP_F")]}
if(length(BinData$TEMP_F)==0){Binned_Merged<-Binned_Merged[c("NUMB","E4_serial","ts","TEMP_C")]}
###save
if(!dir.exists(rdslocation.binnedtemp)==T){dir.create(rdslocation.binnedtemp,recursive=TRUE)}
filename<-paste(rdslocation.binnedtemp,NUMB,"_binnedEDA.rds",sep="")
saveRDS(Binned_Merged,file=filename)
}
}

