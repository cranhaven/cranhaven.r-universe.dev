#' Accelerometer Processing Part 2: Extract and filter accelerometer data
#' This function will allow you to filter acceleromter data (based on the EDA signal) and add metrics like g and the normalized Euclidian distance from origin vector.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.EDA folder location where the RDS files from the first step of the EDA processing are (make sure that it ends in /)
#' @param rdslocation.acc folder location where the RDS files from the first step of the accelerometer processing are
#' @param rdslocation.acc_filtered folder location where you want the filtered acc files to go.
#' @keywords EDA
#' @export
#' @examples
#' E4.Acc_Process.part2.Filter_ConvertAcc(participant_list=c(1001),
#'                                        rdslocation.EDA=paste(system.file(package="E4tools"),
#'                                        "/extdata/output/raw_EDA/",sep=""),
#'                                        rdslocation.acc=paste(system.file(package="E4tools"),
#'                                        "/extdata/output/raw_acc/",sep=""),
#'                                        rdslocation.acc_filtered=paste(tempdir(),
#'                                        "/extdata/output/filtered_acc/",sep=""))

E4.Acc_Process.part2.Filter_ConvertAcc<-function(participant_list,rdslocation.EDA,rdslocation.acc,rdslocation.acc_filtered){

  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(rdslocation.EDA=="helper"){rdslocation.EDA<-get("rdslocation.EDA",envir=E4tools.env)}
  if(rdslocation.acc=="helper"){rdslocation.acc<-get("rdslocation.acc",envir=E4tools.env)}
  if(rdslocation.acc_filtered=="helper"){rdslocation.acc_filtered<-get("rdslocation.acc_filtered",envir=E4tools.env)}

  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))


    if(file.exists(paste(rdslocation.EDA,NUMB,"_EDA.rds",sep=""))==FALSE){
      message(paste("No EDA data for ",NUMB,". Was EDA part 1 not run or did it fail for this participant? Going on to next participant.",sep=""))
      next
    }

    if(file.exists(paste(rdslocation.acc,NUMB,"_acc.rds",sep=""))==FALSE){
      message(paste("No accelerometer data for ",NUMB,". Was accelerometer part 1a not run or did it fail for this participant? Going on to next participant.",sep=""))
      next
    }

    EDA_RAW<-readRDS(paste(rdslocation.EDA,NUMB,"_EDA.rds",sep=""))
    ACC_DATA<-readRDS(paste(rdslocation.acc,NUMB,"_acc.rds",sep=""))




    ACC_DATA$Reject<-0

    if(sum(EDA_RAW$EDA_reject)>0){

#### get EDA data to exclude ###
Exclude_List<-accelerometry::rle2(EDA_RAW$EDA_reject, indices = TRUE)
Exclude_List<-as.data.frame(Exclude_List)
Exclude_List<-Exclude_List[which(Exclude_List$value==1),]
Exclude_Times<-cbind(EDA_RAW$ts[1]+((Exclude_List$start*250)-250),EDA_RAW$ts[1]+((Exclude_List$stop*250)-250))
Exclude_Times<-as.data.frame(Exclude_Times)
names(Exclude_Times)<-c("Start","Stop")



TS_ALL<-ACC_DATA$ts
TS_EXCLUDE_ALL<-NULL

## gets a list of timestamps to exclude
for(EXC_ROW in 1:nrow(Exclude_Times)) {
  TS_EXCLUDE_SINGLE<-TS_ALL[TS_ALL>Exclude_Times[EXC_ROW,1] & TS_ALL<Exclude_Times[EXC_ROW,2]]
  TS_EXCLUDE_ALL<-c(TS_EXCLUDE_ALL,TS_EXCLUDE_SINGLE)
}
if(nrow(ACC_DATA[ACC_DATA$ts %in% TS_EXCLUDE_ALL,])>0){ACC_DATA[ACC_DATA$ts %in% TS_EXCLUDE_ALL,]$Reject<-1}

}
#### Convert to g ####
ACC_DATA$acc_x_g = (ACC_DATA$acc_x * 2) / 128
ACC_DATA$acc_y_g = (ACC_DATA$acc_y * 2) / 128
ACC_DATA$acc_z_g = (ACC_DATA$acc_z * 2) / 128

### " normalized Euclidian distance from origin vector"  -- from https://www.biorxiv.org/content/biorxiv/early/2017/09/01/183772.full.pdf
d=sqrt((max(ACC_DATA[ACC_DATA$Reject==0,]$acc_x_g))^2+(max(ACC_DATA[ACC_DATA$Reject==0,]$acc_y_g))^2+(max(ACC_DATA[ACC_DATA$Reject==0,]$acc_z_g))^2)

## d is only based off of non-rejected datapoints
ACC_DATA$Euc_Dist<-sqrt((ACC_DATA$acc_x_g/d)^2+(ACC_DATA$acc_y_g/d)^2+(ACC_DATA$acc_z_g/d)^2)

#add column with last X,Y,Z value
ACC_DATA<-suppressMessages(DataCombine::slide(data=ACC_DATA, Var="acc_x_g",NewVar="acc_x_g_minus1",slideBy=-1))
ACC_DATA<-suppressMessages(DataCombine::slide(data=ACC_DATA, Var="acc_y_g",NewVar="acc_y_g_minus1",slideBy=-1))
ACC_DATA<-suppressMessages(DataCombine::slide(data=ACC_DATA, Var="acc_z_g",NewVar="acc_z_g_minus1",slideBy=-1))

ACC_DATA<-suppressMessages(DataCombine::slide(data=ACC_DATA, Var="ts",NewVar="ts_1",slideBy=-1))




#Prepare vector of differences from current to last X,Y,Z value
ACC_DATA$dd<-pmax(abs(ACC_DATA$acc_x_g-ACC_DATA$acc_x_g_minus1),
                  abs(ACC_DATA$acc_y_g-ACC_DATA$acc_x_g_minus1),
                  abs(ACC_DATA$acc_x_g-ACC_DATA$acc_x_g_minus1))

if(nrow(ACC_DATA[!is.na(ACC_DATA$dd) & (ACC_DATA$ts-ACC_DATA$ts_1)>50,])>0){ACC_DATA[!is.na(ACC_DATA$dd) & (ACC_DATA$ts-ACC_DATA$ts_1)>50,]$dd<-NA}



ACC_DATA$acc_x_g_minus1<-NULL
ACC_DATA$acc_y_g_minus1<-NULL
ACC_DATA$acc_z_g_minus1<-NULL

if(!dir.exists(rdslocation.acc_filtered)==TRUE){dir.create(rdslocation.acc_filtered,recursive=TRUE)}
filename<-paste(rdslocation.acc_filtered,NUMB,"_acc_filtered.rds",sep="")
saveRDS(ACC_DATA,file=filename)
}


}

