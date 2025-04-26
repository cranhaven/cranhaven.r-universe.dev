#' Set global file locations to make other functions easier
#'
#' This function will allow you to pre-define file locations that are used in multiple functions so you only have to type them once and so that your folder structure will be well-organized.
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param dataroot folder where you want your data to be stored.
#' @keywords acc
#' @export
#' @examples
#' \dontrun{E4.Acc_Process.Part1.ExtractRawAcc(participant_list=c(1001:1002),
#' ziplocation="~/documents/study/data/",
#' rdslocation.acc="~/documents/study/data/acc/")}

E4.Step0.FileHelper<-function(participant_list,ziplocation,dataroot){
  assign("ziplocation",ziplocation,envir=E4tools.env)
  assign("participant_list",participant_list,envir=E4tools.env)
  assign("rdslocation.EDA",paste(dataroot,"raw_data/EDA/",sep=""),envir=E4tools.env)
  assign("summarylocation",paste(dataroot,"metadata/summaries/",sep=""),envir=E4tools.env)
  assign("rdslocation.buttonpress",paste(dataroot,"raw_data/tags/",sep=""),envir=E4tools.env)
  assign("rdslocation.MatchedEDA",paste(dataroot,"matched_data/EDA_matched/",sep=""),envir=E4tools.env)
  assign("rdslocation.BinnedMatchedEDA",paste(dataroot,"matched_data/EDA_binned_matched/",sep=""),envir=E4tools.env)
  assign("rdslocation.acc",paste(dataroot,"raw_data/acc/",sep=""),envir=E4tools.env)
  assign("rdslocation.acc_filtered",paste(dataroot,"filtered_data/acc_filtered/",sep=""),envir=E4tools.env)
  assign("rdslocation.temp",paste(dataroot,"raw_data/temperature/",sep=""),envir=E4tools.env)
  assign("plotlocation.EDA",paste(dataroot,"plots/eda_plots/",sep=""),envir=E4tools.env)
  assign("plotlocation.temp",paste(dataroot,"plots/temp_plots/",sep=""),envir=E4tools.env)
  assign("rdslocation.binnedtemp",paste(dataroot,"binned_data/temp_binned/",sep=""),envir=E4tools.env)
  assign("rdslocation.binnedEDA",paste(dataroot,"binned_data/EDA_binned/",sep=""),envir=E4tools.env)
  assign("csvlocation.GGIRout",paste(dataroot,"raw_data/GGIR_out/",sep=""),envir=E4tools.env)
}



