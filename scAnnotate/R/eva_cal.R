#' @title eva_cal
#'
#' @description calculate the F1 score of each cell population, mean of F1 score and overall accuracy
#'
#' @param prediction A vector of annotate cell type labels
#' @param cell_label A vector of original cell type labels
#'
#' @return A matrix contain the F1 score of each cell population, mean of F1 score and overall accuracy
#'
#' @export
#' @examples
#' data(predict_label)
#' data(pbmc2)
#' eva_cal(prediction = predict_label,cell_label = pbmc2[,1])
#'
eva_cal=function(prediction,cell_label){
  cellnames=names(table(cell_label))
  F1=matrix(NA,nrow = 1,ncol = length(cellnames)+2)
  colnames(F1)=c(cellnames,"mean F1 score","accuracy")
  for(jj in 1:length(cellnames)){
    tp=length(which(cell_label==cellnames[jj]&prediction==cellnames[jj]))#true positive
    fp=length(which(cell_label!=cellnames[jj]&prediction==cellnames[jj]))#false positive
    fn=length(which(cell_label==cellnames[jj]&prediction!=cellnames[jj]))#false negative
    precision=tp/(tp+fp)#precision
    recall=tp/(tp+fn)#recall
    F1[1,jj]=2*(precision*recall)/(precision+recall)
  }
  #average F1 error rate
  F1[1,][is.nan(F1[1,])]=0
  F1[1,length(cellnames)+1]=mean(F1[1,1:length(cellnames)])

  #accuracy:
  lab_c=table(prediction==cell_label)["TRUE"]
  accuracy=lab_c/length(cell_label)
  F1[1,length(cellnames)+2]=accuracy
  return(F1)
}

