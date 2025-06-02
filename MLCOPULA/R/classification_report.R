classification_report <- function(y_true,y_pred){
  tabla <- table(y_true,y_pred)
  acc <- sum(diag(tabla))/length(y_true)
  prec <- diag(tabla)/apply(tabla,2,sum)#Precicion
  recall <- diag(tabla)/apply(tabla,1,sum)#Recall 
  f1_score <- 2 * (prec * recall)/(prec + recall)
  metrics <- data.frame(prec,recall,f1_score)
  colnames(metrics) <- c("precision","recall","f1-score")
  mi <- mi_report(theta = aux_mi_report(y_true,y_pred))
  return(list(metrics = metrics, confusion_matrix = tabla,
              accuracy = acc, mutual_information = mi))
}
