#' Plot Receiver Operating Curves
#'

"roc_curve" <-
  function(out, truth) {
    
    pred<-prediction(out[,3],truth[,3])
    curve<-performance(pred,"tpr","fpr")
    plot(curve,main="ROC Curve",xlab="False Positive Rate",ylab="True Positive Rate",lwd=3)
    
    out<-performance(pred,"auc")@y.values[[1]]
    return(out)
  }