#' To Get the Best Cutoff Value for ROC Curve
#' @description Youden index is used for seeking the best cutoff value for ROC Curve.
#' @param score continuous value
#' @param class bianary value, 0 and 1
#'
#' @return If the auc of a variate is lower than 0.5, we treat it as negative
#'     classification and return information about the negative prediction.
#'     Otherwise, The variate will be treated as positive one.
#' @export
#'
#' @examples
#' roc(score = mtcars$qsec,class = mtcars$am)
#' roc(score = mtcars$drat,class = mtcars$am)
roc <- function(score,class){
    roc.p=ROCit::rocit(score = score,class = class)
    if (roc.p$AUC>0.5){
        cutoff=roc.p$Cutoff[which.max(roc.p$TPR-roc.p$FPR)]
        sensitivity=roc.p$TPR[which.max(roc.p$TPR-roc.p$FPR)]
        specificity=1-roc.p$FPR[which.max(roc.p$TPR-roc.p$FPR)]
        df=data.frame(type='positive classification',
                      auc=round(roc.p$AUC,2),cutoff=cutoff,
                      sensitivity=sensitivity,specificity=specificity)
        return(df)
    }else{
        cutoff=roc.p$Cutoff[which.min(roc.p$TPR-roc.p$FPR)]
        sensitivity=roc.p$TPR[which.min(roc.p$TPR-roc.p$FPR)]
        specificity=1-roc.p$FPR[which.min(roc.p$TPR-roc.p$FPR)]
        df=data.frame(type='negative classification',
                      auc=1-round(roc.p$AUC,2),cutoff=cutoff,
                      sensitivity=1-sensitivity,specificity=1-specificity)
        return(df)
    }
}
