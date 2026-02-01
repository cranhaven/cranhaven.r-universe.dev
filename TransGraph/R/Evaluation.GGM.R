#' Evaluation function for the estimated GGM.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @usage Evaluation.GGM(est.precision, true.precision)
#'
#' @description Evaluation function for the estimated GGM.
#' @param est.precision The estimated precision matrix.
#' @param true.precision The true precision matrix.
#'
#'
#' @return A result list including Recall, FDR, F1score, MCC, Hamming Distance,and estimated error of adjacency matrix on F-norm.
#'
#' @export
#'
#' @import EvaluationMeasures
#'
#'
#'

Evaluation.GGM = function(est.precision, true.precision){

  p=ncol(true.precision)

  est.precision_01=est.precision
  est.precision_01[which(est.precision!=0)]=1

  true.precision_01=true.precision
  true.precision_01[which(true.precision!=0)]=1

  Fnorm=norm(est.precision-true.precision,type = "F")/norm(true.precision, type = "F")
  Recall=ifelse(sum(true.precision_01), sum(true.precision_01*est.precision_01)/sum(true.precision_01), 0) #recall
  Precision=ifelse(sum(true.precision_01), sum(true.precision_01*est.precision_01)/(sum(est.precision_01)+1e-6), 0) # precision
  F1score=2*Recall*Precision/(Recall+Precision+1e-6)  # F1 score

  FDR=ifelse(sum(est.precision_01), 1 - sum(true.precision_01*est.precision_01)/sum(est.precision_01), 0) #FDR

  Eval_result=c(Recall,FDR,F1score,Fnorm)

  Eval_result=as.data.frame(t(Eval_result))
  names(Eval_result) = c("Recall","FDR","F1score","Fnorm")
  return(Eval_result)
}


