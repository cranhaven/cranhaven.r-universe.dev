#' Evaluation function for the estimated DAG.
#'
#' @author Ruixaun Zhao <ruixuanzhao2-c@my.cityu.edu.hk>.
#' @references Zhao, R., He X., and Wang J. (2022). Learning linear non-Gaussian directed acyclic graph with diverging number of nodes. Journal of Machine Learning Research.
#' @usage Evaluation.DAG(estimated.adjace, true.adjace, type.adj=2)
#'
#' @description Evaluation function for the estimated DAG.
#' @param estimated.adjace The target data, a n * p matrix, where n is the sample size and p is data dimension.
#' @param true.adjace The auxiliary data in K auxiliary domains, a list with K elements, each of which is a nk * p matrix, where nk is the sample size of the k-th auxiliary domain.
#' @param type.adj The type of adjacency matrix. 1: the entries of matrix contains just two value, 0 and 1, which indicate the existence of edges; 2 (default): the matrix also measures connection strength, and 0 means no edge.
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

Evaluation.DAG = function(estimated.adjace, true.adjace, type.adj=2){

  p=ncol(true.adjace)

  if (type.adj==1){
    true.adjace[which(true.adjace!=0)]=1

    Fnorm=NA
    Recall=ifelse(sum(true.adjace), sum(true.adjace*estimated.adjace)/sum(true.adjace), 0) #recall
    Precision=ifelse(sum(true.adjace), sum(true.adjace*estimated.adjace)/(sum(estimated.adjace)+1e-6), 0) # precision
    F1score=2*Recall*Precision/(Recall+Precision+1e-6)  # F1 score

    HM=hammingDistance(estimated.adjace,true.adjace)/(p*(p-1)) # normalized hammdis
    FDR=ifelse(sum(estimated.adjace), 1 - sum(true.adjace*estimated.adjace)/sum(estimated.adjace), 0) #FDR
    MCC=EvaluationMeasures::EvaluationMeasures.MCC(as.vector(true.adjace),as.vector(estimated.adjace)) #MCC
    if (is.nan(MCC)){MCC=0}
    Eval_result=c(Recall,FDR,F1score,MCC,HM,Fnorm)
  } else if (type.adj==2){
    estimated.adjace_01=estimated.adjace
    estimated.adjace_01[which(estimated.adjace!=0)]=1


    true.adjace_01=true.adjace
    true.adjace_01[which(true.adjace!=0)]=1

    Fnorm=norm(estimated.adjace-true.adjace,type = "F")/norm(true.adjace, type = "F")
    Recall=ifelse(sum(true.adjace_01), sum(true.adjace_01*estimated.adjace_01)/sum(true.adjace_01), 0) #recall
    Precision=ifelse(sum(true.adjace_01), sum(true.adjace_01*estimated.adjace_01)/(sum(estimated.adjace_01)+1e-6), 0) # precision
    F1score=2*Recall*Precision/(Recall+Precision+1e-6)  # F1 score

    HM=hammingDistance(estimated.adjace_01,true.adjace_01)/(p*(p-1)) # normalized hammdis
    FDR=ifelse(sum(estimated.adjace_01), 1 - sum(true.adjace_01*estimated.adjace_01)/sum(estimated.adjace_01), 0) #FDR
    MCC=EvaluationMeasures::EvaluationMeasures.MCC(as.vector(true.adjace_01),as.vector(estimated.adjace_01)) #MCC
    if (is.nan(MCC)){MCC=0}
    Eval_result=c(Recall,FDR,F1score,MCC,HM,Fnorm)
  }

  Eval_result=as.data.frame(t(Eval_result))
  names(Eval_result) = c("Recall","FDR","F1score","MCC","HM","Fnorm")
  return(list(Eval_result=Eval_result))
}
