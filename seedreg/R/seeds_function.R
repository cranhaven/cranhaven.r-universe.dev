#' Param: Seeds
#'
#' @description Simplification of functions: acc, iv, tm and tml.
#'
#' @param data Data.frame containing the responses of the evaluations in separate columns side by side and without the columns with the identification of the factors
#' @param trat Vector of treatments with n repetitions
#' @param nrep Number of repetitions
#' @param time Vector containing time
#' @export
#'
#' @return Returns a data.frame with the indices
#' @examples
#' data("substrate")
#' seeds(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       time = 1:16)

seeds=function(data,trat,nrep,time){
  iv=iv(data,trat,nrep,time)
  tm=tm(data,trat,nrep,time)
  aac=aac(data,trat,nrep,time)
  tml=tml(data,trat,nrep,time)
  v=1/tm
  vl=1/tml
  dataset=data.frame(iv=iv,tm=tm,v=v,aac=aac,tml=tml,vl=vl)
  rownames(dataset)=1:length(dataset$iv)
  dataset}
