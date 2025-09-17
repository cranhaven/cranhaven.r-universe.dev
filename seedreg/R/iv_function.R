#' Param: Index for germination speed
#'
#' @description Calculates the emergence or germination speed index according to Maguire (1962)
#' @param data Data.frame containing the responses of the evaluations in separate columns side by side and without the columns with the identification of the factors
#' @param trat Vector of treatments with n repetitions
#' @param nrep Number of repetitions
#' @param time Vector containing time
#' @export
#' @return Returns the vector with the index
#' @references Maguire JD (1962). Seed of germination - aid in selection and evaluation  for  seedling  emergence  and  vigour.  J  Crop  Sci 2:176-177.
#' @examples
#' data("substrate")
#' iv(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       time = 1:16)

iv=function(data,trat,nrep,time){
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  n=length(colnames(data))
  resp=unlist(data)
  temp1=rep(time,e=length(trat))
  trat=rep(trat,n)
  trat=factor(trat,unique(trat))
  temp=as.factor(temp1)
  resp1=resp
  block=rep(paste("B",1:nrep),
            length(unique(trat)),n)
  data=data.frame(trat,
                  bloco=block,
                  temp1,
                  temp,
                  resp1)
  iv=rowSums(data.frame(matrix(resp/temp1,ncol = n)))
  iv=list(iv)[[1]]}
