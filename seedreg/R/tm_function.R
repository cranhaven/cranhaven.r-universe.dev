#' Param: Average time
#'
#' @description Calculates the average germination/emergence time according to Silva and Nakagawa (1995)
#' @param data data.frame containing the responses of the evaluations in separate columns side by side and without the columns with the identification of the factors
#' @param trat vector of treatments with n repetitions
#' @param nrep Number of repetitions
#' @param time vector containing time
#' @export
#' @return Returns the vector with the average time.
#' @references SILVA, J. B. C.; NAKAGAWA, J. Estudos de formulas para calculo de germinacao. Informativo ABRATES, Londrina, v. 5, n. 1, p. 62-73, 1995.
#' @examples
#' data("substrate")
#' tm(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       time = 1:16)

tm=function(data,trat,nrep,time){
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
  total=resp[temp1==unique(temp1)[n]]
  area1=as.list(seq(1,length(levels(as.factor(data$temp1)))))
  area1[[1]]=resp[temp1==unique(temp1)[1]]
  for(i in 2:n){
    area1[[i]]=resp[temp1==unique(temp1)[i]]-resp[temp1==unique(temp1)[i-1]]
  }
  resp=unlist(area1)
  tm=rowSums(data.frame(matrix(resp*temp1,ncol = n)))/total
  tm=list(tm)[[1]]}
