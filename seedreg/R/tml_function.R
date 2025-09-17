#' Param: Logistic average time
#' @param dados Data.frame containing the responses of the evaluations in separate columns side by side and without the columns with the identification of the factors
#' @param trat Vector of treatments with n repetitions
#' @param nrep Number of repetitions
#' @param time Vector containing time
#'
#' @return Returns the vector with the average time.
#' @export
#' @examples
#' data("substrate")
#' tml(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       time = 1:16)

tml=function(dados,trat,nrep,time){
  requireNamespace("drc")
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  requireNamespace("sf")

  n=length(colnames(dados))
  resp=unlist(dados)
  temp1=rep(time,e=length(trat))
  trat=rep(trat,n)
  temp=as.factor(temp1)
  resp1=resp
  block=rep(paste("B",1:nrep),
            length(unique(trat)),n)
  data=data.frame(trat,
                  bloco=block,
                  temp1,
                  temp,
                  resp1)
  area1=as.list(seq(1,length(levels(as.factor(data$trat)))))
  for(a in 1:length(levels(as.factor(data$trat)))){
    data1=data[data$trat==unique(data$trat)[a],]
    area=c(1:length(levels(as.factor(data1$bloco))))
    for(i in 1:length(levels(as.factor(data1$bloco)))){
      d1=data1[data1$bloco==unique(data1$bloco)[i],]
      if(d1$resp1[length(d1$trat)]=="0"){area[i]=0}else{
        mod=drm(resp1~temp1,fct=LL.3(),data=d1)
        area[i]=ED(mod,50,display = F)[1]}}
    area1[[a]]=area
    tml=unlist(area1)}
  tml=list(tml)[[1]]}

