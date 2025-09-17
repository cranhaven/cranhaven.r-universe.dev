#' Param: Area below the curve
#'
#' @description Calculates the area under the germination or emergence curve. A parameter that can replace the traditional emergence or germination speed index.
#' @param dados data.frame containing the responses of the evaluations in separate columns side by side and without the columns with the identification of the factors
#' @param trat vector of treatments with n repetitions
#' @param nrep Number of repetitions
#' @param time vector containing time
#' @return Returns a vector with the index
#'
#' @export
#' @examples
#' data("substrate")
#' aac(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       time = 1:16)

aac=function(dados,trat,nrep,time){
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
        xp=seq(min(temp1),max(temp1),length=1000)
        yp=predict(mod,newdata = data.frame(temp1=xp))
        x=c(xp,xp[1000],xp[1])
        y=c(yp,0,yp[1])
        polygon <- st_sfc(st_polygon(list(cbind(x,y)))) %>%
          st_set_crs(32615)
        area[i]=st_area(polygon)}}
    area1[[a]]=area
    aac=unlist(area1)}
  aac=list(aac)[[1]]}
