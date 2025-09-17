#' Analysis: Logistic regression by treatment over time
#'
#' @description Performs the construction of a logistic regression graph by treatment over time
#' @param dados data.frame containing the responses of the evaluations in separate columns side by side and without the columns with the identification of the factors
#' @param trat vector of treatments with n repetitions
#' @param nrep Number of repetitions
#' @param time vector containing time
#' @param n total seeds per repetition
#' @param model logistic model according to drc package
#' @param ylab y-axis name
#' @param xlab x-axis name
#' @param legend.position Legend position
#'
#' @return Returns a logistic regression graph by treatment over time.
#' @export
#' @examples
#' data("substrate")
#' curve(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       n=10,
#'       time = 1:16)

curve=function(dados,
               trat,
               nrep,
               time,
               n,
               model=LL.3(),
               ylab="Emergence (%)",
               xlab="Time (days)",
               legend.position=c(0.2,0.8)){
  requireNamespace("drc")
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  # deviance
  if(is.null(trat)==TRUE){
    #n=length(colnames(dados))
    resp=unlist(dados)
    temp1=rep(time,e=length(rownames(dados)))
    resp1=resp
    data=data.frame(temp1,
                    resp1)
    mod=drm((resp*100/n)~temp1,fct=model)
    xp=seq(min(temp1),max(temp1),length=500)
    yp=predict(mod,newdata = data.frame(temp1=xp))
    data=data.frame(xp,yp)
    graph=ggplot(data,aes(x=xp,y=yp))+geom_line()+
      theme_bw()+theme(axis.text = element_text(size=12,color="black"))+
      labs(x=xlab,y=ylab)
    summary(mod)
    print(graph)}
  if(is.null(trat)==FALSE){
    nc=length(colnames(dados))
    resp=unlist(dados)
    temp1=rep(time,e=length(trat))
    trat=rep(trat,nc)
    resp1=resp
    data=data.frame(trat,
                    temp1,
                    resp1)
    ntrat=length(unique(trat))
    yp=as.list(1:ntrat)
    coefs=(1:ntrat)
    xp=seq(min(temp1),max(temp1),length=300)
    for(i in 1:ntrat){
      d1=data[trat==unique(trat)[i],]
      mod=drm(((d1$resp1*100)/n)~d1$temp1,data = d1,fct=model)
      coefs[i]=paste("b = ",
                  round(mod$coefficients[1],4),
                  "; d =",
                  round(mod$coefficients[2],4),
                  "; e = ",
                  round(mod$coefficients[3],4))
      yp[[i]]=predict(mod,newdata = data.frame(temp1=xp))}
    yp=unlist(yp)
    xp=rep(xp,ntrat)
    trat1=rep(unique(trat),e=length(xp)/ntrat)
    data1=data.frame(trat1,xp,yp)
    graph=ggplot(data1,aes(x=xp,y=yp,color=trat1,group=trat1))+
      geom_line(size=0.8)+
      scale_color_discrete(label=paste(unique(trat)," (",coefs,")",sep=""))+
      theme_bw()+theme(axis.text = element_text(size=12,color="black"),
                       legend.position = legend.position)+
      labs(x=xlab,y=ylab,color="")
    print(graph)
  }}
