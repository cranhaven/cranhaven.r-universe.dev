#' Graph: line chart
#'
#' @description Returns a graph with the frequencies of germinated or emerged seeds
#' @param dados data.frame containing the responses of the evaluations in separate columns side by side and without the columns with the identification of the factors
#' @param trat vector of treatments with n repetitions
#' @param nrep Number of repetitions
#' @param time vector containing time
#' @param nt total seeds per repetition
#' @param percentage y scale in percentage
#' @param ylab y-axis name
#' @param xlab x-axis name
#' @param legend.position Legend position
#' @return Returns a graph with the frequencies of germinated or emerged seeds.
#' @export
#' @examples
#' data("substrate")
#' lineplot(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       time = 1:16)

lineplot=function(dados,
                  trat,
                  nrep,
                  time,
                  ylab="Emergence",
                  xlab="Time (days)",
                  nt=NA,
                  percentage=FALSE,
                  legend.position=c(0.2,0.8)){
  requireNamespace("ggplot2")
  if(percentage==TRUE){dados=(dados*100)/nt
  ylab=paste(ylab,"(%)")}
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
  ggplot(data,aes(y=resp1,x=temp1,fill=trat,
                  color=trat,
                  group=trat))+
    stat_summary(geom="line",
                 fun.data = "mean_se",
                 size=0.8)+
    stat_summary(geom="pointrange",pch=21,
                 color="black",
                 fun.data = "mean_se",size=1)+
    theme_bw()+labs(x=xlab,y=ylab,
                    fill="",color="")+
    theme(axis.text = element_text(size=12),
          legend.text = element_text(size=12),
          legend.position = legend.position)
}
