#' Comparison: correlation between parameters
#'
#' @description Correlation between the logistical model and the traditional model
#' @param seeds Object returned in the seeds function
#'
#' @import sf
#' @importFrom gridExtra grid.arrange
#' @import dplyr
#' @export
#' @return Returns correlation graphs between parameters calculated by traditional methods and by logistic regression
#'
#' @examples
#' data("substrate")
#' a=seeds(substrate[,c(3:18)],
#'       trat = substrate$Trat,
#'       nrep = 4,
#'       time = 1:16)
#'correl(a)

correl=function(seeds){
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  requireNamespace("gridExtra")
  requireNamespace("sf")
  a=seeds
  iv=a$iv
  aac=a$aac
  v=a$v
  vl=a$vl
  graph1=ggplot(a,aes(x=iv,y=aac))+
    labs(title=paste("Pearson correlation (r) = ",
                     format(round(cor(a$iv,a$aac),2),3)))+
    geom_point(size=4,pch=21,fill="blue")+
    theme_bw()+theme(axis.text = element_text(size=12,color="black"))+
    labs(y="AAC",x="IV")+geom_smooth(method = "lm",formula = y~x)
  graph2=ggplot(a,aes(x=tm,y=tml))+
    labs(title=paste("Pearson correlation (r) = ",
                     format(round(cor(a$tm,a$tml),2),3)))+
    geom_point(size=4,pch=21,fill="blue")+
    theme_bw()+theme(axis.text = element_text(size=12,color="black"))+
    labs(y="TML(days)",
         x="TM (days)")+geom_smooth(method = "lm",formula = y~x)
  graph3=ggplot(a,aes(x=v,y=vl))+
    labs(title=paste("Pearson correlation (r) = ",
                     format(round(cor(a$v,a$vl),2),3)))+
    geom_point(size=4,pch=21,fill="blue")+
    theme_bw()+theme(axis.text = element_text(size=12,color="black"))+
    labs(y="vl",x="v")+geom_smooth(method = "lm",formula = y~x)
  grid.arrange(graph1,graph2,graph3,layout_matrix=rbind(c(1,1,2,2),c(NA,3,3,NA)))
}

