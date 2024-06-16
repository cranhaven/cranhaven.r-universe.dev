#' Create power curves for EMA data
#'
#' This allows you to estimate power to detect an effect at three standard effect sizes (d = 0.2, 0.5, and 0.8). It uses the smpsize_lmm function from sjstats to generate data for the curves and ggplot2 to plot them.
#' @param NumbPart Total number of participants (i.e., level-2 unit)
#' @param NumbResp Total max number of responses per participant (e.g., number of days * number of responses per day). You can either enter this OR enter number of days and number of responses per day manually. If all are entered, it will default to NumbResp.
#' @param days Maximum number of days in study.
#' @param respday Maximum number of responses per day.
#' @param Est_ICC Estimated model ICC. Defaults to .05, but you should use a priori information from prior studies.
#' @param COL.8 Color of line for large (d=.8) effect size. Defualt is red, but you can specify colors by name or by hex code (make sure to put colors in quotation marks).
#' @param COL.5 Color of line for medium (d=.5) effect size. Defualt is blue, but you can specify colors by name or by hex code (make sure to put colors in quotation marks).
#' @param COL.2 Color of line for small (d=.2) effect size. Defualt is green, but you can specify colors by name or by hex code (make sure to put colors in quotation marks).
#' @return A ggplot object that displays power curves at three effect sizes (d=.2,.5,.8). You can use this like any other ggplot object (e.g., by adding other ggplot objects to it)
#' @keywords power analysis
#' @examples
#' \dontrun{ema.powercurve(NumbPart=80,days=30,respday=3)}
#'  \dontrun{ema.powercurve(NumbPart=80,NumbResp=200)}
#'   \dontrun{ema.powercurve(NumbPart=80,NumbResp=200,COL.8="orange")}
#'   \dontrun{ema.powercurve(NumbPart=80,NumbResp=200,COL.8="orange",COL.5="#FF5733",COL.3="#8E44AD")}


ema.powercurve=function(NumbPart,NumbResp,days,respday,Est_ICC=.05,COL.8="red",COL.5="blue",COL.2="green"){


  if(!missing(days) & !missing(respday)) {
    NumbResp<-days*respday
  } else {
    NumbResp<-NumbResp
  }

  ### initate matricies ####
  eff8a<-NULL;eff2a<-NULL;eff5a<-NULL

  #### functions for power curves ####

  for (PWR in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99)){
    eff8<-(sjstats::smpsize_lmm(eff.size = 0.8, power = PWR, sig.level = 0.05, k = NumbPart, icc = Est_ICC,n=NumbResp))
    eff8a<-as.data.frame(rbind(eff8a,eff8$`Subjects per Cluster`))
  }

  for (PWR in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99)){
    eff5<-(sjstats::smpsize_lmm(eff.size = 0.5, power = PWR, sig.level = 0.05, k = NumbPart, icc = Est_ICC,n=NumbResp))
    eff5a<-as.data.frame(rbind(eff5a,eff5$`Subjects per Cluster`))
  }


  for (PWR in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99)){
    eff2<-(sjstats::smpsize_lmm(eff.size = 0.2, power = PWR, sig.level = 0.05, k = NumbPart, icc = Est_ICC,n=NumbResp))
    eff2a<-as.data.frame(rbind(eff2a,eff2$`Subjects per Cluster`))
  }


  for (Add99 in c(10,20,30,40,50,60,70,75,80,85,90,95,100,105,110,115,120,125,130,140,150,160,170)){
    eff2a<-as.data.frame(rbind(eff2a,(eff2$`Subjects per Cluster`+Add99)))
    eff5a<-as.data.frame(rbind(eff5a,(eff5$`Subjects per Cluster`+Add99)))
    eff8a<-as.data.frame(rbind(eff8a,(eff8$`Subjects per Cluster`+Add99)))
  }

  ### merging curves ###
  power<-rbind(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99)

  ### creating response rate lines


  NumbRespColumn<-cbind(rep(c((NumbResp*.50),(NumbResp*.75),(NumbResp)),each=34))

  LabResp<-cbind(rep(c("50%","75%","100%"),each=34))

  comp_final<-as.data.frame(cbind((rbind(power,power,power)),NumbRespColumn,LabResp))
  colnames(comp_final)<-c("power","NumbRespColumn","Response_Rate")


  lg<-data.frame(cbind(power,eff8a,"Large (d=0.8)"));colnames(lg)<-c("Power","Resp","Effect_Size")
  md<-data.frame(cbind(power,eff5a,"Medium (d=0.5)"));colnames(md)<-c("Power","Resp","Effect_Size")
  sm<-data.frame(cbind(power,eff2a,"Small (d=0.2)"));colnames(sm)<-c("Power","Resp","Effect_Size")

  eff_final<-rbind(lg,md,sm)



  #### create ggplot ###

  xlab_chart <- paste("Responses per participant (n =",NumbPart,"participants)" )

  #  ggplot2::scale_x_continuous(limits = c(0,(round((NumbResp+40),-1))),breaks =seq(0, (round(NumbResp+40,-1)), by=20))+
  #round_any(NumbResp, 10, f = ceiling)

  if(NumbResp<=10) {Figure_X_Limit<-10}
  if (NumbResp>10 & NumbResp<=15) {Figure_X_Limit<-15}
  if (NumbResp>15 & NumbResp<=20) {Figure_X_Limit<-20}
  if(NumbResp>20) {Figure_X_Limit<-(plyr::round_any(NumbResp, 10, f = ceiling)+10)}

  if(max(eff_final[(eff_final$Resp<NumbResp & eff_final$Effect_Size=="Large (d=0.8)"),]$Power)==0.99) {
    eff_final<-rbind(eff_final,data.frame(Power=0.99,Resp=NumbResp,Effect_Size="Large (d=0.8)"))}

  if(max(eff_final[(eff_final$Resp<NumbResp & eff_final$Effect_Size=="Medium (d=0.5)"),]$Power)==0.99) {
    eff_final<-rbind(eff_final,data.frame(Power=0.99,Resp=NumbResp,Effect_Size="Medium (d=0.5)"))}



  PowerPlot1<-ggplot2::ggplot()+ ggplot2::geom_line(ggplot2::aes(x = Resp,y = Power,color=Effect_Size),size=1, data=eff_final[eff_final$Resp<=NumbResp,])+
    ggplot2::xlab(xlab_chart) +
    ggplot2::ylab("Power (1-beta)") +
    ggplot2::scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.00), limits=c(0.1,1.00))+
    ggplot2::geom_vline(xintercept=(NumbResp*.50),color="grey65", linetype = 3)+
    ggplot2::geom_vline(xintercept=(NumbResp*.75),color="grey65", linetype = 2)+
    ggplot2::scale_x_continuous(limits = c(0,Figure_X_Limit))+
    ggplot2::geom_vline(xintercept=(NumbResp),color="grey65", linetype = 1)+
    ggplot2::geom_line(ggplot2::aes(x = as.numeric(NumbRespColumn), y = as.numeric(power), linetype=Response_Rate), data=comp_final,color="grey65")+
    ggplot2::theme_classic() + ggplot2::scale_linetype(name="Completion rate") +
    ggplot2::scale_color_manual(name="Effect Size",values=c(COL.8,COL.5,COL.2))
  return(PowerPlot1)
}
