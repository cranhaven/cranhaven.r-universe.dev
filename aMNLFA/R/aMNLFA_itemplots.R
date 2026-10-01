#' aMNLFA item plotting function
#'
#' This function generates plots of item endorsement by time, and by each covariate. This is necessary for determining which covariates to use in the MNLFA.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' \dontrun{
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
#'  the.list <- list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
#'  ob <- aMNLFA::aMNLFA.object(dir = wd, 
#'  mrdata = xstudy, 
#'  indicators = paste0("BIN_", 1:12),
#'  catindicators = paste0("BIN_", 1:12), 
#'  meanimpact = c("AGE", "GENDER", "STUDY"), 
#'  varimpact = c("AGE", "GENDER", "STUDY"), 
#'  measinvar = c("AGE", "GENDER", "STUDY"),
#'  factors = c("GENDER", "STUDY"),
#'  ID = "ID",
#'  thresholds = FALSE)
#'  
#'  aMNLFA.itemplots(ob)
#'  }

aMNLFA.itemplots<-function(input.object){

  dir = input.object$dir
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  mytime = input.object$time
  myfactors = input.object$factors
  myauxiliary = input.object$auxiliary
  myID = input.object$ID

  #Visualize indicators as a function of time and moderators
  #re-structure data to allow facet_wrap visualization
  if (!is.null(mytime)) indlong<-reshape2::melt(mrdata,id.vars=c(myauxiliary,mytime,myfactors),measure.vars=myindicators)
  if (is.null(mytime)) indlong<-reshape2::melt(mrdata,id.vars=c(myauxiliary,myfactors),measure.vars=myindicators)

  #attach(indlong) #Should not have attach in here -- downstream references resolved
  indlong$AvgItemResponse<-as.character(indlong$value)
  indlong$AvgItemResponse<-as.numeric(indlong$AvgItemResponse)
  if (!is.null(mytime)) indlong$time<-as.numeric(unlist(indlong[mytime]))
  if (!is.null(mytime)) indlong$time<-round(indlong$time,.1)
  #Changed the following line to comply with CRAN checks -- it was complaining about using "order" 
  mrdata<-plyr::arrange(mrdata,mrdata$ID)
  srdatacheck<-mrdata[!duplicated(mrdata[myID]),]
  N<-dim(srdatacheck)[1]
  min<-.01*N
  if (!is.null(mytime)) aggindlong<-with(indlong,aggregate(AvgItemResponse~variable+time,indlong,FUN="mean"))
  if (!is.null(mytime)) aggindlong2<-with(indlong,aggregate(AvgItemResponse~variable+time,indlong,FUN="length"))
  if (is.null(mytime)) aggindlong<-with(indlong,aggregate(AvgItemResponse~variable,indlong,FUN="mean"))
  if (is.null(mytime)) aggindlong2<-with(indlong,aggregate(AvgItemResponse~variable,indlong,FUN="length"))
  aggindlong$N<-aggindlong2$AvgItemResponse
  aggindlong<-aggindlong[which(aggindlong$N>min),]
  if (!is.null(mytime)) marg<-with(aggindlong,ggplot2::ggplot(aggindlong,ggplot2::aes(x=time,y=AvgItemResponse)))+ with(aggindlong,ggplot2::facet_wrap(~ variable,nrow=1)) + with(aggindlong,ggplot2::geom_point(ggplot2::aes(size=N)))+ with(aggindlong,ggplot2::stat_smooth(se=FALSE)) + with(aggindlong,ggplot2::theme_bw()) + with(aggindlong,labs(title="Average Indicator Responses over Time")) + with(aggindlong,ggplot2::theme(legend.position="bottom"))
  #plot for each moderator
  l<-length(myfactors)

  if (!is.null(mytime)) for (i in 1:l){
    p<-NULL
    keep<-c("AvgItemResponse","time","variable",myfactors[i])
    indlongmod<-indlong[keep]
    names(indlongmod)[4]<-"Moderator"
    ic<-indlongmod$Moderator=="."|is.na(indlongmod$Moderator)|indlongmod$Moderator=="NA"
    cc_long<-indlongmod[which(ic=="FALSE"),]
    aggindlongmod<-with(cc_long,aggregate(AvgItemResponse~variable+time+Moderator,cc_long,FUN="mean"))
    aggindlongmod_2<-with(cc_long,aggregate(AvgItemResponse~variable+time+Moderator,cc_long,FUN="length"))
    aggindlongmod$N<-with(cc_long,aggindlongmod_2$AvgItemResponse)
    title<-paste("Average Indicator Responses over Time by ",myfactors[i],sep="")
    aggindlongmod$Moderator<-as.factor(aggindlongmod$Moderator)
    p<-with(aggindlongmod,ggplot2::ggplot(aggindlongmod,ggplot2::aes(x=time,y=AvgItemResponse)))+ with(aggindlongmod,ggplot2::facet_wrap(~ variable,nrow=round(sqrt(length(myfactors))))) + with(aggindlongmod,ggplot2::geom_point(ggplot2::aes(size=N,colour=Moderator)))+ with(aggindlongmod,ggplot2::stat_smooth(se=FALSE,ggplot2::aes(colour=Moderator))) + with(aggindlongmod,ggplot2::theme_bw())+ with(aggindlongmod,ggplot2::labs(title=title)) + with(aggindlongmod,ggplot2::theme(legend.position="bottom")) + with(aggindlongmod,ggplot2::guides(size=FALSE))

    filename<-fixPath(file.path(dir,paste("itemplots",myfactors[i],".png",sep="")))
    grDevices::png(filename=filename,
        units="in",
        width=11,
        height=8.5,
        pointsize=12,
        res=72)
    print(p)
    grDevices::dev.off()
  }


  if (is.null(mytime)) for (i in 1:l){
    for (j in 1:length(myindicators)){
      p<-NULL
      indlong2<-indlong[which(indlong$variable==myindicators[j]),]
      keep<-c("value",myfactors[i])
      indlongmod<-indlong2[keep]
      names(indlongmod)[2]<-"Moderator"
      ic<-indlongmod$Moderator=="."|is.na(indlongmod$Moderator)|indlongmod$Moderator=="NA"
      cc_long<-indlongmod[which(ic=="FALSE"),]
      title<-paste(myindicators[j],"Responses Distribution by ",myfactors[i],sep="")
      #For some reason the following line was coded as a boxplot; VC changed it on 10/4/2021
      p<-with(cc_long,ggplot2::ggplot(cc_long,ggplot2::aes(y = value, x = factor(Moderator)))) + with(cc_long,ggplot2::geom_bar(stat = "identity")) + with(cc_long,ggplot2::theme_bw()) + with(cc_long,ggplot2::labs(title=title)) + with(cc_long,ggplot2::theme(legend.position="bottom"))
      filename<-fixPath(file.path(dir,paste(myindicators[j]," plots",myfactors[i],".png",sep="")))
      grDevices::png(filename=filename,
          units="in",
          width=11,
          height=8.5,
          pointsize=12,
          res=72)
      print(p)
      grDevices::dev.off()
    }}

  message("Check '", dir, "/' for png file with item plots")
}
