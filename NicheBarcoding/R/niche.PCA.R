

#' Principal component analysis of ecological niche among unknown species and
#' the potential species to which they may belong
#'
#' @description Determine whether unknown species belong to a known species
#' through principal component analysis of ecological niche according to their
#' distribution information.
#'
#' @param  ref.lonlat Data frame, longitude and latitude of the known species.
#' @param  que.lonlat Data frame, longitude and latitude of unknown species.
#' @param  en.vir RasterBrick, the globle bioclimate data obtained from
#' "raster::getData" function.
#'
#' @return A list containing inportance and loadings of the components.
#' @return A dataframe of points that within the 95% confidence interval of
#' the reference dataset ecological space.
#' @return A figure shows whether the query points (blue solid circles) are
#' located in the 95%CI range of the niche space of reference species.
#'
#' @keywords niche.PCA
#' @export
#'
#' @author Cai-qing YANG (Email: yangcq_ivy(at)163.com) and Ai-bing ZHANG
#' (Email:zhangab2008(at)cnu.edu.cn), Capital Normal University (CNU), Beijing,
#' CHINA.
#'
#'
#' @examples
#' data(en.vir)
#' #envir<-raster::getData("worldclim",download=FALSE,var="bio",res=2.5)
#' #en.vir<-raster::brick(envir)
#'
#' data(LappetMoths)
#' ref.infor<-LappetMoths$ref.infor
#' que.infor<-LappetMoths$que.infor
#'
#' #windows() # open a new plot window when the image format is abnormal
#' nPCA<-niche.PCA(ref.lonlat=ref.infor[,3:5],
#'                 que.lonlat=que.infor[,c(2,4:5)],
#'                 en.vir=en.vir)
#' nPCA$summary
#' nPCA$que.CI
#'
#'
#' data<-data.frame(species=rep("Acosmeryx anceus",3),
#'                  Lon=c(145.380,145.270,135.461),
#'                  Lat=c(-16.4800,-5.2500,-16.0810))
#' simuSites<-pseudo.present.points(data,500,4,2,en.vir)
#' ref.lonlat<-simuSites[1:480,]
#' que.lonlat<-simuSites[481:500,]
#'
#' #windows() # open a new plot window when the image format is abnormal
#' nPCA2<-niche.PCA(ref.lonlat,que.lonlat,en.vir=en.vir)
#' nPCA2$summary
#' nPCA2$que.CI


niche.PCA<-function(ref.lonlat,que.lonlat,en.vir=NULL){
  if (is.null(en.vir) == T){  #the parameter "en.vir" is not provided
    cat("Environmental layers downloading ... ")
    envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10)
    en.vir<-raster::brick(envir)
    cat("Done!\n")
  }

  nPCA<-list()
  n.vari<-length(names(en.vir))

  # ref.lonlat must be more than the number of variables!
  if (nrow(ref.lonlat) < n.vari){
    message(paste0("ref.lonlat must be more than ",n.vari," points!\n")) ### updated on 2021-10-07 11:00:21
    message("Generating the pseudo present points ......\n") ### updated on 2021-10-07 11:00:21
    ref.lonlat<-pseudo.present.points(ref.lonlat,n.vari,50,1,en.vir=en.vir)
  }

  # Extract the environment variables
  data.var<-raster::extract(en.vir,ref.lonlat[,2:3])
  que.var<-raster::extract(en.vir,que.lonlat[,2:3])

  # PCA analysis
  env.pr<-stats::princomp(data.var,cor=TRUE)
  summary<-summary(env.pr,loadings=TRUE)
  nPCA$summary<-summary

  # Calculate principal component according to loading coefficient
  data.pca1<-t(apply(data.var,FUN=function(x){sum(summary$loadings[1:n.vari,1]*x)},MARGIN=1))
  data.pca1<-scale(data.pca1[1,],center=T,scale=T)
  data.pca2<-t(apply(data.var,FUN=function(x){sum(summary$loadings[1:n.vari,2]*x)},MARGIN=1))
  data.pca2<-scale(data.pca2[1,],center=T,scale=T)
  que.pca1<-t(apply(que.var,FUN=function(x){sum(summary$loadings[1:n.vari,1]*x)},MARGIN=1))
  que.pca1<-scale(que.pca1[1,],center=T,scale=T)
  que.pca2<-t(apply(que.var,FUN=function(x){sum(summary$loadings[1:n.vari,2]*x)},MARGIN=1))
  que.pca2<-scale(que.pca2[1,],center=T,scale=T)

  # Show all ref points & que points
  #graphics::par(mar=c(6,5,5,8))
  #grDevices::win.graph()
  opar<-graphics::par(mar=c(6,5,5,8)) ### updated on 2021-10-07 10:57:29
  plot(x=data.pca1,y=data.pca2,
       xlab="PCA1",ylab="PCA2",
       main="PCA Analysis in Both Ref & Que")
  graphics::points(x=que.pca1,y=que.pca2,col="blue",pch=19)
  graphics::legend(x="right",
                   inset=-0.2,
                   col=c("black","blue"),
                   pch=c(1,19),
                   legend=c("Ref","Que"),
                   horiz=F,
                   xpd=T,
                   bty="o")

  # Calculate the 95%CI & draw them as boundaries
  CI.pca1<-stats::quantile(data.pca1,prob=c(0.025,0.975),na.rm=T)
  CI.pca2<-stats::quantile(data.pca2,prob=c(0.025,0.975),na.rm=T)
  graphics::abline(v=CI.pca1[1],col="red",lty=2)
  #graphics::text(x=CI.pca1[1],y=min(data.pca2),"2.5%",col="red")
  graphics::abline(v=CI.pca1[2],col="red",lty=2)
  #graphics::text(x=CI.pca1[2],y=min(data.pca2),"97.5%",col="red")
  graphics::abline(h=CI.pca2[1],col="red",lty=2)
  graphics::text(x=min(data.pca1)+0.2,y=CI.pca2[1],"2.5%",col="red")
  graphics::abline(h=CI.pca2[2],col="red",lty=2)
  #graphics::text(x=min(data.pca1)+0.2,y=CI.pca2[2],"97.5%",col="red")
  graphics::text(x=CI.pca1[2]-0.2,y=CI.pca2[2],"97.5%",col="red")

  index=which(que.pca1 < CI.pca1[1] | que.pca1 > CI.pca1[2] | que.pca2 < CI.pca2[1] | que.pca2 > CI.pca2[2])
  que.CI<-que.lonlat[-index,]
  nPCA$que.CI<-que.CI

  on.exit(graphics::par(opar)) ### updated on 2021-10-07 10:58:27

  return(nPCA)
}

# The end of niche.PCA #


