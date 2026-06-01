

#' Ecological niche model building using the randomForest classifier
#'
#' @description  Build a niche model for a given species according to its
#' distribution data.
#'
#' @param  prese Data frame, longitude and latitude of the present data of a
#' species (can be absent when providing prese.env parameter).
#' @param  absen Data frame, longitude and latitude of the absent data of a
#' species.(can be absent when providing absen.env or back parameter).
#' @param  prese.env Data frame, bioclimate variables of present data.
#' (can be absent when providing prese parameter).
#' @param  absen.env Data frame, bioclimate variables of absent data.
#' (can be absent when providing absen or back parameter).
#' @param  model Character, string indicating which niche model will be used.
#' Must be one of "RF" (default) or "MAXENT". "MAXENT" can only be applied when
#' the java program paste(system.file(package="dismo"), "/java/maxent.jar",
#' sep='') exists.
#' @param  en.vir RasterBrick, the global bioclimate data output from
#' "raster::getData" function.
#' @param  bak.vir Matrix, bioclimate variables of random background points.
#'
#' @return randomForest/MaxEnt, a trained niche model object.
#' @return A vector including the specificity, sensitivity and threshold of the
#' trained model.
#'
#' @keywords niche.Model.Build
#' @export
#'
#' @author Cai-qing YANG (Email: yangcq_ivy(at)163.com) and Ai-bing ZHANG
#' (Email:zhangab2008(at)cnu.edu.cn), Capital Normal University (CNU), Beijing,
#' CHINA.
#'
#' @references Breiman, L. 2001. Random forests. Machine Learning 45(1):5-32.
#' @references Liaw, A. and M. Wiener. 2002. Clasification and regression by
#' randomForest. R News, 2/3:18-22.
#' @references Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis.
#' 2005. Very high resolution interpolated climate surfaces for global land areas.
#' International Journal of Climatology, 25(15):1965-1978.
#'
#'
#' @examples
#' data(en.vir)
#' data(bak.vir)
#' #envir<-raster::getData("worldclim",download=FALSE,var="bio",res=2.5)
#' #en.vir<-raster::brick(envir)
#' #back<-dismo::randomPoints(mask=en.vir,n=5000,ext=NULL,extf=1.1,
#' #                          excludep=TRUE,prob=FALSE,
#' #                          cellnumbers=FALSE,tryf=3,warn=2,
#' #                          lonlatCorrection=TRUE)
#' #bak.vir<-raster::extract(en.vir,back)
#'
#' data<-data.frame(species=rep("Acosmeryx anceus",3),
#'                  Lon=c(145.380,145.270,135.461),
#'                  Lat=c(-16.4800,-5.2500,-16.0810))
#' present.points<-pseudo.present.points(data,10,2,1,en.vir)
#' NMB.out<-niche.Model.Build(prese=present.points,absen=NULL,
#'                            prese.env=NULL,absen.env=NULL,
#'                            model="RF",
#'                            en.vir=en.vir,bak.vir=bak.vir)
#' NMB.out
#'
#'
#' prese.env<-raster::extract(en.vir,present.points[,2:3])
#' prese.env<-as.data.frame(prese.env)
#' NMB.out2<-niche.Model.Build(prese=NULL,absen=NULL,
#'                             prese.env=prese.env,absen.env=NULL,
#'                             model="RF",
#'                             en.vir=en.vir,bak.vir=bak.vir)
#' NMB.out2


niche.Model.Build<-function(prese=NULL,absen=NULL,
                            prese.env=NULL,absen.env=NULL,
                            model="RF",
                            en.vir=NULL,bak.vir=NULL)
  {
  search.For.Diff.Absen.From.Prese<-function(prese,absen){
    eucl.dist.two.vect<-function(v1,v2){
      v1minusv2<-v1-v2

      squared.v1minusv2<-v1minusv2*v1minusv2
      out.sqrt<-sqrt(sum(squared.v1minusv2))

      return(out.sqrt)
    }

    ### 0.1 conversion of data type, and remove na data points!
    prese<-stats::na.omit(prese)
    absen<-stats::na.omit(absen)
    prese<-as.matrix(prese)
    absen<-as.matrix(absen)

    # 1 find out the centre of prese, by the function mean
    prese<-apply(prese,MARGIN=2,as.numeric)
    group.mean.prese<-apply(prese, MARGIN=2, mean, na.rm = T)

    # 2.calculate distance of each point of presence data to the center
    dist2center.prese<-apply(prese,1,eucl.dist.two.vect,v2=group.mean.prese)
    #dist2center.prese

    # 3.calculate 95%CI of the distance
    ci95<-stats::quantile(dist2center.prese,prob=c(0.025,0.975),na.rm = T)

    # 4.to calculate the distance of absence data to the center of presence data
    dist2center.absen<-apply(absen,1,eucl.dist.two.vect,v2=group.mean.prese)
    dist2center.absen

    # 5.to check if the distance of absence to the center is within the range of 95% CI
    within.CI95<-function(ci,x){
      if(x>=ci[1]&&x<=ci[2]) return(TRUE)
      else return (FALSE)
    }

    out2<-sapply(dist2center.absen, within.CI95,ci=ci95)
    out2
    return(out2)

  }

  model<-gsub("randomforest|RandomForest|randomForest","RF",model)
  model<-gsub("maxent|Maxent","MAXENT",model)

  if (is.null(prese.env) == T & is.null(en.vir) == T){  #the parameter "en.vir" is not provided
    cat("Environmental layers downloading ... ")
    envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10)
    en.vir<-raster::brick(envir)
    cat("Done!\n")
  }

  if (is.null(prese)==TRUE & is.null(prese.env)==FALSE){
    present.env0<-prese.env
    if (nrow(present.env0) < 3){
      warning ("prese.env has less than 3 records!\n")
    }
  }else{
    if (!is.data.frame(prese)|dim(prese)[2]!=3){
      stop ("The present data must be a dataframe with three columns
            (species name, lon, lat)!\n")
    }else{
      if (is.null(prese.env)==TRUE){
        if (nrow(prese) < 10){
          #present.points<-pseudo.present.points(data,10,10,2)
          prese<-pseudo.present.points(prese,10,10,2,en.vir=en.vir)
          ### updated on 2021-08-22 11:21:16
        }
        present.env0<-raster::extract(en.vir,prese[,2:3])
      }else{
        present.env0<-prese.env
        if (nrow(present.env0) < 3){
          warning ("prese.env has less than 3 records!\n")
        }
      }
    }
  }

  if (is.null(absen)==TRUE & is.null(absen.env)==TRUE){
    outputNum=nrow(present.env0)*10

    if (is.null(bak.vir)==TRUE){
      back<-dismo::randomPoints(mask=en.vir,n=outputNum*2,ext=NULL,
                                extf=1.1,excludep=TRUE,prob=FALSE,
                                cellnumbers=FALSE,tryf=3,warn=2,
                                lonlatCorrection=TRUE)
      bak.vir<-raster::extract(en.vir,back)
      bak0<-bak.vir[,colnames(bak.vir) %in% colnames(present.env0)]

      diff.absen.from.prese<-search.For.Diff.Absen.From.Prese(present.env0,bak0)
      diff<-bak0[which(diff.absen.from.prese==FALSE),]

      samp<-sample(dim(diff)[1],size=outputNum)
      absent.env0<-diff[samp,]
    }else{
      bak0<-bak.vir[,colnames(bak.vir) %in% colnames(present.env0)]

      if (nrow(bak.vir) < outputNum*2){
        more<-outputNum*2-nrow(bak.vir)
        more.back<-dismo::randomPoints(mask=en.vir,n=more,ext=NULL,extf=1.1,
                                       excludep=TRUE,prob=FALSE,cellnumbers=FALSE,
                                       tryf=3,warn=2,lonlatCorrection=TRUE)
        more.bak.vir<-raster::extract(en.vir,more.back)
        more.bak0<-more.bak.vir[,colnames(more.bak.vir) %in% colnames(present.env0)]
        bak0<-rbind(bak0,more.bak0)
      }

      diff.absen.from.prese<-search.For.Diff.Absen.From.Prese(present.env0,bak0)
      diff<-bak0[which(diff.absen.from.prese==FALSE),]

      if (is.null(nrow(diff)) || nrow(diff) == 0){
        ref.mean<-apply(present.env0,FUN=mean,MARGIN=2)
        ref.sd<-apply(present.env0,FUN=stats::sd,MARGIN=2)
        ref.range<-apply(present.env0,FUN=max,MARGIN=2)-apply(present.env0,FUN=min,MARGIN=2)
        for (rs in 1:length(ref.sd)){
          if (ref.sd[rs] == 0){ ref.sd[rs]<-ref.mean[rs]/4 }
          #ref.sd[rs]<-apply(eff.samp.env[,-1],FUN=stats::sd,MARGIN=2)[rs] ### eff.samp.env
          ### updated on 2021-08-22 11:38:26

          if (ref.range[rs] == 0){ ref.range[rs]<-ref.sd[rs]*2 }
        }
        q.01<-stats::qnorm(0.01,mean=ref.mean,sd=ref.sd)
        q.99<-stats::qnorm(0.99,mean=ref.mean,sd=ref.sd)

        absent.env0<-matrix(nrow=outputNum,ncol=ncol(present.env0))

        for (en in 1:ncol(present.env0)){ ### updated on 2021-08-22 11:23:21
          tmp.left<-stats::runif(outputNum/2,min=(q.01[en]-2*ref.range[en]),max=q.01[en]-ref.range[en])
          tmp.right<-stats::runif(outputNum/2,min=q.99[en]+ref.range[en],max=q.99[en]+2*ref.range[en])
          tmp.ab<-c(tmp.left,tmp.right)
          absent.env0[,en]<-as.integer(tmp.ab)
        }
        colnames(absent.env0)<-colnames(present.env0)
        warning ("The pseudoabsence data are randomly generated from the 95%CI of the presence data.\n")

      }else if(nrow(diff) < outputNum){
        absent.env0<-diff

      }else{
        samp<-sample(dim(diff)[1],size=outputNum)
        absent.env0<-diff[samp,]
      }
    }
  }else{
    if (is.null(absen.env)==TRUE){
      absent.env0<-raster::extract(en.vir,absen)
    }else{
      absent.env0<-absen.env
    }
  }

  present.env<-cbind(Count=1,present.env0)
  present.env<-as.data.frame(apply(present.env,FUN=as.numeric,MARGIN=2))
  absent.env<-cbind(Count=0,absent.env0)
  absent.env<-as.data.frame(apply(absent.env,FUN=as.numeric,MARGIN=2))

  Data<-as.data.frame(rbind(present.env,absent.env))
  Data$Count=as.factor(Data$Count)
  if (model == "RF"){
    mod<-randomForest::randomForest(Count ~., Data, ntree=500,
                                    importance=TRUE,
                                    na.action=randomForest::na.roughfix)
    use<-c(rep(1,nrow(present.env)),rep(0,nrow(absent.env)))
    prb<-c(stats::predict(mod,present.env,type="prob")[,2],
           stats::predict(mod,absent.env,type="prob")[,2])
  }else if (model == "MAXENT"){
    jar<-paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
    if (file.exists(jar)){### updated on 2021-08-21 17:04:13
      #if (file.exists(jar) & require(rJava)){
      mod<-suppressWarnings(dismo::maxent(Data[,-1],Data[,1],
                                          args='outputformat=logistic'))
      use<-c(rep(1,nrow(present.env)),rep(0,nrow(absent.env)))
      prb<-c(dismo::predict(mod,present.env,args='outputformat=logistic'),
             dismo::predict(mod,absent.env,args='outputformat=logistic'))
    }else{
      stop(paste("Please insure that the maxent.jar file have been placed into\n",
                 system.file(package="dismo"), "/java",sep=""))
    }
  }

  roc1<-pROC::roc(use,prb,precent=T,auc=T,plot=F,quiet=T)
  SST<-pROC::coords(roc1,x="best",ret=c("specificity","sensitivity","threshold"),
                    transpose=T)

  NMB<-list()
  NMB$model<-mod
  NMB$SST<-SST
  return(NMB)
}

# The end of niche.Model.Build #

