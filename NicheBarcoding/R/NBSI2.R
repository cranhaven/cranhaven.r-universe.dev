

#' Niche-model-Based Species Identification (NBSI) for a prior analysis
#'
#' @description If users already have species identified by other barcodes or
#' methods, they can use this function given the identified species names
#' and corresponding probabilities to make further confirm by environmental
#' niche model.
#'
#' @param  ref.infor Data frame, reference dataset containing sample IDs, taxon
#' information,longitude and latitude of each sample.
#' @param  que.infor Data frame, query samples,containing sample IDs,longitude
#' and latitude of each sample.
#' @param  barcode.identi.result Data frame, species identifications by other
#' methods or barocodes, containing query IDs, species identified, and
#' corresponding probabilities.
#' @param  model Character, string indicating which niche model will be used.
#' Must be one of "RF" (default) or "MAXENT". "MAXENT" can only be applied when
#' the java program paste(system.file(package="dismo"), "/java/maxent.jar",
#' sep='') exists.
#' @param  variables Character, the identifier of selected bioclimate variables.
#' Default of "ALL" represents to use all the layers in en.vir; the alternative
#' option of "SELECT" represents to randomly remove the highly-correlated
#' variables (|r| larger than 0.9) with the exception of one layer.
#' @param  en.vir RasterBrick, the global bioclimate data output from
#' "raster::getData" function.
#' @param  bak.vir Matrix, bioclimate variables of random background points.
#' @param  ref.env  Data frame,containing reference sampleIDs, species names,
#' and a set of environmental variables collected by users.
#' @param  que.env  Data frame,containing query sampleIDs,and a set of
#' corresponding environmental variables collected by users.
#'
#' @return A dataframe of identifications for query samples and their
#' niche-based reliability.
#'
#' @keywords NBSI2
#' @export
#'
#' @author Cai-qing YANG (Email: yangcq_ivy(at)163.com) and Ai-bing ZHANG
#' (Email:zhangab2008(at)cnu.edu.cn), Capital Normal University (CNU), Beijing,
#' CHINA.
#'
#' @references Breiman, L. 2001. Random forests. Machine Learning 45(1):5-32.
#' @references Liaw, A. and M. Wiener. 2002. Clasification and regression by
#' randomForest. R News, 2/3:18-22.
#' @references Phillips, S.J., R.P. Anderson and R.E. Schapire. 2006. Maximum
#' entropy modeling of species geographic distributions. Ecological Modelling,
#' 190:231-259.
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
#' data(LappetMoths)
#' barcode.identi.result<-LappetMoths$barcode.identi.result
#' ref.infor<-LappetMoths$ref.infor
#' que.infor<-LappetMoths$que.infor
#'
#' if(class(en.vir) == "NULL"){
#'  NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
#'                   barcode.identi.result=barcode.identi.result,
#'                   model="RF",variables="SELECT",
#'                   en.vir=NULL,bak.vir=NULL)
#' }else{
#'  NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
#'                   barcode.identi.result=barcode.identi.result,
#'                   model="RF",variables="SELECT",
#'                   en.vir=en.vir,bak.vir=bak.vir)
#' }
#' NBSI2.out
#'
#' ref.env<-LappetMoths$ref.env
#' que.env<-LappetMoths$que.env
#'
#' NBSI2.out2<-NBSI2(ref.env=ref.env,que.env=que.env,
#'                   barcode.identi.result=barcode.identi.result,
#'                   model="RF",variables="ALL",
#'                   en.vir=en.vir,bak.vir=bak.vir)
#' NBSI2.out2


NBSI2<-function(ref.infor=NULL,que.infor=NULL,ref.env=NULL,que.env=NULL,
                barcode.identi.result,model="RF",variables="ALL",
                en.vir=NULL,bak.vir=NULL){

  spe.identified<-as.character(barcode.identi.result[,2])
  spe.identified.uniq<-unique(spe.identified);length(spe.identified.uniq)

  if (is.null(ref.infor) & is.null(ref.env) |
      is.null(que.infor) & is.null(que.env)){
    stop("Please check the input data!")

  }else if (is.null(ref.env) == FALSE & is.null(que.env) == FALSE){
    # Variable selection
    eff.samp.env<-ref.env[,-c(1:2)]
    que.vari<-que.env[,-c(1:3)]
    if (nrow(que.vari) == 1){
      que.vari<-que.vari[,colnames(que.vari) %in% colnames(eff.samp.env)]
      que.vari<-as.data.frame(que.vari)
    }else{
      que.vari<-apply(que.vari,MARGIN=2,as.integer)
      que.vari<-que.vari[,colnames(que.vari) %in% colnames(eff.samp.env)]
    }

  }else if (is.null(ref.env) == TRUE & is.null(que.env) == TRUE){
    spe.identified.uniq<-unique(ref.infor$species)

    ### Bioclimatic variables ####
    ref.range<-data.frame()
    for (su in 1:length(spe.identified.uniq)){
      prese.lonlat<-ref.infor[ref.infor$species %in% spe.identified.uniq[su],]
      range.lon=min(diff(range(prese.lonlat[,4])),(abs(min(prese.lonlat[,4])-(-180))+abs(max(prese.lonlat[,4])-180)))
      range.lat=diff(range(prese.lonlat[,5]))
      ref.range<-rbind(ref.range,cbind(range.lon,range.lat))
    }
    lon.mean<-mean(ref.range[-which(ref.range[,1] == 0),1])
    lat.mean<-mean(ref.range[-which(ref.range[,2] == 0),2])

    if (is.null(en.vir) == T){  #the parameter "en.vir" is not provided
      cat("Environmental layers downloading ... ")
      envir<-raster::getData("worldclim",download=TRUE,
                             var="bio",res=10) ##download=TRUE
      en.vir<-raster::brick(envir)
      cat("Done!\n")

      if (is.null(bak.vir) != T){  #the parameter "bak.vir" is provided
        warning("The random background points and their environmental
              variables will be recreated from the downloaded
              environmental layer!")
      }
      cat("Background extracting ... ")
      back<-dismo::randomPoints(mask=en.vir,n=5000,ext=NULL,extf=1.1,
                                excludep=TRUE,prob=FALSE,cellnumbers=FALSE,tryf=3,warn=2,
                                lonlatCorrection=TRUE)
      bak.vir<-raster::extract(en.vir,back)
      cat("Done!\n")

    }else{
      if (is.null(bak.vir) == T){  #the parameter "bak.vir" is not provided
        cat("Background extracting ... ")
        back<-dismo::randomPoints(mask=en.vir,n=5000,ext=NULL,extf=1.1,excludep=TRUE,
                                  prob=FALSE,cellnumbers=FALSE,tryf=3,warn=2,
                                  lonlatCorrection=TRUE)
        bak.vir<-raster::extract(en.vir,back)
        cat("Done!\n")
      }
    }

    samp.env<-data.frame()
    samp.points<-data.frame()
    for (su in 1:length(spe.identified.uniq)){
      prese.lonlat<-ref.infor[ref.infor$species %in% spe.identified.uniq[su],]
      present.points<-pseudo.present.points(prese.lonlat[,3:5],10,lon.mean,lat.mean,en.vir,map=F)
      present.points[,1]<-gsub("Simulation",present.points[1,1],present.points[,1]);present.points
      samp.points<-rbind(samp.points,present.points)

      prese.env<-raster::extract(en.vir,present.points[,2:3])
      if (!all(is.na(prese.env[,1])==FALSE)){
        nonerr.env<-prese.env[-which(is.na(prese.env[,1])==TRUE),]
        if (is.null(dim(nonerr.env))==TRUE){
          prese.env<-t(as.data.frame(nonerr.env))
          row.names(prese.env)=NULL
        }else if (dim(nonerr.env)[1]==0){
          stop ("Please check the coordinate of ",spe.identified.uniq[su]," !\n")
        }else{
          prese.env<-nonerr.env
        }
      }

      spe.env<-cbind(Species=as.character(spe.identified.uniq[su]),prese.env)
      samp.env<-rbind(samp.env,spe.env)
    }
    samp.env[,2:ncol(samp.env)]<-apply(samp.env[,2:ncol(samp.env)],FUN=as.integer,MARGIN=2)
    #head(samp.env);dim(samp.env)

    # Variable selection
    eff.samp.env<-samp.env
    if (variables == "SELECT"){
      for (eff in 1:ncol(eff.samp.env)){
        rs<-abs(stats::cor(eff.samp.env[,-1]))
        cor.vir<-c()
        for (r in 1:nrow(rs)){
          tmp<-rs[r,which(rs[r,] >= 0.9 & rs[r,] != 1)]
          if (length(tmp) != 0){
            cor.vir<-c(cor.vir,names(tmp))
          }
        }
        freq.vir<-table(cor.vir);freq.vir
        if (length(freq.vir) != 0){
          max.freq.vir<-names(freq.vir[freq.vir == max(freq.vir)])
          max.freq.vir<-max.freq.vir[sample(1:length(max.freq.vir),size=1)]
          eff.samp.env<-eff.samp.env[,-which(colnames(eff.samp.env)==max.freq.vir)]
        }else{
          break
        }
      }
      colnames(eff.samp.env)
      eff.list<-colnames(eff.samp.env)[-1]
    }

    que.vari<-raster::extract(en.vir,que.infor[,4:5])

    if (nrow(que.vari) == 1){
      que.vari<-que.vari[,colnames(que.vari) %in% colnames(eff.samp.env)]
      que.vari<-as.data.frame(t(que.vari))
    }else{
      que.vari<-apply(que.vari,MARGIN=2,as.integer)
      que.vari<-que.vari[,colnames(que.vari) %in% colnames(eff.samp.env)]
    }

  }else{
    stop("There is no matching ecological variable information between REF and QUE!")

  }

  ### Niche modeling ####
  model<-gsub("randomforest|RandomForest|randomForest","RF",model)
  model<-gsub("maxent|Maxent","MAXENT",model)

  # (1) Niche modeling and self-inquery of ref
  spe.niche<-list()
  niche.ref.prob<-list()
  for (siu in 1:length(spe.identified.uniq)){
    prese.env<-eff.samp.env[gsub(".+,","",eff.samp.env[,1]) %in% spe.identified.uniq[siu],-1]
    prese.env<-stats::na.omit(prese.env)
    if (dim(prese.env)[1]==1){
      prese.env<-rbind(prese.env,prese.env)
      warning ("The model may not be accurate because there is only one record of ",spe.identified.uniq[siu],"!\n")
    }

    if (is.null(ref.env) == FALSE & is.null(que.env) == FALSE){ #For user-defined variables
      ref.mean<-apply(prese.env,FUN=mean,MARGIN=2)
      ref.sd<-apply(prese.env,FUN=stats::sd,MARGIN=2)
      ref.range<-apply(prese.env,FUN=max,MARGIN=2)-apply(prese.env,FUN=min,MARGIN=2)
      for (rs in 1:length(ref.sd)){
        if (ref.sd[rs] == 0){
          ref.sd[rs]<-apply(eff.samp.env[,-1],FUN=stats::sd,MARGIN=2)[rs]
        }
        if (ref.range[rs] == 0){
          ref.range[rs]<-ref.sd[rs]*2
        }
      }
      q.01<-stats::qnorm(0.01,mean=ref.mean,sd=ref.sd)
      q.99<-stats::qnorm(0.99,mean=ref.mean,sd=ref.sd)

      bak.env<-matrix(nrow=5000,ncol=ncol(prese.env))
      for (en in 1:ncol(prese.env)){
        tmp.left<-stats::runif(2500,
                               min=(q.01[en]-2*ref.range[en]),
                               max=q.01[en]-ref.range[en])
        tmp.right<-stats::runif(2500,
                                min=q.99[en]+ref.range[en],
                                max=q.99[en]+2*ref.range[en])
        tmp.ab<-c(tmp.left,tmp.right)
        bak.env[,en]<-tmp.ab
      }
      colnames(bak.env)<-colnames(prese.env)

      if (model == "RF"){
        mod<-niche.Model.Build(prese=NULL,absen=NULL,
                               prese.env=prese.env,absen.env=NULL,
                               model="RF",
                               bak.vir=bak.env)
      }else if (model == "MAXENT"){
        mod<-niche.Model.Build(prese=NULL,absen=NULL,
                               prese.env=prese.env,absen.env=NULL,
                               model="MAXENT",
                               bak.vir=bak.env)
      }

    }else if (is.null(ref.env) == TRUE & is.null(que.env) == TRUE){
      if (model == "RF"){
        mod<-niche.Model.Build(prese=NULL,absen=NULL,
                               prese.env=prese.env,absen.env=NULL,
                               model="RF",
                               bak.vir=bak.vir,en.vir=en.vir)
      }else if (model == "MAXENT"){
        mod<-niche.Model.Build(prese=NULL,absen=NULL,
                               prese.env=prese.env,absen.env=NULL,
                               model="MAXENT",
                               bak.vir=bak.vir,en.vir=en.vir)
      }

    }
    spe.niche[[siu]]<-mod$model

    spe.var<-apply(prese.env,FUN=as.numeric,MARGIN=2)
    spe.var<-as.data.frame(spe.var)
    if (model == "RF"){
      ref.HSI<-stats::predict(mod$model,spe.var,type="prob")
      niche.ref.prob[[siu]]<-min(ref.HSI[,2])
    }else if (model == "MAXENT"){
      ref.HSI<-dismo::predict(mod$model,spe.var,args='outputformat=logistic')
      niche.ref.prob[[siu]]<-min(ref.HSI)
    }
  }
  #spe.niche
  niche.ref.prob

  # (2) Prediction of query samples and calculation of Prob(Sid)
  result<-data.frame()
  for(n in 1:nrow(que.vari)){
    if (is.null(ref.env) == FALSE & is.null(que.env) == FALSE){
      que.ID<-as.character(que.env[n,2])
      ref.index<-grep(que.ID,barcode.identi.result[,1])
      target.spe<-as.character(barcode.identi.result[ref.index,2])
      spe_in_ref<-as.character(ref.env[grep(target.spe,ref.env$species),]$species)
    }else{
      que.ID<-as.character(que.infor[n,2])
      ref.index<-grep(que.ID,barcode.identi.result[,1])
      target.spe<-as.character(barcode.identi.result[ref.index,2])
      spe_in_ref<-as.character(ref.infor[grep(target.spe,ref.infor$species),]$species)
    }

    if (length(spe_in_ref) == 0){
      warning ("The identified species ",target.spe,
               " doesn't exist in ref.infor!",
               " Skipping the niche-based procedure ",
               que.ID," ...\n")

      Pb<-as.numeric(as.character(barcode.identi.result[n,3]))
      res0<-cbind(Pb,ref.prob=NA,que.prob=NA,CF=NA,Pbe=NA,NicoB.prob=NA)
      res0<-cbind(as.character(barcode.identi.result[n,1]),target.spe,res0)
      result<-rbind(result,res0)

    }else{
      spe.index<-grep(paste(target.spe,"$",sep=""),spe.identified.uniq,fixed=F)  #the location of target species in spe.identified.uniq
      model.spe<-spe.niche[[spe.index]]  #the model of target species

      if (nrow(que.vari) == 1){
        que.niche<-as.data.frame(que.vari)
      }else{
        que.niche<-t(as.matrix(que.vari[n,]))
        que.niche<-as.data.frame(que.niche)
      }

      if (all(is.na(que.niche)) == TRUE){
        stop ("Please check the coordinate of ",que.ID," !\n")
      }else{
        if (model == "RF"){
          que.HSI<-stats::predict(model.spe,que.niche,type="prob")
          que.prob<-que.HSI[,2]
          ref.prob<-niche.ref.prob[[spe.index]]
        }else if (model == "MAXENT"){
          que.HSI<-dismo::predict(model.spe,que.niche,args='outputformat=logistic')
          que.prob<-que.HSI
          ref.prob<-niche.ref.prob[[spe.index]]
        }
      }

      # Calculation of probability
      Pb<-as.numeric(as.character(barcode.identi.result[n,3]))
      CF=que.prob/ref.prob
      NicoB.prob=Pb*CF
      if (CF > 1){ CF=1 }
      if (NicoB.prob > 1){ NicoB.prob=1 }

      res0<-cbind(Pb,ref.prob,que.prob,CF,Pbe=NA,NicoB.prob)
      res0<-cbind(as.character(barcode.identi.result[n,1]),target.spe,round(res0,4))
      result<-rbind(result,res0)
    }
  }
  colnames(result)<-c("queID","species.identified","P(Bk)","min(P(EK))","P(Ek)",
                      "Prob(Sid).TSI","Prob(Sid).CI.cor","Prob(Sid).CI.unc")
  result<-as.data.frame(result[,-7])

  return(result)
}

# The end of NBSI2 #

