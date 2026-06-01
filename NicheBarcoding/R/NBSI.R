

#' Niche-model-Based Species Identification (NBSI)
#'
#' @description Species identification using DNA barcoding integrated with
#' niche model.
#'
#' @param  ref.seq DNAbin, the reference dataset containing sample IDs,
#' taxon information,longitude and latitude, and barcode sequences of samples.
#' @param  que.seq DNAbin, the query dataset containing sample IDs, longitude
#' and latitude, and barcode sequences of samples.
#' @param  model Character, string indicating which niche model will be used.
#' Must be one of "RF" (default) or "MAXENT". "MAXENT" can only be applied when
#' the java program paste(system.file(package="dismo"), "/java/maxent.jar",
#' sep='') exists.
#' @param  independence Logical. Whether the barcode sequences are related to
#' the ecological variables?
#' @param  ref.add Data.frame, the additional coordinates collected from GBIF
#' or literatures.
#' @param  variables Character, the identifier of selected bioclimate variables.
#' Default of "ALL" represents to use all the layers in en.vir; the alternative
#' option of "SELECT" represents to randomly remove the highly-correlated
#' variables (|r| larger than 0.9) with the exception of one layer.
#' @param  en.vir RasterBrick, the global bioclimate data output from
#' "raster::getData" function.
#' @param  bak.vir Matrix, bioclimate variables of random background points.
#'
#' @return A dataframe of barcoding identification result for each query sample
#' and corresponding niche model-based probability.
#'
#' @keywords NBSI
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
#' @references Zhang, A.B., M.D. Hao, C.Q. Yang and Z.Y. Shi. (2017). BarcodingR:
#' an integrated R package for species identification using DNA barcodes.
#' Methods in Ecology and Evolution, 8:627-634.
#' @references Jin, Q., H.L. Han, X.M. Hu, X.H. Li, C.D. Zhu, S.Y.W. Ho, R.D. Ward
#' and A.B. Zhang. 2013. Quantifying species diversity with a DNA barcoding-based
#' method: Tibetan moth species (Noctuidae) on the Qinghai-Tibetan Plateau.
#' PloS One, 8:e644.
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
#' library(ape)
#' data(LappetMoths)
#' ref.seq<-LappetMoths$ref.seq[1:50,]
#' que.seq<-LappetMoths$que.seq[1:5,]
#' NBSI.out<-NBSI(ref.seq,que.seq,ref.add=NULL,
#'                independence=TRUE,
#'                model="RF",variables="SELECT",
#'                en.vir=en.vir,bak.vir=bak.vir)
#' NBSI.out
#'
#' ### Add a parameter when additional reference coordinates are available ###
#' #ref.add<-LappetMoths$ref.add
#' #NBSI.out2<-NBSI(ref.seq,que.seq,ref.add=ref.add,
#' #                independence=TRUE,
#' #                model="RF",variables="SELECT",
#' #                en.vir=en.vir,bak.vir=bak.vir)
#' #NBSI.out2


NBSI<-function(ref.seq,que.seq,model="RF",independence=TRUE,ref.add=NULL,
               variables="ALL",en.vir=NULL,bak.vir=NULL){

  ref.infor<-extractSpeInfo(rownames(ref.seq))
  que.infor<-extractSpeInfo(rownames(que.seq))

  ### Species identification ####
  Bayesian.all.prob<-function(ref,que,all.ref.sp){
    #rq<-rbind(ref,que)
    #rq<-rq[,ape::seg.sites(rq)]

    #ref2<-as.data.frame(as.character(rq[1:nrow(ref),]))
    #que2<-as.data.frame(as.character(rq[-c(1:nrow(ref)),]))
    #rq2<-rbind(ref2,que2)

    #ref3<-cbind(ref2,species=gsub(".+,","",sampleSpeNames))
    #rownames(ref3)<-1:nrow(ref3)
    #que3<-que2

    ref2<-as.data.frame(as.character(ref))
    que2<-as.data.frame(as.character(que))

    ref3<-cbind(ref2,species=gsub(".+,","",rownames(ref)))
    rownames(ref3)<-1:nrow(ref3)
    que3<-cbind(que2,species=gsub(".+,","",rownames(que)))

    Bayesian.trained <- e1071::naiveBayes(species ~ ., data = ref3)
    spe.inferred<-stats::predict(Bayesian.trained, que3)
    spe.inferred.prob<-stats::predict(Bayesian.trained, que3, type = "raw")

    all.prob<-t(spe.inferred.prob)
    all_prob<-list()
    for (sip in 1:nrow(que)){
      all_prob[[sip]]<-data.frame(usp=rownames(all.prob),tmp.sip=all.prob[,sip])
      rownames(all_prob[[sip]])<-1:length(all.ref.sp)
    }
    all_prob

    Bayesian.prob<-apply(spe.inferred.prob,1,max)

    spe.inferred<-as.character(spe.inferred)

    output.identified<-cbind(queryID=rownames(que),
                             species.identified=as.character(spe.inferred),
                             barcoding.based.prob=Bayesian.prob)
    output.identified<-as.data.frame(output.identified)

    out<-list(output_identified=output.identified,all_prob=all_prob)


    class(out) <- c("BarcodingR")
    return(out)
  }


  row.names(ref.seq)<-gsub("\\,[0-9\\.\\ \\-]*$","",rownames(ref.seq));ref.seq
  row.names(que.seq)<-gsub("\\,[0-9\\.\\ \\-]*$","",rownames(que.seq));que.seq
  #sampleSpeNames<-attr(ref.seq,"dimnames")[[1]]
  #mpattern<-".+,"
  #Spp<-gsub(mpattern,"",sampleSpeNames)
  all.ref.sp<-unique(ref.infor[,3])
  bsi<-Bayesian.all.prob(ref.seq,que.seq,all.ref.sp)
  BSI.out<-bsi$output_identified
  all_prob<-bsi$all_prob

  BSI.out[,1]<-gsub(",unknown","",BSI.out[,1])
  BSI.out$barcoding.based.prob<-as.numeric(BSI.out$barcoding.based.prob)
#  BSI.out<-data.frame(queryID = gsub(",unknown","",rownames(que.seq)),
#                      species.identified = output_identified[,2],
#                      barcoding.based.prob = as.numeric(output_identified[,3])) ### 2021-10-07 11:04:23
  BSI.out


  ### Bioclimatic variables ####
  #ref.range<-data.frame()
  ref.range<-matrix(nrow=length(all.ref.sp),ncol=2) ### 2021-10-05 23:36:44
  prese.lonlat.all<-data.frame()
  for (su in 1:length(all.ref.sp)){
    prese.lonlat<-ref.infor[ref.infor$species %in% all.ref.sp[su],]

    if (is.null(ref.add) == F){
      tmp.add<-ref.add[ref.add$species %in% all.ref.sp[su],]
      if (nrow(tmp.add) != 0){
        colnames(tmp.add)[2:3]<-c("Lon","Lat")
        prese.lonlat<-rbind(prese.lonlat,cbind(no.=NA,seqIDs=NA,tmp.add))
      }
    }

    dups<-duplicated(round(prese.lonlat[,4:5],2))
    prese.lonlat<-prese.lonlat[!dups,]
    prese.lonlat.all<-rbind(prese.lonlat.all,prese.lonlat)

    range.lon=min(diff(range(prese.lonlat[,4])),
                  (abs(min(prese.lonlat[,4])-(-180))+abs(max(prese.lonlat[,4])-180)))
    range.lat=diff(range(prese.lonlat[,5]))
    #ref.range<-rbind(ref.range,cbind(range.lon,range.lat))
    ref.range[su,]<-cbind(range.lon=range.lon,range.lat=range.lat) #2021-10-05 23:50:52
  }
  ref.range<-as.data.frame(ref.range) ### 2021-10-05 23:51:05
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
  #samp.env<-matrix(ncol=length(names(en.vir))+1)
  samp.points<-data.frame()
  for (su in 1:length(all.ref.sp)){
    prese.lonlat<-prese.lonlat.all[prese.lonlat.all$species %in% all.ref.sp[su],]
    present.points<-pseudo.present.points(prese.lonlat[,3:5],10,
                                          lon.mean,lat.mean,en.vir,map=F)
    present.points[,1]<-gsub("Simulation",present.points[1,1],
                             present.points[,1]);present.points
    samp.points<-rbind(samp.points,present.points)

    prese.env<-raster::extract(en.vir,present.points[,2:3])
    if (!all(is.na(prese.env[,1])==FALSE)){
      nonerr.env<-prese.env[-which(is.na(prese.env[,1])==TRUE),]
      if (is.null(dim(nonerr.env))==TRUE){
        prese.env<-t(as.data.frame(nonerr.env))
        row.names(prese.env)=NULL
      }else if (dim(nonerr.env)[1]==0){
        stop ("Please check the coordinate of ",all.ref.sp[su]," !\n")
      }else{
        prese.env<-nonerr.env
      }
    }

    spe.env<-cbind(Species=as.character(all.ref.sp[su]),prese.env)
    samp.env<-rbind(samp.env,as.data.frame(spe.env))

  }
  #samp.env<-as.data.frame(samp.env[-1,]) ### 2021-10-06 00:04:58
  samp.env[,2:ncol(samp.env)]<-apply(samp.env[,2:ncol(samp.env)],
                                     FUN=as.integer,MARGIN=2)
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

  ### Niche modeling ####
  model<-gsub("randomforest|RandomForest|randomForest","RF",model)
  model<-gsub("maxent|Maxent","MAXENT",model)
  colnames(ref.infor)[3]<-gsub("Species|SPECIES","species",colnames(ref.infor)[3])

  # (1) Niche modeling and self-inquery of ref
  spe.niche<-list()
  niche.ref.prob<-list()
  for (siu in 1:length(all.ref.sp)){
    prese.env<-eff.samp.env[gsub(".+,","",eff.samp.env[,1]) %in% all.ref.sp[siu],-1]
    prese.env<-stats::na.omit(prese.env)
    if (dim(prese.env)[1]==1){
      prese.env<-rbind(prese.env,prese.env)
      warning ("The model may not be accurate because there is only one record of ",all.ref.sp[siu],"!\n")
    }

    if (model == "RF"){
      mod<-niche.Model.Build(prese=NULL,absen=NULL,prese.env=prese.env,
                             absen.env=NULL,model="RF",bak.vir=bak.vir,en.vir=en.vir)
      spe.niche[[siu]]<-mod$model
      spe.var<-apply(prese.env,FUN=as.numeric,MARGIN=2)
      spe.var<-as.data.frame(spe.var)
      #ref.HSI<-randomForest::predict(mod$model,spe.var,type="prob")
      ref.HSI<-stats::predict(mod$model,spe.var,type="prob")

      niche.ref.prob[[siu]]<-min(ref.HSI[,2])

    }else if (model == "MAXENT"){
      mod<-niche.Model.Build(prese=NULL,absen=NULL,prese.env=prese.env,
                             absen.env=NULL,model="MAXENT",bak.vir=bak.vir,en.vir=en.vir)
      spe.niche[[siu]]<-mod$model
      spe.var<-apply(prese.env,FUN=as.numeric,MARGIN=2)
      spe.var<-as.data.frame(spe.var)
      ref.HSI<-dismo::predict(mod$model,spe.var,args='outputformat=logistic')
      niche.ref.prob[[siu]]<-min(ref.HSI)
    }
  }
  #spe.niche
  niche.ref.prob

  # (2) Prediction of query samples and calculation of Prob(Sid)
  que.vari<-raster::extract(en.vir,que.infor[,4:5])
  if (all(is.null(que.vari)) == T | all(is.na(que.vari)) == T){
    stop("No variables can be extracted from que.infor!\n")
  }else if (nrow(que.vari) == 1){
    que.vari<-que.vari[,colnames(que.vari) %in% colnames(eff.samp.env)]
    que.vari<-t(as.data.frame(que.vari))
  }else{
    que.vari<-apply(que.vari,MARGIN=2,as.integer)
    que.vari<-que.vari[,colnames(que.vari) %in% colnames(eff.samp.env)]
  }

  #result<-data.frame()
  result<-matrix(nrow=nrow(que.vari),ncol=8) ### 2021-10-05 17:51:03
  for(n in 1:nrow(que.vari)){
    queID<-as.character(que.infor[n,2])
    target.spe<-as.character(BSI.out$species.identified[n])
    if (length(target.spe) == 0){
      stop(paste("Please check the barcoding identification of ", queID,"!\n",sep=""))
      print(BSI.out)
    }
    spe_in_ref<-as.character(ref.infor[grep(target.spe,ref.infor$species),]$species)

    if (length(spe_in_ref) == 0){
      warning ("The identified species ",target.spe, " doesn't exist in ref.infor!",
               " Skipping the niche-based procedure ", queID," ...\n")

      Pb<-as.numeric(as.character(BSI.out[n,3]))
      res0<-cbind(Pb,ref.prob=NA,que.prob=NA,CF=NA,Pbe=NA,NicoB.prob=NA)
      res0<-cbind(as.character(BSI.out[n,1]),target.spe,res0)
      #result<-rbind(result,res0)
      result[n,]<-res0

    }else{
      spe.index<-grep(paste(target.spe,"$",sep=""),all.ref.sp,fixed=F)
      model.spe<-spe.niche[[spe.index]]  #the model of target species

      que.niche<-t(as.matrix(que.vari[n,]))
      que.niche<-as.data.frame(que.niche)
      if (all(is.na(que.niche)) == TRUE){
        stop ("Please check the coordinate of ",queID," !\n")
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

        # (1) Calculation for Bayes Theorem
        sum.pe<-0
        b.prob<-all_prob[[n]]
        b.prob[,2]<-as.numeric(as.character(b.prob[,2]))
        for (pe in 1:length(all.ref.sp)){
          if (model == "RF"){
            que.pe.HSI<-stats::predict(spe.niche[[pe]],que.niche,type="prob")
            que.pe.prob<-que.pe.HSI[,2];que.pe.prob
          }else if (model == "MAXENT"){
            que.pe.HSI<-dismo::predict(spe.niche[[pe]],que.niche,
                                       args='outputformat=logistic')
            que.pe.prob<-que.pe.HSI;que.pe.prob
          }
          bsi.prob<-b.prob[as.character(b.prob[,1]) %in% as.character(all.ref.sp[pe]),2]
          if (length(bsi.prob) == 0) { bsi.prob<-0 }
          tmp.pe<-bsi.prob*que.pe.prob
          sum.pe=sum.pe+tmp.pe
        }

        Pe<-sum.pe
        if (Pe == 0) { Pe<-1e-07 }

        #Pb<-BSI.out[n,3]
        Pb<-as.numeric(BSI.out[n,3]) ### 2021-10-06 00:10:16
        Peb<-que.prob
        Pbe<-(Peb*Pb)/Pe

        # (2) Calculation of probability
        CF=que.prob/ref.prob
        NicoB.prob=Pb*CF
        if (CF > 1){ CF=1 }
        if (NicoB.prob > 1){ NicoB.prob=1 }
      }

      res0<-cbind(Pb,ref.prob,que.prob,CF,Pbe,NicoB.prob)
      #res0<-cbind(as.character(BSI.out[n,1]),target.spe,round(res0,4))
      res0<-cbind(queID,target.spe,round(res0,4)) ### 2021-10-05 9:33:31
      #result<-rbind(result,res0)
      #result<-rbind(result,as.data.frame(res0))
      result[n,]<-res0 ### 2021-10-05 17:51:03
    }
  }
  colnames(result)<-c("queID","species.identified","P(Bk)","min(P(EK))","P(Ek)",
                      "Prob(Sid).TSI","Prob(Sid).CI.cor","Prob(Sid).CI.unc")

  if (independence == TRUE){
    if (nrow(result) == 1){
      result<-as.data.frame(t(result[,-7]))
    }else{
      result<-as.data.frame(result[,-7])
    }

  }else if (independence == FALSE){
    if (nrow(result) == 1){
      result<-as.data.frame(t(result[,-8]))
    }else{
      result<-as.data.frame(result[,-8])
    }
  }

  return(result)
}

# The end of NBSI #
