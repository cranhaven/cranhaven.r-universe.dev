

#' Mantel test between interspecific pairwise genetic distance and ecological
#' distance
#'
#' @description Determine the independence between genetic distance and
#' ecological distance for a reference dataset at the level of species.
#'
#' @param  fas DNAbin, reference dataset containing sample IDs, taxon information,
#' longitude and latitude, and barcode sequences of samples.
#' @param  dna.model Character, specifying the evolutionary model to be used;
#' must be one of "raw" (default), "N", "TS", "TV", "JC69", "K80", "F81", "K81",
#' "F84", "BH87", "T92", "TN93", "GG95", "logdet", "paralin", "indel", or
#' "indelblock".
#' @param  ecol.dist.method Character, distance measure to be used;
#' must be one of "euclidean" (default), "maximum", "manhattan", "canberra",
#' "binary" or "minkowski".
#' @param  mantel.method Character, correlation method, as accepted by cor:
#' "pearson","spearman" (default) or "kendall".
#' @param  permutations Numeric, the number of permutations required.
#' @param  en.vir RasterBrick, the global bioclimate data output from
#' "raster::getData" function.
#'
#' @return The Mantel statistic.
#' @return The empirical significance level from permutations.
#' @return A matrix of interspecific pairwise genetic distance.
#' @return A matrix of interspecific pairwise ecological distance.
#'
#' @keywords spe.mantel.test
#' @export
#'
#' @author Cai-qing YANG (Email: yangcq_ivy(at)163.com) and Ai-bing ZHANG
#' (Email:zhangab2008(at)cnu.edu.cn), Capital Normal University (CNU), Beijing,
#' CHINA.
#'
#' @references Mantel N. 1967. The detection of disease clustering and a
#' generalized regression approach. Can. Res. 27:209-220.
#' @references Oksanen J., F.G. Blanchet, M. Friendly, R. Kindt, P. Legendre,
#' D. McGlinn, P.R. Minchin, R.B. O'Hara, G.L. Simpson, P. Solymos,
#' M.H.H. Stevens, E. Szoecs and H Wagner. 2016. vegan: Community Ecology
#' Package \url{https://CRAN.R-project.org/package=vegan}. r package version 2.5-6.
#'
#'
#' @examples
#' data(en.vir)
#' #envir<-raster::getData("worldclim",download=FALSE,var="bio",res=2.5)
#' #en.vir<-raster::brick(envir)
#'
#' library(ape)
#' data(LappetMoths)
#' ref.seq<-LappetMoths$ref.seq
#'
#' spe.mantel<-spe.mantel.test(fas=ref.seq,en.vir=en.vir)
#' spe.mantel$MantelStat.r
#' spe.mantel$p.value


spe.mantel.test<-function(fas,dna.model="raw",ecol.dist.method="euclidean",
                          mantel.method="spearman",permutations=999,en.vir=NULL){
  niche.stand<-function(x){
    (x-env.range["min",])/(env.range["max",]-env.range["min",])
  }

  if (is.null(en.vir) == T){  #the parameter "en.vir" is not provided
    cat("Environmental layers downloading ... ")
    #envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10,lon=lon,lat=lat)
    envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10)
    en.vir<-raster::brick(envir)
    cat("Done!\n")
  }

  env.range<-rbind(en.vir@data@min,en.vir@data@max)
  rownames(env.range)<-c("min","max");
  colnames(env.range)<-names(en.vir)
  env.range

  infor<-extractSpeInfo(labels(fas)) #;head(infor)
  unique<-unique(infor$species)
  nspe<-length(unique)

  result<-list()
  genet.matrix<-matrix(nrow=nspe,ncol=nspe)
  ecol.matrix<-matrix(nrow=nspe,ncol=nspe)
  for (spe1 in 1:(nspe-1)){
    cat (paste(">>> ",spe1,"/",nspe," ",as.character(unique[spe1]),"\n",sep=""))
    for (spe2 in (spe1+1):nspe){
      sp1.fas<-fas[infor[,3] %in% unique[spe1],]
      sp1.infor<-extractSpeInfo(labels(sp1.fas))
      sp1.vari<-raster::extract(en.vir,sp1.infor[,4:5])
      if (nrow(sp1.vari) == 1){
        sp1.vari<-apply(sp1.vari,FUN=as.numeric,MARGIN=2)
      }else{
        sp1.vari<-apply(apply(sp1.vari,FUN=as.numeric,MARGIN=2),FUN=mean,MARGIN=2)
      }
      sp1.vari<-niche.stand(sp1.vari)

      sp2.fas<-fas[infor[,3] %in% unique[spe2],]
      sp2.infor<-extractSpeInfo(labels(sp2.fas))
      sp2.vari<-raster::extract(en.vir,sp2.infor[,4:5])
      if (nrow(sp2.vari) == 1){
        sp2.vari<-apply(sp2.vari,FUN=as.numeric,MARGIN=2)
      }else{
        sp2.vari<-apply(apply(sp2.vari,FUN=as.numeric,MARGIN=2),FUN=mean,MARGIN=2)
      }
      sp2.vari<-niche.stand(sp2.vari)

      sel.fas<-rbind(sp1.fas,sp2.fas)
      genet.dist<-as.matrix(ape::dist.dna(sel.fas,dna.model))
      genet.dist12<-genet.dist[1:nrow(sp1.fas),(nrow(sp1.fas)+1):(nrow(sp1.fas)+nrow(sp2.fas))]
      if (length(which(is.nan(genet.dist12))) != 0){
        message ("Sequences of ",as.character(unique[spe1])," and ",as.character(unique[spe2]),
                 " are quite different, so that the model of dist.dna have been changed to \"raw\".\n")
        genet.dist<-as.matrix(ape::dist.dna(sel.fas,"raw"))
        genet.dist12<-genet.dist[1:nrow(sp1.fas),(nrow(sp1.fas)+1):(nrow(sp1.fas)+nrow(sp2.fas))]
      }
      genet.matrix[spe2,spe1]<-mean(genet.dist12)

      sel.envir<-rbind(sp1.vari,sp2.vari)
      ecol.dist<-as.matrix(stats::dist(sel.envir,method=ecol.dist.method))
      ecol.matrix[spe2,spe1]<-ecol.dist[2,1]
    }
  }
  row.names(genet.matrix)<-as.character(unique)
  colnames(genet.matrix)<-as.character(unique)
  row.names(ecol.matrix)<-as.character(unique)
  colnames(ecol.matrix)<-as.character(unique)

  test<-vegan::mantel(stats::as.dist(genet.matrix),stats::as.dist(ecol.matrix),
                      mantel.method,permutations);test

  result$MantelStat.r<-test$statistic
  result$p.value<-test$signif
  result$genet.matrix<-genet.matrix
  result$ecol.matrix<-ecol.matrix
  return (result)
}

# The end of spe.mantel.test #

