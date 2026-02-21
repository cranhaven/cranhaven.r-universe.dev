#' @title Permutation
#' @description the permutation test method 1 and method 2 were used to calculate the statistical significance level for these optimal subpathways.
#' @param subpathwayz Optimize intersted subpathways
#' @param zz a vector of z-scores
#' @param nperm1 times of permutation to perform use method1
#' @param method1 permutation analysis method1
#' @param nperm2 times of permutation to perform use method2
#' @param method2 permutation analysis method2
#' @return the statistical significance p value and FDR for these optimal subpathways
#' @importFrom stats p.adjust
#' @importFrom stats pnorm
#' @importFrom stats sd
#' @importFrom graphite convertIdentifiers
#' @importFrom graphite pathways
#' @export
#' @examples
#' require(graphite)
#' keysubpathways<-GetExampleData("keysubpathways")
#' zzz<-GetExampleData("zzz")
#' \donttest{Permutation(keysubpathways,zzz,nperm1=10,method1=TRUE,nperm2=10,method2=FALSE)}
Permutation<-function(subpathwayz,zz,nperm1=1000,method1=TRUE,nperm2=1000,method2=FALSE){
  xx <-apply(subpathwayz[,c(2,5)],1,function(x) paste(x,collapse=";"));
  B <-which(duplicated(xx)==TRUE);
  if(length(B)!=0){
    subpathwayz <-subpathwayz[-B,];
  }
  if(method1==TRUE){
    ori.z <-as.numeric(subpathwayz[,5])
    Perm1 <-matrix(0,dim(subpathwayz)[1],nperm1)
    p1 <- rep(0,dim(subpathwayz)[1])
    for(i in 1:dim(subpathwayz)[1]){
      for(j in 1:nperm1){
        Perm1[i,j]<-sum(as.numeric(zz[sample(length(zz[,"z_score"]),subpathwayz[i,4]),"z_score"]))/sqrt(as.numeric(subpathwayz[i,4]))
      }
      p1[i]<-length(which(Perm1[i,]>ori.z[i]))/nperm1
    }
    fdr1<-p.adjust(p1,"fdr")
    subpathwayz<-cbind(subpathwayz,p1,fdr1)
  }
  if(method2==TRUE){
    p2<-rep(0,dim(subpathwayz)[1])
    uniquepathway<-unique(subpathwayz[,2])
    q<-1
    for(i in 1:length(uniquepathway)){
      sprintf("have permutated %d kinds of subpathway",i)
      r<-length(which(uniquepathway[i]==subpathwayz[,2]))

      t<-q+r-1
      cs<-as.numeric(subpathwayz[q:t,4])

      Perm2<-matrix(0,length(cs),nperm2)
      for(j in 1:length(cs)){
        p<-uniquepathway[i]
        graphite::nodes(convertIdentifiers(pathways("hsapiens", "kegg")[[p]],"symbol"))->Gene
        len1<-length(Gene)
        Gene<-unlist(strsplit(Gene,":"))
        len2<-length(Gene)
        if (len2!=len1) {
          Gene<-Gene[seq(2,length(Gene),2)]
        }

        inn<-intersect(Gene,rownames(zz))
        ss<-cs[j]
        for(m in 1:nperm2){
          subgene<-c()
          subgene<-sample(inn,ss)
          Perm2[j,m]<-sum(as.numeric(zz[subgene,"z_score"]))/sqrt(length(subgene))
        }
      }
      rownames(Perm2)<-cs
      for(n in q:t){
        u<-which(as.numeric(rownames(Perm2))==subpathwayz[n,4])
        p2[n]<-pnorm(q=as.numeric(subpathwayz[n,5]),mean=mean(Perm2[u,]),sd=sd(Perm2[u,]),lower.tail=F)
      }
      q<-q+r
      rm(Perm2)
    }
    fdr2<-p.adjust(p2,"fdr")
    subpathwayz<-cbind(subpathwayz,p2,fdr2)
  }
  return(subpathwayz)
}

