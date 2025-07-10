

#' (Internal function) Select a clustering method to be used for the IPCAPS
#' process.
#'
#' @param node An integer representing the current node number which is being
#' analyzed.
#' @param work.dir A working directory.
#' @param method A clustering method selected from the \code{\link{ipcaps}}
#' function. See \code{\link{ipcaps}} for available methods.
#'
#' @return A vector of cluster assignment, for which cluster each individual
#' belongs.
#'
#' @import stats
#' @import fpc
#' @import LPCM
#' @importFrom apcluster apcluster negDistMat
#' @importFrom Rmixmod mixmodCluster mixmodPredict
#' @import KRIS
#'
#' @seealso \code{\link{ipcaps}}

clustering.mode <- function(node,work.dir,method){
  start.time = Sys.time()

  cat(paste0("Node ",node,": Start clustering\n"))

  PCs <- NULL
  no.significant.PC <- NULL

  #load experiment condition
  file.name = file.path(work.dir,"RData",paste0("node",node,".RData"))
  cat(paste0("Node ",node,": Loading ",file.name,"\n"))
  load(file=file.name)

  if (method == "clara"){
    cl=pamk(PCs,krange=2:2,usepam=FALSE)
    cluster=cl$pamobject$clustering
  }else if (method == "pam"){
    cl=pamk(PCs,krange=2:2,usepam=TRUE)
    cluster=cl$pamobject$clustering
  }else if (method == "mixmod"){
    subPCs=as.data.frame(PCs[,1:no.significant.PC])
    mmc <- mixmodCluster(data=subPCs,nbCluster=1:3,criterion=c("BIC","ICL","NEC"),models=NULL)
    pmm = mixmodPredict(data=subPCs, classificationRule= mmc["bestResult"])
    cluster=pmm['partition']
  }else if (method == "meanshift"){
    subPCs=as.data.frame(PCs[,1:no.significant.PC])
    fit <- ms(subPCs, h=0.095, plot = FALSE)
    cluster=fit$cluster.label
  }else if (method == "apcluster"){
    subPCs=as.data.frame(PCs[,1:no.significant.PC])
    ap1 <- apcluster(negDistMat(r=5),subPCs,q=0.001)
    #ap2 <- aggExCluster(x=ap1)
    cluster=rep(0,dim(subPCs)[1])
    no.cluster <- length(ap1)
    for (i in 1:no.cluster){
      cluster[ap1@clusters[[i]]] <- i
    }
  }else if (method == "hclust"){
    dis=dist(PCs)
    hc=hclust(dis, method = "complete")
    cluster=cutree(hc, k=2)
  }else if (method == "rubikclust"){
    cluster=rubikclust(PCs[,1:3],min.space = 0.15)
  }else{#default Mixed clustering methods
    cluster=rubikclust(PCs[,1:3])
    if (length(unique(cluster)) == 1){ #swith to mixmod
      subPCs=as.data.frame(PCs[,1:no.significant.PC])
      mmc <- mixmodCluster(data=subPCs,nbCluster=1:3,criterion=c("BIC","ICL","NEC"),models=NULL)
      pmm = mixmodPredict(data=subPCs, classificationRule= mmc["bestResult"])
      cluster=pmm['partition']
    }
  }

  end.time = Sys.time()
  cat(paste0("Node ",node,": done for clustering\n"))
  print(end.time - start.time)
  return(cluster)

}



