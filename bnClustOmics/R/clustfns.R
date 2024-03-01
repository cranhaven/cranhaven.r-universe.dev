#'Comparing estimated and ground truth membership
#'
#'This function compares similarity between two clusterings.
#'
#'@param truememb ground truth labels
#'@param estmemb estimated labels
#'@return a list containing different measures of similarity between two different clusterings, including accuracy, adjusted Rand index and precision
#'@import stats
#'@import clue
#'@import BiDAG
#'@import mclust
#'@import RBGL
#'@import RColorBrewer
#'@import graphics
#'@importFrom plotrix draw.circle
#'@importFrom graph edgeWeights
#'@importFrom gRbase is.DAG
#'@export
checkmembership <- function(estmemb,truememb) {
  k<-length(unique(estmemb))
  relabelmatrix<-matrix(nrow=k,ncol=k) #i row_num label, j col_num estmemb
  estlabels<-list()
  truelabels<-list()
  for (i in 1:k) {
    truelabels[[i]]<-which(truememb==i)
  }
  for (i in 1:k) {
    estlabels[[i]]<-which(estmemb==i)
  }
  for (i in 1:k) {
    for (j in 1:k) {
      relabelmatrix[i,j]<-length(which(estlabels[[i]]%in%truelabels[[j]]))
    }
  }
  rowcol<-solve_LSAP(relabelmatrix,maximum = TRUE)
  res<-list()
  res$relabel<-as.vector(rowcol)
  res$ncorr<-0
  for (j in 1:min(k,k)) {
    res$ncorr<-res$ncorr+relabelmatrix[j,res$relabel[j]]
  }
  res$accuracy<-res$ncorr/length(truememb)
  res$ARI<-adjustedRandIndex(estmemb,truememb)
  prec<-precision_clusters(estmemb,truememb)
  res$precision<-prec[1]
  res$recall<-prec[2]
  res$F1<-prec[3]
  res$relabelmatrix<-relabelmatrix

  return(res)
}
#'Relabeling clusters
#'
#'When running simulations studies, discovered cluster labels may differ from the ground truth
#'cluster labels. This functions can be used to perform relabeling in order to compare
#'discovered graphs to the ground truth graphs correctly.
#'
#'@param res object of class 'bnclustOmics'
#'@param trueclusters ground truth clustering
#'@return object of class 'bnclustOmics'
#'@export
relabelSimulation<-function(res,trueclusters){
  relab<-checkmembership(clusters(res),trueclusters)$relabel
  res$DAGs<-relabDAGs(res$DAGs,relab)
  if(!is.null(res$ep)) {
    res$ep<-relabDAGs(res$ep,relab)
  }
  res$memb<-relabMembership(res$memb,relab)
  res$lambdas<-relabLambdas(res$lambdas,relab)
  return(res)
}

# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
propersample <- function(x){if(length(x)==1) x else sample(x,1)}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
calcloglike <- function(samplescores,tau) {
  # samplescores<-t(samplescores)
  maxscorey<-apply(samplescores,1,max) # find the max of each column
  loglike<-sum(log(colSums(t(exp(samplescores-maxscorey))*tau))+maxscorey) # remove max for numerical stability and exponentiate
  return(loglike)
}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
reassignsamples <- function(samplescores,numsamps){
  newclustermembership <-rep(0,numsamps) # to store the new cluster
  for(s in 1:numsamps){ # run through the samples
    clusterscores<-samplescores[s,]
    maxscorey<-max(clusterscores) # take the maximum
    maxscoreelem<-which(clusterscores==maxscorey)
    newclustermembership[s]<-propersample(maxscoreelem)
  }
  return(newclustermembership)
}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
relativeprobs <- function(samplescores,numsamps){
  relativeprobabs <-rep(0,numsamps) # to store the relative probabilities
  for(s in 1:numsamps){ # run through the samples
    clusterscores<-samplescores[s,]
    maxscorey<-max(clusterscores) # take the maximum
    shifty<-exp(clusterscores-maxscorey)
    rescaley<-shifty/sum(shifty)
    relativeprobabs[s]<-max(rescaley) # relative probabilities
  }
  return(relativeprobabs)
}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
allrelativeprobs <- function(samplescores,numsamps){
  relativeprobabs <-samplescores # to store the relative probabilities
  for(s in 1:numsamps){ # run through the samples
    clusterscores<-samplescores[s,]
    maxscorey<-max(clusterscores) # take the maximum
    shifty<-exp(clusterscores-maxscorey)
    rescaley<-shifty/sum(shifty)
    relativeprobabs[s,]<-rescaley # relative probabilities
  }
  return(relativeprobabs)
}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
relativeprobswithtau <- function(sampleprobs,tau){
  temp<-tau*t(sampleprobs)
  relativeprobabswithtau<-1/colSums(temp)*t(temp) # ugly code
  return(relativeprobabswithtau)
}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
avescore <- function(samplescores,numsamps){
  averagescores <-rep(0,numsamps) # to store the relative probabilities
  for(s in 1:numsamps){ # run through the samples
    clusterscores<-samplescores[s,]
    maxscorey<-max(clusterscores) # take the maximum
    shifty<-exp(clusterscores-maxscorey) # exponentiate
    rescaley<-log(mean(shifty))+maxscorey # mean and turn back to log
    averagescores[s]<-rescaley # averagescore
  }
  return(averagescores)
}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
reassignsamplesprop <- function(samplescores,numsamps,gamma){
  newclustermembership <-rep(0,numsamps) # to store the new cluster
  for(s in 1:numsamps){ # run through the samples
    clusterscores<-samplescores[s,]*gamma
    maxscorey<-max(clusterscores) # take the maximum
    shifty<-exp(clusterscores-maxscorey)
    rescaley<-shifty/sum(shifty)
    scorelength<-length(rescaley)
    newclustermembership[s]<-sample.int(scorelength,1,prob=rescaley) # sample according to scores
  }
  return(newclustermembership)
}
# https://doi.org/10.1038/s41467-018-06867-x
# author: Kuipers et al.
comparelik<-function(assignprogress) {
  whichmax<-which.max(unlist(lapply(assignprogress,function(x)x$likel[length(x$likel)])))
}
generatetriple<-function(n) {
  resmat<-matrix(nrow=n,ncol=3)
  for (i in 1:n) {
    ordr<-sample.int(3,3)
    res<-vector(length=3)
    res[1]<-runif(1,min=0,max=1)
    res[2]<-runif(1,min=0,max=1-res[1])
    res[3]<-1-res[1]-res[2]
    resmat[i,ordr]<-res
  }
  return(resmat)
}
generatefour<-function(n) {
  resmat<-matrix(nrow=n,ncol=4)
  for (i in 1:n) {
    ordr<-sample.int(4,4)
    res<-vector(length=4)
    res[1]<-runif(1,min=0,max=1)
    res[2]<-runif(1,min=0,max=1-res[1])
    res[3]<-runif(1,min=0,max=1-res[1]-res[2])
    res[4]<-1-res[1]-res[2]-res[3]
    resmat[i,ordr]<-res
  }
  return(resmat)
}
#changed baseprob
generatevec<-function(n,k,membvec=NULL,all1=FALSE) {
  resmat<-matrix(nrow=n,ncol=k)
  if(is.null(membvec)) {
    for (i in 1:n) {
      res<-sample(10:(15+k),k,replace=TRUE)
      res<-res/sum(res)
      resmat[i,]<-res
    }} else {
      if(!all1) {
        for(i in 1:n) {
          baseprob<-1/(k+2)
          probvec<-rep(baseprob,k)
          probvec[membvec[i]]<-3*baseprob
          resmat[i,]<-probvec
        }
      } else {
        for(i in 1:n) {
          probvec<-rep(0,k)
          probvec[membvec[i]]<-1
          resmat[i,]<-probvec
        }
      }
    }
  return(resmat)
}
generatevec2<-function(n,k,membvec=NULL,baseprob=3/(k+2)) {
  resmat<-matrix(nrow=n,ncol=k)
  if(is.null(membvec)) {
    for (i in 1:n) {
      res<-sample(10:(15+k),k,replace=TRUE)
      res<-res/sum(res)
      resmat[i,]<-res
    }} else {
      for(i in 1:n) {
        probvec<-rep((1-baseprob)/(k-1),k)
        probvec[membvec[i]]<-baseprob
        resmat[i,]<-probvec
      }
    }
  return(resmat)
}
newLabels<-function(relabvec) {
  changetovec<-vector()
  for(i in 1:(length(relabvec))) {
    changetovec[i]<-which(relabvec==i)
  }
  return(changetovec)
}
relabMembership<-function(estMemb, changeto) { #changeto 3,2,1,4
  newMemb<-vector()
  for(i in 1:length(changeto)) {
    whichy<-which(estMemb==i) #find all labels equalling 1
    newMemb[whichy]<-changeto[i] #change to 3, i.e. changeto[1]
  }
  return(newMemb)
}
relabLambdas<-function(estLambdas, changeto) {
  newLamb<-estLambdas
  for(i in 1:length(changeto)) {
    newLamb[,changeto[i]]<-estLambdas[,i]
  }
  return(newLamb)
}
relabDAGs<-function(estDAGs, changeto) {
  newDAGs<-estDAGs
  for(i in 1:length(changeto)) {
    newDAGs[[changeto[i]]]<-estDAGs[[i]]
  }
  return(newDAGs)
}

precision_clusters<-function(clusters,true_clusters) {
  cluster_labels<-unique(clusters)
  n_clust<-length(cluster_labels)
  N<-length(clusters)

  cluster_index<-lapply(cluster_labels,single_clusters,clusters)
  cluster_bins<-lapply(cluster_index,make_bins,true_clusters)

  tot_pairs<-N*(N-1)/2
  tot_pos<-sum(unlist(lapply(cluster_bins,total_pos_pairs)))
  tot_neg<-total_neg_pairs(tot_pairs,tot_pos)

  TP<-sum(unlist(lapply(cluster_bins,pairs_TP)))
  FP<-tot_pos-TP
  FN<-pairs_FP(cluster_bins,cluster_labels)
  TN<-tot_neg-FP

  Pr<-TP/(TP+FP)
  Rec<-TP/(TP+FN)
  F1<-2*(Pr*Rec)/(Pr+Rec)
  return(c(Pr,Rec,F1))
}
single_clusters<-function(i,clusters) {
  return(which(clusters==i))
}
make_bins<-function(index,true_clusters){
  return(true_clusters[index])
}
total_pos_pairs<-function(cluster_bin){
  return(choose(length(cluster_bin),2))
}
pairs_TP<-function(cluster_bin){
  taby<-table(cluster_bin)
  if(length(taby[which(taby>1)])>0) {
    return(sum(sapply(taby[which(taby>1)],choose,2)))
  } else{
    return(0)
  }
}
pairs_FP<-function(cluster_bins,cluster_labels){
  mm_matrix<-matrix(nrow=length(cluster_labels),
                    ncol=length(cluster_bins))
  for(i in 1:length(cluster_bins)) {
    for(j in cluster_labels) {
      mm_matrix[i,j]<-length(which(cluster_bins[[i]]==j))
    }
  }

  mm_tot<-0
  n_row<-nrow(mm_matrix)

  for(i in 1:ncol(mm_matrix)) {
    for(j in 1:(nrow(mm_matrix)-1)) {
      mm_tot<-mm_tot+mm_matrix[j,i]*sum(mm_matrix[(j+1):n_row,i])
    }
  }
  return(mm_tot)
}
total_neg_pairs<-function(tot_pairs,tot_pos) {
  return(tot_pairs-tot_pos)
}





