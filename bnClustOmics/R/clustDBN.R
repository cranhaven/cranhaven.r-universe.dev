#'DBN-based clustering
#'
#'This function can be used for DBN-based clustering. It is the same function as bnclustOmics, but it also works for time series data.
#'
#'@param dbndata data matrix; rows are observations, columns are variables; static nodes have to be in the first column of the data
#'@param staticnodes (integer) number of static nodes in a DBN
#'@param blacklist adjacency matrix containing information about which edges will be blacklisted in structure search
#'@param edgepmat penalization matrix of the edges in structure learning
#'@param kclust the number of clusters (mixture components)
#'@param chixi prior pseudocounts used for computing parameters for binary nodes
#'@param seed integer number set for reproducibility
#'@param err convergence criteria
#'@param maxEM maximum number of EM iterations (structural)
#'@param hardlim maximum number of parents per node when learning networks
#'@param deltahl additional number of parents when sampling from the common search space
#'@param nit number of internal iteration in structural EM
#'@param epmatrix (logical) indicates if the matrices containing posterioir probabilities of single edges should be returned
#'@param plus1it maximum number of search space expansion iterations when performing structure search
#'@param nruns number of runs of the EM algorithm
#'@param startpoint defines which algorithm is used to define starting cluster memberships: possible values "random", "mclustPCA" and "mclust"
#'@param baseprob defines the base probability of cluster membership when "mclustPCA" or "mclust" used as starting point
#'@param commonspace (logical) defines if the sampling has to be performed from the common search space
#'@param verbose defines if the output messages should be printed
#'@param samestruct (logical) defines if initial and intrinsic part of transition structures should be the same
#'@param pickmax (logical) if TRUE only maximum EM run is returned
#'@return object of class 'bnclustOmics' containing the results of Bayesian-network based clustering: cluster assignments, networks representing the clusters
#'@author Polina Suter
#'@export
clustDBN<-function(dbndata, staticnodes = 0, blacklist=NULL, edgepmat=NULL,
                       kclust=2,chixi=0.5, seed=100,err=1e-6, maxEM=10,hardlim=6,
                       deltahl=2, nit=5, epmatrix=TRUE,plus1it=4, nruns=1,
                       startpoint="mclustPCA",baseprob=0.4, commonspace=TRUE,
                       verbose=TRUE,samestruct=TRUE,pickmax=TRUE){
  totn<-ncol(dbndata)
  addcorspace<-NULL
  sampiter<-NULL
  if(is.null(blacklist)) {
    blacklist<-matrix(0,nrow=totn,ncol=totn)
    rownames(blacklist)<-colnames(blacklist)<-colnames(dbndata)
  }
  if(is.null(edgepmat)) {
    edgepmat<-matrix(1,nrow=totn,ncol=totn)
    rownames(edgepmat)<-colnames(edgepmat)<-colnames(dbndata)
  }
  #check if discrete types are present
  scoretype<-"bge"
  startmemb<-NULL
  if(startpoint=="mclust") {
    startmemb<-Mclust(dbndata,G=kclust)$classification
  } else if(startpoint=="mclustPCA") {
    var0<-which(apply(dbndata,2,sd)==0)
    if(length(var0)>0) pcadata<-dbndata[,][,-var0] else pcadata<-dbndata[,]
    pca_res <- prcomp(pcadata, scale. = TRUE)
    startmemb<-Mclust(pca_res$x[,1:(kclust+2)],G=kclust)$classification
  }
  if(nruns==1) {
    res<-clustDBNcore(dbndata,staticnodes,scoretype=scoretype,
                          blacklist=blacklist,edgepmat=edgepmat,
                          epmatrix=epmatrix, seed=seed,
                          kclust=kclust,chixi=chixi,MAP=TRUE,
                          maxEM=maxEM,startmemb=startmemb,
                          baseprob=baseprob,addcorspace=addcorspace,hardlim=hardlim,
                          deltahl=deltahl,
                          sampiter=sampiter,commonspace=commonspace,samestruct=samestruct)
    res$AIC<-bnclustmodelscore(res,score="AIC",ss=nrow(dbndata),nbin=0)
    res$BIC<-bnclustmodelscore(res,score="BIC",ss=nrow(dbndata),nbin=0)
  } else {
    res<-list()
    set.seed(seed)
    allseeds<-sort(sample(100,nruns))
    allseeds[nruns]<-seed

    for(i in 1:nruns) {
      res[[i]]<-clustDBNcore(dbndata,staticnodes,scoretype=scoretype,
                        blacklist=blacklist,edgepmat=edgepmat,
                        epmatrix=FALSE, seed=allseeds[i],
                        kclust=kclust,chixi=chixi,MAP=TRUE,
                        maxEM=maxEM,startmemb=startmemb,
                        baseprob=baseprob,addcorspace=addcorspace,hardlim=hardlim,
                        deltahl=deltahl,
                        sampiter=sampiter,commonspace=commonspace,samestruct=samestruct)
      res[[i]]$AIC<-bnclustmodelscore(res[[i]],score="AIC",ss=nrow(dbndata),nbin=0)
      res[[i]]$BIC<-bnclustmodelscore(res[[i]],score="BIC",ss=nrow(dbndata),nbin=0)
      if(pickmax) {
        maxlik<-which.max(unlist(lapply(res,function(x)max(x$likel))))
        res<-res[[maxlik]]
      }
    }
  }
  return(res)
}
clustDBNcore<-function(dbndata,b,scoretype,
                       blacklist,edgepmat=NULL,epmatrix=FALSE,
                       kclust,chixi=0.5, seed=100,
                       err=1e-6, maxEM=10,hardlim=10,deltahl=2,nit=5,
                       MAP=TRUE,p=0.5, plus1it=6,
                       startmemb=NULL,baseprob=0.5, commonspace=FALSE,
                       verbose=TRUE,addcorspace=NULL,sampiter=NULL,
                       samestruct=TRUE){
  clustercenters<-list()
  n<-ncol(dbndata)
  ss<-nrow(dbndata)
  maxorders<-list()
  ep<-list()
  samplefit<-list()
  if(!MAP) {
    consmodel<-list()
  }

  scoresagainstclusters<-matrix(ncol=kclust,nrow=ss)
  consensusscores<-matrix(ncol=kclust,nrow=ss)
  set.seed(seed)
  diffy<-1
  cnt<-1
  newsp<-list()
  assignprogress<-list()
  assignprogress$corrvec<-numeric()
  assignprogress$likel<-numeric()
  assignprogress$centerscores<-numeric(kclust)

  #generate random assignment of belonging to each cluster for each sample
  if(!is.null(startmemb)) {
    newallrelativeprobabs<-generatevec2(ss,kclust,membvec=startmemb,baseprob=baseprob)
  } else {
    set.seed(seed)
    newallrelativeprobabs<-generatevec(ss,kclust)
  }
  newclustermembership<-reassignsamples(newallrelativeprobabs,nrow(dbndata))

  #outer EM cycle, learning cluster centers
  while (diffy>err & cnt<maxEM) {
    print(cnt)
    allrelativeprobabs<-newallrelativeprobabs
    allprobprev<-newallrelativeprobabs
    coltots<-colSums(allrelativeprobabs) + chixi # add prior to clustersizes
    tauvec<-coltots/sum(coltots)

    #define cluster centers and assign probabilities
    for (k in 1:kclust) {
      #define score parameters
      scorepar<-scoreparameters("bge", dbndata,dbnpar = list(samestruct = TRUE, slices = 2, b = b),DBN=TRUE,
                                weightvector=allrelativeprobabs[,k], edgepmat = edgepmat)
      # learn cluster center
      if(cnt>2) {
        if(is.null(addcorspace)) {
          addspace<-newsp[[k]] } else {
            addspace<-1*(newsp[[k]]|addcorspace)
          }
        toomanypar<-which(apply(addspace,2,sum)>(hardlim-1))
        if(length(toomanypar>0)) {
          for(i in toomanypar) {
            pari<-which(addspace[,i]==1)
            npar<-length(pari)
            par0<-sample(pari,npar-(hardlim-1))
            addspace[par0,i]<-0
          }
        }
      } else {
        addspace<-addcorspace
      }

      addspace<-NULL

      maxfit<-suppressWarnings(iterativeMCMC(scorepar,addspace=addspace,plus1it=plus1it,hardlimit=hardlim+1,
                            blacklist=blacklist,verbose=verbose))
      maxorders[[k]]<-maxfit$maxorder
      newsp[[k]]<-maxfit$endspace
      if(!MAP) {
        samplefit[[k]]<-orderMCMC(scorepar,startspace=maxfit$endspace,MAP=FALSE,chainout=TRUE)
        ep[[k]]<-edgep(samplefit[[k]],pdag=TRUE)
        consmodel[[k]]<-getmodel(ep[[k]],p)
        clustercenters[[k]]<-getrepr(consmodel[[k]],maxfit$DAG)
      } else {
        clustercenters[[k]]<-maxfit$DAG
      }
      assignprogress$centerscores[k]<-maxfit$score
    }

    #internal EM cycle, estimating parameters
    for (i in 1:nit) {
      allrelativeprobabs<-newallrelativeprobabs
      coltots<-colSums(allrelativeprobabs) + chixi # add prior to clustersizes
      tauvec<-coltots/sum(coltots)
      for (k in 1:kclust) {
        scorepar<-scoreparameters(scoretype,as.data.frame(dbndata),dbnpar = list(samestruct = TRUE, slices = 2, b = b), DBN=TRUE,
                                  weightvector=allrelativeprobabs[,k],edgepmat = edgepmat)
        scoresagainstclusters[,k]<-scoreagainstDBN(scorepar,clustercenters[[k]],onlymain=TRUE)
      }
      newallrelativeprobabsnotau<-allrelativeprobs(scoresagainstclusters,nrow(dbndata))
      newallrelativeprobabs<-relativeprobswithtau(newallrelativeprobabsnotau,tauvec)
    }

    diffy<-sum((allprobprev-newallrelativeprobabs)^2)
    newclustermembership<-reassignsamples(newallrelativeprobabs,nrow(dbndata))
    assignprogress$likel[cnt]<-calcloglike(scoresagainstclusters,tauvec)
    cnt<-cnt+1
  }

  assignprogress$lambdas<-newallrelativeprobabs
  assignprogress$DAGs<-clustercenters
  assignprogress$EMit<-cnt-1
  assignprogress$memb<-newclustermembership


  tracelength<-length(assignprogress$likel)
  res<-assignprogress
  if(!MAP) {

    if(scoretype=="mixed") {
      #ep<-lapply(ep,adjustMixedDir,bnnames=bnnames,blacklist=blacklist)
      #consmodel<-lapply(consmodel,adjustMixedDir,bnnames=bnnames,blacklist=blacklist)
    } else {
      ep<-lapply(ep,function(x)x*1*(!blacklist))
      consmodel<-lapply(consmodel,function(x)x*1*(!blacklist))
    }
    res$ep<-ep
    res$consmodel<-consmodel
  } else {
    if(epmatrix) {
      if(commonspace) newsp<-commonSpace(newsp,hardlim+deltahl)
      if(is.null(sampiter)) sampiter<-2*ceiling(6*scorepar$nsmall^2*log(scorepar$nsmall))
      for(i in 1:kclust) {
        print(i)
        scorepar<-BiDAG::scoreparameters("bge",as.data.frame(dbndata), dbnpar = list(samestruct = TRUE, slices = 2, b = b), DBN=TRUE,
                                         weightvector=assignprogress$lambdas[,i],
                                         edgepmat = edgepmat)

        samplefit[[i]]<-suppressWarnings(orderMCMC(scorepar,startspace=newsp[[i]],MAP=FALSE,chainout=TRUE,
                                  blacklist=blacklist,
                                  startorder=maxorders[[i]],iterations = sampiter))
        consensusscores[,i]<-consensusScoresDBN(scorepar,samplefit[[i]]$traceadd$incidence)
        ep[[i]]<-edgep(samplefit[[i]],pdag=TRUE)
      }
      if(scoretype=="mixed") {
        #ep<-lapply(ep,adjustMixedDir,bnnames=bnnames,blacklist=blacklist)
      } else {
        ep<-lapply(ep,function(x)x*1*(!blacklist))
      }

      res$ep<-ep
      consensusprobs<-allrelativeprobs(consensusscores,nrow(dbndata))
      consensusprobs<-relativeprobswithtau(consensusprobs,tauvec)
      res$consmemb<-reassignsamples(consensusprobs,nrow(dbndata))
      res$consensusscores<-consensusscores
    }

  }
  res$seed<-seed
  res$p<-p
  res$algorithm=ifelse(MAP,"mcmcMAP","mcmcsample")
  class(res)<-"bnclustOmics"
  return(res)
}
splitDBNdata<-function(datatoscore,param) {
  datasplit<-list()
  if(param$bgn>0) {
    datasplit$init<-datatoscore[,c(1:param$nsmall+param$bgn,1:param$bgn)]
    datasplit$trans<-datatoscore[,c(1:param$nsmall+param$nsmall+param$bgn,1:param$bgn,1:param$nsmall+param$bgn)]
  } else {
    datasplit$init<-datatoscore[,1:param$nsmall]
    datasplit$trans<-datatoscore[,c(1:param$nsmall+param$nsmall,1:param$nsmall)]
  }
  return(datasplit)
}


