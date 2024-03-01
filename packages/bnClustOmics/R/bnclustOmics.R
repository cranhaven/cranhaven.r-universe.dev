#'Bayesian network based clustering of multi-omics data
#'
#'Bayesian network-based clustering of multi-omics data. This function implements network-based clustering for multiomics data.
#'The mandatory input is a list of matrices consisting from binary, ordinal or continuous variables. Each matrix corresponds to one omics type. At least one
#'matrix with continuous variables must be present. Optional output includes the prior information about interactions between genes and gene products. This can be
#'passed via parameters blacklist and edgepmat. Interactions in blacklist are excluded from the search space. Edgepmat imposes a graphical prior which
#'penalizes certain interactions by a certain penalization factor.
#'The output includes cluster assigments and MAP directed acycluc graphs (DAGs) representing discovered clusters.
#'Optionally, the output may include posterior probabilities of all edges in the discovered graphs.
#'
#'@param omicdata a list of matrices corresponding to omics types. For example, "M" (mutations), "CN" (copy numbers), "T" (transcriptome), "P" (proteome) and "PP" (phosphoproteome); at least one continuous type must be present
#'@param bnnames object of class 'bnInfo'; see constructor function \link{bnInfo}
#'@param blacklist adjacency matrix containing information about which edges will be blacklisted in structure search
#'@param edgepmat penalization matrix of the edges in structure learning
#'@param kclust the number of clusters (mixture components)
#'@param chixi prior pseudocounts used for computing parameters for binary nodes
#'@param seed integer number set for reproducibility
#'@param err convergence criteria
#'@param maxEM maximum number of outer EM iterations (structural search)
#'@param hardlim maximum number of parents per node when learning networks
#'@param deltahl additional number of parents when sampling from the common search space
#'@param nit number of internal iteration (of parameter estimation) in the EM
#'@param epmatrix (logical) indicates if the matrices containing posterior probabilities of single edges are be returned
#'@param plus1it maximum number of search space expansion iterations when performing structure search
#'@param startpoint defines which algorithm is used to define starting cluster memberships: possible values "random", "mclustPCA" and "mclust"
#'@param baseprob defines the base probability of cluster membership when "mclustPCA" or "mclust" used as starting point
#'@param commonspace (logical) defines if the sampling has to be performed from the common search space
#'@param verbose defines if the output messages should be printed
#'@return object of class 'bnclustOmics' containing the results of Bayesian-network based clustering: cluster assignments, networks representing the clusters
#'@author Polina Suter, Jack Kuipers
#'@examples
#' bnnames<-bnInfo(simdata,c("b","c"),c("M","T"))
#'\donttest{
#'fit<-bnclustOmics(simdata,bnnames,maxEM=4, kclust=2, startpoint = "mclustPCA")
#'clusters(fit)
#'checkmembership(clusters(fit),simclusters)
#'}
#'@export
bnclustOmics<-function(omicdata, bnnames, blacklist=NULL, edgepmat=NULL,
                       kclust=2,chixi=0, seed=100,err=1e-6, maxEM=10,hardlim=6,
                       deltahl=5, nit=5, epmatrix=TRUE,plus1it=4,
                       startpoint="mclustPCA",baseprob=0.4, commonspace=TRUE,
                       verbose=TRUE){

  addcorspace<-NULL
  sampiter<-NULL
  if(is.null(blacklist)) blacklist<-blInit(bnnames)
  if(is.null(edgepmat)) {
    edgepmat<-matrix(1,nrow=bnnames$totn,ncol=bnnames$totn)
    rownames(edgepmat)<-colnames(edgepmat)<-bnnames$allnamesonebn
  }
  #check if discrete types are present
  if(bnnames$no+bnnames$nb>0) bgnodes<-c(1:(bnnames$no+bnnames$nb)) else bgnodes<-NULL
  learnsep<-FALSE

  if(bnnames$nb>0) scoretype<-"mixed" else scoretype<-"bge"

  omicdata<-Reduce('cbind',omicdata)
  colnames(omicdata)<-bnnames$allnamesonebn
  startmemb<-NULL
  if(startpoint=="mclust") {
    startmemb<-Mclust(omicdata[,1:bnnames$nc+bnnames$no+bnnames$nb],G=kclust)$classification
  } else if(startpoint=="mclustPCA") {
    var0<-which(apply(omicdata[,1:bnnames$nc+bnnames$no+bnnames$nb],2,sd)==0)
    if(length(var0)>0) pcadata<-omicdata[,1:bnnames$nc+bnnames$no+bnnames$nb][,-var0] else pcadata<-omicdata[,1:bnnames$nc+bnnames$no+bnnames$nb]
    pca_res <- prcomp(pcadata, scale. = TRUE)
    startmemb<-Mclust(pca_res$x[,1:(kclust+2)],G=kclust)$classification
  }
  res<-bnclustOmicsCore(omicdata,bnnames,scoretype=scoretype,bgnodes=bgnodes,
                        blacklist=blacklist,edgepmat=edgepmat,
                        epmatrix=epmatrix, seed=seed,
                        kclust=kclust,chixi=chixi,MAP=TRUE,
                        maxEM=maxEM,startmemb=startmemb,
                        baseprob=baseprob,addcorspace=addcorspace,hardlim=hardlim,
                        deltahl=deltahl, plus1it=plus1it,
                        sampiter=sampiter,commonspace=commonspace)

  res$AIC<-bnclustmodelscore(res,score="AIC",ss=nrow(omicdata),nbin=bnnames$nb)
  res$BIC<-bnclustmodelscore(res,score="BIC",ss=nrow(omicdata),nbin=bnnames$nb)
  return(res)
}
bnclustOmicsCore<-function(omicdata,bnnames,scoretype,bgnodes=NULL,
                           blacklist,edgepmat=NULL,epmatrix=FALSE,
                           kclust,chixi=0.5, seed=100,
                           err=1e-6, maxEM=10,hardlim=10,deltahl=2,nit=5,
                           MAP=TRUE,p=0.5, plus1it=4,
                           startmemb=NULL,baseprob=0.5, commonspace=FALSE,
                           verbose=TRUE,addcorspace=NULL,sampiter=NULL){
  clustercenters<-list()
  n<-ncol(omicdata)
  ss<-nrow(omicdata)
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
  newclustermembership<-reassignsamples(newallrelativeprobabs,nrow(omicdata))

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
      scorepar<-scoreparameters("bge", omicdata, weightvector=allrelativeprobabs[,k], bgnodes=bgnodes,edgepmat = edgepmat)
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
        scorepar<-scoreparameters(scoretype,as.data.frame(omicdata),bgnodes=bgnodes,mixedpar=list(nbin=bnnames$nb),
                                  weightvector=allrelativeprobabs[,k],edgepmat = edgepmat)
        scoresagainstclusters[,k]<-scoreagainstDAG(scorepar,clustercenters[[k]],onlymain=FALSE)
      }
      newallrelativeprobabsnotau<-allrelativeprobs(scoresagainstclusters,nrow(omicdata))
      newallrelativeprobabs<-relativeprobswithtau(newallrelativeprobabsnotau,tauvec)
    }

    diffy<-sum((allprobprev-newallrelativeprobabs)^2)
    newclustermembership<-reassignsamples(newallrelativeprobabs,nrow(omicdata))
    assignprogress$likel[cnt]<-calcloglike(scoresagainstclusters,tauvec)
    cnt<-cnt+1
    #if(!is.null(truememb)) print(checkmembership(truememb,newclustermembership))
  }

  assignprogress$lambdas<-newallrelativeprobabs
  assignprogress$DAGs<-clustercenters
  assignprogress$EMit<-cnt-1
  assignprogress$memb<-newclustermembership


  tracelength<-length(assignprogress$likel)
  res<-assignprogress
  if(!MAP) {
    if(scoretype=="mixed") {
      ep<-lapply(ep,adjustMixedDir,bnnames=bnnames,blacklist=blacklist)
      consmodel<-lapply(consmodel,adjustMixedDir,bnnames=bnnames,blacklist=blacklist)
    } else {
      ep<-lapply(ep,function(x)x*1*(!blacklist))
      consmodel<-lapply(consmodel,function(x)x*1*(!blacklist))
    }
    res$ep<-ep
    res$consmodel<-consmodel
  } else {
    if(commonspace) newsp<-commonSpace(newsp,hardlim+deltahl)
    if(is.null(sampiter)) sampiter<-2*ceiling(6*bnnames$nc^2*log(bnnames$nc))
    if(epmatrix) {
      for(i in 1:kclust) {
        scorepar<-BiDAG::scoreparameters("bge",as.data.frame(omicdata),bgnodes=bgnodes,
                                         weightvector=assignprogress$lambdas[,i],
                                         edgepmat = edgepmat)
        samplefit[[i]]<-suppressWarnings(orderMCMC(scorepar,startspace=newsp[[i]],MAP=FALSE,chainout=TRUE,
                                  blacklist=blacklist,
                                  startorder=maxorders[[i]],iterations = sampiter))
        consensusscores[,i]<-consensusScores(scorepar,samplefit[[i]]$traceadd$incidence)
        ep[[i]]<-edgep(samplefit[[i]],pdag=TRUE)
      }
      if(scoretype=="mixed") {
        ep<-lapply(ep,adjustMixedDir,bnnames=bnnames,blacklist=blacklist)
      } else {
        ep<-lapply(ep,function(x)x*1*(!blacklist))
      }

      ep<-cleanDAGs0(ep,bnnames,omicdata,res$memb)
      res$ep<-ep
      consensusprobs<-allrelativeprobs(consensusscores,nrow(omicdata))
      consensusprobs<-relativeprobswithtau(consensusprobs,tauvec)
      res$consmemb<-reassignsamples(consensusprobs,nrow(omicdata))
      res$consensusscores<-consensusscores
    }
    res$DAGs<-cleanDAGs0(res$DAGs,bnnames,omicdata,res$memb)
  }
  res$p<-p
  res$algorithm=ifelse(MAP,"mcmcMAP","mcmcsample")
  class(res)<-"bnclustOmics"
  return(res)
}
bnclustOmicsCoreSep<-function(omicdata,bnnames,scoretype,bgnodesT=NULL,bgnodesPP=NULL,bgnodes=NULL,
                              blacklist,edgepmat=NULL,epmatrix=FALSE,
                              kclust,chixi=0.5, seed=100,err=1e-6,
                              maxEM=10,hardlim=10,nit=5,
                              plus1it=4,startmemb=NULL,baseprob=0.5,
                              verbose=TRUE, addcorspace=NULL){
  clustercenters<-list()
  n<-ncol(omicdata)
  ss<-nrow(omicdata)

  ep<-list()
  samplefit<-list()


  scoresagainstclusters<-matrix(ncol=kclust,nrow=ss)

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
  newclustermembership<-reassignsamples(newallrelativeprobabs,nrow(omicdata))

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
      scorepar<-list()
      scorepar[["T"]]<-scoreparameters("bge", omicdata, weightvector=allrelativeprobabs[,k], bgnodes=bgnodesT,edgepmat = edgepmat)
      scorepar[["PP"]]<-scoreparameters("bge", omicdata, weightvector=allrelativeprobabs[,k], bgnodes=bgnodesPP,edgepmat = edgepmat)

      #learn cluster center
      if(cnt>2) {
        addspace<-newsp[[k]]
        toomanypar<-which(apply(addspace,2,sum)>hardlim-4)
        if(length(toomanypar>0)) {
          for(i in toomanypar) {
            pari<-which(addspace[,i]==1)
            npar<-length(pari)
            par0<-sample(pari,npar-(hardlim-5))
            addspace[par0,i]<-0
          }
        }
      } else {
        addspace<-NULL
      }
      maxfit<-list()
      start1<-Sys.time()
      maxfit[["T"]]<-iterativeMCMC(scorepar[["T"]],iterations=1001,plus1it=plus1it,hardlimit=hardlim+1,
                            blacklist=blacklist,verbose=verbose)
      start2<-Sys.time()
      maxfit[["PP"]]<-iterativeMCMC(scorepar[["PP"]],addspace=addspace,plus1it=plus1it,hardlimit=hardlim+1,
                                   blacklist=blacklist,verbose=verbose)
      end2<-Sys.time()

      newsp[[k]]<-combOmicMat(bnnames,matT=maxfit[["T"]]$endspace,matPP=maxfit[["PP"]]$endspace)
      clustercenters[[k]]<-combOmicMat(bnnames,maxfit[["T"]]$DAG,maxfit[["PP"]]$DAG)
      assignprogress$centerscores[k]<-maxfit[["PP"]]$score
    }

    #internal EM cycle, estimating parameters
    for (i in 1:nit) {
      allrelativeprobabs<-newallrelativeprobabs
      coltots<-colSums(allrelativeprobabs) + chixi # add prior to clustersizes
      tauvec<-coltots/sum(coltots)
      for (k in 1:kclust) {
        scorepar<-scoreparameters(scoretype,as.data.frame(omicdata),bgnodes=bgnodes,mixedpar=list(nbin=bnnames$nb),
                                  weightvector=allrelativeprobabs[,k],edgepmat = edgepmat)
        scoresagainstclusters[,k]<-scoreagainstDAG(scorepar,clustercenters[[k]],onlymain=FALSE)
      }
      newallrelativeprobabsnotau<-allrelativeprobs(scoresagainstclusters,nrow(omicdata))
      newallrelativeprobabs<-relativeprobswithtau(newallrelativeprobabsnotau,tauvec)
    }

    diffy<-sum((allprobprev-newallrelativeprobabs)^2)
    newclustermembership<-reassignsamples(newallrelativeprobabs,nrow(omicdata))
    assignprogress$likel[cnt]<-calcloglike(scoresagainstclusters,tauvec)
    cnt<-cnt+1
  }

  assignprogress$lambdas<-newallrelativeprobabs
  assignprogress$DAGs<-clustercenters
  assignprogress$EMit<-cnt-1
  assignprogress$memb<-newclustermembership


  tracelength<-length(assignprogress$likel)
  res<-assignprogress
  if(epmatrix) {
      for(i in 1:k) {
        scorepar<-list()
        scorepar[["T"]]<-BiDAG::scoreparameters("bge",as.data.frame(omicdata),bgnodes=bgnodesT,
                                         weightvector=assignprogress$lambdas[,k],edgepmat = edgepmat)
        scorepar[["PP"]]<-BiDAG::scoreparameters("bge",as.data.frame(omicdata),bgnodes=bgnodesPP,
                                                weightvector=assignprogress$lambdas[,k],edgepmat = edgepmat)
        samplefit[["T"]]<-orderMCMC(scorepar[["T"]],startspace=newsp[[i]],MAP=FALSE,chainout=TRUE)
        samplefit[["PP"]]<-orderMCMC(scorepar[["PP"]],startspace=newsp[[i]],MAP=FALSE,chainout=TRUE)

        #samplefit[["T"]]$info$DBN<-FALSE
        #samplefit[["PP"]]$info$DBN<-FALSE
        ep_localT<-edgep(samplefit[["T"]],pdag=TRUE)
        ep_localPP<-edgep(samplefit[["PP"]],pdag=TRUE)
        ep[[i]]<-combOmicMat(bnnames,ep_localT,ep_localPP)
      }
      if(scoretype=="mixed") {
        ep<-lapply(ep,adjustMixedDir,bnnames=bnnames,blacklist=blacklist)
      } else {
        ep<-lapply(ep,function(x)x*1*(!blacklist))
      }
      res$ep<-ep
  }

  res$algorithm="mcmcMAP"
  class(res)<-"bnclustOmics"
  return(res)
}
addspaceOmics<-function(omicdata,bnnames,rho=0.5,maxp=4) {
  addspace<-matrix(0,nrow=length(bnnames$allnamesonebn),ncol=length(bnnames$allnamesonebn))
  colnames(addspace)<-rownames(addspace)<-bnnames$allnamesonebn
  cnodes<-1:bnnames$nc+bnnames$nb+bnnames$no
  for(i in cnodes) {
    cnode<-bnnames$allnamesonebn[i]
    omictype<-names(bnnames$omicranges)[which(unlist(lapply(bnnames$omicranges,function(x)i%in%x))==TRUE)]
    cors<-apply(omicdata$M,2,function(x)cor(x,bnnames[[omictype]][,cnode]))
    cors<-sort(cors,decreasing=TRUE)
    highcorl<-length(which(cors>rho))
    if(highcorl>0) {
      if(highcorl>maxp) pars<-names(cors)[1:maxp] else pars<-names(cors)[1:highcorl]
      addspace[pars,cnode]<-1
    }
  }
  return(addspace)
}
