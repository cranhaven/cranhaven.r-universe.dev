#' @title simMinimalEnsemble
#' @description It performs simulations of minimal ensembles of genotypes
#' @param ped pedigree
#' @param QP QP
#' @param testID test ID
#' @param freqs frequencies
#' @param numCores number of cores
#' @param seed seed
#' @param dep check dependency fbnet
#' @param bVerbose boolean to print information
#' @param bJustGetNumber boolean to just get the number of runs
#' @param bdbg boolean to debug
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom fbnet buildCPTs buildBN
#' @return list of results
#' @export
simMinimalEnsemble <- function(ped,QP,testID,freqs,numCores=1,seed=123457,bVerbose=TRUE,bJustGetNumber=FALSE,bdbg=FALSE, dep = TRUE){ #nolint
  if (dep == FALSE) {
  initBN <- ped_fbnet <- NULL}
  lLangeGoradia <- list()
  numGeno<-c()
  markerNames <- unlist(lapply(ped$MARKERS,function(x){attr(x,'name')}))
  for(imarker in seq_along(markerNames)){
    alleles <- attr(ped$MARKERS[[imarker]],'alleles')
    a<-elimLangeGoradia(ped,iMarker=imarker,bitera=TRUE,bverbose = FALSE)[testID]
    #desarmo el alelo 666
    a<-lapply(a,function(x){
      #desarmo 666's
      ma <- ma0 <- t(apply(matrix(as.numeric(strsplit2(x,'/')),byrow=FALSE,ncol=2),1,sort))
      if(any(ma[,1]=='666')){  #si esto es verdad tengo 666/666 asi que no hay ninguna restriccion
        maux <- expand.grid(alleles,alleles)
        return(unique(apply(maux,1,function(x){paste(sort(x),collapse='/')})))
      }else{
        i666 <- grep('666',ma[,2])
        if(length(i666)>0){
          for(ii in i666){
            mnew <- cbind(rep(ma0[ii,1],length(alleles)),as.numeric(alleles))
            mnew <- t(apply(mnew,1,sort))
            ma <- rbind(ma,mnew)
          }
          ma <- ma[-i666,]
        }
      }
      return(unique(apply(ma,1,paste,collapse='/')))
    })
    lLangeGoradia[[markerNames[imarker]]]<-a
    numGeno <- rbind(numGeno,unlist(lapply(a,length)))
  }
  rownames(numGeno) <- markerNames
  nruns <- apply(numGeno,2,max)
  if(bVerbose | bJustGetNumber){ #nolint
    cat(paste('ID:',testID,'Number of fbnet runs:',nruns,'  (',rownames(numGeno)[which.max(numGeno[,1])],')\n'))
    cat('\nGenotypes per marker to explore:\n')
    print(numGeno)
  }
  if(bJustGetNumber){
    return()
  }
  maxNumGenotypes <- apply(numGeno,2,max)
  lMatrixGenotype <- list()
  for(ip in seq_along(maxNumGenotypes)){
    maux <- matrix(NA,ncol=maxNumGenotypes[ip],nrow=length(lLangeGoradia))
    rownames(maux) <- names(lLangeGoradia)
    for(i in 1:nrow(maux)){ #nolint
      jmax  <- numGeno[i,ip]
      x     <- lLangeGoradia[[i]][[ip]]
      maux[i,] <- c(x,rep(x[jmax],ncol(maux)-jmax))  
    }
    lMatrixGenotype[[ip]] <- maux
  }
  dim(lMatrixGenotype[[1]])
  if(numCores>1){
    doParallel::registerDoParallel(cores=numCores)
  }
  lprobG0 <- list()
  for(inew in seq_along(testID)){
    cat('running testID:',testID[inew],'\n')
    pednew <- ped
    lprobG0[[as.character(testID[inew])]] <- list()
    if(bdbg) cat('', file=paste0("mylog.",inew,".txt"), append=FALSE)
    if(numCores==1){
      #set genotype for i-newopeople  
      for(irun in 1:ncol(lMatrixGenotype[[inew]])){ #nolint
        cat(paste0('\n contributor: ',inew,'/',length(testID),' - ',irun,'/',nruns[inew],' runs.\n'))
        genos <- lMatrixGenotype[[inew]][,irun]
        #cargo genotipos en markerdata
        pedMarkers <- unlist(lapply(pednew$MARKERS,function(x){attr(x,'name')}))
        for(i in seq_along(genos)){
          imarker  <- match(names(genos)[i],pedMarkers)
          moi      <- pednew$MARKERS[[imarker]]
          alleles  <- attr(moi,'alleles')
          ialleles <- match(unlist(strsplit(genos[pedMarkers[imarker]],'/')),alleles)
          moi[testID[inew],] <- ialleles
          pednew$MARKERS[[imarker]] <-moi
        }
        pednew$available <- sort(c(pednew$available,as.numeric(testID[inew])))
        pbn  <- initBN(ped_fbnet)
        bnet <- fbnet::buildBN(pbn,QP=QP)
        bn1  <- fbnet::buildCPTs(bnet) 
        resQ <- fbnet::velim.bn(bn1,ordMethod="min_fill",verbose=FALSE)
        lprobG0[[as.character(testID[inew])]][[paste0('sample_',irun)]] <- genotypeProbTable_bis(bn1,resQ,freq = freqs)[[1]]
      }
    }else {
      a <- foreach::foreach(irun=1:ncol(lMatrixGenotype[[inew]])) %dopar% { #nolint
        genos <- lMatrixGenotype[[inew]][,irun]
        #cargo genotipos en markerdata
        pedMarkers <- unlist(lapply(pednew$MARKERS,function(x){attr(x,'name')}))
        for(i in seq_along(genos)){
          imarker  <- match(names(genos)[i],pedMarkers)
          moi      <- pednew$MARKERS[[imarker]]
          alleles  <- attr(moi,'alleles')
          ialleles <- match(unlist(strsplit(genos[pedMarkers[imarker]],'/')),alleles)
          moi[testID[inew],] <- ialleles
          pednew$MARKERS[[imarker]] <-moi
        }
        pednew$available <- sort(c(pednew$available,as.numeric(testID[inew])))
        pbn  <- initBN(ped_fbnet)
        bnet <- fbnet::buildBN(pbn,QP=QP)
        bn1  <- fbnet::buildCPTs(bnet) 
        resQ <- fbnet::velim.bn(bn1,ordMethod="min_fill",verbose=FALSE)
        res <- genotypeProbTable_bis(bn1,resQ,freq=freqs)[[1]]
        if(FALSE) cat(res[[10]],file=paste0('run_',irun,'.csv'))
        if(bdbg)cat(irun,res[[10]][1,2],'\n',file=paste0("mylog.",inew,".txt"),append=TRUE)
        res
      }
      names(a)<-paste0('sample_',seq_along(a))
      lprobG0[[as.character(testID[inew])]]  <- a
    }
  }
    lprobGenoMOI <- list()
    ITtable<-c()
    for(iCDI in seq_along(lprobG0)){
      lprobGenoMOI[[iCDI]] <- list()
      for(moi in names(lprobG0[[1]][[1]])){
        a<-lapply(lprobG0[[iCDI]][1:numGeno[moi,iCDI]],function(x){
          x[[moi]]
        })
        saux     <-lLangeGoradia[[moi]][[iCDI]]
        saux     <- unlist(lapply(strsplit(saux,'/'),function(y){paste(sort(as.numeric(y)),collapse='/')}))
        names(a) <- saux 
        lprobGenoMOI[[iCDI]][[moi]]<-a
        compa <- compareBnetPopGenoPDFs(a)
        ITtable <- rbind(ITtable,cbind(cdi=names(lprobG0)[iCDI],marker=moi,allele=rownames(compa),compa))
      }
    }
    names(lprobGenoMOI)<-names(lprobG0)
  return(list(lprobGenoMOI=lprobGenoMOI,lprobG=lprobG0,lMatrixGenotype=lMatrixGenotype,ITtable=ITtable))
}
