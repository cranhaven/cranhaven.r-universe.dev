#'Initializing blacklist
#'
#'This function can be used to initialize a blacklist matrix for bnclustOmics clustering
#'
#'@param bnnames object of class bnInfo; see \link{bnInfo}
#'@param bldiag logical, defines if diagonal should be blacklisted, TRUE by default
#'@param intra (optional) a vector of characters defining omic types for which intra-type edges will be blacklisted
#'@param interXX (optional) a list containing two vectors of characters defining omic types between which same gene (X.type.from -> X.type.to) edges will be blacklisted
#'@param interXY (optional) a list containing two vectors of characters defining omic types between which different gene edges  (X.type.from -> Y.type.to) will be blacklisted
#'@return returns a binary matrix where 1 defines prohibited edges and 0 defines allowed edges
#'@author Polina Suter
#'@export
blInit<-function(bnnames,bldiag=TRUE,intra=NULL,interXX=list(from=NULL,to=NULL),
                 interXY=list(from=NULL,to=NULL)){
  blacklist<-matrix(0,nrow=bnnames$totn,ncol=bnnames$totn)
  rownames(blacklist)<-colnames(blacklist)<-bnnames$allnamesonebn
  if(bldiag){
    diag(blacklist)<-1
  }
  if(!is.null(intra)) {
    for(i in intra) {
      blacklist[bnnames$omicranges[[i]],bnnames$omicranges[[i]]]<-1
    }
  }

  if(!is.null(interXX$from) & !is.null(interXX$to)) {
    for(i in 1:length(interXX$from)) {
      for(j in 1:nrow(bnnames$allnames[[ interXX$from[i] ]])){
        currgene<-bnnames$allnames[[ interXX$from[i] ]][j,"gene"]
        coln<-which(bnnames$allnames[[ interXX$to[i] ]][,"gene"]==currgene)
        if(length(coln)>0) {
          blacklist[bnnames$omicranges[[ interXX$from[i] ]][j],bnnames$omicranges[[ interXX$to[i] ]][coln]]<-1
        }
      }
    }
  }
  if(!is.null(interXY$from) & !is.null(interXY$to)) {
    for(i in 1:length(interXY$from)) {
      for(i in 1:length(interXY$from)) {
        for(j in 1:nrow(bnnames$allnames[[ interXY$from[i] ]])){
          currgene<-bnnames$allnames[[ interXY$from[i] ]][j,"gene"]
          coln<-which(bnnames$allnames[[ interXY$to[i] ]][,"gene"]!=currgene)
          if(length(coln)>0) {
            blacklist[bnnames$omicranges[[ interXY$from[i] ]][j],bnnames$omicranges[[ interXY$to[i] ]][coln]]<-1
          }
        }
      }
    }
  }
  return(blacklist)
}

#'Updating  blacklist
#'
#'This function can be used to update a blacklist matrix by blacklisting an edge between a pair of variables
#'
#'@param blacklist object of class 'blacklist'
#'@param node1 name of omic variable from which the edge is prohibited
#'@param node2 name of omic variable to which the edge is prohibited
#'@return returns a binary matrix where 1 defines prohibited interactions and 0 defines allowed interactions
#'@author Polina Suter
#'@export
blUpdate<-function(blacklist,node1,node2) {
  blacklist[node1,node2]<-1
  return(blacklist)
}

#'Initializing penalization matrix
#'
#'This function can be used to initialize a penalization matrix for bnclustOmics clustering
#'
#'@param bnnames object of class bnInfo; see \link{bnInfo}
#'@param pfbase a numeric value more or equal to 1, base penalization factor; 1 by default (no penalization)
#'@param intpf (optional) a numeric value more or equal to 1, this value will be used to penalize interactions from 'intlist'
#'@param intlist (optional) a matrix or data frame containing a list of interactions and optionally their scores; 2 columns are necessary 'gene1' and 'gene2'
#'@param intsame penalization factor for edges connecting the same genes
#'@param usescore (logical) when TRUE, interactions score from column 'score' of the parameter 'intlist' will be used to define penalization factor
#'@return returns a square matrix containing edge specific penalization factors
#'@export
#'@author Polina Suter
penInit<-function(bnnames,pfbase=1,intpf=pfbase, intlist=NULL,intsame=1, usescore=FALSE) {
  penmat<-matrix(pfbase,nrow=bnnames$totn,ncol=bnnames$totn)
  if(!is.null(intlist)) {
  if(!usescore) intlist$score<-rep(0,nrow(intlist))
  if(intpf!=pfbase | usescore) {
  allgeneone<-as.vector(unlist(lapply(bnnames$allnames,function(x)x[,3])))
  rownames(penmat)<-colnames(penmat)<-allgeneone
  #first pick only interactions that are applicable to our set
  intlist<-intlist[which(intlist$gene1%in%allgeneone & intlist$gene2%in%allgeneone),]
  if(nrow(intlist)>0) {
    penmat<-fillPM(as.matrix(intlist),penmat,intpf,usescore=usescore)
    penmat<-fillPM(as.matrix(intlist)[,c(2,1,3)],penmat,intpf,usescore=usescore)
      }
    }
  }
  if(intsame!=pfbase) {
    dups<-which(duplicated(allgeneone))
    if(length(dups>0)) {
      dupgenes<-cbind(allgeneone[dups],allgeneone[dups])
      colnames(dupgenes)<-c("gene1","gene2")
      penmat<-fillPM(as.matrix(dupgenes),penmat,intsame)
    }
  }
  diag(penmat)<-pfbase
  rownames(penmat)<-colnames(penmat)<-bnnames$allnamesonebn
  return(penmat)
}

#'Updating penalization matrix (intra one omics type)
#'
#'This function can be used to update an existing penalization matrix
#'
#'@param penmat a square penalization matrix; to initialize use \link{penInit}
#'@param bnnames object of class bnInfo; see \link{bnInfo}
#'@param type name of omic type
#'@param intlist a list of known interactions; columns 'gene1' and 'gene2' must be present
#'@param pfbase a numeric value more or equal to 1, base penalization factor; 2 by default (1 corresponds to no penalization)
#'@param intpf (optional) a numeric value more or equal to 1, this value will be used to penalize interactions from 'intlist'
#'@param intlist (optional) a matrix or data frame containing a list of interactions and optionally their scores; 2 columns are necessary 'gene1' and 'gene2'
#'@param intsame penalization factor for edges connecting the same genes
#'@param bi (logical) indicates if interactions should be considered bi-directed
#'@return returns a square matrix containing edge specific penalization factors
#'@author Polina Suter
#'@export
penUpdateIntra<-function(penmat,bnnames,type,intlist,pfbase=2,intpf=1,intsame=1,bi=FALSE) {

  allnameslocal<-as.vector(bnnames$allnamesonebn[bnnames$omicranges[[type]]])
  penlocal<-penmat[bnnames$omicranges[[type]],bnnames$omicranges[[type]]]
  allgenelocal<-as.vector(bnnames$allnames[[type]][,"gene"])
  rownames(penlocal)<-colnames(penlocal)<-allgenelocal
  intlist<-intlist[which(intlist$gene1%in%allgenelocal & intlist$gene2%in%allgenelocal),]
  if(nrow(intlist)>0) {
    penlocal<-fillPM(as.matrix(intlist),penlocal,intpf)
    if(bi) {
      penlocal<-fillPM(as.matrix(intlist)[,c(2,1),drop=FALSE],penlocal,intpf)
    }
  }
  if(intsame!=pfbase) {
    dups<-which(duplicated(allgenelocal))
    dupgenes<-cbind(allgenelocal[dups],allgenelocal[dups])
    colnames(dupgenes)<-c("gene1","gene2")
    penlocal<-fillPM(as.matrix(dupgenes),penlocal,intsame)
  }
  rownames(penlocal)<-colnames(penlocal)<-allnameslocal
  penmat[bnnames$omicranges[[type]],bnnames$omicranges[[type]]]<-penlocal
  return(penmat)
}

#'Updating penalization matrix (between two omics types)
#'
#'This function can be used to update an existing penalization matrix
#'
#'@param penmat a square penalization matrix; to initialize use \link{penInit}
#'@param bnnames object of class bnInfo; see \link{bnInfo}
#'@param type1 name of omics type (from)
#'@param type2 name of omics type (to)
#'@param intlist a list of known interactions; columns 'gene1' and 'gene2' must be present
#'@param pfbase a numeric value more or equal to 1, base penalization factor; 2 by default (1 corresponds to no penalization)
#'@param intpf (optional) a numeric value more or equal to 1, this value will be used to penalize interactions from 'intlist'
#'@param intlist (optional) a matrix or data frame containing a list of interactions and optionally their scores; 2 columns are necessary 'gene1' and 'gene2'
#'@param intsame penalization factor for edges connecting the same genes
#'@param bi (logical) indicates if interactions should be considered bi-directed
#'@return returns a square matrix containing edge specific penalization factors
#'@author Polina Suter
#'@export
penUpdateInter<-function(penmat,bnnames,type1,type2,intlist=NULL, pfbase=2, intpf=1,intsame=1,bi=FALSE){
  allnameslocal1<-as.vector(bnnames$allnamesonebn[bnnames$omicranges[[type1]]])
  allnameslocal2<-as.vector(bnnames$allnamesonebn[bnnames$omicranges[[type2]]])
  penlocal12<-penmat[bnnames$omicranges[[type1]],bnnames$omicranges[[type2]]]

  allgenelocal1<-as.vector(bnnames$allnames[[type1]][,"gene"])
  allgenelocal2<-as.vector(bnnames$allnames[[type2]][,"gene"])

  rownames(penlocal12)<-allgenelocal1
  colnames(penlocal12)<-allgenelocal2

  if(bi) {
    penlocal21<-penmat[bnnames$omicranges[[type2]],bnnames$omicranges[[type1]]]
    rownames(penlocal21)<-allgenelocal1
    colnames(penlocal21)<-allgenelocal1
  }

  if(!is.null(intlist)) {
  intlist12<-intlist[which(intlist$gene1%in%allgenelocal1 & intlist$gene2%in%allgenelocal2),]
  if(nrow(intlist)>0) {
    penlocal12<-fillPM(as.matrix(intlist12),penlocal12,intpf)
  }
  }

  if(!is.null(intlist)) {
  if(bi) {
    intlist21<-intlist[which(intlist$gene1%in%allgenelocal2 & intlist$gene2%in%allgenelocal1),]
    if(nrow(intlist)>0) {
      penlocal21<-fillPM(as.matrix(intlist),penlocal21,intpf)
    }
  }
  }

  if(intsame!=pfbase) {
    dups<-intersect(allgenelocal1,allgenelocal2)
    if(length(dups>0)) {
    dupgenes<-cbind(dups,dups)
    colnames(dupgenes)<-c("gene1","gene2")
    penlocal12<-fillPM(as.matrix(dupgenes),penlocal12,intsame)
    if(bi) {
      penlocal21<-fillPM(as.matrix(dupgenes),penlocal21,intsame)
    }
    }
  }
  rownames(penlocal12)<-allnameslocal1
  colnames(penlocal12)<-allnameslocal2
  penmat[bnnames$omicranges[[type1]],bnnames$omicranges[[type2]]]<-penlocal12
  if(bi) {
    rownames(penlocal21)<-allnameslocal2
    colnames(penlocal21)<-allnameslocal1
    penmat[bnnames$omicranges[[type2]],bnnames$omicranges[[type1]]]<-penlocal21
  }
  return(penmat)
}
fillPMcore<-function(intpair,pfmatrix,pf) {
  pfmatrix[rownames(pfmatrix)==intpair[1],colnames(pfmatrix)==intpair[2]]<-pf
  return(pfmatrix)
}
fillPM<-function(intlocal,pfmatrix,pf,usescore=FALSE) {
  if(usescore) {
    for(i in 1:nrow(intlocal)) {
      pfmatrix<-fillPMcore(intlocal[i,],pfmatrix,max(1,pf-2*as.numeric(intlocal[i,"score"])))
    }
  } else {
    for(i in 1:nrow(intlocal)) {
      pfmatrix<-fillPMcore(intlocal[i,],pfmatrix,pf)
    }
  }
  return(pfmatrix)
}

