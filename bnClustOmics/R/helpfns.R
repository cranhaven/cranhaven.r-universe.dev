#'Adjusting the PDAG matrix to model constraints
#'This function can be used to adjust the adjacency matrix to model constraints, such as blacklist and background nodes
#'@param adj adjacency matrix representing a graph or posterior probabilities of all its edges
#'@param bnnames object of class bnNames
#'@param blacklist a square matrix with same dimensions as adj representing edges prohibited by the model
#'@return returns a matrix where entries prohibited by the model or blacklist are 0 and equal to correspondnding values of adj otherwise
adjustMixedDir<-function(adj,bnnames,blacklist){

  if(bnnames$nb+bnnames$no>0) {
    adj[1:(bnnames$nb+bnnames$no),1:(bnnames$nb+bnnames$no)]<-0
    adj[,1:(bnnames$nb+bnnames$no)]<-0
  }
  adj<-adj*1*(!blacklist)
  return(adj)
}
combOmicMat<-function(bnnames, matT,matPP) {
  jointmat<-matrix(0,nrow=bnnames$totn,bnnames$totn)
  colnames(jointmat)<-rownames(jointmat)<-bnnames$allnamesonebn
  jointmat<-matT
  PPrange<-as.vector(unlist(bnnames$omicranges[c("P","PP")]))
  jointmat[,PPrange]<-matPP[,PPrange]
  return(jointmat)
}
getmixed<-function(dag,nbin){
  dag[1:nbin,1:nbin]<-0
  return(dag)
}
sortdag<-function(DAG,n) {
  adj<-t(graph2m(DAG))
  sort<-as.numeric(tsort(DAG))
  sortback<-vector()
  sortedadj<-matrix(rep(0,n*n),nrow=n,ncol=n)
  newedgeweights<-edgeWeights(DAG)
  from<-c()
  to<-c()
  for (i in 1:n) {sortback[i]<-which(sort==i)}

  for (i in 1:n){
    for (j in 1:n){
      if(adj[i,j]>0){
        colindex<-which(sort==j)
        rowindex<-which(sort==i)
        sortedadj[rowindex,colindex]<-1
      }
    }
  }
  return(m2graph(t(sortedadj),nodes=c(1:n)))
}
orderdag<-function(adj) {
  n<-ncol(adj)
  allnodes<-c(1:n)
  curnodes<-c(1)
  order<-c()
  cntr<-1
  while(length(curnodes)<n & cntr<n) {
    npar<-apply(adj,2,sum)
    curnodes<-which(npar==0)
    order<-c(setdiff(curnodes,order),order)
    adj[curnodes,]<-0
    cntr<-cntr+1
  }

  if(sum(adj)==0) return(order)
  else stop("not a DAG")

}
getrepr<-function(pdag,dag) {
  n<-ncol(pdag)
  for(i in 1:n) {
    for(j in 1:n) {
      if(pdag[i,j]==pdag[j,i] & pdag[i,j]==1) {
        pdag[i,j]<-dag[i,j]
        pdag[j,i]<-dag[j,i]
      }
    }
  }
  if(is.DAG(m2graph(pdag))) {
    return(pdag)
  } else {
    stop("not possible to resolve cycles!")
  }
}
modifydag<-function(dag,shd) {
  n<-ncol(dag)
  ord<-orderdag(dag)
  edges<-which(dag==1)
  nedges<-length(edges)
  delE<-min(nedges,floor(shd/2))
  addE<-shd-delE

  deledges<-sample(edges,delE)
  addedges<-sample(ord[1:(n-1)],addE,replace=TRUE)
  for(i in addedges) {
    pos<-which(ord==i)
    pari<-ord[(pos+1):n]
    if(length(pari)==1) {
      newpar<-pari
      dag[newpar,i]<-1
    } else {
      newpar<-sample(pari,1)
      if(dag[newpar,i]!=1) {
        dag[newpar,i]<-1 } else {
          newpar<-sample(pari,1)
          dag[newpar,i]<-1
        }
    }
  }
  dag[deledges]<-0
  return(dag)

}
consensusScores<-function(scorepar,chain,startstep=500,onlymain=FALSE) {
  membscores<-list()
  for(i in startstep:length(chain)) {
    membscores[[i-startstep+1]]<-scoreagainstDAG(scorepar,chain[[i]],onlymain=onlymain)
  }
  membscores<-Reduce('+',membscores)/length(membscores)
  return(membscores)
}
consensusScoresDBN<-function(scorepar,chain,startstep=500,onlymain=TRUE) {
  membscores<-list()
  for(i in startstep:length(chain)) {
    membscores[[i-startstep+1]]<-scoreagainstDBN(scorepar,chain[[i]],onlymain=onlymain)
  }
  membscores<-Reduce('+',membscores)/length(membscores)
  return(membscores)
}
commonSpace<-function(spaces,hardlim) {
  newspaces<-list()
  k<-length(spaces)
  commonspace<-1*Reduce('|', spaces)
  parsets<-apply(commonspace,2,sum)
  toobig<-which(parsets>hardlim)
  for(i in 1:length(spaces)) {
    newspaces[[i]]<-commonspace
    otherlocal<-1*Reduce('|', spaces[c(1:k)[-i]])
   for(j in toobig) {
     newspaces[[i]][,j]<-spaces[[i]][,j]
     npar<-sum(newspaces[[i]][,j])
     if(npar<(hardlim-1)) {
       ones<-which(otherlocal[,j]==1)
       newspaces[[i]][sample(ones,hardlim-npar),j]<-1
     }
   }
  }
  return(newspaces)
}
cleanDAGs0<-function(dags,bnnames,omicdata,memb,thresh=1){
  k<-length(unique(memb))
  #omicdata<-Reduce("cbind",omicdata)
  colnames(omicdata)<-bnnames$allnamesonebn
  n<-ncol(dags[[1]])
  for(j in c(1:k)) {
    if(length(which(memb==j))>0) {
    data<-as.data.frame(omicdata[which(memb==j),,drop=FALSE])
    dag<-dags[[j]]
    for(i in 1:n) {
      pari<-which(dag[,i]!=0)
      if(length(pari)>0) {
        for(p in 1:length(pari)) {
          data_local<-data[,pari[p],drop=FALSE]
          if(length(which(data_local!=0))<1) dags[[j]][pari[p],i]<-0
        }
      }
    }
    }
  }
  return(dags)
}
paramfit<-function(dag,data) {
  res<-list()
  ordery<-orderdag(dag)
  if(!is.data.frame(data)) {
    data<-as.data.frame(data)
  }
  colnames(dag)<-colnames(data)
  for(i in ordery) {
    pari<-which(dag[,i]==1)
    vars<-colnames(dag)[pari]
    if(length(pari)>0) {
      res[[i]]<-lm(as.formula(paste(paste("V", i, sep = ""), "~", paste(vars,sep="+"), sep="")), data)
    } else {
      res[[i]]<-lm(as.formula(paste(paste("V", i, sep = ""), "~ 1")), data)

    }
  }
  res
}
transformInt<-function(int,bnnames,cols=c(1,2)) {
  for(i in cols) {
    int[,paste("gene",i,sep="")]<-getGenes(bnnames,int[,i])
  }
  return(int)
}
getGenes<-function(bnnames,namevec) {
  bnnames<-Reduce("rbind",bnnames$allnames)
  #bnnames<-bnnames[-which(duplicated(rownames(bnnames))),]
  return(bnnames[namevec,"gene"])
}
addpost<-function(intlist,bnres) {
  addco<-NULL
  for(i in 1:nrow(intlist)) {
    #if(grepl(".CN",intlist$from[i])) intlist$from[i]<-sub("\\.CN","",intlist$from[i])
    localp<-c()
    for(j in 1:length(bnres$ep)) {
      localp<-c(localp,bnres$ep[[j]][intlist$from[i],intlist$to[i]])
    }
    addco<-rbind(addco,localp)
  }
  colnames(addco)<-paste("pcl",1:length(bnres$ep),sep="")
  return(cbind(intlist,addco))
}
checkInt<-function(int2check,intlist,newname="foundflag") {
  foundvec<-rep(FALSE,length=nrow(int2check))
  for(i in 1:nrow(int2check)) {
    findflag<-which((intlist$gene1==int2check$gene1[i] & intlist$gene2==int2check$gene2[i]) |
                      (intlist$gene2==int2check$gene1[i] & intlist$gene1==int2check$gene2[i]))
    if(length(findflag)>0) foundvec[i]<-TRUE
  }
  int2check<-cbind(int2check,foundvec)
  colnames(int2check)[ncol(int2check)]<-newname
  return(int2check)
}
getcors_local<-function(omicdata,node1,node2,type1,type2,memb,k) {
  a<-getLevels(omicdata,type1,node1,memb,k)
  a1<-getLevels(omicdata,type2,node2,memb,k)
  return(cor(a,a1))
}
getLevels<-function(omicdata,type,node,memb,k=NULL) {
    return(omicdata[[type]][which(memb==k),node])
}

