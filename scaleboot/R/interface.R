##
##  scaleboot: R package for multiscale bootstrap
##  Copyright (C) 2006-2007 Hidetoshi Shimodaira
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
######################################################################
### MAIN: INTERFACE TO OTHER SOFTWARE

### interface to pvclust for hierarchical clustering
##
## library(pvclust) defines class "pvclust"
## x : pvclust object
## y : sbfits object
## y <- sbfits(x)
sbfit.pvclust <- function(x,...)
  sbfit(as.matrix(x$count)/x$nboot,x$nboot,1/x$r,...)
##
## y <- sbfits(x.old)
## x.new <- sbpvclust(x.old,y) # use "k.2"
## x.new <- sbpvclust(x.old,y,k=3) # use "k.3"
sbpvclust <- function(x,mbs,k=3,select="average",...) {
  k=k[1] # uses only one k value
  a <- summary(mbs,k=c(k,1),hypothesis="alternative",...) # compute k.k and k.1
  selpv <- selectpv(a,select)
  ## bp uses k.1
  bv <- sapply(selpv$pvpe,function(b) b$pv[2])  
  be <- sapply(selpv$pvpe,function(b) b$pe[2])  
  x$edges[,"bp"] <- bv
  x$edges[,"se.bp"] <- be
  ## au uses k.k
  pv <- sapply(selpv$pvpe,function(b) b$pv[1])  
  pe <- sapply(selpv$pvpe,function(b) b$pe[1])  
  x$edges[,"au"] <- pv
  x$edges[,"se.au"] <- pe
  ## si uses sk.k
  spv <- sapply(selpv$pvpe,function(b) b$spv[1])  
  spe <- sapply(selpv$pvpe,function(b) b$spe[1])  
  x$edges[,"si"] <- spv
  x$edges[,"se.si"] <- spe
  ## (v,c) uses (beta0,beta1)
  aa <- sapply(selpv$pvpe,function(b) sbgetbetapar1(b$betapar)$beta)
  x$edges[,"v"] <- aa["beta0",]
  x$edges[,"c"] <- aa["beta1",]
  ## pchi is not defined, so let it zero
  x$edges[,"pchi"] <- 0
  ## method overwrite
  x$method <- c("scaleboot", paste("k=",k,sep=""))
  ## new class: sbclust
  class(x) <- c("sbclust", class(x))

  x
}

## 
## plot(x.new)  # by r.suzuki
##
plot.sbclust <- function(x, ...) {
  cl <- match.call()
  if(!("main" %in% names(cl))) {
    cl$main <- paste("Cluster p-values (%) by", paste(x$method, collapse=" "))
  }
  
  class(x) <- "pvclust"
  cl[[1L]] <- quote(plot)
  cl[[2L]] <- bquote(.(x))
  
  eval(cl, parent.frame())
}

##########################################################################
### PHYLOGENETIC ANALYSIS WITH CONSEL
### 
### interface to CONSEL for phylogenetic inference
##
### prepare table for phylogenetic inference (sort by stat)
## trees, edges: outout from relltest
## edge2tree: ass information from treeass (consel)
## treename: vector of tree names (often, topologies)
## edgename: vector of edge names (often, clade patterns)
##
## just sorting the trees and edges by their stat values (log-likelihood values)
##
## Primary input is (trees, edges, edge2tree), but they can be computed from
## (relltest, ass). So either of the two inputs should be specified.
##
sbphylo <- function(relltest,ass,
                    trees,edges,edge2tree,
                    treename=NULL,edgename=NULL,taxaname=NULL,mt=NULL,sort=TRUE) {
  ## we need: trees, edges, edge2tree
    if(!missing(ass)) {
    trees.names <- attr(ass,"trees")
    edges.names <- attr(ass,"edges")
  }
  if(missing(edge2tree) & !missing(ass)) edge2tree <- ass[edges.names]
  if(missing(trees)) trees <- relltest[trees.names]
  if(missing(edges)) edges <- relltest[edges.names]
  
  ## compute reverse mapping  (actually stored in ass file, but ignored when reading)
  tree2edge <- revmap(edge2tree)
  
  if(!sort) {
    ## not sorting
    order.tree <- invorder.tree <- seq(along=trees)
    names(order.tree) <- names(invorder.tree) <- names(trees)
    order.edge <- invorder.edge <- seq(along=edges)
    names(order.edge) <- names(invorder.edge) <- names(edges)
  } else {
    ## use the log-likelihood for sorting
    stat.tree <- attr(trees,"stat") # likelihood
    stat.edge <- attr(edges,"stat") # likelihood
  
    order.tree <- order(stat.tree)
    names(order.tree) <- paste("T",seq(along=order.tree),sep="")
    order.edge <- order(stat.edge)
    names(order.edge) <- paste("E",seq(along=order.edge),sep="")
    invorder.tree <- invperm(order.tree)
    names(invorder.tree) <- names(trees)
    invorder.edge <- invperm(order.edge)
    names(invorder.edge) <- names(edges)
  
    trees <- trees[order.tree]; names(trees) <- names(order.tree)
    edges <- edges[order.edge]; names(edges) <- names(order.edge)

    if(!is.null(treename)) {
      treename <- treename[order.tree]; names(treename) <- names(order.tree)
    }
    if(!is.null(edgename)) {
      edgename <- edgename[order.edge]; names(edgename) <- names(order.edge)
    }
    if(!is.null(mt)) {
      mt <- mt[,order.tree]; colnames(mt) <- names(order.tree)
    }
  
    edge2tree <- lapply(edge2tree, function(a) unname(invorder.tree[a]))[order.edge]
    names(edge2tree) <- names(order.edge)
    tree2edge <- lapply(tree2edge, function(a) unname(invorder.edge[a]))[order.tree]
    names(edge2tree) <- names(order.edge)
  }
  
  ans <- list(trees=trees,edges=edges,edge2tree=edge2tree, tree2edge=tree2edge, 
          order.tree=order.tree,order.edge=order.edge,invorder.tree=invorder.tree,invorder.edge=invorder.edge,
          treename=treename, edgename=edgename, taxaname=taxaname,mt=mt)
  class(ans) <- "sbphylo"
  ans
}


print.sbphylo <- function(x,...) {
  cat(x$taxa,"\n")
  cat(length(x$trees),"trees and",length(x$edges),"edges\n")
}

## x: edge2tree
## output: tree2edge
revmap <- function(x,lab="t") {
  n <- max(unlist(x))
  y <- vector("list",n)
  if(!is.null(lab)) names(y) <- paste(lab,seq(along=y),sep="")
  for(i in seq(along=x)) {
    for(j in x[[i]]) y[[j]] <- c(y[[j]],i)
  }
  y
}

## x: permutation of {1,2,...,n}
## output: inverse permutation
invperm <- function(x) order(x)

###
### prepare tables for phylogenetic analysis
### 
## x: output from sbphylo
##

summary.sbphylo <- function(object,k=2,...) {
  opt.percent <- sboptions("percent",FALSE); opt.digits.pval <- sboptions("digits.pval",1)
  ## which pvalue to use
  bp <- "k.1"  ### using au(k=1) instead of raw bp
  k <- k[k!=1] # remove k=1 from k
  auk <- paste("k.",k,sep="") ### using au(k=k) for AU
  sik <- paste("sk.",k,sep="") ### using si(k=k) for SI
  
  ## names
  natree <- names(object$trees)
  naedge <- names(object$edges)
  
  ## table for trees
  tab0 <- formatting.relltest(object$trees) # rownames of shtest is not correct
  tab1 <- formatting.summary.scalebootv(summary(object$trees,k=c(1,k)))
  a <- cbind(tab1$character,tab0$character) 
  out.tree <- a[,c("stat","shtest",bp,auk,sik,"beta0","beta1")]
  if(!is.null(object$treename)) {
    out.tree <- cbind(out.tree, object$treename); colnames(out.tree)[ncol(out.tree)] <- "tree"
  }
  out.tree <- cbind(out.tree, sapply(object$tree2edge,function(a) paste(naedge[sort(a)],collapse=",")))
  colnames(out.tree)[ncol(out.tree)] <- "edge"
  
  ## table for edges
  tab2 <- formatting.summary.scalebootv(summary(object$edges,k=c(1,k)))
  a1 <- tab2$character
  out.edge <- a1[,c(bp,auk,sik,"beta0","beta1")]
  if(!is.null(object$edgename))   {
    out.edge <- cbind(out.edge, object$edgename); colnames(out.edge)[ncol(out.edge)] <- "edge"
  }
  out.edge <- cbind(out.edge, sapply(object$edge2tree,function(a) paste(natree[sort(a)],collapse=",")))
  colnames(out.edge)[ncol(out.edge)] <- "tree"
  
  ## quit
  sboptions("percent", opt.percent); sboptions("digits.pval", opt.digits.pval)
  
  ans <- list(tree=list(character=out.tree,value=cbind(tab1$value,tab0$value)),
            edge=list(character=out.edge,value=tab2$value),
            taxa=object$taxaname)
  class(ans) <- "summary.sbphylo"
  ans
}

print.summary.sbphylo <- function(x,...) {
  cat(x$taxa,"\n")
  catmat(x$tree$character)
  catmat(x$edge$character)
}
  
##
## read "mt" format of consel
##  (a matrix of site-wise log-likelihood values)
##
## ntree: number of trees (i.e., number of items)
## nsite: number of sites (i.e., sample size)
## data: x[i,j] = tree-i, site-j
##
## output: x = matrix of size nsite*ntree
##
## Details of file format:
##
## int ntree, nsite;
## double x[1,1], x[1,2],...,x[1,nsite];
## double x[2,1], x[2,2],...,x[2,nsite];
## ...
## double x[ntree,1], x[ntree,2],...,x[ntree,nsite];
##
read.mt <- function(file,tlab="t") {
  a <- scan(file,comment.char="#",quiet=T)
  ntree <- a[1]
  nsite <- a[2]
  if(length(a) != (2+ntree*nsite))
    stop(length(a),"!= 2+",ntree,"*",nsite)
  cat("Read",ntree,"items of length",nsite,"\n")
  x <- matrix(a[-c(1,2)],nsite,ntree)
  if(!is.null(tlab)) colnames(x) <- paste(tlab,seq(ncol(x)),sep="")  
  x
}
## read "ass" format of consel
##   (association between trees and edges)
##
## nedge: number of edges (i.e., number of hypotheses)
## ntree: number of trees (i.e., number of items)
## association of i-th edge:
## x[i] = {x[i][1],...,x[i][nt[i]]}
## (x[i][j] is a tree-id starting from 0, but adjust to be from 1)
## association of i-th tree:
## y[i] = {y[i][1],...,y[i][ne[i]]}
## (y[i][j] is a edge-id starting from 0, but adjust to be from 1)
##
## output: list(x,y)
##
## Details of file format:
##
## int nedge;
## int nt[1],x[1][1],...,x[1][nt[1]];
## int nt[2],x[2][1],...,x[2][nt[2]];
## ...
## int nt[nedge],x[nedge][1],...,x[nedge][nt[nedge]];
## int ntree;
## int ne[1],y[1][1],...,y[1][ne[1]];
## int ne[2],y[2][1],...,y[2][ne[2]];
## ...
## int ne[ntree],y[ntree][1],...,y[ntree][ne[ntree]];
##
read.ass <- function(file,identity=TRUE,tlab="t",elab="e") {
  ## reading file
  a <- scan(file,comment.char="#",quiet=T)
  
  ## reading edge2tree association
  k <- 1
  nedge <- a[k]; k <- k+1
  x <- vector("list",nedge)
  for(i in 1:nedge) {
    nt <- a[k]; k <- k+1
    x[[i]] <- 1+a[seq(k,length=nt)]; k <- k+nt
  }
  if(!is.null(elab)) names(x) <- paste(elab,seq(along=x),sep="")
  
  ## reading tree2edge association
  ntree <- a[k]; k <- k+1
  y <- vector("list",ntree)
  for(i in 1:ntree) {
    ne <- a[k]; k <- k+1
    y[[i]] <- 1+a[seq(k,length=ne)]; k <- k+ne
  }
  if(!is.null(tlab)) names(y) <- paste(tlab,seq(along=y),sep="")
  if(k-1 != length(a)) stop("size mismatch")
  
  ##
  if(identity) {
    ## generate identity matching for trees and discard tree2edge association
    x0 <- vector("list",ntree)
    for(i in 1:ntree) x0[[i]] <- i  # identity associations
    if(!is.null(tlab)) names(x0) <- paste(tlab,seq(along=x0),sep="")
    ## returns identity and edge2tree
    ans <- c(x0,x) # returns only x without y
    attr(ans,"trees") <- names(x0)
    attr(ans,"edges") <- names(x)
  } else {
    ## returns edge2tree and tree2edge
    ans <- list(x=x,y=y)
  }

  cat("Read",nedge,"items for",ntree,"elements\n")
  ans
}
## read "cnt" format of consel
##   (counts of hypotheses)
##
## ntree: number of trees (or hypotheses, in general)
## nscale: number of scales
## id: tree id's (starting from 0, but adjusted to be from 1)
## lik: observed lik differences of trees
## rs: relative sample sizes (n'/n) ; sa = 1/rs
## nb: numbers of bootstrap replicates
## cnt[i,j]: counts for tree-i, scale-j.
##
## output:
##  list(bps,nb,sa,cnt,id,val)
##   bps = cnt/nb
##   sa = 1/rs
##   val = lik
##
## Details of file format:
##
## int ntree,id[1],id[2],...,id[ntree];
## int ntree; double lik[1],...,lik[ntree];
## int ntree, nscale;
## double rs[1],rs[2],...,rs[nscale];
##  @ repeated ntree times
## int ntree, nscale;
## int nb[1],nb[2],...,nb[nscale];
##  @ repeated ntree times
## int ntree, nscale;
## int cnt[1,1],cnt[1,2],...,cnt[1,nscale];
## int cnt[2,1],cnt[2,2],...,cnt[2,nscale];
## ...
## int cnt[ntree,1],cnt[ntree,2],...,cnt[ntree,nscale];
##
read.cnt <- function(file) {
  a <- scan(file,comment.char="#",quiet=T)
  k <- 1
  ntree <- a[k]; k <- k+1
  id <- 1+a[seq(k,length=ntree)]; k <- k+ntree
  if(a[k]!=ntree) stop(a[k],"!=",ntree); k <- k+1
  lik <- a[seq(k,length=ntree)]; k <- k+ntree
  if(a[k]!=ntree) stop(a[k],"!=",ntree); k <- k+1
  nscale <- a[k]; k <- k+1
  rs <- a[seq(k,length=nscale)]; k <- k+nscale
  for(i in seq(length=ntree-1)) {
    if(any(a[seq(k,length=nscale)]!=rs)) stop("rs mismatch")
    k <- k+nscale
  }
  if(a[k]!=ntree) stop(a[k],"!=",ntree); k <- k+1
  if(a[k]!=nscale) stop(a[k],"!=",nscale); k <- k+1
  nb <- a[seq(k,length=nscale)]; k <- k+nscale
  for(i in seq(length=ntree-1)) {
    if(any(a[seq(k,length=nscale)]!=nb)) stop("nb mismatch")
    k <- k+nscale
  }
  if(a[k]!=ntree) stop(a[k],"!=",ntree); k <- k+1
  if(a[k]!=nscale) stop(a[k],"!=",nscale); k <- k+1
  cnt <- matrix(a[seq(k,length=nscale*ntree)],nscale,ntree)
  k <- k+nscale*ntree
  if(k-1 != length(a)) stop("size mismatch" ) 
  bps <- cnt/nb
  cnt <- t(cnt); bps <- t(bps)
  
  cat("Read",ntree,"items for",nscale,"scales\n")  
  list(bps=bps,nb=nb,sa=1/rs,cnt=cnt,id=id,val=lik)
}


