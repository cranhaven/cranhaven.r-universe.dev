
GLP<-function(X,y,m.max=4,components=NULL,alpha=0.05,c.poly=0.5,clust.alg='kmeans',perm=0,
              combine.criterion='pvalue',multiple.comparison=TRUE,
              compress.algorithm=FALSE,nbasis=8, return.LPT=FALSE,return.clust=FALSE){
  #combine.criterion: kernel or pvalue
  num.g<-length(unique(y))
  lp.sig<-c()

  ##create LP tables
  if(!is.null(components)){
       m.max<-max(components)
       m.length<-length(components)
  }else{
       m.length<-m.max
  }
     
  if(compress.algorithm==FALSE){
     t.res<-matrix(0,m.length,3)
     colnames(t.res)<-c('component','comp.GLP','pvalue')
     # check if centralize adj is needed:
     if(m.max<1 | m.max!=floor(m.max)){
       stop("maximum order m.max must be a positive integer")
     }
     m.i<-1
     w0<-W.Gen(X,m.i,c.poly)
     res1<-graph.clust.test(y,w0$W,method=clust.alg,perm=perm)
     centralize_p<-c(m.i,res1$LPINFOR,res1$p.val)
     
     
     
     if(is.null(components)){
       t.res[1,]<-centralize_p
       if(m.max>1){
         if(centralize_p[3]<alpha){
           X.even<-apply(X,2, function(x,Y){ x - rep(aggregate(x,list(Y),median)$x, 
                                                     as.vector(table(Y)))}, Y=y)
           for(i in 2:m.length){
             m.i<-i
             if(floor(m.i/2)==m.i/2){
               w0<-W.Gen(X.even,m.i,c.poly)
               res1<-graph.clust.test(y,w0$W,method=clust.alg,perm=perm)
               t.res[i,]<-c(m.i,res1$LPINFOR,res1$p.val)
             }else{
               w0<-W.Gen(X,m.i,c.poly)
               res1<-graph.clust.test(y,w0$W,method=clust.alg,perm=perm)
               t.res[i,]<-c(m.i,res1$LPINFOR,res1$p.val)
             }
           }	
         }else{
           for(i in 2:m.length){
             m.i<-i
             w0<-W.Gen(X,m.i,c.poly)
             res1<-graph.clust.test(y,w0$W,method=clust.alg,perm=perm)
             t.res[i,]<-c(m.i,res1$LPINFOR,res1$p.val)
           }
         } 
       }
     }else{
       if(centralize_p[3]<alpha){
         X.even<-apply(X,2, function(x,Y){ x - rep(aggregate(x,list(Y),median)$x, 
                                                   as.vector(table(Y)))}, Y=y)
         for(i in 1:m.length){
           m.i<-components[i]
           if(floor(m.i/2)==m.i/2){
             w0<-W.Gen(X.even,m.i,c.poly)
             res1<-graph.clust.test(y,w0$W,method=clust.alg,perm=perm)
             t.res[i,]<-c(m.i,res1$LPINFOR,res1$p.val)
           }else{
             w0<-W.Gen(X,m.i,c.poly)
             res1<-graph.clust.test(y,w0$W,method=clust.alg,perm=perm)
             t.res[i,]<-c(m.i,res1$LPINFOR,res1$p.val)
           }
         }	
       }else{
         for(i in 1:m.length){
           m.i<-components[i]
           w0<-W.Gen(X,m.i,c.poly)
           res1<-graph.clust.test(y,w0$W,method=clust.alg,perm=perm)
           t.res[i,]<-c(m.i,res1$LPINFOR,res1$p.val)
         }
       }
     }
     
     ##adjusted pval:
     if(multiple.comparison==TRUE){
       pval.adj<-p.adjust(t.res[,3],method='BH')
       lp.sig.ind<-as.numeric(which(pval.adj<=alpha))
     }else{
       lp.sig.ind<-as.numeric(which(t.res[,3]<=alpha))
     }
     
     #record significant components
     lp.sig<-sort(t.res[lp.sig.ind,1])
     
     #overallW based on significant comp
     W.overall<-matrix(0,nrow(X),nrow(X))
     if(return.LPT==TRUE){
       LPTmat<-list()
     }
     if(length(lp.sig)>0){
       if(centralize_p[3]<alpha){
         for(i in 1:length(lp.sig)){
           m.i<-lp.sig[i]
           if(floor(m.i/2)==m.i/2){
             w0<-W.Gen(X.even,m.i,c.poly)
           }else{
             w0<-W.Gen(X,m.i,c.poly)
           }
           W.overall<-W.overall+w0$W
           if(return.LPT==TRUE){
             LPTmat[[m.i]]<-w0$LPT
           } 
         }
       }else{
         for(i in 1:length(lp.sig)){
           m.i<-lp.sig[i]
           w0<-W.Gen(X,m.i,c.poly)
           W.overall<-W.overall+w0$W
           if(return.LPT==TRUE){
             LPTmat[[m.i]]<-w0$LPT
           } 
         }
       }
     }
     
     
     if(combine.criterion=='kernel'){
       #graph-based test:
       if(length(lp.sig)>0){  #when there are significant components
         res0<-graph.clust.test(y,W.overall,method=clust.alg,perm=perm,return.clust)
       }else{                 # otherwise, p-value forced to 1
         res0<-list()
         res0$LPINFOR<-0
         res0$p.val<-1
         res0$test<-list()
         res0$test$LPINFOR<-0
         res0$test$p.val<-1
         res0$clust<-NULL
       }
       
       #output
       out<-list()
       if(return.clust==FALSE){
         result<-res0
       }else if(return.clust==TRUE){
         result<-res0$test
       }
       
       
       out$GLP<-result$LPINFOR
       out$pval<-result$p.val
       out$table<-t.res
       out$component<-lp.sig
       if(return.LPT==TRUE){
         out$LPT<-do.call(cbind,LPTmat)
       }
       
       
       if(return.clust==TRUE){
         out$clust<-res0$clust
       }
       
     }else if(combine.criterion=='pvalue'){
       pval.list<-t.res[,3]
       out <- list()
       out$pval <- pchisq(-2*sum(log(pval.list)),df=2*m.max,lower.tail=FALSE)
       out$table <- t.res
       out$component<-lp.sig
       if(return.LPT==TRUE){
         out$LPT<-do.call(cbind,LPTmat)
       }
     }
  }else if(compress.algorithm==TRUE){
    t.res <- matrix(0, m.length, 2)
    colnames(t.res) <- c("component", "pvalue")
    if (m.max < 1 | m.max != floor(m.max)) {
      stop("maximum order m.max must be a positive integer")
    }
    m.i <- 1
    w0 <- LPKsample::W.Gen(X, m.i, c.poly)
    res1 <- LP.struct.test(w0$W, m=nbasis)$pval
    centralize_p <- c(m.i, res1)
    if (is.null(components)) {
      t.res[1, ] <- centralize_p
      if (m.max > 1) {
        if (centralize_p[2] < alpha) {
          X.even <- apply(X, 2, function(x, Y) {
            x - rep(aggregate(x, list(Y), median)$x, as.vector(table(Y)))
          }, Y = y)
          for (i in 2:m.length) {
            m.i <- i
            if (floor(m.i/2) == m.i/2) {
              w0 <- LPKsample::W.Gen(X.even, m.i, c.poly)
              res1 <- LP.struct.test(w0$W, m=nbasis)$pval
              t.res[i, ] <- c(m.i, res1)
            }
            else {
              w0 <- LPKsample::W.Gen(X, m.i, c.poly)
              res1 <- LP.struct.test(w0$W, m=nbasis)$pval
              t.res[i, ] <- c(m.i, res1)
            }
          }
        }
        else {
          for (i in 2:m.length) {
            m.i <- i
            w0 <- LPKsample::W.Gen(X, m.i, c.poly)
            res1 <- LP.struct.test(w0$W, m=nbasis)$pval
            t.res[i, ] <- c(m.i, res1)
          }
        }
      }
    }else {
      if (centralize_p[2] < alpha) {
        X.even <- apply(X, 2, function(x, Y) {
          x - rep(aggregate(x, list(Y), median)$x, as.vector(table(Y)))
        }, Y = y)
        for (i in 1:m.length) {
          m.i <- components[i]
          if (floor(m.i/2) == m.i/2) {
            w0 <- LPKsample::W.Gen(X.even, m.i, c.poly)
            res1 <- LP.struct.test(w0$W, m=nbasis)$pval
            t.res[i, ] <- c(m.i, res1)
          }
          else {
            w0 <- LPKsample::W.Gen(X, m.i, c.poly)
            res1 <- LP.struct.test(w0$W, m=nbasis)$pval
            t.res[i, ] <- c(m.i, res1)
          }
        }
      }
      else {
        for (i in 1:m.length) {
          m.i <- components[i]
          w0 <- LPKsample::W.Gen(X, m.i, c.poly)
          res1 <- LP.struct.test(w0$W, m=nbasis)$pval
          t.res[i, ] <- c(m.i, res1)
        }
      }
    }
    if (multiple.comparison == TRUE) {
      pval.adj <- p.adjust(t.res[, 2], method = "BH")#may need bonferoni
      lp.sig.ind <- as.numeric(which(pval.adj <= alpha))
    }
    else {
      lp.sig.ind <- as.numeric(which(t.res[, 2] <= alpha))
    }
    lp.sig <- sort(t.res[lp.sig.ind, 1])
    W.overall <- matrix(0, nrow(X), nrow(X))
    if (return.LPT == TRUE) {
      LPTmat <- list()
    }
    if (length(lp.sig) > 0) {
      if (centralize_p[2] < alpha) {
        for (i in 1:length(lp.sig)) {
          m.i <- lp.sig[i]
          if (floor(m.i/2) == m.i/2) {
            w0 <- LPKsample::W.Gen(X.even, m.i, c.poly)
          }
          else {
            w0 <- LPKsample::W.Gen(X, m.i, c.poly)
          }
          W.overall <- W.overall + w0$W
          if (return.LPT == TRUE) {
            LPTmat[[m.i]] <- w0$LPT
          }
        }
      }
      else {
        for (i in 1:length(lp.sig)) {
          m.i <- lp.sig[i]
          w0 <- LPKsample::W.Gen(X, m.i, c.poly)
          W.overall <- W.overall + w0$W
          if (return.LPT == TRUE) {
            LPTmat[[m.i]] <- w0$LPT
          }
        }
      }
    }
    if(combine.criterion=='kernel'){
      if (length(lp.sig) > 0) {
        res0 <- LP.struct.test(W.overall, m=nbasis)$pval
      }
      else {
        res0 <- 1
      }
      out <- list()
      out$pval <- res0
      out$table <- t.res
      out$component <- lp.sig
      if (return.LPT == TRUE) {
        out$LPT <- do.call(cbind, LPTmat)
      }
    }else if(combine.criterion=='pvalue'){
      pval.list<-t.res[,2]
      
      out <- list()
      out$pval <- pchisq(-2*sum(log(pval.list)),df=2*m.max,lower.tail=FALSE)
      out$table <- t.res
      out$component <- lp.sig
      if (return.LPT == TRUE) {
        out$LPT <- do.call(cbind, LPTmat)
      }
    }
  }
  
  return(out)

}
