"haplodata" <-
function(dat){
 if (!is.matrix(dat)) dat <- as.matrix(dat)
 nloci <- ncol(dat)
 P.obj <- allelefreqs(dat)
 if (!P.obj$all.polym)
    cat(c("\nSome loci (", P.obj$non.polym, ") are not polymorphic.\n\n"))
 P <- P.obj$freqs
 Q <- qnorm(P)
 C <- cor(dat)
 null.mat <- matrix(0, nrow=nloci,ncol=nloci) 
 vmat <- .C("covariance", as.integer(nloci),as.double(C),as.double(P),as.double(Q),
            rlt=as.double(null.mat), PACKAGE="hapsim")$rlt
 V <- matrix(vmat, nrow=nloci, ncol=nloci)
 if (!checkpd(V)) V <- makepd(V)
 D <- divlocus(dat)
 return(list(freqs=P, cor=C, cov=V, div=D))
}

