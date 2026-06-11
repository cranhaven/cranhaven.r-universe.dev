
#' @title Modified SVDmiss function (package  SpatioTemporal) 
#' @description Modify ncomp = min(ncol(X),nrow(X),ncomp) for the matrix with nrow(X)<ncol(X)
#'
#'
#' @param X \code{X}  Data matrix, with missing values marked by 'NA'.
#' @param niter \code{niter}  Maximum number of iterations to run before exiting, 'Inf' will run until the 'conv.reldiff' criteria is met.
#' @param ncomp \code{ncomp}  Number of SVD components to use in the reconstruction (>0).
#' @param conv.reldiff \code{conv.reldiff}  Assume the iterative procedure has converged when the relative difference between two consecutive iterations is less than 'conv.reldiff'.  
#'
#'
#'  
#' @import abind  
#'
#' @export
#' @details See SVDmiss(package:SpatioTemporal) for details. 
#'  
#' @return See SpatioTemporal:: SVDmiss for details 
#'
#' 
#'


   


SVDmiss2<-function (X, niter = 200, ncomp = dim(X)[2], conv.reldiff = 0.001) {

    ncomp = min(ncol(X),nrow(X),ncomp) #-- gw-- number of components.
    print(paste("ncomp=",ncomp,sep=""))
    niter <- max(niter, 1)
    if (ncomp < 1) {
        stop("ncomp should be >0, is: ", ncomp)
    }
    Ina <- is.na(X)
    if (all(!Ina)) {
        svd0 <- svd(X)
        XF <- X
        i <- diff <- reldiff <- 0
    }
    else {
        u1 <- rowMeans(X, na.rm = TRUE)
        XM <- matrix(1, nrow(X), ncol(X))
        XM[Ina] <- 0
        XZ <- X
        XZ[Ina] <- 0
        v1 <- diag(t(XZ) %*% (XM * u1))/diag(t(XM * u1) %*% (XM *
            u1))
        XF <- X
        XF[Ina] <- (matrix(u1, ncol = 1) %*% matrix(v1, nrow = 1))[Ina]
        if (any(is.na(XF)))
            stop("Unable to complete matrix, too much missing data")
        reldiff <- conv.reldiff + 1
        i <- 0
        while (i < niter && reldiff > conv.reldiff) {
            svd0 <- svd(XF)
            Xnew <- X
            Xnew[Ina] <- (svd0$u[, 1:ncomp] %*% diag(svd0$d[1:ncomp],
                nrow = length(svd0$d[1:ncomp])) %*% t(svd0$v[,
                1:ncomp]))[Ina]
            diff <- max(abs(Xnew - XF))
            reldiff <- diff/max(abs(XF[Ina]))
            XF <- Xnew
            i <- i + 1
        }
    }
    final.diff <- c(diff, reldiff, i, niter)
    names(final.diff) <- c("diff", "rel.diff", "n.iter", "max.iter")
    return(list(svd = svd0, Xfill = XF, status = final.diff))
}
 
 





################################################################################################################################

#' @title Modified jive.predict function  (package: r.jive)    
#' @description Replace SVDmiss by SVDmiss2 in the function 
#'
#'
#' @param data.new \code{data.new}  A list of two or more linked data matrices on which to estimate JIVE scores.  These matrices must have the same column dimension N, which is assumed to be common. 
#' @param jive.output \code{jive.output} An object of class "jive", with row dimensions matching those for data.new. 
#' 
#' @import abind  
#'
#' @export
#' @details See jive.predict(package:r.jive) for details. 
#'
#' @return See r.jive:: jive.predict for details 
#'
#' 


jive.predict2<-function (data.new, jive.output) {
    l <- length(data.new)
    samp <- ncol(data.new[[1]])
    dim <- NULL
    label <- list()
    n <- NULL
    Si<-NULL # Si=nonzero rank domains
    for (i in 1:l)  if ( jive.output$rankA[i] >=1) Si<-c(Si,i) 
  

    for (i in Si) {
        dim[i] <- nrow(data.new[[i]])
        label[[i]] <- rep(i, time = dim[i])
        n[i] <- nrow(data.new[[i]]) * ncol(data.new[[i]])
    }
    label = unlist(label)
    for (i in Si) {
        if (sum(is.na(data.new[[i]])) > 0) {
            temp.2 <- SVDmiss2(data.new[[i]], ncomp = ncol(data.new[[i]]))[[1]]
            data.new[[i]] <- temp.2$u %*% diag(x = temp.2$d) %*%
                t(temp.2$v)
        }
    }
    scaled.new.data <- list()
    for (i in Si) {
        center <- matrix(jive.output$scale$`Center Values`[[i]],
            nrow = dim[i], ncol = samp, byrow = FALSE)
        scaled.new.data[[i]] <- (data.new[[i]] - center)
        scaled.new.data[[i]] <- scaled.new.data[[i]]/jive.output$scale$`Scale Values`[i]
        temp <- SVDmiss2(scaled.new.data[[i]], ncomp = ncol(scaled.new.data[[i]]))[[1]]
        scaled.new.data[[i]] <- temp$u %*% diag(x = temp$d) %*%
            t(temp$v)
    }
    data.new <- scaled.new.data
    n_joint <- jive.output$rankJ
    SVD = svd(do.call(rbind, jive.output$joint), nu = n_joint, nv = n_joint)  # Compute the singular-value decomposition of a rectangular matrix.
    joint.load <- SVD$u
    indiv.load <- list()
    n_indiv <- list()
    for (i in Si) {
        n_indiv[[i]] <- jive.output$rankA[i]
        SVDI = svd(jive.output$individual[[i]], nu = n_indiv[[i]],  nv = n_indiv[[i]])
        indiv.load[[i]] <- SVDI$u
    }
    score.indiv <- list()
   
    for (i in Si) {
        score.indiv[[i]] <- matrix(0, nrow = n_indiv[[i]], ncol = samp) 
    }
    
    #----------------------------------------------------------------------------------------
    error.pre = 1
    converged = 0
    count <- 2
    error.collect <- c()
    error.collect[1] <- error.pre
    sum.sq <- c()
    for (i in Si) {
        sum.sq[i] <- sum(data.new[[i]]^2)
    }
    while (converged == 0) {
        error.pre <- error.collect[count - 1]
        Indiv.diff <- list()
        for (i in Si) {
            Indiv.diff[[i]] <- data.new[[i]] - (indiv.load[[i]]) %*% score.indiv[[i]]  #error when indiv.load is zero rank in some domain
           # print(dim(Indiv.diff[[i]]))
        }
        Indiv.all.diff <- abind(Indiv.diff, along = 1)
        dim(Indiv.all.diff)
        dim(t(joint.load))
        score.joint = t(joint.load) %*% Indiv.all.diff
        j.split.load <- list()
        for (i in Si) {
            j.split.load[[i]] <- joint.load[(label == i), ]
        }
        for (i in Si) {
            score.indiv[[i]] <- t(indiv.load[[i]]) %*% (data.new[[i]] -
                j.split.load[[i]] %*% score.joint)
        }
        errors <- c()
        for (i in Si) {
            errors[i] <- sum((data.new[[i]] - (j.split.load[[i]]) %*%
                score.joint - indiv.load[[i]] %*% score.indiv[[i]])^2)
        }
        error.new <- sum(errors)/sum(sum.sq)
        error.collect <- c(error.collect, error.new)
        count <- count + 1
        converged = ifelse(abs(error.pre - error.new) < 1e-09,
            1, 0)
    }
    return(list(joint.scores = score.joint, indiv.scores = score.indiv,
        errors = error.collect, joint.load = joint.load, indiv.load = indiv.load))
}
 

################################################################################################################################
# install.packages("r.jive")
# library(r.jive) 
# Error in svd0$u[, 1:ncomp] : subscript out of bounds
# Reason: When we call jive.predict(), SVDmiss used to handle matrix(p genes/features * n sample), where p>n as shown in the sample. However, in our activity data P=10,n=600, which leads dimension error in svd(XF)$u =p*p when ncomp=n in svd0$u[, 1:ncomp].  
 

#  install.packages("SpatioTemporal")
#  library(SpatioTemporal)
#  install.packages("abind")
#  library(abind)

#  data(BRCA_data)
#  jive.data1<- Data 
#  res1 = jive(jive.data1) 
#  data.new=jive.data;  jive.output=res
#  J_Est <- jive.predict2(data.new=jive.data1,jive.output=res1)  
################################################################################################################################

