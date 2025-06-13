#' imputeIPZ
#'
#' Impute data according to IPZ method. Output is supposed to be passed to permute.IPZ
#'
#' @param data matrix as returned by as.matrix(generateData(param))
#' @param pool if TRUE impute events times from pooled Kaplan-Meier estimator (default: TRUE)
#' 
#' @return original data with 4 new columns (V1 and V2) containing the imputed observations
#' @references
#' Wang, R., Lagakos, S.~W. and Gray, R.~J. Testing and interval estimation for two-sample survival comparisons with small sample sizes and unequal censoring. Biostatistics, 11(4), 676--692, January 2010.
#' 
imputeIPZ <- function(data, pool=TRUE) {
    time <- data[,1]
    status <- data[,2]
    tmax <- max(time)
    
    ## split data set by treatment groups
    g <- data[,3] == 0
    data1 <- data[g,]
    data2 <- data[!g,]

    ## extract variable because indexing is not allowed in Surv function
    time1 <- data1[,1]
    status1 <- data1[,2]

    time2 <- data2[,1]
    status2 <- data2[,2]

    if(pool) {
        fitS1 <- survfit(Surv(time, status) ~ 1)
        fitS2 <- fitS1
    } else {
        fitS1 <- survfit(Surv(time1, status1) ~ 1)
        fitS2 <- survfit(Surv(time2, status2) ~ 1)
    }
   
    ## KM for censoring time in each group
    fit1 <- survfit(Surv(time1, 1-status1) ~ 1)
    fit2 <- survfit(Surv(time2, 1-status2) ~ 1)

    f <- function(work.data, trt.level, fitS, fitK) {
        U <- work.data[,1]
        delta <- as.logical(work.data[,2])
        Tdelta <- delta
        T <- U
      
        ## only impute event times for censored observations
        v <- !delta
        if(any(v)) {
            tmp <- sampleFromCondKM(U[v], fitS, tmax, 1)
            T[v] <- tmp[1,]
            Tdelta[v] <- tmp[2,]
        }
                
        n <- length(U)

        C <- sampleFromKM(n, fitK, 0, tmax, 0)[1,]

        sel1 <- delta & (U <= C)
        sel2 <- delta & (U > C)
        sel3 <- !delta & (U > C)
        sel4 <- !delta & (T <= C)
        sel5 <- !delta & (U < C) & (C < T)

        time <- data[,1]
        status <- data[,2]

        time1 <- numeric(n)
        status1 <- logical(n)

        if(any(sel1)) {
            time1[sel1] <- U[sel1]
            status1[sel1] <- TRUE
        }

        s <- sel2 | sel3 | sel5
        if(any(s)) {
            time1[s] <- C[s]
            status1[s] <- FALSE
        }

        if(any(sel4)) {
            time1[sel4] <- T[sel4]
            status1[sel4] <- Tdelta[sel4] ##TRUE
        }

        time[data[,3] == trt.level] <- time1
        status[data[,3] == trt.level] <- status1

        matrix(c(time, status), nrow=length(time), ncol=2)
    }

    V1 <- f(data2, 1, fitS2, fit1)

    V2 <- f(data1, 0, fitS1, fit2)

    ## columns: time, status, trt, entry, id, block, rnd.block, time1, status1, time2, status2
    cbind(data[,1:3], V1, V2)
}

#' permuteIPZ
#'
#' Permute treatment assignment after imputation (IPZ)
#'
#' @param data matrix as returned by impute.IPT
#' @param pZ vector of permuted indices if index is TRUE, else binary vector of treatment assignments
#' @param index indicates if pZ is a vector of indices or a binary vector of treatment assignments
#'
#' @return matrix with time, status, Z columns
#' @references
#' Wang, R., Lagakos, S.~W. and Gray, R.~J. Testing and interval estimation for two-sample survival comparisons with small sample sizes and unequal censoring. Biostatistics, 11(4), 676--692, January 2010.
#' 
permuteIPZ <- function(data, pZ, index=FALSE) {
    Z <- data[,3]
    if(index) pZ <- data[pZ, 3]

    time <- data[,1]
    status <- data[,2]

    if(length(pZ) != length(Z)) browser()
    
    a <- pZ > Z
    b <- pZ < Z
    ## pZ == Z: keep as is
    
    time[a] <- data[a, 6]
    status[a] <- data[a, 7]

    time[b] <- data[b, 4]
    status[b] <- data[b, 5]
    
    matrix(c(time, status, pZ), nrow=nrow(data), ncol=3)
}
