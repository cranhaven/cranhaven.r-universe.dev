#' exactLR
#'
#' One-sided exact / approximate permutation and asymptotic log-rank test
#'
#' This function performs a standard exact or approximate permutation test
#' which is only valid under the extended null hypothesis of equal survival
#' AND censoring distributions.
#'
#' @param B number of random permutations (only used if type="approximate")
#' @param formula a formula object, as used by \code{\link{coxph}}, left hand side must be a 'Surv' object, right hand side may only consist of a single term (treatment indicator)
#' @param data data.frame or list containing the variables in "formula", by default "formula" is evaluated in the parent frame
#' @param type if type="exact" performs complete enumeration of all permutations, if type="approximate" draw random permutations, if type="asymptotic" perform asymptotic log-rank test
#'
#' @return A list containing the exact or approximate permutation p-value and the observed test statistic
#' @examples
#' T <- rexp(20)
#' C <- rexp(20)
#' data <- data.frame(time=pmin(T, C), status=(T<=C), trt=rbinom(20, 1, 0.5))
#'
#' # Approximate permutation test using 1000 random permutations
#' x <- exactLR(1000, Surv(time, status) ~ trt, data, "approximate")
#'
#' print(paste("Approximate permutation p-value:", x$p))
#'
#' # Exact permutation test
#' y <- exactLR(0, Surv(time, status) ~ trt, data, "exact")
#' print(paste("Exact permutation p-value:", y$p))
#'
#' @export
exactLR <- function(B, formula, data=parent.frame(), type="exact") {
    data <- parseFormula(formula, data)    
    trt <- data$trt

    ## calculate logrank scores
    x <- coin::logrank_trafo(Surv(data$time, data$status))

    ## centered logrank statistic
    obsS <- sum(x * trt)

    if(sum(trt) == 0) return(list(p=1, obsS=0))

    if(type == "exact") { ## complete enumeration of all combinations
        co <- utils::combn(1:length(trt), sum(trt))
        S <- vapply(1:ncol(co), function(i) sum(x[co[,i]]), NA_real_)
        p <- mean(S < obsS)
    } else if(type == "approximate") { ## Monte-Carlo sampling of permutation distribution
        S <- vapply(1:B, function(b) sum(x * sample(trt)), NA_real_)
        p <- (sum(S < obsS) + 1) / (B + 1)
    } else if(type == "asymptotic") {
        fit <- survdiff(Surv(time, status) ~ trt, data=data)       
        Z <- -sqrt(fit$chisq) * sign(fit$obs[2] - fit$exp[2])
        p <- 1 - pnorm(Z)
    } else stop(paste("Unknown type:", type))

    list(p=p, obsS=obsS)
}

#' shuffleBlock
#' Permute block preserving group sizes, randomization blocks
#' @param block vector of row indices to be permuted
#' @param strata factor defining strata with block
#' @return random permutation of each stratum within block
shuffleBlock <- function(block, strata=0) {
    len <- length(block)
    urb <- unique(strata)
    if(length(urb) == 1) block[sample.int(len, len)]
    else do.call(c, lapply(urb, function(j) {
        ## equivalent to sample(block[which(rnd.block == j)]) but faster
        spln <- block[which(strata == j)]
        spln[sample.int(length(spln), length(spln))]
    }))
}

#' createPermGS
#' 
#' Create permGS object representing a permutational group-sequential trial.
#'
#' @param B number of random permutations
#' @param restricted if TRUE only permute within strata
#' @param method imputation/permuation method IPZ, IPT, Heinze or none (default: IPZ)
#' @param pool if TRUE impute event times from Kaplan-Meier estimator calculated from pooled data
#' @param imputeData user-supplied imputation function (ignored if method is given)
#' @param permuteData user-supplied permutation function (ignore if method is given)
#' @param type logrank weights to be used with coin::logrank_trafo
#' @return object of class permGS
#'
#' @examples
#' ## standard permutation test (no imputation, free permutations)
#' x <- createPermGS(1000, FALSE, "none")
#' summary(x)
#' ## imputation using IPT method, restricted permutations
#' y <- createPermGS(1000, TRUE, "IPT")
#' summary(y)
#' @export
createPermGS <- function(B=1000, restricted=TRUE, method="IPZ", pool=TRUE,
                         type=c("logrank", "Gehan-Breslow", "Tarone-Ware",
                                "Prentice", "Prentice-Marek",
                                "Andersen-Borgan-Gill-Keiding",
                                "Fleming-Harrington", "Self"),
                         imputeData=NULL, permuteData=NULL) {

    type <- match.arg(type)
    
    if(!is.null(method)) {
        if(method == "IPT") {
            imputeData <- imputeIPT
            permuteData <- permuteIPT
        } else if(method == "IPZ") {
            imputeData <- imputeIPZ
            permuteData <- permuteIPZ
        } else if(method == "Heinze") {
            imputeData <- imputeHeinze
            permuteData <- permuteHeinze
        } else if(method == "none") {
            imputeData <- function(data, pool=FALSE) data
            permuteData <- NULL
        } else stop(paste("Unknown imputation method:", method))
    } else method <- "user"

    ## calculation of (weighted) logrank scores
    trafo <- function(data) coin::logrank_trafo(Surv(data[,1], data[,2]), ties.method="mid-ranks", type=type)

    x <- list(method=method, pool=pool, imputeData=imputeData, permuteData=permuteData, type=type, trafo=trafo,
              B=B, restricted=restricted, S=matrix(nrow=B, ncol=0), perms=matrix(nrow=0, ncol=B), strata=NULL,
              results=data.frame(cv.l=double(), cv.u=double(), std.cv.l=double(), std.cv.u=double(), obs=double(), reject=logical(), alpha.l=double(), alpha.u=double(), p=double()))
    class(x) <- c("permGS", "list")
    x
}

#' summary of permGS object
#' @param object permGS object as returned by \code{\link{createPermGS}}
#' @param ... additional parameters (currently unused)
#' @method summary permGS
#' @return nothing
#' @method summary permGS
#' @export
summary.permGS <- function(object, ...) {
    print(paste("Permutations:", object$B))
    print(paste("Restricted:", object$restricted))
    print(paste("Imputation method:", object$method))
    print(paste("Weights:", object$type[1]))
    print(paste("Stage:", nrow(object$results)))
    print(object$results)
}

#' nextStage
#'
#' Imputation permutation group-sequential log-rank test.
#' Random permutations of a block a reused in all later stages. This automatically
#' results in blockwise permutations.
#'
#' @param pgs.obj permGS object as returned by \code{\link{createPermGS}}
#' @param alpha alpha at current stage
#' @param formula a formula object, as used by \code{\link{coxph}}, left hand side must be a 'Surv' object, right hand side must only consist of a factor (treatment indicator) and optionally a special strata() term identifying the permutation strata
#' @param data a data.frame or list containing the variables in "formula", by default "formula" is evaluated in the parent frame
#' @return An updated permGS object.
#' @examples
#' ## Two-stage design with one-sided O'Brien-Fleming boundaries using IPZ method
#' x <- createPermGS(1000, TRUE, "IPZ")
#'
#' t1 <- 9  ## calendar time of interim analysis
#' t2 <- 18  ## calendar time of final analysis
#'
#' T <- rexp(100) ## event times
#' R <- runif(100, 0, 12)  ## recruitment times
#' Z <- rbinom(100, 1, 0.5)  ## treatment assignment
#' C <- rexp(100) ## drop-out times
#' 
#' ## Stage 1 data
#' data.t1 <- data.frame(time=pmin(T, C, max(0, (t1-R))), status=(T<=pmin(C, t1-R)), trt=Z)
#' data.t1 <- data.t1[R <= t1,]
#'
#' ## Stage 2 data
#' data.t2 <- data.frame(time=pmin(T, C, max(0, (t2-R))), status=(T<=pmin(C, t2-R)), trt=Z)
#' data.t2 <- data.t2[R <= t2,] 
#' x <- nextStage(x, 0.00153, Surv(time, status) ~ trt, data.t1)
#' summary(x)
#'
#' if(!x$results$reject[1]) {
#'    data.t2$strata <- rep.int(c(1,2), c(nrow(data.t1), nrow(data.t2)-nrow(data.t1)))
#'    x <- nextStage(x, 0.025, Surv(time, status) ~ trt + strata(strata), data.t2)
#'    summary(x)
#' }
#' @export
nextStage <- function(pgs.obj, alpha, formula, data=parent.frame()) {
    if(any(pgs.obj$results$reject)) return(pgs.obj)

    data <- parseFormula(formula, data)
    
    n <- nrow(data)
    strata <- data$strata

    trafo <- pgs.obj$trafo
    B <- pgs.obj$B
    index <- 1:B
    stage <- nrow(pgs.obj$results) + 1

    cv.l <- pgs.obj$results$cv.l
    cv.u <- pgs.obj$results$cv.u
    S <- pgs.obj$S
    permuteData <- pgs.obj$permuteData

    ## perform imputation step
    data <- as.matrix(data)
    imp.data <- pgs.obj$imputeData(data, pgs.obj$pool)

    ## observed scores
    scores <- trafo(data) ##data[,1], data[,2])

    rejected.l <- logical(B)
    rejected.u <- logical(B)
    
    if(pgs.obj$restricted) {
        if(stage > 1) {
            rejected.l <- vapply(index, function(i) {
                any(sapply(1:(stage-1), function(j) S[i,j] < cv.l[j]))
            }, FALSE)
            rejected.u <- vapply(index, function(i) {
                any(sapply(1:(stage-1), function(j) S[i,j] > cv.u[j]))
            }, FALSE)
        }
        ## We only need to shuffle the one new block of the current stage B times
        ## perms = matrix of blockwise permuted 1:n vectors (1 perm = 1 column), B columns
        new.strata <- setdiff(unique(strata), pgs.obj$strata)
        if(length(new.strata) > 0) {
            sb <- strata %in% new.strata
            spln <- which(sb)
            srb <- strata[sb]
            pgs.obj$perms <- rbind(pgs.obj$perms, sapply(index, function(i) shuffleBlock(spln, srb)))
        }
        if(nrow(data) != nrow(pgs.obj$perms)) stop("nextStage: Invalid data (New patients but no new strata defined)!")
        pgs.obj$strata <- unique(strata)
    } else {
        spln <- 1:nrow(data)
        srb <- rep.int(1, nrow(data))
        pgs.obj$perms <- sapply(index, function(i) shuffleBlock(spln, srb))
    }
    
    perms <- pgs.obj$perms
    newS <- numeric(B)
    newS[rejected.u] <- Inf
    newS[rejected.l] <- -Inf
    rejected <- rejected.l | rejected.u
    
    ## calculate linear rank statistic for each permutation (scores are calculated once for observed data)
    if(pgs.obj$method == "none") {
        newS[!rejected] <- vapply(index[!rejected], function(i) sum(scores * data[perms[,i], 3]), NA_real_)
        d <- sum(data[,2])
    } else { ## scores need to be re-calculated for every permutation        
        tmp <- vapply(index[!rejected], function(i) {
            pdata <- permuteData(imp.data, perms[,i], TRUE)
            ##c(sum(pdata[,2]), sum(logrank_trafo(pdata[,1], pdata[,2]) * pdata[,3]))
            c(sum(pdata[,2]), sum(trafo(pdata) * pdata[,3]))
        }, c(NA_real_, NA_real_))

        d <- mean(tmp[1,])
        newS[!rejected] <- tmp[2,]
    }

    ## observed linear rank statistic
    obsS <- sum(scores * data[,3])
    
    ## critical value
    if(length(alpha) == 2) {
        alpha.l <- alpha[1]
        alpha.u <- alpha[2]
        cv.l <- quantile(c(obsS, newS), probs=alpha.l, names=FALSE)
    } else {
        alpha.l <- 0
        alpha.u <- alpha
        cv.l <- -Inf
    }    
    cv.u <- quantile(c(obsS, newS), probs=1-alpha.u, names=FALSE)
    
    sdS <- sd(newS[is.finite(newS)])
       
    ## pvalue
    if(stage > 1) p <- NA
    else p <- (sum(newS < obsS) + 1)/(B + 1)

    reject <- (obsS < cv.l) | (obsS > cv.u)

    pgs.obj$S <- cbind(pgs.obj$S, newS)
    colnames(pgs.obj$S) <- NULL
    pgs.obj$results <- rbind(pgs.obj$results,
                             data.frame(cv.l=cv.l, cv.u=cv.u, std.cv.l=cv.l/sdS, std.cv.u=cv.u/sdS,
                                        reject=reject, alpha.l=alpha.l, alpha.u=alpha.u, p=p, Z=obsS/sdS))
    pgs.obj
}

#' Convenience function which calls createPermGS and nextStage to perform fixed sample size permutation test with IPT method
#'
#' @param formula a formula object, as used by \code{\link{coxph}}, left hand side must be a 'Surv' object, right hand side must only consist of a factor (treatment indicator) and optionally a special strata() term identifying the permutation strata
#' @param data a data.frame or list containing the variables in "formula", by default "formula" is evaluated in the parent frame
#' @param B number of random permutations (default: 1000)
#' @param alpha significance level (default: 0.05)
#' @param pool if TRUE impute event times from Kaplan-Meier estimator calculated from pooled data
#' @param type logrank weights to be used with coin::logrank_trafo
#' @return An object of class permGS
#' @examples
#' T <- rexp(30) ## event times
#' Z <- rbinom(30, 1, 0.5)  ## treatment assignment
#' C <- rexp(30) ## drop-out times
#' data <- data.frame(time=pmin(T,C), status=T<=C, Z=Z)
#' x <- permIPT(Surv(time, status) ~ Z, data)
#' summary(x)
#' @export
permIPT <- function(formula, data, B=1000, alpha=0.05, pool=TRUE,
                    type=c("logrank", "Gehan-Breslow", "Tarone-Ware",
                           "Prentice", "Prentice-Marek",
                           "Andersen-Borgan-Gill-Keiding",
                           "Fleming-Harrington", "Self")) {
    x <- createPermGS(B, FALSE, "IPT", pool, type)
    nextStage(x, alpha, formula, data)
}

#' Convenience function which calls createPermGS and nextStage to perform fixed sample size permutation test with IPZ method
#'
#' @param formula a formula object, as used by \code{\link{coxph}}, left hand side must be a 'Surv' object, right hand side must only consist of a factor (treatment indicator) and optionally a special strata() term identifying the permutation strata
#' @param data a data.frame or list containing the variables in "formula", by default "formula" is evaluated in the parent frame
#' @param B number of random permutations (default: 1000)
#' @param alpha significance level (default: 0.05)
#' @param pool if TRUE impute event times from Kaplan-Meier estimator calculated from pooled data
#' @param type logrank weights to be used with coin::logrank_trafo
#' @return An object of class permGS
#' @examples
#' T <- rexp(30) ## event times
#' Z <- rbinom(30, 1, 0.5)  ## treatment assignment
#' C <- rexp(30) ## drop-out times
#' data <- data.frame(time=pmin(T,C), status=T<=C, Z=Z)
#' x <- permIPZ(Surv(time, status) ~ Z, data)
#' summary(x)
#' @export
permIPZ <- function(formula, data, B=1000, alpha=0.05, pool=TRUE,
                    type=c("logrank", "Gehan-Breslow", "Tarone-Ware",
                           "Prentice", "Prentice-Marek",
                           "Andersen-Borgan-Gill-Keiding",
                           "Fleming-Harrington", "Self")) {
    x <- createPermGS(B, FALSE, "IPZ", pool, type)
    nextStage(x, alpha, formula, data)
}

#' Convenience function which calls createPermGS and nextStage to perform fixed sample size permutation test with Heinze method
#'
#' @param formula a formula object, as used by \code{\link{coxph}}, left hand side must be a 'Surv' object, right hand side must only consist of a factor (treatment indicator) and optionally a special strata() term identifying the permutation strata
#' @param data a data.frame or list containing the variables in "formula", by default "formula" is evaluated in the parent frame
#' @param B number of random permutations (default: 1000)
#' @param alpha significance level (default: 0.05)
#' @param pool if TRUE impute event times from Kaplan-Meier estimator calculated from pooled data
#' @param type logrank weights to be used with coin::logrank_trafo
#' @return An object of class permGS
#' @examples
#' T <- rexp(30) ## event times
#' Z <- rbinom(30, 1, 0.5)  ## treatment assignment
#' C <- rexp(30) ## drop-out times
#' data <- data.frame(time=pmin(T,C), status=T<=C, Z=Z)
#' x <- permHeinze(Surv(time, status) ~ Z, data)
#' summary(x)
#' @export
permHeinze <- function(formula, data, B=1000, alpha=0.05, pool=TRUE,
                       type=c("logrank", "Gehan-Breslow", "Tarone-Ware",
                              "Prentice", "Prentice-Marek",
                              "Andersen-Borgan-Gill-Keiding",
                              "Fleming-Harrington", "Self")) {
    x <- createPermGS(B, FALSE, "Heinze", pool, type)
    nextStage(x, alpha, formula, data)
}

#' Convenience function which calls createPermGS and nextStage to perform fixed sample size permutation test without imputation
#'
#' @param formula a formula object, as used by \code{\link{coxph}}, left hand side must be a 'Surv' object, right hand side must only consist of a factor (treatment indicator) and optionally a special strata() term identifying the permutation strata
#' @param data a data.frame or list containing the variables in "formula", by default "formula" is evaluated in the parent frame
#' @param B number of random permutations (default: 1000)
#' @param alpha significance level (default: 0.05)
#' @param pool if TRUE impute event times from Kaplan-Meier estimator calculated from pooled data
#' @param type logrank weights to be used with coin::logrank_trafo
#' @return An object of class permGS
#' @examples
#' ## Two-sided permutation test
#' T <- rexp(100) ## event times
#' Z <- rbinom(100, 1, 0.5)  ## treatment assignment
#' C <- rexp(100) ## drop-out times
#' data <- data.frame(time=pmin(T,C), status=T<=C, Z=Z)
#' x <- permLR(Surv(time, status) ~ Z, data, alpha=c(0.025, 0.025))
#' summary(x)
#' @export
permLR <- function(formula, data, B=1000, alpha=0.05, pool=TRUE,
                   type=c("logrank", "Gehan-Breslow", "Tarone-Ware",
                          "Prentice", "Prentice-Marek",
                          "Andersen-Borgan-Gill-Keiding",
                          "Fleming-Harrington", "Self")) {
    x <- createPermGS(B, FALSE, "none", pool, type)
    nextStage(x, alpha, formula, data)
}
