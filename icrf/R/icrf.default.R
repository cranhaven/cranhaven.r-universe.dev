## mylevels() returns levels if given a factor, otherwise 0.
mylevels <- function(x) if (is.factor(x)) levels(x) else 0

#' @rdname icrf
#' @export
"icrf.default" <-
    function(x, L, R, tau = max(R[is.finite(R)]) * 1.5 , bandwidth = NULL,
             quasihonesty = TRUE, initialSmoothing = TRUE,
             timeSmooth = NULL, xtest = NULL, ytest = NULL,
             nfold = 5L, ntree=500L, mtry =  ceiling(sqrt(p)), #max(floor(ncol(x)/3), 1),
             split.rule = c("Wilcoxon", "logrank", "PetoWilcoxon", "PetoLogrank",
                            "GWRS", "GLR", "SWRS", "SLR"),
             ERT = FALSE, uniformERT = ERT, returnBest = sampsize < n, imse.monitor = 1,
             replace = !ERT, sampsize = ifelse(ERT, 0.95, .632) * n,
             nodesize = 6L, maxnodes = NULL,
             importance = FALSE, nPerm=1,
             proximity, oob.prox = ifelse(sampsize == n & !replace, FALSE, proximity),
             do.trace=FALSE,
             keep.forest = is.null(xtest),
             keep.inbag = FALSE, ...) {
      # note: instead of `y`, `L` and `R`
    time.record = data.frame(begin = Sys.time(), end = NA, elapsed = NA)
    n <- nrow(x)
    p <- ncol(x)
    t0 <- 0
    if (n == 0) stop("data (x) has 0 rows")
    x.row.names <- rownames(x)
    x.col.names <- if (is.null(colnames(x))) 1:ncol(x) else colnames(x)
    if (is.null(colnames(x))) {colnames(x) <- x.col.names}
    sampsize = ceiling(sampsize)
    if (replace && (sampsize > n)) stop("sampsize cannot be greater than n when replace is TRUE.")
    # x$obs = seq_len(n)
    # dataset <- dataset[, c(x.col.names, left, right, "obs")] # make sure no other variables come into play
    # need = ceiling(n/nmin) <- nrnodes?

    # split.rule
    split.rule <- match.arg(split.rule)
    split.rule.no <- which(c("Wilcoxon", "logrank", "PetoWilcoxon", "PetoLogrank",
                             "GWRS", "GLR", "SWRS", "SLR")
                           %in% split.rule)
    # deals with the aliases
    split.rule.no <- (split.rule.no - 1) %% 4 + 1
    split.rule    <- c("Wilcoxon", "logrank", "PetoWilcoxon", "PetoLogrank")[split.rule.no]

    imse.monitor <- as.integer(imse.monitor)
    if (!imse.monitor %in% 1:2) stop("imse.type.monitor should be eiter 1 or 2")
    if (returnBest && sampsize == n) stop("The best iteration (returnBest = TRUE) cannot be obtained without out-of-bag sample. sampsize should be less than the sample size")
    bestFold = 0

    ## overcome R's lazy evaluation:
    keep.forest <- keep.forest

    testdat <- !is.null(xtest)
    if (testdat) {
        if (ncol(x) != ncol(xtest))
            stop("x and xtest must have same number of columns")
        ntest <- nrow(xtest)
        xts.row.names <- rownames(xtest)
        if (!is.null(ytest)) {
          if (nrow(ytest) != ntest)
            stop("xtest and ytest must have same number of rows")
          # sanity check for time is rough. This is only for simulation purpose.
          if (ncol(ytest) != length(timeSmooth))
            stop("timepoints to be evaluated (timeSmooth) and number of columns in ytest must be the same")
        }
    }

    ## Make sure mtry is in reasonable range.
    if (mtry < 1 || mtry > p)
        warning("invalid mtry: reset to within valid range")
    mtry <- max(1, min(p, round(mtry)))
    if (!is.null(L)|| !is.null(R)) {
        if (length(L) != n || length(R) != n) stop("length of response (L and R) must be the same as predictors")
    } else {
      stop("L and R should not be NULL.")
    }

    ## Check for NAs.
    if (any(is.na(x))) stop("NA not permitted in predictors")
    if (testdat && any(is.na(xtest))) stop("NA not permitted in xtest")
    if (any(is.na(L))||any(is.na(R))) stop("NA not permitted in L and R")
    if (any(is.na(timeSmooth))) stop("NA not permitted in timeSmooth")
    if (any(R == -Inf)) stop("-Inf not permitted in R")
    if (any(is.infinite(L))) stop("infinite not permitted in L")
    if (any(L > R)) stop("R should be greater or equal to L")
    if (!is.null(ytest) && any(is.na(ytest))) stop("NA not permitted in ytest")
    if (!is.null(timeSmooth)) if (!identical(timeSmooth, sort(timeSmooth))) stop("timeSmooth are not in the order.")
    if (any(L < t0) || any(R < t0)) stop("All values of L and R should be no less than t0")

    if (is.data.frame(x)) {
        xlevels <- lapply(x, mylevels)
        ncat <- sapply(xlevels, length)
        ## Treat ordered factors as numerics.
        ncat <- ifelse(sapply(x, is.ordered), 1, ncat)
        x <- data.matrix(x)
### to be updated ###
        if(testdat) {
            if(!is.data.frame(xtest))
                stop("xtest must be data frame if x is")
            xfactor <- which(sapply(xtest, is.factor))
            if (length(xfactor) > 0) {
                for (i in xfactor) {
                    if (any(! levels(xtest[[i]]) %in% xlevels[[i]]))
                        stop("New factor levels in xtest not present in x")
                    xtest[[i]] <-
                        factor(xlevels[[i]][match(xtest[[i]], xlevels[[i]])],
                               levels=xlevels[[i]])
                }
            }
            xtest <- data.matrix(xtest)
        }
    } else {
      ncat <- rep(1, p)
	    names(ncat) <- colnames(x)
      xlevels <- as.list(rep(0, p))
    }
    maxcat <- max(ncat)
    if (maxcat > 2 && split.rule == "logrank")
      stop("The current version does not support categorical predictor(s) when 'split.rule = logrank' is used. Try other rules")

    if (maxcat > 53)
        stop("Can not handle categorical predictors with more than 53 categories.")

    #if (missing(proximity)) proximity <- addclass
    if (missing(proximity)) proximity <- FALSE
    if (proximity) {
        prox <- matrix(0.0, n, n)
        proxts <- if (testdat) matrix(0, ntest, ntest + n) else double(1)
    } else {
        prox <- proxts <- double(1)
    }

    # if (localImp) {
    #     importance <- TRUE
    #     impmat <- matrix(0, p, n)
    # } else
    localImp = FALSE    # localImp is being ignored.
    impmat <- double(1) # localImp is being ignored.

    if (importance) {
        if (nPerm < 1) nPerm <- as.integer(1) else nPerm <- as.integer(nPerm)
        impout <- array(0.0, dim = c(p, nfold, 3),
                        dimnames = list(x.col.names, 1:nfold, c("%IncIMSE1", "%IncIMSE2", "IncNodePurity")))
        impSD <- array(0.0, dim = c(p, nfold, 2),
                       dimnames = list(x.col.names, 1:nfold, c("IMSE1", "IMSE2")))
    } else {
        impout <- array(0.0, dim = c(p, nfold, 1))  # node impurity only
        impSD <- double(1)
    }

    ## For regression trees, need to do this to get maximal trees.
    nrnodes <- 2 * sampsize + 1

    if (!is.null(maxnodes)) {
        ## convert # of terminal nodes to total # of nodes
        maxnodes <- 2 * maxnodes - 1
        if (maxnodes > nrnodes) warning("maxnodes exceeds its max value.")
        nrnodes <- min(c(nrnodes, max(c(maxnodes, 1))))
    }
    ## Compiled code expects variables in rows and observations in columns.
    x <- t(x)
    storage.mode(x) <- "double"

    if (testdat) {
        xtest <- t(xtest)
        storage.mode(xtest) <- "double"
        if (is.null(ytest)) {
            ytest <- labelts <- 0
        } else {
            labelts <- TRUE
        }
    } else {
        xtest <- double(1)
        ytest <- double(1)
        ntest <- 1
        labelts <- FALSE
    }

    nt <- if (keep.forest) ntree else 1

    # time_interest is for npmle, prob calculation. not for smoothing
    time_interest = sort(unique(c(t0, L, R, tau)))
    ntime <- length(time_interest)

    # Remove virtual redundancy
    for (i in 1:(ntime-1)) {
      # If there are virtually redundant time points, nullify the smaller ones.
      if (any(time_interest[i] + 1e-6 >= time_interest[(i+1):ntime])) time_interest[i] = 0
    }
    time_interest = sort(unique(time_interest))

    time_interest <- c(time_interest[time_interest <= tau], Inf)
    ntime <- length(time_interest) # renew.
    t.names <- paste0("t", seq_along(time_interest))
    names(time_interest) <- t.names

    npmle.smooth <- isdSm (LR = cbind(L, R), grid.smooth = time_interest,
                           btt = c(bandwidth = ifelse(initialSmoothing, NA, 0), t0 = t0, tau = tau))
    npmle.smooth <- data.frame(x = c(time_interest[time_interest <= tau], Inf),
                               y = c(npmle.smooth[time_interest <= tau], 1))
    # time_interest = npmle.smooth$x
    # tmp.data <<- list(LR = cbind(L, R), grid.smooth = time_interest,
    #                         btt = c(bandwidth = ifelse(initialSmoothing, NA, 0), t0 = t0, tau = tau))
    # tmp.npmle.smooth <<- npmle.smooth

    if (is.null(bandwidth)) {
      npmle.discrete <- isdSm (LR = cbind(L, R), grid.smooth = time_interest,
                             btt = c(bandwidth = 0, t0 = t0, tau = tau))
      npmle.discrete <- data.frame(x = c(time_interest[time_interest <= tau], Inf),
                                   y = c(npmle.discrete[time_interest <= tau], 1))

      iqr =
        lin.interpolate(0.75, npmle.discrete$y, npmle.discrete$x)["y.interpolate"] -
        lin.interpolate(0.25, npmle.discrete$y, npmle.discrete$x)["y.interpolate"]
      if (iqr > tau/2) iqr = tau/2
      bandwidth = iqr * nodesize^(-0.2)  # the bandwidth has been changed from n to nodesize (Jan 4, 2021)
      # bandwidth = as.numeric(iqr) * nodesize^-0.5 # the bandwidth has been changed from n to nodesize (May 10, 2021)
      # cat("iqr-nodesize-bw: ", iqr, " ", nodesize, " ", bandwidth, "\n")
    }
    # smooth2nd = (bandwidth > 0)


    # time_interest <- c(time_interest[time_interest <= tau], Inf)
    interval.prob.mat <- interval2mat(L = L, R = R, Fn = npmle.smooth,
                                      t0 = t0, tau = tau, t.points = time_interest)
    # interval.prob.mat <- interval.prob.mat[, time_interest <= tau]
    # interval.prob.mat <- cbind(interval.prob.mat, 1 - apply(interval.prob.mat, 1, sum))
    interval.prob.mat <- data.matrix(interval.prob.mat)
    # tmp.interval.prob.mat <<- interval.prob.mat
    if (dim(interval.prob.mat)[2] != ntime) stop("number of columns of interval.prob.mat and ntime do not match")
    r.inf = is.infinite(R)  # Inf index for R
    lr = c(L, R)
    lr[n + which(r.inf)] <- -1
    t.inf = is.infinite(time_interest) # Inf index for time_interest
    time_interest_fin = time_interest
    time_interest_fin[which(t.inf)] <- -1

    if (is.null(timeSmooth)) {
      timeSmooth = time_interest
      ntimeSm = ntime
      t.inf.Sm = t.inf
      timeSmooth_fin = time_interest_fin
      t.names.Sm = t.names
    } else {
      t.inf.Sm = is.infinite(timeSmooth)  # Inf index for timeSmooth
      timeSmooth_fin = timeSmooth
      timeSmooth_fin[which(t.inf.Sm)] <- -1
      ntimeSm = length(timeSmooth)
      t.names.Sm = paste0("s", seq_along(timeSmooth))
      names(timeSmooth) <- t.names.Sm
    }


    if (split.rule.no >= 3) { # when split.rule = PetoLogrank
      # marginalSurv <- interval2mat(L = 0, R = Inf, Fn = npmle.smooth,
      #                              t0 = t0, tau = tau, t.points = time_interest)
      marginalSurv <- 1 - cdf.mass(L = L, R = R, Fn = npmle.smooth, t0 = t0, tau = tau,
                                           t.points = time_interest)$cdf
      # print(marginalSurv)
      ypredNO = matrix(as.double(marginalSurv), nrow = n, ncol = ntime, byrow = TRUE)
    } else {
      ypredNO = matrix(double(n * ntime), ncol = ntime)
    }
    ypredNOSm = matrix(double(n * ntimeSm), ncol = ntimeSm)

# return(list(lr, r.inf, t.inf, time_interest_fin, interval.prob.mat, npmle.smooth))
    rfout <- .C("survRF",
                x,
                as.double(lr),
                as.double(t(interval.prob.mat)),
                as.integer(c(n, p, ntime, ntimeSm, nt)),
                as.double(time_interest_fin),
                as.double(timeSmooth_fin),
                as.double(tau),
                as.integer(r.inf),
                as.integer(t.inf),
                as.integer(t.inf.Sm), #10
                as.integer(sampsize),
                as.integer(nodesize),
                as.integer(nrnodes),
                as.integer(ntree),
                as.integer(mtry),
                as.integer(nfold),
                as.integer(c(importance, localImp, nPerm)),
                as.integer(ncat),
                as.integer(maxcat),
                as.integer(do.trace), #20
                as.integer(proximity),
                as.integer(oob.prox),
                updateNPMLE = as.integer(quasihonesty),
                as.integer(returnBest),
                as.integer(imse.monitor),
              # included in output from here (PART 1)
                bestFold = as.integer(bestFold),
                ypred = matrix(double(n * ntime), ncol = ntime),
                ypredNO = ypredNO,
                ypredNOSm = ypredNOSm,
                impout = impout,
                impmat = impmat,
                impSD = impSD,
                prox = prox,
                ndbigtree = integer(ntree), #30
                nodestatus = matrix(integer(nrnodes * nt), ncol=nt),
                leftDaughter = matrix(integer(nrnodes * nt), ncol=nt),
                rightDaughter = matrix(integer(nrnodes * nt), ncol=nt),
                nodepred = array(as.double(1:(ntime * nrnodes * nt)), dim = c(ntime, nrnodes, nt)),
                nodepredSm = array(double(ntimeSm * nrnodes * nt), dim = c(ntimeSm, nrnodes, nt)),
                bestvar = matrix(integer(nrnodes * nt), ncol=nt),
                xbestsplit = matrix(double(nrnodes * nt), ncol=nt),
                imse = double(2 * nfold), #40
                imseNO = double(2 * nfold),
              # included in output up to here (PART 1)
                wilcoxon = as.integer(split.rule.no),
                ert = as.integer(ERT),
                uniErt = as.integer(uniformERT),
                keep = as.integer(c(keep.forest, keep.inbag)),
                replace = as.integer(replace),
                testdat = as.integer(testdat),
                bandwidth = as.double(bandwidth),
                xts = xtest,
                ntest = as.integer(ntest), #50
                yts = as.double(ytest),
                labelts = as.integer(labelts),
              # included in output from here (PART 2)
                ytestpred = if (testdat)
                  double(ntest * ntimeSm) else double(1),
                proxts = proxts,
                interrts = double(if (labelts) 2 * nfold else 1),
                oob.times = integer(n),
                inbag = if (keep.inbag)
                  matrix(integer(n * ntree), n) else integer(1),
                keep.inbag = as.integer(keep.inbag), #60
              # included in output up to here (PART 2)
                #DUP=FALSE,
                PACKAGE="icrf")[c(26:43, 55:59)]
    ## Format the forest component, if present.
    if (keep.forest) {
        max.nodes <- max(rfout$ndbigtree)
        rfout$nodestatus <-
            rfout$nodestatus[1:max.nodes, , drop=FALSE]
        rfout$bestvar <-
            rfout$bestvar[1:max.nodes, , drop=FALSE]
        rfout$nodepred <-
            rfout$nodepred[, 1:max.nodes, , drop=FALSE]
        rfout$nodepredSm <-
          rfout$nodepredSm[, 1:max.nodes, , drop=FALSE]
        rfout$xbestsplit <-
            rfout$xbestsplit[1:max.nodes, , drop=FALSE]
        rfout$leftDaughter <-
            rfout$leftDaughter[1:max.nodes, , drop=FALSE]
        rfout$rightDaughter <-
            rfout$rightDaughter[1:max.nodes, , drop=FALSE]
    }
    cl <- match.call()
    cl[[1]] <- as.name("icrf")
    ## Make sure those obs. that have not been OOB get NA as prediction.

    rfout$times = time_interest
    if (any(rfout$oob.times < 1)) {
      rfout$ypred[rfout$oob.times == 0, ] <- NA
    }
    time.record["end"] <- Sys.time()
    time.record["elapsed"] <- difftime(time.record[["end"]], time.record[["begin"]], units = "min")
    out <- list(call = cl,
                method = data.frame(split.rule = split.rule, ERT = ERT, subsampleRatio = sampsize/n,
                                    quasihonesty = ifelse(quasihonesty, "quasihonesty", "exploitative"),
                                    bandwidth = as.numeric(bandwidth)),
                bestFold = data.frame(bestFold = ifelse(rfout$bestFold, rfout$bestFold, NA),
                                      imse.type = imse.monitor,
                                      imse.best = if (returnBest) rfout$imse[rfout$bestFold + (imse.monitor - 1) * nfold] else NA),
                #predicted.OOB = structure(rfout$ypred, dimnames = list(x.row.names, t.names)),
                predicted = structure(rfout$ypredNO,
                                      dimnames = list(x.row.names, t.names),
                                      time = time_interest),
                predicted.Sm = structure(rfout$ypredNOSm,
                                         dimnames = list(x.row.names, t.names.Sm),
                                         time = timeSmooth),
                time.points = time_interest,
                time.points.smooth = timeSmooth,
                imse.oob = matrix(rfout$imse, ncol = 2, dimnames = list(1:nfold, c("imse.type1", "imse.type2"))),
                imse.NO  = matrix(rfout$imseNO, ncol = 2, dimnames = list(1:nfold, c("imse.type1", "imse.type2"))),
                oob.times = rfout$oob.times,
                importance = if (importance) {
                  array(rfout$impout, dim = c(p, nfold, 3),
                        dimnames = list(x.col.names, 1:nfold, c("%IncIMSE1", "%IncIMSE2", "IncNodePurity")))
                } else {
                  array(rfout$impout, dim = c(p, nfold, 1),
                        dimnames=list(x.col.names, 1:nfold, "IncNodePurity"))
                },
                # importanceSD = if (importance) rfout$impSD else NULL,
                # localImportance = if (localImp) matrix(rfout$impmat, p, n, dimnames=list(x.col.names, x.row.names)) else NULL,
                proximity = if (proximity) matrix(rfout$prox, n, n, dimnames = list(x.row.names, x.row.names)) else NULL,
                nfold = nfold,
                ntree = ntree,
                mtry = mtry,
                forest = if (keep.forest)
                            c(rfout[c("ndbigtree", "nodestatus", "leftDaughter",
                                      "rightDaughter", "nodepred", "nodepredSm", "bestvar", "xbestsplit")],
                              list(ncat = ncat), list(nrnodes = max.nodes),
                              list(ntree = ntree), list(xlevels = xlevels)) else NULL,
                intervals = data.frame(L = L, R = R),
                test = if(testdat) {
                    list(predicted = structure(matrix(rfout$ytestpred, ntest, ntimeSm),
                                               dimnames = list(xts.row.names, t.names.Sm),
                                               time = timeSmooth),
                         testerror = if(labelts)
                           t(matrix(rfout$interrts, 2, dimnames = list(c("int.error", "sup.error"), 1:nfold))) else NULL,
                         proximity = if (proximity) {
                           matrix(rfout$proxts / ntree, nrow = ntest,
                                  dimnames = list(xts.row.names, c(xts.row.names, x.row.names)))
                         } else NULL
                         )
                } else NULL,
                inbag = if (keep.inbag) matrix(rfout$inbag, nrow(rfout$inbag),
                                               dimnames=list(x.row.names, NULL)) else NULL,
                runtime = time.record
               )
    class(out) <- "icrf"

    return(out)
}
