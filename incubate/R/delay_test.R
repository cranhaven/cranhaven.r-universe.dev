#' Goodness-of-fit (GOF) test statistic (experimental!)
#'
#' The GOF-test is performed for a fitted delay-model that was
#' fit using MPSE.
#' There are different GOF-tests implemented:
#' * __Moran GOF__ is based on spacings, like the MPSE-criterion itself.
#' * __Pearson GOF__ uses categories and compares observed to expected frequencies.
#'
#' Note that the GOF-tests are currently only implemented for models
#' fitted with maximum product of spacings estimation (MPSE).
#' These tests (Moran & Pearson) are still experimental.
#' So, use with caution. Experimental code!
#' @param delayFit delay_model fit object
#' @param method character(1). which method to use for GOF. Default is 'moran'.
#' @param estimated flag. Moran test: was the parameter estimated?
#' @param verbose integer. Verbosity level. The higher the more verbose debugging output.
#' @return An `htest`-object containing the GOF-test result
#' @export
test_GOF <- function(
  delayFit,
  method = c("moran", "pearson", "nikulin", "NRR"),
  estimated = TRUE,
  verbose = 0
) {
  stopifnot(inherits(delayFit, what = "incubate_fit"))
  if (delayFit$method != "MPSE") {
    stop(
      "Goodness-of-fit test only supported for models that are fit with maximum product of spacings estimation (MPSE)!",
      call. = FALSE
    )
  }

  method <- match.arg(method)
  twoGroup <- isTRUE(delayFit$twoGroup)
  isSurv <- isTRUE(delayFit$cens$isSurv)
  distO <- delayFit$distO
  data_name <- if (twoGroup) {
    paste(names(delayFit$data), collapse = " and ")
  } else {
    "x"
  }
  nObs <- delayFit$nobs[seq_len(1L + twoGroup)]
  params <- coef.incubate_fit(delayFit, transformed = FALSE)
  k <- length(params)

  # required variables
  methStr <- statist <- dgf <- p_val <- NULL

  switch(
    method,
    moran = {
      # Moran's GOF test
      methStr <- "Moran's Goodness-of-fit (GOF) test"

      EUL_MAS <- -digamma(1L)

      # Moran test statistic is negative sum of logged spacings, see Cheng & Stephens (1989)
      #
      # The provided MPSE-criterion is the negative average value, hence, before being used, it is multiplied by (n+1) to transform it as sum (so it matches the definition of Cheng & Stephens)
      # @param mpseCrit: the negative avg logged cumulative spacings, length 1 or 2.
      # @param n nbr of observations, length 1 or 2
      # @param k nbr of parameters to be estimated
      # @return Moran's test statistic, length 1 or 2
      testStat_mo <- function(mpseCrit, nObs, k) {
        mo_m <- (nObs + 1L) *
          (log(nObs + 1L) + EUL_MAS) -
          .5 -
          1 / (12L * (nObs + 1L))
        mo_v <- (nObs + 1L) * (pi**2L / 6L - 1L) - .5 - 1 / (6L * (nObs + 1L))

        C1 <- mo_m - sqrt(.5 * nObs * mo_v)
        C2 <- sqrt(mo_v / (2L * nObs))

        # factor (n+1) takes -avg to -sum
        ((nObs + 1L) * mpseCrit + (if (estimated) .5 * k else 0) - C1) / C2
      } #fn Moran's test statistic

      # we resolve ties in the back-transformed 0-1 space via equal spacing (see Cheng & Stephens)
      statist <- if (twoGroup) {
        ##  && length(delayFit$bind) < length(oNames) # not needed!?
        sum(testStat_mo(
          mpseCrit = delayFit$objFun(
            pars = params,
            isOrig = TRUE,
            aggregated = FALSE,
            ties. = "equispaced"
          ), #criterion per group
          nObs = nObs,
          k = k / 2
        ))
      } else {
        # single group
        testStat_mo(
          mpseCrit = delayFit$objFun(
            pars = params,
            isOrig = TRUE,
            ties. = "equispaced"
          ),
          nObs = nObs,
          k = k
        )
      }

      # statist sometimes negative, in particular with ties because of conservative tie-fix
      # e.g. for exponential model on single group data
      # x = c(8, 9, 9, 10, 11, 12, 14, 16)
      statist <- max(0, statist) #ensure non-negative
      statist <- rlang::set_names(statist, nm = "X^2")
      # in case of two groups: sum of two independent chi-sq. is chi-sq
      dgf <- sum(nObs)
      p_val <- stats::pchisq(q = statist, df = dgf, lower.tail = FALSE)
    },

    pearson = {
      # Pearson GOF-test
      methStr <- "Pearson's Goodness-of-fit (GOF) test" # (per group) ## this is our standard

      if (!isSurv) {
        # non-Surv case
        # ordinary Pearson-Fisher X2-test statistic

        nCl <- if (twoGroup) {
          pmax.int(k / 2 + 2L, ceiling(2L * nObs**.4))
        } else {
          max(k + 2L, ceiling(2L * nObs**.4))
        }

        # under H0, expect frequency counts of back-transformed data per group according to uniform distribution
        # use fixed number of cells that are equally spaced in back-transformed 0-1 interval
        # nbr of classes as recommended by David S. Moore (chapter "Tests of Chi-squared Type", 1986)

        statist <- local({
          datr <- transform.incubate_fit(delayFit)

          rlang::set_names(
            sum(purrr::map2_dbl(
              .x = if (is.numeric(datr)) list(x = datr) else datr,
              .y = nCl,
              .f = function(.x, .y) {
                tab_transf <- tabulate(
                  findInterval(
                    .x,
                    vec = seq.int(from = 0L, to = 1L, length.out = .y + 1L),
                    rightmost.closed = TRUE,
                    all.inside = TRUE
                  ),
                  nbins = .y
                )
                sum((tab_transf - mean(tab_transf))**2L) / mean(tab_transf)
              }
            )),
            nm = "X^2"
          )
        })

        # inference based on chi-squared distribution.
        #+use adjusted degrees of freedom (loose one df for each parameter estimated)
        dgf <- sum(nCl) - (k + 1L) - twoGroup
        p_val <- stats::pchisq(q = statist, df = dgf, lower.tail = FALSE)
      } else {
        # Surv-response
        # generalized Pearson-Fisher chi-squared test,
        #+see Nikulin (2007), referring to Li and Doss (1993) in turn

        cdfF <- distO$cdf
        densF <- distO$pdf

        # estimate variance-covariance matrix for specified groups
        # @return variance-covariance matrix or NULL in case of failures
        vcovF <- function(parEst, grpIdx = 1, boundaries) {
          stopifnot(is.numeric(parEst), rlang::is_named(parEst))
          stopifnot(is.numeric(boundaries), length(boundaries) >= 2L)

          # number of classes
          nCl <- length(boundaries) - 1
          # set parameter vector into CDF and PDF functions
          cdfF2 <- purrr::partial(
            .f = cdfF,
            !!!c(as.list(parEst), lower.tail = TRUE)
          )
          pdfF2 <- purrr::partial(.f = densF, !!!c(as.list(parEst)))
          # a similar calculation is done for the xiF-function
          pIntPred <- diff(cdfF2(q = boundaries))
          # or: -diff(survF(q = boundaries)))) #negative sign because we have survF(=1-F) instead of F

          integrandF <- function(x) {
            survInd <- c(1L, -1L)[grpIdx] * seq_len(length(x)) # works only for 1- or 2-group setting
            x *
              pdfF2(x = x) /
              ((1 - cdfF2(q = x))^2 *
                summary(delayFit$cens$rcens, times = x, extend = TRUE)$surv[
                  survInd
                ])
          }

          # variance function, see Nikulin (2017), 2.1 (p. 33)
          # @param t numeric time points up to where to integrate
          # @return numeric variance estimate per time point
          varF <- function(t) {
            # vectorize over t
            purrr::map_dbl(.x = t, .f = function(.x) {
              # integrate up to upper bound .x
              retVal <- NA_real_
              try(
                expr = {
                  intVal <- stats::integrate(
                    f = integrandF,
                    lower = 0,
                    upper = .x,
                    # less stringent settings for convergence, but allow for more subdivisions
                    rel.tol = 1.5e-3,
                    subdivisions = 1001L
                  )
                  if (intVal$message == "OK") retVal <- intVal$value
                },
                silent = TRUE
              )
              retVal
            })
          } #fn varF

          Dmat <- diag(1 / sqrt(pIntPred))
          # apply() transposes the partial derivative matrix as required (see Nikulin, p. 33)
          Cmat <- Dmat %*%
            apply(cdfF2(q = boundaries, grad = TRUE), MARGIN = 1L, FUN = diff)
          CtC_inv <- NULL
          try(
            expr = {
              CtC_inv <- solve(crossprod(Cmat))
            },
            silent = TRUE
          )
          if (is.null(CtC_inv)) {
            return(NULL)
          }
          Pmat <- diag(nrow = nCl) - Cmat %*% CtC_inv %*% t(Cmat)

          varVals <- varF(boundaries[2L:nCl]) # nCl-1 entries (=number of inner boundaries)
          if (any(!is.finite(varVals))) {
            return(NULL)
          }
          S1mat <- matrix(data = -1, nrow = nCl - 1, ncol = nCl - 1)
          for (ro in 1L:(nCl - 1)) {
            for (co in ro:(nCl - 1)) {
              S1mat[ro, co] <- (1 - cdfF2(q = boundaries[1L + ro])) *
                (1 - cdfF2(q = boundaries[1L + co])) *
                varVals[ro]
            } #rof co
          } #rof ro
          S1mat[lower.tri(S1mat)] <- t(S1mat)[lower.tri(S1mat)]

          Jmat <- diag(nrow = nCl, ncol = nCl - 1L)
          Jmat[row(Jmat) - 1L == col(Jmat)] <- -1

          # return vcov-matrix
          Pmat %*% Dmat %*% Jmat %*% S1mat %*% t(Jmat) %*% Dmat %*% Pmat
        } #fn vcovF

        # calculates test statistic or generalized Pearson-Fisher GOF-test per group
        # choose boundaries greedily
        # @return list with boundaries, test statistic and degrees of freedom
        genPearsonFisher <- function(group = "x") {
          grpIdx <- if (!twoGroup) 1L else 1L + (group != "x")

          # index of survival-information from KM-fit: we have so many unique event or censor times
          srvIdx <- if (twoGroup && "strata" %in% names(delayFit$kmFit)) {
            stopifnot(length(delayFit$kmFit$strata) == 2L) #two groups
            c(1L, -1L)[[grpIdx]] * seq_len(delayFit$kmFit$strata[[1L]])
          } else {
            seq_along(delayFit$kmFit$n.event)
          }

          nCl_min <- k / (1L + twoGroup) + 2L # ensures at least 1df for chi-sq per group

          nEvGr <- delayFit$kmFit$n.event[srvIdx]
          timeGr <- delayFit$kmFit$time[srvIdx]
          # index of observed event times *within the selected group*
          srvIdxGrpEv <- which(nEvGr > 0)
          # check if we have enough unique observed event times per group
          if (length(srvIdxGrpEv) < nCl_min) {
            # extreme case with too few classes to really get an insight
            warning(
              "Pearson-GOF: less than the required ",
              nCl_min,
              " classes for group ",
              group,
              call. = FALSE
            )
            return(list(boundaries = NA_real_, testStat = 0, dgf = NA_real_))
          } #fi

          # min: at most as many classes as we have unique observed event times
          nCl <- min(
            length(srvIdxGrpEv),
            ceiling(
              2L * (nObs[grpIdx] - delayFit$cens$n[[grpIdx]][["any"]])**.4
            )
          )

          # find boundaries so that we have at least an observed event per class

          # OLD approach using KM-fit but this could yield identical boundaries
          #+in particular if we have few observed event times
          # boundaries <- quantile(delayFit$kmFit,
          #                        probs = c(0.001, #to get an upper bound for delay1 later on
          #                                  seq.int(from = 1L - max(delayFit$kmFit$surv[srvIdx]),
          #                                          to = 1L - min(delayFit$kmFit$surv[srvIdx]),
          #                                          length.out = nCl + 1L)))
          #if (twoGroup && NROW(boundaries) > 1L) boundaries <- boundaries[grpIdx,]
          #delay1Upper <- boundaries[[1L]] # upper bound for delay1
          #boundaries <- as.numeric(boundaries[-1L])
          #[[.]] always extracts numeric entry, first observed event or censoring time
          #boundaries[[1L]] <- if (twoGroup) delayFit$data[[group]][[1L]] else delayFit$data[[1L]]
          #boundaries <- unique(boundaries) #ensure we have unique boundaries
          #stopifnot(nCl == length(boundaries) - 1L)

          boundaries <- numeric(nCl + 1L)
          nbrAvgEv_Cl <- sum(nEvGr[srvIdxGrpEv]) / nCl
          # outer boundaries are beyond the first and last observed (event/censored) time
          #[[.]] always extracts numeric entry, first observed event or censoring time
          boundaries[[1L]] <- .995 *
            if (twoGroup) delayFit$data[[group]][[1L]] else delayFit$data[[1L]]
          boundaries[[length(boundaries)]] <- 1.01 * timeGr[length(timeGr)]

          # # simple heuristics to always have a solution:
          # # take as many elements as long as their sum remains ≤ target value, if already the first is too big, go just a single step
          # # #another idea: find indices with highest event counts and use as single intervals
          # # ind_evGr_dec <- sort.list(x = nEvGr, decreasing = TRUE)
          # boundaries0 <- boundaries
          # j <- 1L
          # for (ind_b in 2L:nCl) {
          #   stayBelow <- cumsum(nEvGr[srvIdxGrpEv][seq.int(from = j, to = length(srvIdxGrpEv))]) <= nbrAvgEv_Cl
          #   ind_shift <- if (any(stayBelow)) max(which(stayBelow)) else 1L
          #   j <- min(j + ind_shift, length(srvIdxGrpEv))
          #   boundaries0[ind_b] <- (timeGr[srvIdxGrpEv][j-1] + timeGr[srvIdxGrpEv][j]) / 2L
          # } #rof

          # set inner boundaries in turn,
          # use greedy approach for simplicity
          #+ example: nEvGr <- c(0, 1, 1, 0, 4, 1, 2); timeGr <- c(6, 7, 8, 9, 10, 11, 12); srvIdxGrpEv <- c(2L, 3L, 5L, 6L, 7L); nCl <- 5
          j <- 1L #idx within srvIdxGrpEv: from where to look at, inclusively
          for (ind_b in 2L:nCl) {
            #used before: which(cumsum(nEvGr[srvIdxGrpEv][seq.int(from = j, to = length(srvIdxGrpEv))]) > nbrAvgEv_Cl)
            ind_shift <- which.min(abs(
              cumsum(nEvGr[srvIdxGrpEv][seq.int(
                from = j,
                to = length(srvIdxGrpEv)
              )]) -
                nbrAvgEv_Cl
            ))
            if (verbose > 1) {
              cat(
                "j =",
                j,
                "and best ind_shift is",
                ind_shift,
                "with nbr events",
                sum(nEvGr[srvIdxGrpEv][j:(j + ind_shift - 1)]),
                "  "
              )
            }
            # min() establish upper bound for ind_shift:
            # we need at least one event more than 0s in boundary-vector still to fill after this current (pending) one
            # nbr of events still free after smallest pending boundary update (i.e., ind_shift = 1) - 0s in boundary still to fill after this one
            #ind_shift <- min(ind_shift, length(srvIdxGrpEv) - (j + ind_shift - 1) + 1 - (nCl-1-(ind_b-1)))
            #ind_shift <- min(ind_shift, length(srvIdxGrpEv) - j - (nCl-1-(ind_b-1)))
            # length(srvIdxGrpEv) - j + 1: still free event counts (before choosing boundaries[ind_b])
            # nCl - ind_b
            ind_shift <- min(
              ind_shift,
              length(srvIdxGrpEv) - j + 1 - (nCl - 1 - (ind_b - 1) + 1)
            )
            if (verbose > 1) {
              cat("... new ind_shift ", ind_shift, "\n")
            }
            if (length(ind_shift) != 1L || ind_shift < 1L) {
              stop(
                "Choosing boundaries at ",
                ind_b,
                " failed! Think of using a simple heuristic instead.",
                call. = FALSE
              )
              break
            }
            # update idx within srvIdxGrpEv
            if (j + ind_shift > length(srvIdxGrpEv)) {
              stop("j bigger than length(srvIdxGrpEv)!", call. = FALSE)
            } #never?!
            #j <- min(j + ind_shift, length(srvIdxGrpEv)) # at most last event time idx #+not needed?
            j <- j + ind_shift
            # choose boundary which lies betw (j-1)th and j-th observed time
            boundaries[ind_b] <- (timeGr[srvIdxGrpEv][j - 1] +
              timeGr[srvIdxGrpEv][j]) /
              2L
          } #rof
          rm(list = c("j", "ind_b", "ind_shift")) #clean-up

          # there are better ways to repeatedly improve a partitioning looking at the biggest offenders
          # cf https://stackoverflow.com/questions/35517051/split-a-list-of-numbers-into-n-chunks-such-that-the-chunks-have-close-to-equal (link thx to FU)
          # example from SO: nEvGr <- c(95, 15, 75, 25, 85, 5); timeGr <- 1:6; srvIdxGrpEv <- 1:6; nCl <- 3
          #+ it should group the first two together! The greedy single-pass run without look-ahead groups first event alone

          # ideally, we have equal nbr of observed events per class
          # evaluate balancedness of observed events for given boundaries
          # grping <- findInterval(timeGr[srvIdxGrpEv], vec = unique(boundaries))
          # sum((tapply(X = nEvGr[srvIdxGrpEv], INDEX = grping, FUN = sum) - nbrAvgEv_Cl)^2)

          # we do not expect duplicate boundaries
          stopifnot(all(boundaries[-1] > 0), !any(duplicated(boundaries)))

          # observed probabilities per interval, minus sign because we have diff of survival, not diff of CDF
          pIntObs <- -diff(summary(
            delayFit$kmFit,
            times = boundaries,
            extend = TRUE
          )$surv[c(1L, -1L)[[grpIdx]] * seq_along(boundaries)])
          stopifnot(all(pIntObs >= 0))

          # xi: vector of normalized differences between observed and expected counts per interval
          # @param pars numeric parameter vector for single group
          xiF <- function(pars) {
            pIntPred <- diff(rlang::exec(
              cdfF,
              !!!c(list(q = boundaries), pars)
            ))
            sqrt(nObs[grpIdx]) * (pIntObs - pIntPred) / sqrt(pIntPred)
          }

          # minimize the distance measure (xi^t * xi) as function of theta to get parameter estimate for GOF-test
          # parameters are not specifically transformed for better optimization (the main optimization for parameter estimation does this)
          parStart <- coef(delayFit, group = group)
          minChisqOpt <- stats::optim(
            par = parStart,
            fn = function(pars) as.numeric(crossprod(xiF(pars))),
            method = "L-BFGS-B",
            lower = c(0, rep_len(TOL_NUM, length(parStart) - 1L)),
            upper = c(boundaries[[1L]], rep_len(+Inf, length(parStart) - 1L)),
            control = list(
              factr = 3e7 # less stringent for convergence than default factor 1e7
              #trace = 1, REPORT = 5))
            )
          )
          # minimum X2-estimate for parameter vector theta
          coef_minX2 <- if (minChisqOpt$convergence > 0) {
            if (verbose > 0) {
              warning(
                "minimum chi^2 parameter estimate for group ",
                group,
                " did not converge!",
                call. = FALSE
              )
            }
            # fall back to start value
            parStart
          } else {
            minChisqOpt$par
          }

          Smat <- vcovF(
            parEst = coef_minX2,
            grpIdx = grpIdx,
            boundaries = boundaries
          )

          # return value of test statistic
          testStat <- if (!is.null(Smat)) {
            as.numeric(
              t(xiF(coef_minX2)) %*% MASS::ginv(Smat) %*% xiF(coef_minX2)
            )
          }

          list(
            boundaries = boundaries,
            testStat = testStat,
            dgf = nCl - k / (1L + twoGroup) - 1
          )
        } #fn

        genPF_x <- genPearsonFisher(group = "x")
        genPF_y <- if (twoGroup) genPearsonFisher(group = "y")

        statist <- if (is.numeric(genPF_x[["testStat"]])) {
          c(`X^2` = genPF_x[["testStat"]] + (genPF_y[["testStat"]] %||% 0))
        }

        dgf <- genPF_x[["dgf"]] + (genPF_y[["dgf"]] %||% 0)
        p_val <- if (is.numeric(statist)) {
          stats::pchisq(q = statist, df = dgf, lower.tail = FALSE)
        }

        # #
        # ##
        # ### OLD
        # ##
        # #
        #
        # nCl <- local({
        #   # for number of classes, only consider observed event times
        #   nCensAny <- if (! isSurv) 0 else {
        #     if (twoGroup) c(delayFit$cens$n$x["any"], delayFit$cens$n$y["any"]) else delayFit$cens$n$x["any"]
        #   }
        #
        #   if (twoGroup) {
        #     pmin.int(
        #
        #       pmax.int(k/2 + 2L, ceiling(2L * (nObs - nCensAny)**.4)))
        #   } else {
        #     # min: at most nbr of classes as we have unique observed event times
        #     min(sum(delayFit$kmFit$n.event > 0), max(k + 2L, ceiling(2L * (nObs-nCensAny)**.4)))
        #   }
        # })
        #
        # stopifnot(! twoGroup) #XXX implement two group setting!
        # myGroup <- "x"
        # nCl <- nCl[1]
        #
        #
        # # interval boundaries:
        # #+choose so that we have equal observed events, from lowest to highest value of observed CDF
        # #+XXX ensure we observe events in each interval!
        # boundaries <- quantile(delayFit$kmFit,
        #                        probs = seq.int(from = 1L-max(delayFit$kmFit$surv), to = 1L-min(delayFit$kmFit$surv), length.out = nCl + 1L))
        # if (twoGroup) boundaries <- boundaries[1L + (myGroup != "x"),]
        # boundaries <- as.numeric(boundaries)
        # boundaries[[1L]] <- delayFit$data[[1L]]
        # #boundaries[length(boundaries)] <- +Inf
        #
        # # observed probabilities per interval
        # pIntObs <- rep_len(1/nCl, length.out = nCl)
        #
        # # xi: vector of normalized differences between observed and expected counts per interval
        # xiF <- function(pars) {
        #   pIntPred <- diff(rlang::exec(cdfF, !!! c(list(q = boundaries), pars)))
        #   sqrt(nObs[1L+(myGroup != "x")]) * (pIntObs - pIntPred) / sqrt(pIntPred)
        # }
        #
        # # minimize the distance measure (xi^t * xi) as function of theta to get parameter estimate for GOF-test
        # # parameters are not specifically transformed for better optimization (the main optimization for parameter estimation does this)
        # parStart <- coef(delayFit, group = myGroup)
        # minChisqOpt <- stats::optim(par = parStart,
        #                             fn = function(pars) as.numeric(crossprod(xiF(pars))),
        #                             method = "L-BFGS-B",
        #                             lower = c(0, rep_len(TOL_NUM, length(parStart)-1L)),
        #                             upper = c(delayFit$data[[1L]], rep_len(+Inf, length(parStart)-1L)),
        #                             # less stringent for convergence than default factor 1e7
        #                             control = list(factr = 1.5e7))#trace = 1, REPORT = 5))
        # # minimum X2-estimate for parameter vector theta
        # if (minChisqOpt$convergence > 0) {
        #    warning("minimum chi^2 parameter estimate for group ", myGroup, " did not converge!", call. = FALSE)
        # }
        # coef_minX2 <- minChisqOpt$par #coef(delayFit, group = myGroup))
        #
        # # variance estimate
        # survF <- purrr::partial(.f = cdfF, !!! c(as.list(coef_minX2), lower.tail = FALSE))
        # pIntPred <- diff(survF(q = rev(boundaries))) #rev because we have survF = 1-F instead of F #XXX rev() is wrong, use minus-sign instead
        #
        # # variance function, see Nikulin (2017), 2.1 (p. 33)
        # # @return numeric. variance estimate per time point
        # varF <- function(t) {
        #   # vectorize over t
        #   purrr::map_dbl(.x = t,
        #                  .f = function(.x) {
        #                    retVal <- NA_real_
        #                    intVal <- stats::integrate(f = function(x) {
        #                      survInd <- c(-1,1)[1+(myGroup == "x")]*seq_len(length(x))
        #                      x * rlang::exec(densF, !!! c(as.list(coef_minX2), list(x = x))) / (survF(q=x)^2 * summary(delayFit$cens$rcens, times = x, extend = TRUE)$surv[survInd])
        #                    }, lower = 0, upper = .x)
        #                    if (intVal$message == "OK") retVal <- intVal$value
        #                    retVal
        #                  })
        # } #fn
        #
        # Dmat <- diag(1/sqrt(pIntPred))
        # # apply() transposes the partial derivative matrix (as required, see Nikulin, p. 33)
        # Cmat <- Dmat %*% apply(pd_cdfF(distribution = distO$dist, par = coef_minX2, q = boundaries), MARGIN = 1L, FUN = diff)
        # Pmat <- diag(nCl) - Cmat %*% solve(crossprod(Cmat)) %*% t(Cmat)
        #
        # S1mat <- matrix(data = -1, nrow = nCl-1, ncol = nCl-1)
        # for (ro in 1L:(nCl-1)) {
        #   for (co in ro:(nCl-1)) {
        #     S1mat[ro, co] <- survF(q = boundaries[1L+ro]) * survF(q = boundaries[1L+co]) * varF(boundaries[1L+min(ro, co)])
        #   } #rof co
        # } #rof ro
        # S1mat[lower.tri(S1mat)] <- t(S1mat)[lower.tri(S1mat)]
        #
        # Jmat <- diag(nrow = nCl, ncol = nCl-1L)
        # Jmat[row(Jmat)-1L == col(Jmat)] <- -1
        #
        # Smat <- Pmat %*% Dmat %*% Jmat %*% S1mat %*% t(Jmat) %*% Dmat %*% Pmat
        #
        # # OLD stuff commented out!
        # # statist <- c(`X^2` = as.numeric(t(xiF(coef_minX2)) %*% MASS::ginv(Smat) %*% xiF(coef_minX2)))
        # #
        # # dgf <- nCl - (k + 1L)
        # # p_val <- stats::pchisq(q = statist, df = dgf, lower.tail = FALSE)
        #
        # #
        # ##
        # ### OLD END
        # ##
        # #
      } #esle isSurv
    },

    NRR = ,
    nikulin = {
      methStr <- "Nikulin-Rao-Robson's Goodness-of-fit (GOF) test"

      if (isSurv) {
        #+see book, Nikulin, 2017, chapter 2
        switch(
          distO$dist,
          exponential = {
            # with specific test statistics for exponential
            # see chapter 2, 2.5.1 (p. 51ff) in Nikulin (2017)

            if (twoGroup) {
              stop("XXX currently works only for single group!")
            }
            dat <- delayFit$data
            stopifnot(is.Surv(dat)) # and not list of Surv!
            nCl <- nCl[[1L]]
            evInd <- delayFit$cens$ind[[1L]]$obs

            # shift observations back by delay-estimate
            obsXc <- dat[, 1] - coef(delayFit, group = "x")[1L] # here quick-fix for single group only. XXX fixme

            # build interval boundaries (data dependent)
            S <- c(
              0,
              (length(obsXc) - seq_along(obsXc)) * obsXc + cumsum(obsXc)
            )
            a <- numeric(length = nCl + 1)
            a[nCl + 1] <- obsXc[length(obsXc)]
            for (j in seq_len(nCl) - 1L) {
              jksn <- j / nCl * S[[length(S)]]
              i <- which.max(jksn >= S)
              a[j + 1] <- (jksn -
                if (i > 1) cumsum(obsXc[seq_len(i - 1)]) else 0) /
                (length(obsXc) - i + 1)
            } #rof
            U <- tabulate(
              findInterval(
                x = obsXc[evInd],
                vec = a,
                rightmost.closed = TRUE,
                all.inside = TRUE
              ),
              nbins = nCl
            )
            stopifnot(any(U > 0))
            # drop empty intervals
            U <- U[U > 0]

            statist <- sum((U - length(evInd) / length(U))^2 / U)
          },
          weibull = {
            stop("Weibull fixme XXX")
          },
          stop(
            glue("This distribution {distO$dist} is not handled here!"),
            call. = FALSE
          )
        ) #switch
      } else {
        stop("NRR for non-Surv here, please. XXX")
        statist <- -99
        dgf <- -99
      }

      p_val <- stop("use pchisq to fix me. XXX")
    },

    AD = ,
    ad = ,
    anderson = {
      stop(
        "Anderson-Darling GOF-test is currently not supported!",
        call. = FALSE
      )
      # EDF-based GOF-test
      # Anderson-Darling (AD) test statistic
      # cf Stephens, Tests based on EDF Statistics p.101, (4.2)

      methStr <- "Anderson-Darling Goodness-of-fit (GOF) test (per group)"

      if (isSurv) {
        stop("Censored observations are not supported here!", call. = FALSE)
      }

      testStat_ad <- function(datr, n) {
        i <- seq_along(datr)
        # in fact, A2 utilizes rev-order in its 2nd summation term
        -n -
          mean(
            (2L * i - 1L) * log(datr) + (2L * n - (2L * i - 1L)) * log(1 - datr)
          )
      }

      A2 <- if (twoGroup) {
        purrr::map2_dbl(.x = transform(delayFit), .y = nObs, .f = testStat_ad)
      } else {
        testStat_ad(datr = transform(delayFit), n = nObs)
      }

      p_val <- switch(
        distO$dist,
        exponential = {
          # modification for Exponential (cf Stephens, Table 4.14, p.138)
          # the correction factor approaches 1 from above.
          # We keep the number of N as all observations, independent of the number of parameters estimated in the null-model.
          # QQQ Should we increase N by the number of parameters p estimated less 2 ( p -2 because 2 parameters are estimated in standard delayed exponential)
          A2_mod <- A2 * pmax.int(1L, 1L + 5.4 / nObs - 11 / nObs**2L)

          # .ad_pval was defined in data-raw/ad_pval.R. Has been moved to scratch/test_GOF_ad_pval.R
          ##.ad_pval[['exponential']](A2_mod)
          NA_real_ # dummy return value
        },
        weibull = {
          # P-value for Weibull based on Lockhart, 1994 (Table 1)
          # interpolation model on logits using critical value and inverse of shape parameter
          params_ntr <- coef.incubate_fit(delayFit, transformed = FALSE)
          # .ad_pval was defined in data-raw/ad_pval.R. Has been moved to scratch/test_GOF_ad_pval.R
          ##.ad_pval[['weibull']](A2, params_ntr[grepl('shape', names(params_ntr), fixed = TRUE)])
          NA_real_ # dummy return value
        },
        stop("This distribution is not handled here (yet)!", call. = FALSE)
      )

      if (twoGroup) {
        A2 <- paste(signif(A2, 4), collapse = ' and ')
        # use Liptak to bring both P-values of AD-tests per group together
        p_val <- stats::pnorm(
          sum(sqrt(nObs) * stats::qnorm(p_val)) / sqrt(sum(nObs))
        )
      }

      statist <- c(`A^2` = A2)
    },
    # catch all
    stop('This GOF-test method is not supported!')
  )

  p_val <- if (!is.null(p_val)) as.numeric(p_val)

  # return test object

  # stats:::print.htest recognizes:
  #+parameter, alternative, null.value, conf.int, estimate
  structure(
    list(
      method = methStr,
      data.name = data_name,
      statistic = statist,
      df = dgf,
      p.value = p_val
    ),
    class = "htest"
  )
}


#' Test the difference for model parameter(s) between two uncorrelated groups
#'
#' The test is in fact a model comparison between a null model where the
#' parameters are enforced to be equal and an unconstrained full model. The
#' model parameters can be fit with various methods: MPSE, MLEn, MLEc or MLEw.
#' Parametric bootstrap tests and likelihood ratio tests are supported. As test
#' statistic for the bootstrap test we use twice the difference in best
#' (=lowest) objective function value, i.e. 2 * (`val_0` - `val_1`). The factor
#' 2 does not matter but it becomes reminiscent of a likelihood ratio test
#' statistic albeit the objective function is not a negative log-likelihood in
#' all cases (e.g. it is the negative of the maximum product spacing metric for
#' the MPSE-method).
#'
#' High values of this difference speak against the null model (i.e., high
#' `val_0` indicates bad fit under the null model0 and/or low values of `val_1`
#' indicate a good fit under the more general model1. The test is implemented as
#' a parametric bootstrap test, i.e., we
#'
#' 1. take the given null-model fit as ground truth
#' 2. regenerate data according to this model fit
#' 3. recalculate the test statistic
#' 4. appraise the observed test statistic in light of the generated distribution under H0
#'
#'
#' @param x data from reference/control group.
#' @param y data from the treatment group.
#' @param distribution Name of the distribution to use or distribution object.
#' @param twoPhase logical(1). Do we model two phases per group? Default is `FALSE`, i.e. a single delay phase per group.
#' @param type character. Which type of tests to perform?
#' @param param character. Names of parameters to test difference for. Default value is `'delay1'`. You can specify multiple parameters,
#'   by providing multiple parameter names or by concatenating them with a `+`
#'   in a single string. Ignored for non-parametric tests.
#' @param method character. Which method to fit the models.
#' @param profiled logical. Use the profiled likelihood?
#' @param ties character. How to handle ties in data vector of a group?
#' @param doLogrank logical. Do also non-parametric logrank tests?
#' @param R numeric(1). Number of bootstrap samples to evaluate the distribution of the test statistic.
#' @param chiSqApprox logical flag. In bootstrap, should we estimate the best degrees of freedom for chi-square to match the distribution of the test statistic under H0?
#' @param verbose numeric. How many details are requested? Higher value means more details. 0=off, no details.
#' @return list with the results of the test. Element P contains the different
#'   P-values, for instance from parametric bootstrap
#' @examples
#' set.seed(123)
#' # generate example data
#' grA <- rweib_delayed(n = 70, delay1 = 5, shape1 = 2, scale1 = 8)
#' grB <- rweib_delayed(n = 60, delay1 = 7, shape1 = 1.8, scale1 = 6)
#'
#' # difference in delay parameter is significant at 5% level
#' test_diff(x = grA, y = grB,
#'   distribution = "weibull", param = "delay1",
#'   type = "bootstrap", method = "MPSE", R = 50)
#'
#' # but the non-parametric logrank test is not significant
#' # no need to specify parameters
#' test_diff(x = grA, y = grB,
#'   type = "logrank")
#' @export
test_diff <- function(
  x,
  y = stop("Provide data for group y!"),
  distribution = c("exponential", "weibull"),
  twoPhase = FALSE,
  type = c("all", "bootstrap", "GOF", "moran", "pearson", "logrank", "LRT"),
  param = "delay1",
  method = c("MPSE", "MLEw", "MLEc", "MLEn"),
  profiled = method != "MPSE",
  ties = c("density", "equispaced", "error"),
  doLogrank = TRUE,
  R = 400,
  chiSqApprox = FALSE,
  verbose = 0
) {
  # setup ----

  # verbose arg
  if (is.logical(verbose)) {
    verbose <- as.numeric(verbose)
  }
  if (is.null(verbose) || !is.numeric(verbose) || !is.finite(verbose)) {
    verbose <- 0
  }
  verbose <- verbose[[1]]

  distO <- if (is.list(distribution)) {
    distribution
  } else {
    buildDist(match.arg(arg = distribution))
  }
  type <- match.arg(arg = type)
  isNonParametric <- type == "logrank"

  ties <- match.arg(arg = ties)
  method <- if (length(method) == 1 && toupper(method) == "MSE") {
    message(
      "The method name 'MPSE' is preferred over the previously used name 'MSE'!"
    )
    "MPSE"
  } else {
    method[1L]
  }
  stopifnot(is.logical(doLogrank), length(doLogrank) == 1L)

  method <- match.arg(method)

  if (type %in% c("moran", "pearson", "GOF") && method != "MPSE") {
    warning(
      "Goodness-of-fit (GOF) tests are only supported with MPSE currently!",
      call. = FALSE
    )
    return(invisible(NULL))
  } #fi

  # bitmask for test types: start all FALSE
  testMask <- rlang::set_names(
    logical(5L),
    nm = c("bootstrap", "pearson", "moran", "logrank", "LRT")
  )

  switch(
    EXPR = type,
    all = {
      testMask <- testMask | TRUE
      doLogrank <- TRUE
      testMask[c("pearson", "moran")] <- method == "MPSE"
    },
    # bootstrap #use better flags? like doBootstrap=, doLRT=?!
    bootstrap = {
      testMask["bootstrap"] <- TRUE
    },
    GOF = {
      stopifnot(method == "MPSE")
      testMask[c("pearson", "moran")] <- TRUE
    },
    pearson = {
      stopifnot(method == "MPSE")
      testMask["pearson"] <- TRUE
    },
    moran = {
      stopifnot(method == "MPSE")
      testMask["moran"] <- TRUE
    },
    logrank = {
      doLogrank <- TRUE
    }, #do only logrank tests, see below
    LRT = {
      testMask["LRT"] <- TRUE
    }, #likelihood ratio test

    stop("This type of test is not supported!", call. = FALSE)
  )

  # separate switch for additionally doing logrank tests
  testMask["logrank"] <- doLogrank

  if (!any(testMask)) {
    warning("Specify which test(s) to do!", call. = FALSE)
    return(invisible(NULL))
  }

  stopifnot(is.numeric(R), length(R) == 1L, R >= 1L)

  # what kind of data
  respL <- prepResponseVar(x0 = x, y0 = y, simplify = TRUE)
  stopifnot(is.list(respL), identical(names(respL), c("x", "y")))
  x <- respL[["x"]]
  y <- respL[["y"]]
  rm(list = "respL")

  # flag if we have Surv-data or not
  #+a glimpse at x is enough as it is either-or for both groups
  isSurv <- is.Surv(x)

  ts_obs <- t0_dist <- NULL
  GOF_mo0 <- GOF_mo1 <- NULL
  GOF_pears0 <- GOF_pears1 <- NULL
  P_LRT <- NULL
  P_boot <- chisq_df_hat <- NULL
  P_logrank <- P_logrank_pp <- NULL

  if (!isNonParametric) {
    # check if anything is required beyond logrank test,
    # if not, we can skip the model fitting and go directly to logrank test
    # there is something requested beyond logrank test
    stopifnot(any(testMask[names(testMask) != "logrank"]))

    # parameters to test differences for
    stopifnot(`param= should be character` = is.character(param))
    param <- param[!is.na(param) & nzchar(param)]
    param <- unique(param)
    # eventually split multiple parameter names separated by "+"
    param <- unlist(strsplit(param, split = "+", fixed = TRUE))
    # trim leading and trailing whitespace from parameter names
    param <- trimws(param)

    if (any(grepl(pattern = "_tr", param, fixed = TRUE))) {
      stop(
        "Parameter names in param= refer to the distribution parameters and not to the transformed parameters of the objective function.",
        call. = FALSE
      )
    }

    # translate convenience names (for single phase) to canonical names
    unNmbrdIdx <- !endsWith(param, suffix = "1") &
      !endsWith(param, suffix = "2")
    if (any(unNmbrdIdx)) {
      param[unNmbrdIdx] <- paste0(param[unNmbrdIdx], "1") #interpret un-numbered parameters as referring to phase 1
      if (verbose > 0L) {
        cat(
          "The unnumbered parameter names in param= are taken to refer to the initial phase. They are translated to canonical parameter names.\n"
        )
      }
    }

    onames <- distO$param(
      twoPhase = FALSE,
      twoGroup = FALSE,
      transformed = FALSE
    )
    stopifnot(
      is.numeric(x),
      length(x) > length(onames),
      is.numeric(y),
      length(y) > length(onames)
    )

    # retain only valid names in canonical order
    param <- intersect(onames, param)

    if (!length(param)) {
      stop(
        "Provide valid parameter names from the distribution to test for differences in two groups.",
        call. = FALSE
      )
    }

    # test statistic ----

    # Test statistic calculated from the given data, method and the model specification.
    #
    # The test statistic takes non-negative values.
    # High values of the test statistic speak in favour of H1:
    # @param strict logical. Accept models only if they converged flawlessly, i.e., if convergence=0?
    # @return list containing value of test statistic and null model fit. Or `NULL` in case of trouble.
    testStat <- function(x, y, strict = TRUE) {
      fit0 <- delay_model(
        x = x,
        y = y,
        distribution = distO$dist,
        twoPhase = twoPhase,
        method = method,
        bind = param,
        control = list(profiled = profiled, ties = ties)
      )
      fit1 <- delay_model(
        x = x,
        y = y,
        distribution = distO$dist,
        twoPhase = twoPhase,
        method = method,
        control = list(profiled = profiled, ties = ties)
      )

      if (
        is.null(fit0) ||
          is.null(fit0$optimizer) ||
          is.null(fit0$optimizer$valOpt) ||
          is.null(fit1) ||
          is.null(fit1$optimizer) ||
          is.null(fit1$optimizer$valOpt)
      ) {
        return(invisible(NULL))
      } #fi

      # if the more restricted model (fit0) yields better fit (=lower value in optimization) than the more general model (fit1)
      #+we are in trouble, possibly due to non-convergence, e.g., optim's convergence code 52
      #+we re-fit the general fit1 again using parameter-values from fit0
      if (
        fit0[["optimizer"]][["valOpt"]] + TOL_NUM <
          fit1[["optimizer"]][["valOpt"]] &&
          !is.null(fit1oa <- purrr::pluck(fit1, "optimizer", "optim_args"))
      ) {
        if (verbose > 0) {
          warning(
            "Restricted model with better fit (=smaller criterion) than unrestricted model.",
            call. = FALSE
          )
        } #fi

        # re-run fit1 with start values based on fitted parameters of reduced model fit0
        stopifnot(is.list(fit1oa), "par" %in% names(fit1oa))

        coef0 <- coef.incubate_fit(fit0, transformed = TRUE)
        pn1 <- names(fit1[["optimizer"]][["parOpt"]])
        # take over optimization coefficients for start values of fit1
        # QQQ Would match() or pmatch() help avoid the for-loop?
        for (na0 in names(fit0[["optimizer"]][["parOpt"]])) {
          fit1oa[["par"]][startsWith(pn1, prefix = na0)] <- coef0[[na0]]
        } #rof

        fit1oa[["control"]][["parscale"]] <- scalePars(parV = fit1oa[["par"]])
        fit1 <- update.incubate_fit(fit1, optim_args = fit1oa)

        if (
          is.null(fit1) ||
            is.null(fit1$optimizer) ||
            is.null(fit1$optimizer$valOpt) ||
            fit0[["optimizer"]][["valOpt"]] + TOL_NUM <
              fit1[["optimizer"]][["valOpt"]]
        ) {
          warning(
            "Restricted model with better fit (=smaller criterion in optimization) than unrestricted model even after refit of the unrestricted model!",
            call. = FALSE
          )
          return(invisible(NULL))
        } #fi
      } #fi bad fit1

      # check convergence of re-fits when in strict mode only:
      if (
        strict &&
          (purrr::chuck(fit0, "optimizer", "convergence") != 0 ||
            purrr::chuck(fit1, "optimizer", "convergence") != 0)
      ) {
        return(invisible(NULL))
      } #fi

      # mkuhn, 2024-08-28
      SHAPE_TEST <- TRUE
      # for the time being: add crude check for Weibull (tailored for MLEw) whether fit is completely unreasonable
      #XXX replace with better local maximum check (motivated by fitting routine for MLEw)
      if (strict && SHAPE_TEST && fit0$distO$dist == "weibull") {
        coefs <- c(coef.incubate_fit(fit0), coef.incubate_fit(fit1))

        if (any(coefs[startsWith(names(coefs), "shape")] > 7.1)) {
          if (verbose > 0) {
            warning(
              "Weibull fit with very high shape parameter (>7.1) rejected",
              call. = FALSE
            )
          }
          return(invisible(NULL))
        } #fi
      } #fi

      # higher values of T-val speak in favour of H1:
      #   1. fit0 (bind model) has high value (=bad fit)
      #   2. fit1 (free model) has low value (=good fit)
      #
      # we evaluate the fit with the criterion (e.g., MLE for all MLE-methods)
      # could also think about the optimization criterion
      # max(0L, fit0[["optimizer"]][["valOpt"]] - fit1[["optimizer"]][["valOpt"]]),
      list(
        val = 2 * max(0, fit0[["criterion"]][[1]] - fit1[["criterion"]][[1]]),
        fit0 = fit0,
        fit1 = fit1
      )
    } #fn testStat

    # observed test statistic
    ts_obs <- testStat(x, y, strict = TRUE)
    if (
      is.null(ts_obs) ||
        !is.list(ts_obs) ||
        !is.numeric(ts_obs[["val"]]) ||
        ts_obs[["val"]] < -TOL_NUM
    ) {
      stop(
        "Delay model failed for restricted null-model or free full model",
        call. = FALSE
      )
    } #fi

    fit0 <- ts_obs[["fit0"]] # restricted (bind=)
    fit1 <- ts_obs[["fit1"]] # unrestricted

    # P-values on parameters -----

    # GOF-test results
    #+ H0: simpler/restricted model 0 is sufficient
    #+ the GOF-test solely builds on fit0
    #+ take fitted parameters for both groups under null-model
    #+ and transform the observed data for both groups via cumulative distribution functions

    # spacings-based GOF-test
    if (testMask[["moran"]]) {
      GOF_mo0 <- test_GOF(delayFit = fit0, method = "moran")
      GOF_mo1 <- test_GOF(delayFit = fit1, method = "moran")
      #if (verbose > 0L) cat("Moran test stat for fit0: ", GOF_mo0$statistic, "\n")
    }

    # Pearson GOF-test based on Chi-square distribution.
    # under H0, expect counts according to uniform distribution
    if (testMask[["pearson"]]) {
      GOF_pears0 <- test_GOF(delayFit = fit0, method = "pearson")
      GOF_pears1 <- test_GOF(delayFit = fit1, method = "pearson")
    }

    if (testMask[["LRT"]]) {
      # likelihood ratio test (LRT), based on the criterion that was requested (MPSE or ML-based)
      P_LRT <- stats::pchisq(
        q = ts_obs[["val"]],
        df = length(param),
        lower.tail = FALSE
      )
    }

    if (testMask[["bootstrap"]]) {
      # parametric bootstrap:
      # generate R samples (x, y) by random sampling from the fitted H0-model (e.g. common delay through bind=),
      #+where all nuisance parameters are at their fitted value
      # calculate the test statistic on the simulated data
      # estimate P as proportion of simulated test statistics that exceed the observed test statistic t_obs

      # arguments to the random function generation
      # XXX censoring: we actually expect/support only right-censoring (but here, we still count *any* censoring)
      ranFunArgsX <- c(
        list(n = length(x), cens = fit0$cens$n[["x"]]["any"] / length(x)),
        coef.incubate_fit(fit0, group = "x", transformed = FALSE)
      )
      ranFunArgsY <- c(
        list(n = length(y), cens = fit0$cens$n[["y"]]["any"] / length(y)),
        coef.incubate_fit(fit0, group = "y", transformed = FALSE)
      )

      retL <- 1L + (verbose > 0L)
      t0_dist <- future.apply::future_vapply(
        X = seq_len(R),
        FUN.VALUE = double(retL),
        FUN = function(dummy) {
          # generate new data according to given fitted null-model
          # sort is not needed here, as it goes through the whole pipeline (factory method)
          ts_boot <- testStat(
            x = rlang::exec(distO$random, !!!ranFunArgsX),
            y = rlang::exec(distO$random, !!!ranFunArgsY),
            strict = FALSE
          )
          if (is.null(ts_boot)) {
            rep.int(NA_real_, times = retL)
          } else {
            c(
              ts_boot[["val"]],
              # verbose-mode: include convergence code
              purrr::chuck(ts_boot, "fit0", "optimizer", "convergence")
            )[seq_len(retL)]
          }
        },
        future.packages = c("incubate", "purrr", "rlang"),
        future.seed = TRUE,
        future.globals = TRUE #c("retL", "distO", "ranFunArgsX", "ranFunArgsY", "testStat", "delay_model", "MLEw_approx"),
      )

      if (verbose > 0L) {
        stopifnot(NROW(t0_dist) == 2L)
        fit0_conv <- t0_dist[2L, ]
        cat(
          glue(
            "Proportion of model failures: {as_percent(length(which(is.na(fit0_conv)))/length(fit0_conv))}",
            "Proportion of conv =  0: {as_percent(length(which(fit0_conv == 0))/ length(fit0_conv))}",
            "Proportion of conv = 52: {as_percent(length(which(fit0_conv == 52))/length(fit0_conv))}",
            .sep = "\n"
          ),
          "\n"
        )
        t0_dist <- t0_dist[1L, , drop = TRUE] #retain only ts_boot[['val']]
      } #fi
      t0_dist <- t0_dist[is.finite(t0_dist)]

      if (chiSqApprox && length(t0_dist) > 7L) {
        try(
          expr = {
            chisq_df_hat <- coef(MASS::fitdistr(
              x = t0_dist,
              densfun = "chi-squared",
              start = list(df = length(param)),
              method = "Brent",
              lower = .001,
              upper = 1001
            ))
          },
          silent = TRUE
        )
      } #fi

      # keep P-value from bootstrap only when at least half the nominal simulation runs have succeeded
      if (length(t0_dist) >= (R + 1) / 2 + 1) {
        P_boot <- (1L + sum(t0_dist >= ts_obs[["val"]])) /
          (length(t0_dist) + 1L)
      } else {
        warning(
          "Bootstrap failed as less than half of the simulations succeeded!",
          call. = FALSE
        )
      }
    } #fi bootstrap
  } #fi !isNonParametric

  # Log-rank tests
  if (testMask[["logrank"]]) {
    # data in long format
    dat_2gr <- tibble::tibble(
      evtime = if (isSurv) c(x, y) else Surv(c(x, y)),
      group = rep.int(c("x", "y"), times = c(length(x), length(y)))
    )
    P_logrank <- stats::pchisq(
      q = survival::survdiff(evtime ~ group, rho = 0, data = dat_2gr)$chisq,
      df = 1L,
      lower.tail = FALSE
    )
    # Peto & Peto modified Gehan-Wilcoxon test
    P_logrank_pp <- stats::pchisq(
      q = survival::survdiff(evtime ~ group, rho = 1, data = dat_2gr)$chisq,
      df = 1L,
      lower.tail = FALSE
    )
  } #fi logrank

  # compact cleanses NULL entries
  structure(
    purrr::compact(
      list(
        # two initial model fits
        #fit0 = fit0, fit1 = fit1, # debug only?!

        distribution = distribution,
        t_obs = ts_obs[["val"]],
        testDist = t0_dist,
        R = if (testMask[["bootstrap"]]) length(t0_dist),
        chisq_df_hat = chisq_df_hat,
        # param will be dropped if NULL (due to compact)
        param = if (!isNonParametric) param,
        # save only non-NULL p-values
        P = purrr::compact(list(
          bootstrap = P_boot,
          LRT = P_LRT,
          moran = as.vector(GOF_mo0$p.value),
          moran1 = as.vector(GOF_mo1$p.value),
          pearson = as.vector(GOF_pears0$p.value),
          pearson1 = as.vector(GOF_pears1$p.value),
          logrank = P_logrank,
          logrank_pp = P_logrank_pp
        ))
      )
    ),
    class = "incubate_test"
  )
}

#' @export
print.incubate_test <- function(x, ...) {
  if (is.null(x$param)) {
    cat(
      glue(
        "Test for difference in distribution between two groups.",
        "Alternative hypothesis: the distribution is different between the two groups.",
        "Log-rank P-value: {if (is.numeric(x$P$logrank)) format.pval(x$P$logrank) else '-'}",
        "Peto & Peto modified Gehan-Wilcoxon P-value: {if (is.numeric(x$P$logrank_pp)) format.pval(x$P$logrank_pp) else '-'}",
        .sep = "\n"
      ),
      '\n'
    )
  } else {
    params <- paste(x$param, collapse = ' & ')
    P_boot_str <- if (is.numeric(x$P$bootstrap)) {
      format.pval(x$P$bootstrap)
    } else {
      '-'
    }
    cat(
      glue(
        "Test for difference in {x$distribution} {if (length(x$param) > 1) 'parameters' else 'parameter'} {params} between two groups.",
        "Alternative hypothesis: {params} {c('is', 'are')[[1L + (length(x$param) > 1)]]} different between the two groups.",
        "Parametric Bootstrap P-value: {P_boot_str}",
        .sep = "\n"
      ),
      '\n'
    )
  }
}


#' @export
plot.incubate_test <- function(x, y, title, subtitle, ...) {
  stopifnot(inherits(x, "incubate_test"))

  if (is.null(x$param)) {
    message(
      'No parameter specified for the test, cannot plot distribution of test statistic under H0.'
    )
    return(invisible(NULL))
  }

  rlang::check_installed(
    pkg = 'ggplot2',
    reason = 'to get plots',
    version = '3.3'
  )

  testDist <- x[["testDist"]]

  if (is.null(testDist)) {
    message('No bootstrap test to visualize.')
    return(invisible(NULL))
  }

  # set R = "-" when entry R is NULL or absent
  R <- purrr::pluck(x, "R", .default = "-")
  distrib <- purrr::pluck(x, "distribution", .default = "unknown distribution")

  if (missing(title)) {
    title <- glue(
      "Distribution of test statistic under H0 for parameter {paste(x$param, collapse = ' & ')}"
    )
  }
  if (missing(subtitle) && !is.null(x$P$bootstrap)) {
    subtitle <- glue(
      'Sampling distribution, based on {R} parametric bootstrap draws, using the {distrib} model',
      'Bootstrap P-value = {format.pval(x$P$bootstrap, eps = 1e-3)}'
    )
    #"Approximated by a chi-square distribution with df={signif(x[['chisq_df_hat']], 2)}.")
  }

  p <- ggplot2::ggplot(
    tibble::tibble(testDist = testDist),
    mapping = ggplot2::aes_(x = ~testDist, y = ~ ggplot2::after_stat(density))
  ) +
    ggplot2::geom_histogram(bins = 11L + ceiling(sqrt(R)))

  # extract maximum density value
  ymax <- max(ggplot2::layer_data(p)[['y']])
  ymax <- ceiling(max(ymax + .1, ymax * 1.01))

  if (!is.null(x[['chisq_df_hat']]) && is.numeric(x[['chisq_df_hat']])) {
    p <- p +
      ggplot2::geom_function(
        inherit.aes = FALSE,
        fun = stats::dchisq,
        args = list(df = x[['chisq_df_hat']]),
        col = "red",
        linetype = "dotted"
      )
  }

  p +
    ggplot2::geom_vline(
      xintercept = x[["t_obs"]],
      linetype = "dashed",
      colour = "grey"
    ) +
    ggplot2::coord_cartesian(ylim = c(0L, ymax)) +
    ggplot2::labs(x = "Test statistic", title = title, subtitle = subtitle)
}


#' Power simulation function for a two-group comparison
#'
#' Simulate power for a test of difference between two groups for a given distribution with delay.
#' The effect is specified in terms of the model parameters for both groups.
#' There are two modes of operation:
#' 1. `power=NULL`: simulate power based on given sample size `n` (post-hoc power estimation)
#' 2. `n=NULL`: search iteratively for a suitable sample size `n` for a given power
#'
#' The power is estimated by simulating data according to the specified model and testing for differences in the simulated data.
#' The proportion of simulated datasets where the test rejects the null hypothesis at the given significance level is the estimated power.
#' The test can be a parametric bootstrap test or a non-parametric logrank test. For logrank tests, the `param=` argument should not be specified.
#' Specify the effect size (`eff=`) as a list with two elements, each element holds the parameter vector of the distribution of the response variable of one group.
#' The fitting method (`method=`) and the kind of significance test (`test=`) are handed down to [test_diff()].
#' The more power simulation rounds (parameter `nPowerSim=`) the more densely the space of possible data
#' according to the specified model is sampled.
#'
#' Note that estimating sample size `n` is computationally intensive.
#' The iterative search uses heuristics to find a sample size `n` within the
#' provided range (`nRange=`), and the estimated sample size might yield a
#' slightly different power level. Hence, check the reported power in the
#' output. The search algorithm comes to better results when the admissible
#' range for sample size (`nRange=`) is chosen sensibly and not too wide.
#' In case the estimated sample size and the achieved power is too high it might
#' pay off to rerun the function with an adapted admissible range for the sample
#' size by giving a narrower range in `nRange=`.
#'
#' @param distribution character. Which assumed distribution is used for the
#'   power calculation. Default is `"exponential"`.
#' @param twoPhase logical(1). Do we model two phases per group? Default is
#'   `FALSE`, i.e. a single delay phase per group.
#' @param eff list of length 2. The two list elements must be numeric vectors that
#'   contain the model parameters (as understood by the delay-distribution
#'   functions provided by this package) for the two groups.
#' @param param character. Parameter name(s) which are to be tested for
#'   difference and for which to simulate the power. Default value is
#'   `'delay1'`. You can specify multiple parameters, by giving a vector or
#'   by concatenating them with a `+` in a single string. For logrank tests, this argument is ignored.
#' @param test character. Which test to use for this power estimation? Defaults
#'   to `"bootstrap"`. Non-parametric logrank test is also possible (either
#'   `"logrank"` or `"logrank_pp"`). See also [test_diff()].
#' @param method character. Which fitting method to use in case of a parametric test.
#' @param n integer. Number of observations per group for the power simulation
#'   or `NULL` when n is to be estimated for a given power.
#' @param power numeric. `NULL` when power is to be estimated for a given sample
#'   size or a desired power is specified (and `n` is estimated).
#' @param r numeric. Ratio of both groups sizes, ny / nx. Default value is 1,
#'   i.e. balanced group sizes. Must be positive.
#' @param sig.level numeric. Significance level. Default is 0.05.
#' @param nPowerSim integer. Number of simulation rounds. Default value 1600
#'   yields a standard error of 0.01 for power if the true power is 80%.
#' @param R integer. Number of bootstrap samples for test of difference
#'   within each power simulation. It affects the resolution of the
#'   P-value for each simulation round. A value of around `R=200` gives a
#'   resolution of 0.5% which might be enough for power analysis.
#' @param nRange integer. Admissible range for sample size when power is
#'   specified and sample size is requested. The routine might not find the
#'   optimal sample size when this range is set too wide.
#' @param verbose numeric. How many details are requested? Higher value means
#'   more details. 0=off, no details.
#' @returns List of results of power simulation. Or `NULL` in case of errors.
#' @seealso [test_diff()]
#' @examples
#' # Simulate power for a given sample size:
#' # test for any difference in a delay-exponential model using logrank test
#' # the assumed effect is given in terms of model parameters for both groups
#' power_diff(
#'   eff = list(ctrl = c(delay1 = 5, rate1 = .09),
#'              trtm = c(delay1 = 7, rate1 = .12)),
#'   test = "logrank",
#'   n = 16, power = NULL, nPowerSim = 300)
#'
#' \dontrun{
#' # test for difference in delay in a delay-exponential model via a bootstrap test:
#' # the power is estimated based on nPowersim = 520 simulated datasets
#' # for real applications, use a higher nPowerSim (e.g. 1600) for more
#' # precise power estimation and a higher R (e.g. 400) for more precise
#' # P-value estimation in each simulation round
#' set.seed(123) # for reproducibility
#' power_diff(
#'   eff = list(grA = c(delay1 = 5.2, rate1 = .1),
#'              grB = c(delay1 = 7, rate1 = .12)),
#'   param = "delay1",
#'   test = "bootstrap", method = "MPSE",
#'   n = 16, power = NULL,
#'   nPowerSim = 520, R = 160)
#'
#' # test for difference in rate in a delay-exponential model via a bootstrap test:
#' # required sample size is estimated for a given power.
#' # provide a suitable range for the sample size search via nRange=.
#' # The search for n takes more time than the previous example, as the function
#' # iteratively evaluates different sample sizes in order to find the right one.
#' set.seed(1234) # for reproducibility
#' power_diff(
#'   eff = list(grA = c(delay1 = 5, rate1 = .07),
#'              grB = c(delay1 = 7, rate1 = .18)),
#'   param = "rate1",
#'   test = "bootstrap", method = "MPSE",
#'   n = NULL, power = 0.8,
#'   nPowerSim = 480, R = 150,
#'   nRange = c(18, 48))
#' }
#' @export
power_diff <- function(
  distribution = c("exponential", "weibull"),
  twoPhase = FALSE,
  eff = stop("Provide parameters for both groups that reflect the effect!"),
  param = "delay1",
  test = c("bootstrap", "logrank", "logrank_pp", "LRT"), #"pearson", "moran",
  method = c("MPSE", "MLEw", "MLEc", "MLEn"),
  n = NULL,
  r = 1,
  sig.level = 0.05,
  power = NULL,
  nPowerSim = 1600,
  R = 200,
  nRange = c(5, 250),
  verbose = 0
) {
  TOL_POW <- sqrt(TOL_NUM)
  distribution <- match.arg(distribution)
  distO <- buildDist(distribution)
  #if (!missing(test)) test <- tolower(test)
  test <- match.arg(arg = test)
  stopifnot(length(test) == 1, nzchar(test))
  # category: e.g. test name w/o _pp suffix
  test_cat <- sub(pattern = "[_].+$", replacement = "", x = test, fixed = FALSE)
  if (test_cat != test) {
    stopifnot(startsWith(test, prefix = "logrank"))
  } #fi
  isNonParametric <- test_cat == "logrank"
  method <- match.arg(arg = method)
  ranFun <- distO$random
  onames <- distO$param(
    twoPhase = twoPhase,
    twoGroup = FALSE,
    transformed = FALSE
  )

  # handle param argument:
  # for logrank tests, we ignore the parameter names and set param to NULL, as logrank tests do not rely on distribution parameters.
  # For other tests, we check and preprocess the parameter names.
  param <- if (isNonParametric) {
    if (!missing(param) && !is.null(param)) {
      warning(
        "Parameter names in param= are ignored for logrank tests.",
        call. = FALSE
      )
    }
    NULL
  } else {
    stopifnot(`param= arg must be character` = is.character(param))
    param <- param[!is.na(param) & nzchar(param)]
    param <- unique(param)
    # eventually split multiple parameter names separated by "+"
    param <- unlist(strsplit(param, split = "+", fixed = TRUE))
    # trim leading and trailing whitespace from parameter names
    param <- trimws(param)

    # preprocess parameter names
    # parameters for which to test difference and for which power is requested
    if (any(grepl(pattern = "_tr", param, fixed = TRUE))) {
      stop(
        "Parameter names in param= refer to the distribution parameters and not to the transformed parameters of the objective function.",
        call. = FALSE
      )
    }

    # translate convenience names (for single phase) to canonical names
    #+interpret un-numbered parameters as referring to phase 1
    unNmbrdIdx <- !grepl(pattern = "[12]$", param, fixed = FALSE)
    if (any(unNmbrdIdx)) {
      param[unNmbrdIdx] <- paste0(param[unNmbrdIdx], "1")
      if (verbose > 0L) {
        message(
          "Unnumbered parameter names in param= are taken to refer to initial phase and are translated to canonical parameter names.\n"
        )
      }
    }

    # only valid names in canonical order
    param <- intersect(onames, param)

    if (!length(param)) {
      stop(
        "Provide valid parameter names from the distribution to test for differences in two groups.",
        call. = FALSE
      )
    }
    match.arg(param, choices = onames, several.ok = TRUE)
  } #else param

  stopifnot(is.null(n) || (is.numeric(n) && length(n) == 1L && is.finite(n)))
  stopifnot(
    is.null(power) ||
      (is.numeric(power) && length(power) == 1L && power > 0L && power < 1L)
  )
  if (!xor(is.null(n), is.null(power))) {
    stop('Either set `n=NULL` or `power=NULL`!', call. = FALSE)
  }

  stopifnot(
    length(sig.level) == 1L,
    is.numeric(sig.level),
    is.finite(sig.level),
    sig.level > 0L,
    sig.level < 1L
  )
  stopifnot(is.numeric(r), length(r) == 1L, r > 0L)
  stopifnot(length(nPowerSim) == 1L, is.numeric(nPowerSim), nPowerSim >= 5L)
  stopifnot(length(R) == 1L, is.numeric(R), R >= 5L)
  stopifnot(
    length(nRange) == 2L,
    is.numeric(nRange),
    nRange[[1L]] >= 1L,
    nRange[[2L]] >= 1L
  )
  # make sure we have a proper interval from small to large
  nRange <- sort.int(ceiling(nRange))
  nPowerSim <- ceiling(nPowerSim)
  if (R > 5000) {
    R <- 5000
    message(
      "Capping R at 5000. ",
      "Higher values are not necessary for power simulations ",
      "as R affects only the resolution of the P-value for each simulation round. ",
      "The number of simulation rounds is controlled by parameter `nPowerSim=`."
    )
  }
  R <- ceiling(R)

  stopifnot(is.list(eff), length(eff) == 2L)
  parx <- eff[[1L]]
  pary <- eff[[2L]]

  stopifnot(is.numeric(parx), is.numeric(pary))
  stopifnot(length(parx) == length(onames), length(pary) == length(onames))
  parx <- rlang::set_names(parx, onames)
  pary <- rlang::set_names(pary, onames)

  # Simulate power for given sample sizes. An internal helper function.
  # @param B number of simulations to estimate power
  # @param R number of bootstrap samples for testing difference (only used for bootstrap test)
  # @returns power as prop of p-values smaller than alpha, or NA
  simulatePower <- function(nx, ny, B = nPowerSim, R) {
    nx <- ceiling(nx)
    ny <- ceiling(ny)

    # repeatedly test for difference in parameter on bootstrapped data
    P_dist <- future.apply::future_vapply(
      X = seq_len(B),
      FUN.VALUE = double(1L),
      FUN = function(dummy) {
        # generate data according to chosen model
        #+and with the specified effect
        datx <- purrr::exec(ranFun, !!!c(n = nx, parx))
        daty <- purrr::exec(ranFun, !!!c(n = ny, pary))

        P_val <- NA_real_
        try(
          expr = {
            P_val <- purrr::pluck(
              test_diff(
                x = datx,
                y = daty,
                method = method,
                distribution = distO,
                twoPhase = twoPhase,
                param = param,
                type = test_cat,
                R = R
              ),
              "P",
              test,
              .default = NA_real_
            )
          },
          silent = TRUE
        )
        P_val
      },
      future.seed = TRUE
    )

    P_dist <- P_dist[is.finite(P_dist)]

    if (!length(P_dist)) {
      warning("No valid power simulation results.", call. = FALSE)
      return(invisible(NULL))
    }

    if (length(P_dist) < 100L) {
      warning("Low resultion for power estimate.", call. = FALSE)
    }

    # return
    if (length(P_dist)) {
      sum(P_dist < sig.level) / length(P_dist)
    } else {
      NA_real_
    }
  } #fn simulatePower

  nx <- ny <- -1
  powerGrid <- NULL

  if (is.null(power)) {
    # easy case: estimate power once, for given n
    nx <- ceiling(n)
    ny <- ceiling(r * n)
    if (nx < length(onames) || ny < length(onames)) {
      warning("Too few observations to fit parameters.", call. = FALSE)
      return(invisible(NULL))
    }

    # estimate power through simulation
    power <- simulatePower(nx = nx, ny = ny, B = nPowerSim, R = R)
  } else {
    # estimate n for specified power
    stopifnot(is.null(n))

    # quick first screening round
    B1 <- min(250L, nPowerSim)
    R1 <- min(100L, R)
    i2 <- -1L

    # 1st iteration
    nx_cand1 <- unique(ceiling(seq.int(
      from = nRange[[1L]],
      to = nRange[[2L]],
      length.out = 5L
    )))
    nbr_nx_cand1 <- length(nx_cand1)

    # if single n lives within range, return the power for it (no search for n necessary)
    if (nbr_nx_cand1 == 1L) {
      # recursive call: but easy case now
      return(power_diff(
        distribution = distribution,
        twoPhase = twoPhase,
        eff = eff,
        param = param,
        test = test,
        method = method,
        n = nx_cand1[[1L]],
        r = r,
        sig.level = sig.level,
        power = NULL,
        nPowerSim = nPowerSim,
        R = R,
        verbose = verbose
      ))
    } #fi

    pow_cand1 <- rep_len(-1, length.out = nbr_nx_cand1)
    for (i1 in seq_along(nx_cand1)) {
      nxc <- nx_cand1[[i1]]
      pow_cand1[[i1]] <- simulatePower(nx = nxc, ny = nxc * r, B = B1, R = R1)

      # are we in the vicinity of the target power already?
      if (pow_cand1[[i1]] >= power - TOL_POW) break
    } #rof

    # store preliminary power estimates
    powerGrid <- tibble(
      nx = nx_cand1[pow_cand1 > 0],
      ny = ceiling(nx * r),
      power = pow_cand1[pow_cand1 > 0],
      iter = 1L,
      B = B1,
      R = R1
    )

    if (NROW(powerGrid) < 1L) {
      # no valid power estimate
      stop(
        "Simulations unsuccessful in finding power estimates within specified range in first round!",
        call. = FALSE
      )
    } #fi

    # flag
    refine <- TRUE #NROW(powerGrid) >= 2L

    # issue warning when preliminary analysis stops at the extreme ends
    # first iteration
    if (i1 == 1L) {
      warning(
        "Smallest n within nRange already exceeds requested power in first round! ",
        "Consider enlarging `nRange=` downwards.",
        call. = FALSE
      )
      refine <- FALSE
    }

    # check last iteration
    if (
      i1 == nbr_nx_cand1 &&
        pow_cand1[[nbr_nx_cand1]] > -1 &&
        pow_cand1[[nbr_nx_cand1]] < power - TOL_POW
    ) {
      warning(
        glue(
          "Failed to reach requested power in first round with maximally allowed n. ",
          "Consider enlarging nRange= upwards. ",
          "n={nx_cand1[[nbr_nx_cand1]]} yields a power of only {as_percent(pow_cand1[[nbr_nx_cand1]])}."
        ),
        call. = FALSE
      )
      refine <- FALSE
    } #fi i1

    if (!refine) {
      # simply run again simulatePower with requested precision (B and R)
      nx <- ceiling(nx_cand1[[i1]])
      ny <- ceiling(nx_cand1[[i1]] * r)
      power <- if (nPowerSim > B1 || R > R1) {
        stats::weighted.mean(
          x = c(pow_cand1[[i1]], simulatePower(nx, ny, B = nPowerSim, R = R)),
          w = c(B1, nPowerSim)
        )
      } else {
        pow_cand1[[i1]]
      }
    } else {
      # refine: 2nd iteration with more simulation rounds
      powerMod <- if (NROW(powerGrid) <= 2L) {
        #XXX use nx+ny?
        stats::lm(power ~ nx, data = powerGrid)
      } else {
        stats::lm(power ~ stats::poly(nx, degree = 2), data = powerGrid)
      }
      powerPred <- tibble(
        nx = seq.int(from = nRange[[1L]], to = nRange[[2L]], by = 1L),
        predpower = stats::predict.lm(powerMod, newdata = data.frame(nx = nx)),
        diffpower = .data$predpower - power
      )
      # examine close neighbourhood of predicted best n
      powerPredInd <- intersect(
        seq_len(NROW(powerPred)),
        c(-1L, 0L, 1L, 2L, 3L) + which.max(powerPred$diffpower >= 0L)
      )

      nx_cand2 <- powerPred$nx[powerPredInd]
      pow_cand2 <- rep_len(-1, length.out = length(nx_cand2))

      for (i2 in seq_along(nx_cand2)) {
        nxc <- nx_cand2[[i2]]
        pow_cand2[[i2]] <- simulatePower(
          nx = nxc,
          ny = nxc * r,
          B = nPowerSim,
          R = R
        )
        # power already strong enough, stop searching for higher n
        if (pow_cand2[[i2]] >= power + TOL_POW) break
      } #rof i2

      powerGrid2 <- tibble(
        nx = nx_cand2[pow_cand2 > 0],
        ny = ceiling(nx * r),
        power = pow_cand2[pow_cand2 > 0],
        iter = 2L,
        B = nPowerSim,
        R = R
      )

      # store 2nd round (refinement) power estimates
      powerGrid <- rbind(powerGrid, powerGrid2)

      # pick sample size
      if (!any(powerGrid2$power >= power - TOL_POW)) {
        cat("Failed to reach requested power in second round of refinement!\n")
        print(powerGrid)
        #cat(paste(powerGrid2$power, collapse = " - "), "\n") ##debug
        stop(
          "Consider setting a better nRange= that more narrowly covers the region of promising sample sizes.",
          call. = FALSE
        )
      }

      nx <- powerGrid2$nx[which.max(powerGrid2$power >= power - TOL_POW)]
      ny <- ceiling(nx * r)
      power <- powerGrid2$power[which(powerGrid2$nx == nx)]
    } #esle !refine

    stopifnot(nx > 0L, ny > 0L, power > 0L)
  } #esle is.null(n)

  purrr::compact(
    list(
      name = "Difference in delayed model for time-to-event data in two groups",
      distribution = distribution,
      twoPhase = twoPhase,
      param = param,
      method = method,
      test = test,
      eff = eff,
      sig.level = sig.level,
      nx = nx,
      ny = ny,
      N = nx + ny,
      #P_dist = P_dist, ##debug
      powerGrid = powerGrid,
      power = power
    )
  )
}
