#' Make that utils.R is loaded first
#' @keywords internal
#' @noRd
#' @include utils.R
NULL


#' Factory method for objective function
#'
#' Given the observed data this factory method produces an objective function
#' which is either the negative of the MPSE-criterion H or some flavour of the negative log-likelihood for MLE.
#' Implemented variants of MLE-objective functions are naive MLE (`'MLEn'`), corrected MLE (`'MLEc'`) or weighted MLE (`'MLEw'`).
#' In any case, the objective function is to be **minimized**.
#'
#' @details
#' The objective function takes a vector of model parameters as argument.
#' From the observations, negative or infinite values are discarded during pre-processing.
#'
#' Profiling is implemented for single-phase models for Weibull and Exponential distributions:
#' profiling allows to estimate scale1 parameter (resp. rate1= for exponential) based on delay1 (and shape1 for Weibull).
#' Except for MLEw, the formula is derived from the conventional log-likelihood through equating its partial derivative with respect to scale1 to zer0.
#' This leads to the candidate value for scale1. This approach can be used also for the methods MPSE and MLEc.
#' For MLEw (weighted MLE) we have a weighting factor also in the formula for scale1.
#'
#' @param x numeric. observations
#' @param y numeric. observations in second group.
#' @param distO distribution object
#' @param method character(1). Specifies the method for which to build the objective function. Default value is `MPSE`. `MLEn` is the naive MLE-method, calculating the likelihood function as the product of density values. `MLEc` is the modified MLE.
#' @param twoPhase logical flag. Do we allow for two delay phases where event rate may change? Default is `FALSE`, i.e., a single delay phase.
#' @param bind character. parameter names that are bound together (i.e. equated) between both groups
#' @param control list. Fine-tune parameters for optimization. Needs to be set!
#' @return the objective function (e.g., the negative MPSE criterion) for given choice of model parameters or `NULL` upon errors
objFunFactory <- function(
  x,
  y = NULL,
  distO,
  method = c("MPSE", "MLEn", "MLEc", "MLEw"),
  twoPhase = FALSE,
  bind = NULL,
  control
) {
  # setup -----

  stopifnot(
    is.numeric(x),
    length(x) > 0,
    is.null(y) || is.numeric(y) && length(y) > 0
  )
  method <- match.arg(method)
  stopifnot(
    !missing(distO),
    is.list(distO),
    all(c("dist", "param", "twoPhaseAllowed") %in% names(distO))
  )
  stopifnot(is.null(bind) || is.character(bind) && length(bind) >= 1)

  stopifnot(is.logical(twoPhase), length(twoPhase) == 1L)
  # enforce either TRUE or FALSE
  twoPhase <- isTRUE(twoPhase) && distO$twoPhaseAllowed

  # original names: standard names of distribution (say, for a single group)
  oNames <- distO$param(
    twoPhase = twoPhase,
    twoGroup = FALSE,
    bind = NULL,
    transformed = FALSE,
    profiled = FALSE
  )

  stopifnot(
    is.list(control),
    all(c("verbose", "profiled", "ties", "pen_shape") %in% names(control))
  )
  verbose <- control$verbose
  profiled <- control$profiled
  stopifnot(is.logical(profiled), length(profiled) == 1)
  ties <- control$ties
  stopifnot(is.logical(control$pen_shape), length(control$pen_shape) == 1)

  # data preparation ----

  # unify Surv-type but keep numeric if no censoring
  respL <- prepResponseVar(x0 = x, y0 = y, simplify = TRUE)
  stopifnot(is.list(respL), identical(names(respL), c("x", "y")))
  x <- respL[["x"]]
  y <- respL[["y"]]
  rm(list = "respL")

  # flag if we have Surv-data or not
  isSurv <- inherits(x, what = "Surv")

  # Data preprocessing per group:
  # negative, NA and infinite values are dropped. Data gets sorted.
  # @param obs: data vector of one group
  # @return sorted, cleaned up data vector or NULL in case of trouble
  preprocessF <- function(obs) {
    if (is.null(obs) || !is.numeric(obs)) {
      return(NULL)
    }

    if (!isSurv) {
      # numeric response, non-Surv

      # fix numeric instabilities to have proper ties (when observations are pretty close)
      obs <- survival::aeqSurv(Surv(obs), tolerance = TOL_NUM)[,
        1L,
        drop = TRUE
      ]

      ind_neg <- which(obs < 0L)
      if (length(ind_neg) && !distO$negAllowed) {
        warning(
          "Negative values in data",
          deparse(substitute(obs)),
          "! These are dropped.",
          call. = FALSE
        )
        obs <- obs[-ind_neg]
      } # fi
      # drop NA and +/-Inf & sort #XXX sort.int?
      obs <- sort(obs[is.finite(obs)])

      if (!length(obs)) {
        warning(
          "Insufficient data! Only ",
          if (!distO$negAllowed) "non-negative and ",
          "finite real values are valid.",
          call. = FALSE
        )
        return(invisible(NULL))
      } # fi

      # check spread in data
      if (obs[[length(obs)]] < obs[[1L]] + 3L * TOL_NUM) {
        # && method %in% c("MPSE", "MLEc")) {
        warning(
          "Too small spread in data for this estimation method!",
          call. = FALSE
        )
        return(invisible(NULL))
      }
    } else {
      # Surv response
      survType <- attr(obs, which = "type", exact = TRUE)
      # for MPSE: check we only have right-censoring
      if (method == "MPSE" && survType != "right") {
        warning(
          "MPSE-fitting supports only right censored observations currently.",
          call. = FALSE
        )
        return(invisible(NULL))
      }

      # fix numeric instabilities to have proper ties
      obs <- survival::aeqSurv(obs, tolerance = TOL_NUM)

      # drop negative times
      ind_neg <- which(obs[, 1L] < 0L)
      if (length(ind_neg) && !distO$negAllowed) {
        warning(
          "Negative values in data",
          deparse(substitute(obs)),
          "! These are dropped.",
          call. = FALSE
        )
        obs <- obs[-ind_neg, , drop = FALSE]
      }
      # check finite for right-, left-censored or interval-censored Surv-times
      if (survType %in% c("right", "left", "interval")) {
        obs <- obs[which(is.finite(obs[, 1L])), , drop = FALSE]
      }
      # sort by time (first column)
      obs <- sort(obs)

      if (
        !length(obs) ||
          length(which(obs[, "status"] == 1L)) <
            1L + method %in% c("MPSE", "MLEc")
      ) {
        warning(
          glue(
            "Insufficient data! Only ",
            if (!distO$negAllowed) "non-negative and ",
            "finite real values are valid, ",
            "at least {c('one observed event time', 'two observed event times')[[1L + method %in% c('MPSE', 'MLEc')]]}",
            "required for estimation method {method}."
          ),
          call. = FALSE
        )
        return(invisible(NULL))
      } # fi

      # check spread in data (any, observed or censored times)
      if (obs[length(obs), 1L] < obs[1L, 1L] + 3L * TOL_NUM) {
        #&& method %in% c("MPSE", "MLEc")) {
        warning(
          "Too small spread in data for this estimation method!",
          call. = FALSE
        )
        return(invisible(NULL))
      }
    } #esle isSurv

    obs
  } #fn preprocessF

  # overwrite the data vectors with pre-processed data
  if (
    is.null({
      x <- preprocessF(obs = x)
    })
  ) {
    return(invisible(NULL))
  }
  y <- preprocessF(obs = y)

  # do we have two groups after pre-processing?
  twoGroup <- isTRUE(!is.null(y) && is.numeric(y) && length(y))

  # tie-informations for a group
  # @param obs data from a single group.
  # @returns list with tie indices or NULL if no data is given
  tieInformationF <- function(obs) {
    ##obs <- graphite[12:20][-4] ##test case: twins + triplicate
    if (is.null(obs)) {
      return(invisible(NULL))
    }

    roundOffPrecision <- estimRoundingError(obs, n_obs = 1001L)
    if (verbose > 0L) {
      cat(glue("Round-off error has magnitude {roundOffPrecision}."), "\n")
    }

    if (!length(obs)) {
      return(invisible(NULL))
    }

    # for Surv, we only consider duplicated observed event times, here.
    # as we only need to fix ties within observed times, ties in censored times use interpolation!?
    dupInd <- if (isSurv) {
      which(duplicated(obs) & obs[, "status"] == 1)
    } else {
      which(duplicated(obs))
    }

    # # reduce obs to only the observed event times for the remainder of the function
    # obs <- obs[which(obs[, "status"] == 1), 1L]

    tieGrp <- matrix(NA_real_, nrow = 0, ncol = 0)
    # index vector for cumDiff where spacing will be zer0 due to ties
    cumDiffInd <- integer(0L)

    # rounding radius:
    # used to break ties later on when evaluating MPSE-criterion
    # it can't be wider than smallest observed diff.
    # plogis to mitigate the effect of sample size: the larger the sample the more we can 'trust' the observed minimal diff
    diffObs <- if (isSurv) {
      outInd <- union(dupInd, which(obs[, "status"] != 1))
      diff(obs[if (length(outInd)) -outInd else TRUE, 1L])
    } else {
      diff(obs[if (length(dupInd)) -dupInd else TRUE]) #use unique???
    }

    rRad <- TOL_NUM +
      .5 *
        min(
          roundOffPrecision,
          # obs[1L] = min(obs) = diff of minimal obs with 0
          abs(
            if (isSurv) obs[which.max(obs[, "status"] == 1), 1L] else obs[[1L]]
          ), # very first time obs[[1L]] should be non-negative, anyways.
          #stats::plogis(q = .1+length(diffObs), scale = 17) * diffObs,
          diffObs,
          na.rm = TRUE
        )

    if (length(dupInd)) {
      stopifnot(dupInd[[1]] > 1L) # duplicated entries start at least 2
      if (ties == "error") {
        stop(
          "Ties within data are not allowed (ties == 'error')!",
          call. = FALSE
        )
      }

      gapsInDupInds <- c(1L, which(diff(dupInd) > 1) + 1) # +1 to be on dupInd-scale
      nbrTieGroups <- length(gapsInDupInds)
      if (verbose > 0L) {
        cat(glue(
          '{nbrTieGroups + length(dupInd)} tied observations ',
          'in {nbrTieGroups} group(s) within data vector.\n'
        ))
      }
      # start one position before duplicated-indices
      startInd <- dupInd[gapsInDupInds] - 1L
      # tabulate instead of for-loop
      len <- tabulate(
        bin = if (isSurv) {
          factor(obs[c(startInd, dupInd), 1L], levels = obs[startInd, 1L])
        } else {
          factor(obs[c(startInd, dupInd)], levels = obs[startInd])
        },
        nbins = length(startInd)
      )
      # len <- rep_len(-1L, length.out = length(startInd))
      # for (i in seq_along(len)) {
      #   j <- 1
      #   while (startInd[[i]]+j <= length(obs) && ! obs[[startInd[[i]]+j]] > obs[[startInd[i]]]) { j <- j+1 }
      #   len[[i]] <- j
      # } #rof
      stopifnot(all(len >= 2)) # at least 2 observations per tie-group
      tieGrp <- cbind(startInd, len) # 2-column matrix
      # index vector for cumDiff where spacing will be zer0 due to ties
      # unlist could become purrr::list_c (req v1.0.0)
      cumDiffInd <- rep.int(startInd, times = len - 1L) +
        unlist(purrr::map(.x = len - 1, .f = function(.x) {
          seq_len(length.out = .x)
        }))
    } #fi dupInd

    # return tie information (per group)
    list(
      tieGrp = tieGrp,
      cumDiffInd = cumDiffInd,
      numPrecision = c(precEstim = roundOffPrecision, rRad = rRad)
    )
  } #tieInformationF

  # store tie information per group
  tieInfo <- purrr::compact(list(
    strategy = ties,
    x = tieInformationF(obs = x),
    y = tieInformationF(obs = y)
  ))

  # adjust bind:
  #+enforce the canonical order of dist-parameters and drop unused parameters and empty strings
  #+set to NULL if not effectively two group setting
  bind <- intersect(oNames, bind)

  if (!twoGroup && !is.null(bind) && length(bind)) {
    bind <- NULL
    warning(
      "'bind=' was specified in vain as we have only a single group!",
      call. = FALSE
    )
  } #fi

  # adjust profiled:
  #+profiling is not implemented for some cases (like normal distribution model)!
  #+profiling is only possible if rate1/scale1 is not bound and single phase. Surv data is supported, however!
  #+We have genuine profiling formulas for scale1 only for MLEn and MLEw. For the other methods we re-use the formula from MLEn.
  #+When not supported, we set profiling=FALSE and issue a warning if it was requested!
  profiled0 <- profiled
  profiled <- profiled &&
    (!any(c("rate1", "scale1") %in% bind) || length(bind) == length(oNames)) &&
    !twoPhase

  if (xor(profiled0, profiled)) {
    warning(
      glue(
        "Option `profiled={profiled0}` was reversed to profiled={profiled}!"
      ),
      call. = FALSE
    )
    # update control-list as well
    control$profiled <- profiled
  }
  rm("profiled0")

  # KM fit (used for plotting later)
  survDat <- tibble(
    time = if (isSurv) c(x, y) else Surv(c(x, y)),
    groupVar = rep.int(c("x", "y"), times = c(length(x), length(y)))
  )
  kmFit <- survival::survfit(
    time ~ groupVar,
    data = survDat,
    start.time = 0,
    se.fit = FALSE,
    conf.type = "none"
  )

  cens <- local({
    # little helper function to count the censored observed by type (right, left, interval)
    censDescF <- function(.x, what = c("n", "ind", "rcens")) {
      what <- match.arg(what)

      # mock survfit-object
      rcensDummy <- list(surv = rlang::rep_along(kmFit$surv, 1))

      if (!isSurv) {
        return(list(
          n = c(right = 0L, left = 0L, interval = 0L, any = 0L),
          ind = list(
            right = integer(0),
            left = integer(0),
            interval = integer(0),
            obs = seq_along(.x)
          ),
          rcens = rcensDummy
        )[[what]])
      }

      switch(
        what,
        n = {
          nvctr <- switch(
            attr(.x, which = "type", exact = TRUE),
            right = c(sum(.x[, "status"] == 0), 0, 0),
            left = c(0, sum(.x[, "status"] == 0), 0),
            interval = tabulate(.x[, "status"] + 1L, nbins = 4L)[-2L],
            stop("This type of censoring is not supported!", call. = FALSE)
          )

          rlang::set_names(
            append(nvctr, sum(nvctr)),
            nm = c("right", "left", "interval", "any")
          )
        },
        ind = {
          switch(
            attr(.x, which = "type", exact = TRUE),
            right = list(
              right = which(.x[, "status"] == 0),
              left = integer(0L),
              interval = integer(0L),
              obs = which(.x[, "status"] == 1)
            ),
            left = list(
              right = integer(0L),
              left = which(.x[, "status"] == 0),
              interval = integer(0L),
              obs = which(.x[, "status"] == 1)
            ),
            interval = list(
              right = which(.x[, "status"] == 0),
              left = which(.x[, "status"] == 2),
              interval = which(.x[, "status"] == 3),
              obs = which(.x[, "status"] == 1)
            ),
            stop("This type of censoring is not supported!", call. = FALSE)
          )
        },
        rcens = {
          stopifnot(is.numeric(.x), length(.x) == 1L, .x >= 0L) #.x is nbr of right censorings in the data
          if (.x > 0L) {
            # treat right-censorings as events and rest as censoring
            survival::survfit(
              Surv(time[, 1L], event = !time[, "status"], type = "right") ~
                groupVar,
              data = survDat,
              conf.type = "none",
              se.fit = FALSE
            )
          } else {
            rcensDummy
          }
        },
        stop(
          "This request ",
          sQuote(what, q = FALSE),
          " is not supported here!",
          call. = FALSE
        )
      )
    } #fn censDescF

    retL <- list(
      isSurv = isSurv,
      n = purrr::compact(list(
        x = censDescF(x, what = "n"),
        y = if (twoGroup) censDescF(y, what = "n")
      )),
      ind = purrr::compact(list(
        x = censDescF(x, what = "ind"),
        y = if (twoGroup) censDescF(y, what = "ind")
      ))
    )
    # add KM estimator for right-censorings (combines all data, even when two groups, in one object)
    retL[["rcens"]] <- censDescF(
      retL$n$x[["right"]] + if (twoGroup) retL$n$y[["right"]] else 0,
      what = "rcens"
    )

    # stored as cens
    retL
  })

  # kmFit and rcens have same number of rows
  stopifnot(length(kmFit$surv) == length(cens$rcens$surv))

  # indices of first two relevant observations
  indForefront <- local({
    # little helper function to get the indices for the first two smallest observed values (non-censorings)
    #+in a sorted vector of observations
    forefrontIndF <- function(group) {
      stopifnot(!missing(group), is.character(group), length(group) == 1L)
      obs <- if (group == "y") y else x

      ind_obs1 <- ind_next <- integer()

      if (isSurv) {
        # Surv-response
        cindo_gr <- cens$ind[[group]]$obs
        stopifnot(length(cindo_gr) >= 2L)

        # check for easy case: no tie at first two observed event times
        if (obs[cindo_gr[2L], 1L] > obs[cindo_gr[1L], 1L] + TOL_NUM) {
          ind_obs1 <- cindo_gr[1L]
          ind_next <- cindo_gr[2L]
        } else {
          # walk down the observed event times
          i1 <- 2L
          while (obs[cindo_gr[i1], 1L] == obs[cindo_gr[1L], 1L]) {
            i1 <- i1 + 1L
          }
          ind_obs1 <- cindo_gr[seq_len(i1 - 1L)]
          if (length(cindo_gr) >= i1) ind_next <- cindo_gr[i1]
        } #esle
      } else {
        # numeric response, non-Surv
        stopifnot(length(obs) >= 2L)
        # check for easy case: no tie at beginning
        if (obs[[2L]] > obs[[1L]] + TOL_NUM) {
          ind_obs1 <- 1L
          ind_next <- 2L
        } else {
          # get indices for 1st and 2nd observation. Try with few first observations first (for better performance)
          for (l in sort.int(unique(c(
            5,
            10,
            50,
            100,
            500,
            1000,
            length(obs)
          )))) {
            if (l > length(obs)) {
              break
            } #fi
            obs_r <- rank(obs[seq_len(l)], ties.method = "min", na.last = TRUE)
            #which.max(obs_r > 1) # 1st index of 2nd obs
            firstTwoRanks <- unique(obs_r)[c(1L, 2L)]
            # check that there are two distinct values
            if (anyNA(firstTwoRanks)) {
              if (l < length(obs)) {
                next
              }
              if (method %in% c("MPSE", "MLEc")) {
                stop(
                  "At least two different distinct observation values per group required!",
                  call. = FALSE
                )
              } #fi method
              #else warning("Only a single unique distinct observation value in a group.", call. = FALSE)
            } #fi anyNA

            ind_obs1 <- which(obs_r == firstTwoRanks[1L])
            ind_next <- which(obs_r == firstTwoRanks[2L])
            if (length(ind_next)) ind_next <- ind_next[1L]
          } #rof l
        } #esle
      } #esle (non-Surv)

      list(inds_obs1 = ind_obs1, ind_next = ind_next)
    } #fn forefrontIndF

    purrr::compact(list(
      x = forefrontIndF(group = "x"),
      y = if (twoGroup) forefrontIndF(group = "y")
    ))
  }) #indForefront

  # set some coefficient names:
  # coefficient names (now that we have settled the profiling flag)
  # transformed (within optimization function)
  trNames <- distO$param(
    twoPhase = twoPhase,
    twoGroup = FALSE,
    bind = NULL,
    profiled = profiled,
    transformed = TRUE
  )
  # full parameter names (for the whole parameter vector spanning all groups)
  # original (including parameters that are profiled out in optimization)
  oNamesFull <- distO$param(
    twoPhase = twoPhase,
    twoGroup = twoGroup,
    bind = bind,
    profiled = FALSE,
    transformed = FALSE
  ) # profiled = FALSE because we consider here original parameters
  # transformed
  trNamesFull <- distO$param(
    twoPhase = twoPhase,
    twoGroup = twoGroup,
    bind = bind,
    profiled = profiled,
    transformed = TRUE
  ) # profiled as requested because we consider optimization parameters

  # checks ------------------------------------------------------------------

  # MLEw works only with profiling
  stopifnot(method != 'MLEw' || profiled)

  # check that there is enough data (here we also look at bind= if twoGroup)
  if (
    (!twoGroup && length(x) < length(oNames)) ||
      (twoGroup &&
        length(x) + length(y) < 2L * length(oNames) - length(bind) &&
        min(length(x), length(y)) < length(oNames) - length(bind))
  ) {
    warning("Too few valid observations provided!", call. = FALSE)
    return(invisible(NULL))
  }

  # parameter handling ----

  weights <- if (method != "MLEw") {
    list(W1 = c(x = 1, y = 1))
  } else {
    local({
      # Little helper to get the so-called z-values z_i := -log(1-F_i) = log(1/(1-F_i))
      #+which define the weights W1-W3
      # Median rank (mr) is a general way to estimate F_i (using a binomial model)
      # Benard's approximation estimates F_i as (i - a) / (N + 1 - 2*i) for some a
      #+a=.3 is recommended by Fothergill (1990) ***
      #+a=.3175 due to Filliben, "The probability plot.." (1975)
      # Assuming 3-parameter Weibull holds, we have z_i = ((x_(i) - a)/gamma)^k ~ Exp(1).
      # Exact values for 1st and last (=nth) entry are known (see "A reliable algorithm..", Jacquelin, 1993)
      # Cousineau uses MC-simulation, drawing from Exp(1). He does not use the observed data to derive F_i.
      # He chooses weights W1-W3 as median of their sampling distribution in MC irrespective of the concrete sample.
      # @param group Specifies for which group to estimate the z's
      # @param method How to estimate the z's. mr = median rank method to estimate F_i. mr_exact for short data sample
      # @param propagateTies logical. Should ties in the observations lead to ties in the z's as well?
      # @return numeric vector of ordered z's, same length as number of observed event time values in group
      zF <- function(
        group = "x",
        method = c("mr_exact", "mr_benard"),
        a = 0.3,
        propagateTies = FALSE
      ) {
        method <- match.arg(method)
        nObs <- if (group == "y") length(y) else length(x)

        if (!isSurv) {
          # numeric response, non-Surv

          if (propagateTies) {
            nObs0 <- nObs # save original length just to double-check
            obs <- if (group == "y") y else x
            ind_doz <- which(diff(obs) == 0)
            nObs <- nObs - length(ind_doz)
          }

          z0 <- if (nObs < 2) {
            .5
          } else if (method == "mr_exact" && nObs < 89L) {
            #use exact median rank values if not too many observations
            stats::qbeta(
              p = .5,
              shape1 = seq_len(nObs),
              shape2 = rev(seq_len(nObs))
            )
          } else {
            # Benard-style approximation for long observation vectors
            # 1st and last entry are still exact median rank values
            z_n <- .5^(1 / nObs)
            c(
              1 - z_n,
              stats::ppoints(n = nObs, a = a)[1L + seq_len(nObs - 2L)],
              z_n
            )
          }

          if (propagateTies && length(ind_doz)) {
            ind_rept <- rep_len(1L, length.out = length(z0))
            # index to update ind_rept
            iupd <- ind_doz[1L]
            idoz <- 1L

            while (idoz <= length(ind_doz)) {
              tie_cnt <- 1
              # count ties in group
              while (
                idoz + tie_cnt <= length(ind_doz) &&
                  ind_doz[idoz + tie_cnt] == ind_doz[idoz + tie_cnt - 1] + 1
              ) {
                tie_cnt <- tie_cnt + 1
              } #elihw

              # update tie count (for rep-times)
              ind_rept[iupd] <- ind_rept[iupd] + tie_cnt
              # update indices
              idoz <- idoz + tie_cnt - 1 # to end of tie group
              if (idoz < length(ind_doz)) {
                iupd <- iupd + ind_doz[idoz + 1] - ind_doz[idoz] - 1 # move iupd for next update
              }
              # move on
              idoz <- idoz + 1
            } # elihw

            z0 <- rep.int(z0, times = ind_rept)
            stopifnot(length(z0) == nObs0)
          } #fi

          return(-log(1 - z0))
        } #fi !isSurv

        stopifnot(isSurv)
        switch(
          EXPR = attr(x, which = "type", exact = TRUE),
          right = {
            # nbr of events observed
            n_ev <- nObs - cens$n[[group]][["right"]]

            # Cousineau estimates the weights from Monte-Carlo simulation study (MCSS) irrespective of the concrete sample
            # But here we have censorings which also effect F_i and we hence chose to estimate F_i from the concrete sample with its censoring scheme
            # Estimate F_i via Kaplan-Meier (copes with censorings) with Benard-style median rank estimation to avoid 0 and 1
            # z is an estimate for the ordered z_i = -log(1-F_i) = ((x_(i) - a)/gamma)^k ~ Exp(1)
            # unique event times (in all available groups)
            ind_evKM <- which(kmFit$n.event > 0.99) #at least one event (type=left/interval makes that we get fractional numbers here [but 0 is 0 also for interval!?])
            # get the right subset of indices for specified group (when having two groups)
            if (twoGroup) {
              # Cave: works only for two groups (x or y) as I only use the strata[[1L]] as cutpoint
              ind_evKM <- if (group == "x") {
                ind_evKM[ind_evKM <= kmFit$strata[[1L]]]
              } else {
                ind_evKM[ind_evKM > kmFit$strata[[1L]]]
              }
            }
            # n.event is generally not integer for type=interval/left. It is increased by a fraction (depending on number of events) and sums to nbr of events+1 (per group)
            # floor(n.event + n.censor) = n
            stopifnot(
              sum(
                as.integer(kmFit$n.event[ind_evKM]),
                if (twoGroup) {
                  kmFit$n.censor[
                    (if (group == "x") 1 else -1) * seq_len(kmFit$strata[[1L]])
                  ]
                } else {
                  kmFit$n.censor
                }
              ) ==
                kmFit$n[[if (group == "x") 1L else 2L]]
            )

            # estimated survival probabilities for event times, replicated
            # n.event is not always integer for Surv-type=interval/left. rep.int truncates floats & it should always work.
            kmSurvProb <- rep.int(
              kmFit$surv[ind_evKM],
              times = kmFit$n.event[ind_evKM]
            )
            stopifnot(length(kmSurvProb) == n_ev)

            # Benard-style median-rank estimation (avoid 0 and 1)
            -log(1 - ((1 - kmSurvProb) * n_ev - a) / (n_ev + 1 - 2 * a))
          },
          stop("This Surv-type is not handled here!", call. = FALSE)
        )
      } #fn zF

      z_x <- zF(group = "x", propagateTies = TRUE)
      z_y <- if (twoGroup) zF(group = "y", propagateTies = TRUE)

      # How to calculate the weights W1-W3?
      #we use now always 'sdist_median' (also for isSurv-data, cf. test-delay_estimation.R, line 802)
      #method_w <- if (isSurv) "sample" else "sdist_median"

      # Calculates W1-weight (as function of n)
      # W1 = mean(z_i) follows a gamma-dist with parameters shape=n and scale=1/n and we estimate W1 as median of it.
      # W1 is also used to get scale parameter during un-profiling.
      # We count all events because it is used to get scale parameter (and in this formula we already correct for censorings),
      #+e.g., nObs = length(x), even when there is cens$n$x[["any"]]
      # @param method By which method to calculate weights W1? 'sample' will use the mean of the provided sample of z-values, sdist_median uses the median of the sampling distribution (MC-sim)
      w1F <- function(
        nObs,
        z,
        method = c(
          "sdist_median",
          "sample",
          "hybrid",
          "cousineauGH",
          "cousineau2009"
        )
      ) {
        method <- match.arg(method)

        stopifnot(is.numeric(nObs))
        if (method %in% c("sample", "hybrid") && length(nObs) > 1) {
          stop("Please provide only a single group sample size.", call. = FALSE)
        } #fi

        switch(
          EXPR = method,
          sdist_median = {
            # Use results of own MC-simulation
            w1Fint(nObs)
          },
          cousineauGH = {
            W1_cousGH <- MLEw_approx$MCsim_cousineauGH[
              MLEw_approx$MCsim_cousineauGH$type == "W1" &
                MLEw_approx$MCsim_cousineauGH$location == "J",
            ]
            if (!all(nObs %in% W1_cousGH$n)) {
              stop(
                "W1-weights from Cousineau (GH) are not available for all requested sample group size n.",
                call. = FALSE
              )
            }
            W1_cousGH$value[W1_cousGH$n %in% nObs]
          },
          cousineau2009 = {
            #+cf. Cousineau's simulation results for median of W1's sampling distribution
            #+"Nearly unbiased estimators.." (2009), Table 2, column J_1
            W1_cous09 <- MLEw_approx$MCsim_cousineau2009[
              MLEw_approx$MCsim_cousineau2009$type == "W1" &
                MLEw_approx$MCsim_cousineau2009$location == "J",
            ]
            if (!all(nObs %in% W1_cous09$n)) {
              stop(
                "W1-weights from Cousineau are not available for all requested sample group size n.",
                call. = FALSE
              )
            }
            W1_cous09$value[W1_cous09$n %in% nObs]
          },
          sample = {
            if (missing(z) || !is.numeric(z) || length(z) == 0L) {
              stop(
                "Please provide the vector of z's to estimate W1!",
                call. = FALSE
              )
            }
            mean(z)
          },
          hybrid = {
            (w1Fint(nObs) + mean(z)) / 2L
          },
          stop(
            "This method for estimating W1 is not handled here!",
            call. = FALSE
          )
        )
      } #fn w1F

      w2F <- function(
        nObs,
        z,
        method = c(
          "sdist_median",
          "sample",
          "hybrid",
          "cousineauGH",
          "cousineau2009"
        )
      ) {
        method <- match.arg(method)

        stopifnot(is.numeric(nObs))
        if (method %in% c("sample", "hybrid") && length(nObs) > 1) {
          stop("Please provide only a single group sample size.", call. = FALSE)
        } #fi

        switch(
          EXPR = method,
          sdist_median = {
            # W2-approximation via asymptotic regression model SSasymp on log(n):
            # We hence model: W2 = 1 + (R0 - 1) * n^(-r)
            w2Fint(nObs)
          },
          cousineauGH = {
            W2_cousGH <- MLEw_approx$MCsim_cousineauGH[
              MLEw_approx$MCsim_cousineauGH$type == "W2" &
                MLEw_approx$MCsim_cousineauGH$location == "J",
            ]
            if (!all(nObs %in% W2_cousGH$n)) {
              stop(
                "W2-weights from Cousineau (GH) are not available for all requested sample group size n.",
                call. = FALSE
              )
            }
            W2_cousGH$value[W2_cousGH$n %in% nObs]
          },
          cousineau2009 = {
            # MC-simulation on W2 for n=1..16
            # cf. Cousineau's simulation results for median of W2's sampling distribution
            #+"Nearly unbiased estimators.." (2009), Table 3, column J_2
            W2_cous09 <- MLEw_approx$MCsim_cousineau2009[
              MLEw_approx$MCsim_cousineau2009$type == "W2" &
                MLEw_approx$MCsim_cousineau2009$location == "J",
            ]
            if (!all(nObs %in% W2_cous09$n)) {
              stop(
                "W2-weights from Cousineau are not available for all requested sample group size n.",
                call. = FALSE
              )
            }
            W2_cous09$value[W2_cous09$n %in% nObs]
          },
          sample = {
            sum(z * log(z)) / sum(z) - mean(log(z))
          },
          hybrid = {
            # mean betw med-approx and sample estimate
            (w2Fint(nObs) + sum(z * log(z)) / sum(z) - mean(log(z))) / 2L
          },
          stop(
            "This method for W2-estimation is not handled here!",
            call. = FALSE
          )
        )
      } #nf w2F

      # Factory for w3 function that gives the W3-weight for the given shape
      # parameter.
      # @returns W3 as function of shape k for given n. The function is *not*
      #   vectorized in argument k
      w3FF <- function(
        nObs,
        z,
        method = c(
          "sdist_median",
          "sample",
          "hybrid",
          "cousineauGH",
          "cousineau2009"
        )
      ) {
        # catch all for n = 1
        stopifnot(is.numeric(nObs))
        if (length(nObs) > 1) {
          stop("Please provide only a single group sample size.", call. = FALSE)
        } #fi
        if (nObs < 2L) {
          return(function(k) 1)
        }

        # case nObs > 1
        method <- match.arg(method)

        # fn of shape k
        switch(
          EXPR = method,
          sdist_median = {
            w3FFint(nObs)
          },
          cousineauGH = {
            W3_cousGH <- MLEw_approx$MCsim_cousineauGH[
              MLEw_approx$MCsim_cousineauGH$type == "W3" &
                MLEw_approx$MCsim_cousineauGH$location == "J",
            ]
            if (!nObs %in% W3_cousGH$n) {
              stop(
                "W3-weights from Cousineau (GH) are not available for requested sample group size n.",
                call. = FALSE
              )
            } #fi

            #focus on requested n
            W3_cousGH <- W3_cousGH[W3_cousGH$n == nObs, ]

            # use approximation function with linear interpolation:
            #we add points for extreme shape:
            # n==1 is already dealt with above!
            ##df <- tibble(nObs = 2:89) |>
            ##rowwise() |>
            ##mutate(W3_minShape = w3FFint(nObs = {nObs})(1e-7)) |>
            ##ungroup()
            #==> regression line for W3 for tiny shape
            ##summary(lm(W3_minShape ~ nObs, data = df))
            ##-0.228954 + 1.434439 * nObs
            ##nlme::gls(W3_minShape ~ nObs, data = df, correlation = nlme::corAR1(.4))
            #= -0.43907 + 1.43505 * nObs
            stats::approxfun(
              x = c(1e-4, W3_cousGH$shape, 50),
              y = c(-0.439 + 1.435 * nObs, W3_cousGH$value, 1),
              method = "linear",
              ties = "ordered",
              # safe to extrapolate on even more extreme
              # shape parameters on both sides
              rule = 2
            )
          },
          cousineau2009 = {
            W3_cous09 <- MLEw_approx$MCsim_cousineau2009[
              MLEw_approx$MCsim_cousineau2009$type == "W3" &
                MLEw_approx$MCsim_cousineau2009$location == "J",
            ]
            if (!nObs %in% W3_cous09$n) {
              stop(
                "W3-weights from Cousineau (2009) are not available for requested sample group size n.",
                call. = FALSE
              )
            } #fi

            #focus on requested n
            W3_cous09 <- W3_cous09[W3_cous09$n == nObs, ]

            # use approximation function with linear interpolation:
            #we add points for extreme shape:
            #+ for huge shape W3 approaches 1
            #+ for minimal shape there is a linear dependence on nObs
            # n==1 is already dealt with above!
            ##df <- tibble(nObs = 2:89) |>
            ##rowwise() |>
            ##mutate(W3_minShape = w3FFint(nObs = {nObs})(1e-7)) |>
            ##ungroup()
            #==> regression line for W3 for tiny shape
            ##summary(lm(W3_minShape ~ nObs, data = df))
            ##-0.228954 + 1.434439 * nObs
            ##nlme::gls(W3_minShape ~ nObs, data = df, correlation = nlme::corAR1(.4))
            #= -0.43907 + 1.43505 * nObs

            # function of shape by linear interpolation
            # stats::approxfun relies on current R version
            # (but its code is more robust since R v3.0.0)
            # no issue here as this code runs at run-time within the user's R session
            stats::approxfun(
              x = c(1e-4, W3_cous09$shape, 50),
              y = c(-0.439 + 1.435 * nObs, W3_cous09$value, 1),
              method = "linear",
              ties = "ordered",
              # safe to extrapolate on even more extreme
              # shape parameters on both sides
              rule = 2
            )
          },
          sample = function(k) {
            stopifnot(is.numeric(k), length(k) == 1)
            w1F(nObs = nObs, z = z, method = method) *
              if (log(k) < -5) {
                1
              } else if (k == 1) {
                mean(1 / z)
              } else {
                sum(1 / z^(1 / k)) / sum(z^((k - 1) / k))
              }
          },
          hybrid = function(k) {
            stopifnot(is.numeric(k), length(k) == 1)
            # calculate mean from sdist_median and sample
            (w3FFint(nObs)(k) +
              w1F(nObs = nObs, z = z, method = method) *
                if (log(k) < -5) {
                  1
                } else if (k == 1) {
                  mean(1 / z)
                } else {
                  sum(1 / z^(1 / k)) / sum(z^((k - 1) / k))
                }) /
              2
          },
          stop(
            "This method for W3 approximation is not handled here!",
            call. = FALSE
          )
        )
      } #nf w3FF

      #QQQ for W1 weights: do we use full length (even when censored obs are present?!) (W1 is used for un-profiling scale!)
      ##or #nObs = length(x) - cens$n$x[["any"]]),
      #+==> maybe best to turn off profiling when data isSurv
      # return list of weights
      list(
        W1 = c(
          x = w1F(nObs = length(x), z = z_x, method = control$MLEw_weight),
          # else 1: do we need W1$y also when only single group??
          y = if (twoGroup) {
            w1F(nObs = length(y), z = z_y, method = control$MLEw_weight)
          } else {
            1
          }
        ),
        W2 = c(
          x = w2F(nObs = length(x), z = z_x, method = control$MLEw_weight),
          y = if (twoGroup) {
            w2F(nObs = length(y), z = z_y, method = control$MLEw_weight)
          }
        ),
        W3 = purrr::compact(list(
          x = w3FF(nObs = length(x), z = z_x, method = control$MLEw_weight),
          y = if (twoGroup) {
            w3FF(nObs = length(y), z = z_y, method = control$MLEw_weight)
          }
        ))
      )
    }) #lacol
  } #esle weights

  stopifnot(!twoPhase) #XXX twoPhase not implemented yet!!

  # provide indices for x and for y
  # where to find the parameters per group in the parameter vector of the objective function
  extractParOptInd <- if (!twoGroup) {
    # single group!
    list(x = seq_along(trNames)) ##XXX Cave: trNames reacts to twoPhase-setting (which I've not thought through, yet)
  } else {
    # two group!
    #XXX exponential && profiled: indices are not correct for two groups, yet!!
    #+(this would allow to run simul_test.R!) #YYY already done?!
    if (is.null(bind)) {
      switch(
        distO$dist,
        exponential = {
          if (profiled) {
            list(x = c(1L), y = c(2L))
          } else {
            list(x = c(1L, 2L), y = c(3L, 4L))
          }
        },
        weibull = {
          if (profiled) {
            list(x = c(1L, 2L), y = c(3L, 4L))
          } else {
            list(x = c(1L, 2L, 3L), y = c(4L, 5L, 6L))
          }
        },
        normal = {
          stopifnot(!profiled)
          list(x = c(1L, 2L), y = c(3L, 4L))
        },
        stop(glue("Unsupported distribution {distO$dist}!"), call. = FALSE)
      )
    } else if (length(oNames) == length(bind)) {
      # twoGroup, but all parameters are bound!
      switch(
        distO$dist,
        exponential = {
          # profiled can actually be true (as each group leads to own scale/rate
          #+but it will be averaged (see mergePars!!)
          if (profiled) {
            #warning("Did not expect `profiled=TRUE` and full bind on all parameters!", call. = FALSE)
            list(x = c(1L), y = c(1L))
          } else {
            list(x = c(1L, 2L), y = c(1L, 2L))
          }
        },
        weibull = {
          if (profiled) {
            #warning("Did not expect `profiled=TRUE` and full bind on all parameters!", call. = FALSE)
            list(x = c(1L, 2L), y = c(1L, 2L))
          } else {
            list(x = c(1L, 2L, 3L), y = c(1L, 2L, 3L))
          }
        },
        normal = {
          stopifnot(!profiled)
          list(x = c(1L, 2L), y = c(1L, 2L))
        },
        stop(glue("Unsupported distribution {distO$dist}!"), call. = FALSE)
      )
    } else {
      # twoGroups & non-trivial bind

      local({
        # param with profiled=TRUE & transformed = FALSE removes the profile parameters (although it is original scale)
        #+ as we need the original parameter names without those of profiling
        oNamesFullProf <- if (!profiled) {
          oNamesFull
        } else {
          distO$param(
            twoPhase = twoPhase,
            bind = bind,
            twoGroup = TRUE,
            profiled = TRUE,
            transformed = FALSE
          )
        }

        # locally, drop "rate1/scale1" from oNames when in profiling mode
        # Cave: not robust! Think about (e.g.) twoPhase when profiling! (currently profiling is switched off when twoPhase)
        if (profiled) {
          oNames <- setdiff(oNames, c("rate1", "scale1"))
        } # only *local* temporary change
        nonbind <- setdiff(oNames, bind)
        list(
          x = as.vector(rlang::set_names(
            charmatch(c(bind, paste0(nonbind, ".x")), oNamesFullProf),
            nm = c(bind, nonbind)
          )[oNames]),
          y = as.vector(rlang::set_names(
            charmatch(c(bind, paste0(nonbind, ".y")), oNamesFullProf),
            nm = c(bind, nonbind)
          )[oNames])
        )
      }) #lacol
    } #esle
  } #esle twoGroup

  # provide indices for x and for y
  # where to find the parameters per group in the common parameter vector
  extractParInd <- if (!profiled) {
    # = optimization indices when no profiling
    extractParOptInd
  } else {
    # profiled!
    if (!twoGroup) {
      # profiled single group!
      list(x = seq_along(oNames)) ## Cave: oNames reacts to twoPhase-setting (which I've not thought through, yet)
      #if (distO$dist == 'exponential') list(x = c(1L, 2L)) else list(x = c(1L, 2L, 3L))
    } else {
      # twoGroup && profiled
      stopifnot(distO$dist != "normal")
      if (is.null(bind)) {
        if (distO$dist == 'exponential') {
          list(x = c(1L, 2L), y = c(3L, 4L))
        } else {
          # weibull
          list(x = c(1L, 2L, 3L), y = c(4L, 5L, 6L))
        }
      } else {
        if (length(oNames) == length(bind)) {
          # profiled can actually be true (as each group leads to own scale/rate
          #+but it will be averaged (see mergePars!!)
          #warning("Did not expect `profiled=TRUE` and full bind on all parameters!", call. = FALSE)
          if (distO$dist == 'exponential') {
            list(x = c(1L, 2L), y = c(1L, 2L))
          } else {
            # weibull
            list(x = c(1L, 2L, 3L), y = c(1L, 2L, 3L))
          }
        } else {
          # twoGroup & non-trivial bind
          local({
            # we consider the parameter names on original scale, with profiled parameters also back in!
            nonbind <- setdiff(oNames, bind)
            list(
              x = as.vector(rlang::set_names(
                charmatch(c(bind, paste0(nonbind, ".x")), oNamesFull),
                nm = c(bind, nonbind)
              )[oNames]),
              y = as.vector(rlang::set_names(
                charmatch(c(bind, paste0(nonbind, ".y")), oNamesFull),
                nm = c(bind, nonbind)
              )[oNames])
            )
          })
        }
      }
    }
  } #esle !profiled

  # parameter transformation matrices (for single group)
  paramTransf <- list(
    M = switch(
      distO$dist,
      exponential = matrix(
        c(1, 0, 0, 0, 0, 1, 0, 0, -1, 0, 1, 0, 0, 0, 0, 1),
        nrow = 4L,
        byrow = TRUE,
        dimnames = list(c("delay1_tr", "rate1_tr", "delay2_tr", "rate2_tr"))
      ),
      weibull = matrix(
        c(
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          0,
          -1,
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1
        ),
        nrow = 6L,
        byrow = TRUE,
        dimnames = list(c(
          "delay1_tr",
          "shape1_tr",
          "scale1_tr",
          "delay2_tr",
          "shape2_tr",
          "scale2_tr"
        ))
      ),
      normal = matrix(
        c(1, 0, 0, 1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(c("mean_tr", "sd_tr"))
      ),
      stop("Unknown distribution!", call. = FALSE)
    ),
    Minv = switch(
      distO$dist,
      exponential = matrix(
        c(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1),
        nrow = 4L,
        byrow = TRUE,
        dimnames = list(c("delay1", "rate1", "delay2", "rate2"))
      ),
      weibull = matrix(
        c(
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          0,
          1,
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          0,
          0,
          1
        ),
        nrow = 6L,
        byrow = TRUE,
        dimnames = list(c(
          "delay1",
          "shape1",
          "scale1",
          "delay2",
          "shape2",
          "scale2"
        ))
      ),
      normal = matrix(
        c(1, 0, 0, 1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(c("mean", "sd"))
      ),
      stop("Unknown distribution", call. = FALSE)
    ),
    F = list(
      exponential = c(stats::qlogis, log, log, log),
      weibull = c(
        stats::qlogis,
        log, #log1p, #identity, #=shape1
        log,
        log,
        log,
        log
      ),
      normal = c(identity, identity)
    )[[distO$dist]],
    Finv = list(
      exponential = c(stats::plogis, exp, exp, exp),
      weibull = c(
        stats::plogis,
        exp, #expm1, #identity, #=shape1
        exp,
        exp,
        exp,
        exp
      ),
      normal = c(identity, identity)
    )[[distO$dist]]
  )

  # transform parameter vector for a single group
  #
  # The transformed parameters are used within optimization.
  # It does not use parameter names.
  # The transformation ensures side-conditions (e.g. log-transformation ensures non-negativity of original parameter)
  # @param parV1 parameter vector for a single group
  # @param obs1 numeric. Can be used for transformation of first parameter delay1
  # @param inverse logical. `inverse=TRUE` takes optimization parameters back to original parameters
  # @return transformed parameter vector, unnamed!
  transformPars1 <- function(parV1, obs1 = 1, inverse = FALSE) {
    # b as a normalizing vector for the parameter vector parV1
    b <- rlang::rep_along(parV1, 1)
    if (distO$hasDelay && distO$dist %in% c("exponential", "weibull")) {
      b[[1]] <- max(obs1, DELAY_MIN) #indForefront[[group]][[ind_obs1]][[1]]
    }

    #QQQ is rlang::exec (with lapply) an alternative to mapply?
    #QQQ or better even: direct implementation of transformations here?
    if (inverse) {
      # param = Ainv %*% (Finv(param') * b)
      local({
        p <- as.numeric(
          paramTransf[["Minv"]][seq_along(parV1), seq_along(parV1)] %*%
            (as.numeric(.mapply(
              FUN = function(f, x) f(x),
              dots = list(paramTransf[["Finv"]][seq_along(parV1)], parV1),
              MoreArgs = NULL
            )) *
              b)
        )
        if (distO$hasDelay) {
          # make sure that delay1 stays slightly below obs1
          p[[1]] <- min(obs1 * (1 - .Machine$double.neg.eps), p[[1]])
        } #fi
        p
      })
    } else {
      # param' = F((A %*% param) / b)
      as.numeric(.mapply(
        FUN = function(f, x) f(x),
        dots = list(
          paramTransf[["F"]][seq_along(parV1)],
          as.numeric(
            paramTransf[["M"]][seq_along(parV1), seq_along(parV1)] %*% parV1
          ) /
            b
        ),
        MoreArgs = NULL
      ))
    }
  } #fn transformPars1

  # merge two parameter vectors
  # @param isOpt logical. Are the parameters on optimization scale?
  # @return merged parameter vector
  mergePars <- function(parx, pary, isOpt) {
    exParInd <- if (isOpt) extractParOptInd else extractParInd
    # aggregate parameters (via mean if isOpt or geometric mean if on original scale).
    #+this is necessary for merging start vector for parameter-optimization
    res <- as.vector(tapply(
      X = c(parx, pary),
      INDEX = unlist(exParInd),
      # arithmetic or geometric mean
      FUN = function(x) {
        stopifnot(length(x) <= 2L)
        if (length(x) <= 1L) {
          x
        } else if (isOpt) {
          (x[[1L]] + x[[2L]]) / 2L
        } else {
          #mean(x)
          sqrt(x[[1L]] * x[[2L]])
        } #prod(x)^(1/length(x))
      },
      simplify = TRUE
    ))
    # .. only for delay1 we use minimum as aggregation function
    #+(in this case first entry in exParInd$x and exParInd$y is 1!)
    if (distO$hasDelay && exParInd$x[[1L]] + exParInd$y[[1L]] == 2) {
      res[[1L]] <- min(parx[[1L]], pary[[1L]])
    }

    res
  }

  # Extract parameter vector for a specified group
  #
  # if parameters are for optimization and transformation is requested,
  # profiling is undone (if relevant)
  # @param group character. Extract parameters for the given group. If NULL, keep all parameters.
  # @param isOpt logical. Are the given parameters on optimization function scale?
  # @param transform logical. Transform parameters?
  # @param named logical. Extract parameters as named vector?
  # @return parameter vector
  extractPars <- function(
    parV,
    group = NULL,
    isOpt = TRUE,
    transform = FALSE,
    named = FALSE
  ) {
    if (is.null(parV)) {
      return(NULL)
    }
    # result is on optimization scale?
    resIsOpt <- xor(isOpt, transform) #TRUE if different

    # basically, ignore group= when single group: in this case use always canonical "x"
    if (!twoGroup) {
      group <- "x"
    }

    if (is.null(group)) {
      stopifnot(twoGroup)
      # give pars for both groups
      return(local({
        # recursive calls for the individual groups
        parx <- extractPars(
          parV,
          group = "x",
          isOpt = isOpt,
          transform = transform,
          named = FALSE
        )
        pary <- extractPars(
          parV,
          group = "y",
          isOpt = isOpt,
          transform = transform,
          named = FALSE
        )

        # merge the two parameter vectors back together (after a potential transformation)
        res0 <- mergePars(parx = parx, pary = pary, isOpt = resIsOpt)
        if (named) {
          res0 <- rlang::set_names(
            res0,
            nm = if (resIsOpt) trNamesFull else oNamesFull
          )
        }
        res0
      }))
    } #fi is.null(group)

    # index vector for specified group
    ind <- if (isOpt) extractParOptInd[[group]] else extractParInd[[group]]

    if (is.null(ind)) {
      return(NULL)
    }

    res <- if (!transform) {
      parV[ind]
    } else {
      # do transform
      local({
        frstInd <- indForefront[[group]][["inds_obs1"]][1]
        # myObs1 as numeric by [[ (even when survival)
        myObs1 <- if (group == "x") x[[frstInd]] else y[[frstInd]]
        res0 <- transformPars1(parV[ind], obs1 = myObs1, inverse = isOpt)

        if (profiled) {
          stopifnot(distO$dist != 'normal')
          # un-profile (when going from profiled par_opt to par_orig)
          if (isOpt) {
            # access observations for specified group
            obs <- if (group == "y") y else x
            k <- if (distO$hasShape) res0[[2L]] else 1L
            # calculate scale parameter
            scale0 <- if (!isSurv) {
              (mean((obs - res0[[1L]])^k) / weights$W1[[group]])^(1 / k)
            } else {
              # Surv: only right-censored observations currently implemented!
              stopifnot(attr(obs, which = "type", exact = TRUE) == 'right')

              # we consider all times (including censorings) but divide (for mean) only by the number of events
              # right-censored obs prior to delay candidate are set to zer0
              (sum(pmax.int(0, obs[, 1L] - res0[[1L]])^k) /
                (length(obs) - cens$n[[group]][["right"]]) /
                weights$W1[[group]])^(1 / k)

              # # consider only event times: we take them from the KM-fit
              # # was used as a (wrong) work-around for the problem of right-censorings earlier than the delay estimate
              # (mean((summary(kmFit)$time - res0[[1L]])^k) / weights$W1[[group]])^(1/k)
            }
            # add scale/rate parameter at the end of parameter vector
            res0 <- append(
              res0,
              values = if (distO$dist == 'exponential') 1 / scale0 else scale0
            )
          } else {
            # extract only remaining parameters
            res0 <- res0[extractParOptInd[[group]]]
          }
        } #fi profiled

        res0
      })
    } #esle

    # single group names
    if (named) {
      rlang::set_names(res, nm = if (resIsOpt) trNames else oNames)
    } else {
      as.vector(res)
    }
  } #fn extractPars

  # optimization arguments -----

  # Get optimization start values and upper limits based on observations from a single group
  # for `twoPhase=TRUE` there will be more parameters
  # with profiling no scale parameter is returned (as it is not optimized)
  # @param obs vector of observations from single group
  # @return list with transformed par for single group and upper limits for delay parameters, in canonical order (bind has no effect here!)
  getParSetting.gr <- function(obs) {
    # contract: obs is sorted!

    if (
      isSurv && (cens$n$x[["left"]] %||% 0) + (cens$n$y[["left"]] %||% 0) > 0
    ) {
      stop("Left-censoring is not supported here!", call. = FALSE)
    }
    # extract first event time (we assume there is no left-censoring!)
    firstEvTime <- if (isSurv) {
      obs[which(obs[, "status"] == 1)[[1L]], 1L]
    } else {
      obs[[1L]]
    }

    # Surv: convert to numeric, quick fix, use only event times as numeric vector that are observed or right censored
    # XXX improve here?, e.g., use flatten_surv from lme4cens?! # could use cens-list here
    if (isSurv) {
      obs <- obs[, 1L, drop = TRUE][obs[, "status", drop = TRUE] <= 1]
    }

    parV <- switch(
      EXPR = distO$dist,
      # min(obs) = obs[1L]
      exponential = {
        parV0 <- c(
          max(DELAY_MIN, obs[[1L]] - 3 / (length(obs) + 1)),
          mean(obs - obs[[1L]] + 2 / length(obs))^-1L
        )

        # two extra parameters when exponential with *two* phases
        if (twoPhase) {
          parV0 <- c(parV0, obs[[floor(.5 + length(obs) / 2L)]], parV0[[2L]])
        }

        #parV0 <- rlang::set_names(parV0, nm = oNames)
        # transform start-parameters for optfun-parametrization
        parV0 <- transformPars1(parV0, obs1 = firstEvTime, inverse = FALSE)

        # drop scale if profiling
        if (profiled) {
          stopifnot(!twoPhase) #XXX not implemented, yet!
          parV0 <- parV0[1L] #keep delay (and drop "rate1")
        }
        parV0
      },
      weibull = {
        # start values from 'Weibull plot'
        #+using the empirical distribution function
        ## in MASS::fitdistr they simplify:
        # lx <- log(x)
        # m <- mean(lx)
        # v <- var(lx)
        # shape <- 1.2/sqrt(v)
        # scale <- exp(m + 0.572/shape)

        parV0 <- local({
          start_delay <- max(DELAY_MIN, obs[[1L]] - 3 / (length(obs) + 1))

          # use median rank approximation for empirical Weibull CDF: F(i,n) = (i - 0.3) / (n + 0.4)
          # and then ordinate is log(1/(1-F)) = -log(1-F) on log-scale
          start_y <- log(-log(1 - stats::ppoints(n = length(obs), a = .3)))

          # log of centred observations
          # avoid negative values (as DELAY_MIN is positive)
          lobs0 <- log(pmax.int(DELAY_MIN, obs - start_delay))

          # simple linear regression of Y=start_y vs X=log-obs
          # cf. lm.fit(x = cbind(1, log(obs)), y = start_y)$coefficients
          # weighted version with more weight in the middle:
          # w <- seq_along(obs); w <- w * (max(w)+1-w) #or use plogis-weights to downweight the early obs
          # lm.wfit(x = cbind(1, log(obs)), y = start_y, w = plogis(-2:(length(obs)-3)))$coefficients
          start_shape <- stats::cor(lobs0, start_y) *
            stats::sd(start_y) /
            stats::sd(lobs0)
          start_scale <- exp(mean(lobs0) - mean(start_y) / start_shape) # scale from intercept

          c(start_delay, start_shape, start_scale)
        })

        if (verbose > 3) {
          cat(
            "Start values 1st phase (a single group): ",
            paste(
              c("delay", "shape", "scale"),
              round(parV0, 2),
              sep = ": ",
              collapse = ", "
            ),
            "\n"
          )
        } #fi verbose

        # support 2-phase with additional start parameters
        if (twoPhase) {
          parV0 <- c(parV0, obs[[floor(.5 + length(obs) / 2L)]], parV0[-1L])
        }

        #parV0 <- rlang::set_names(parV0, nm = oNames)
        # transform start-parameters for optfun-parametrization
        parV0 <- transformPars1(parV0, obs1 = firstEvTime, inverse = FALSE)

        # drop scale if profiling
        if (profiled) {
          stopifnot(!twoPhase) #XXX not implemented, yet!
          parV0 <- parV0[c(1L, 2L)] # drop "scale1"
        }
        parV0
      },
      normal = {
        # robust start values
        # IQR in normal is 1.349 times the std. deviation
        c(stats::median(obs), stats::IQR(obs) / 1.349)
      },
      # default:
      stop(
        glue("Distribution {sQuote(distO$dist)} is not implemented!"),
        call. = FALSE
      )
    )

    list(
      par = parV,
      # exponential: prior transformation used #log(firstEvTime) #iso 0
      delay1_upper = Inf,
      delay2_upper = log(max(
        DELAY_MIN,
        obs[[length(obs)]] - .02 / length(obs),
        obs[[length(obs)]] * .999
      ))
    )
  } #fn getParSetting.gr

  # profile likelihood: maximize profiled log-lik f directly
  # if FALSE, go indirectly: consider min(f'^2) to hunt for *local* extremum as these local extrema have f'^2 == 0 as necessary condition
  #profiled_llik_directly <- TRUE

  # parameter bounds: set lower & upper bounds
  lowerB <- upperB <- rlang::rep_named(names = trNamesFull, x = NA_real_)

  #XXX #QQQ Should this go up to extractPars-function where the transformations are defined???
  PAR_BOUNDS <- list(
    delay1 = c(lower = -Inf, upper = NA_real_),
    delay2 = c(lower = -Inf, upper = NA_real_),
    rate = c(lower = -Inf, upper = +Inf),
    # shape lower bound for MLEnp (actually for shape1)
    #shape = c(lower = if (profiled && method == 'MLEn' && !profiled_llik_directly) 1.49e-8 else -Inf, upper = +Inf),
    shape = c(lower = -Inf, upper = 3.5), # exp(3.5) = 33 is already huge for shape [exp(1.6) = 5, exp(4.5) = 90]
    scale = c(lower = -Inf, upper = +Inf),
    mean = c(lower = -Inf, upper = +Inf),
    sd = c(lower = 0, upper = +Inf)
  )

  # set bounds from lookup table PAR_BOUNDS
  # alas, purrr::iwalk did not work for me here
  for (nam in names(PAR_BOUNDS)) {
    idx <- startsWith(trNamesFull, prefix = nam)
    if (any(idx)) {
      lowerB[idx] <- purrr::chuck(PAR_BOUNDS, nam, 'lower')
      upperB[idx] <- purrr::chuck(PAR_BOUNDS, nam, 'upper')
    } #fi
  } #rof

  par0_x <- getParSetting.gr(x)
  if (verbose > 2) {
    cat(
      "Start parameters for opt, group x: ",
      paste(round(par0_x$par, 3), collapse = ", "),
      "\n"
    )
  }
  parV <-
    if (!twoGroup) {
      # set parameter vector for group 1 and finish upper bound: match delay1 and delay2
      upperB[['delay1_tr']] <- par0_x[['delay1_upper']]
      if (twoPhase) {
        upperB[['delay2_tr']] <- par0_x[['delay2_upper']]
      }

      par0_x[['par']]
    } else {
      #twoGroup

      # all parameters are bound
      if (length(bind) == length(oNames)) {
        # treat x and y as a single group for upper limit & start value heuristic
        par0_xy <- getParSetting.gr(c(x, y))

        upperB['delay1_tr'] <- par0_xy[['delay1_upper']]
        if (twoPhase) {
          upperB[['delay2_tr']] <- par0_xy[['delay2_upper']]
        }

        par0_xy[['par']]
      } else {
        #twoGroup, not all params bound!
        par0_y <- getParSetting.gr(y)

        start_x <- par0_x[['par']]
        start_y <- par0_y[['par']]

        # set upper bound for delay parameter(s)!
        if ('delay1' %in% bind) {
          upperB['delay1_tr'] <- min(
            par0_x[['delay1_upper']],
            par0_y[['delay1_upper']]
          )
        } else {
          upperB['delay1_tr.x'] <- par0_x[['delay1_upper']]
          upperB['delay1_tr.y'] <- par0_y[['delay1_upper']]
        } # fi

        if (twoPhase) {
          if ('delay2' %in% bind) {
            upperB[['delay2_tr']] <- max(
              par0_x[['delay2_upper']],
              par0_y[['delay2_upper']]
            )
          } else {
            upperB['delay2_tr.x'] <- par0_x[['delay2_upper']]
            upperB['delay2_tr.y'] <- par0_y[['delay2_upper']]
          }
        } #fi twoPhase

        # return start value
        if (is.null(bind)) {
          # two groups unbound
          c(start_x, start_y)
        } else {
          mergePars(parx = start_x, pary = start_y, isOpt = TRUE)
        }
      } #esle not all params bound!
    } #esle twoGrp

  # ensure we have names of transformed parameters
  parV <- rlang::set_names(parV, nm = trNamesFull)

  stopifnot(!any(is.na(lowerB), is.na(upperB)))
  # clean up env. # use local() more???
  remove(list = c("PAR_BOUNDS", "par0_x"))

  if (verbose > 1L) {
    cat(
      "Start values for opt: ",
      paste(names(parV), round(parV, 3), sep = "=", collapse = ", "),
      "\n"
    )
  }

  optim_args <- list(
    par = parV,
    method = "L-BFGS-B",
    lower = lowerB,
    upper = upperB,
    # most parameters are on log-scale.
    control = list(parscale = scalePars(parV, lowerB = 1e-3, upperB = 1e3))
  )

  # objective function ----

  # Penalization for high values of shape per group
  #
  # For Weibull distribution in MLEw method it penalizes high shape values. The
  # penalization factor increases with sample size as location (median) and
  # spread (mad) of the ML-objective function grow with sample size: most
  # clearly so for MLEn and MLEc. MLEw is less regular (maybe due to bad fits)
  #
  # Older idea was to use start values and estimate lowish average of log-density
  # for the observed values in the group (or: for range of possible values)
  #+But: important is not so much the base level of log-likelihood
  #+but what reduction is possible through optimization of start values, no?!
  #
  # @seealso simulations in `MLEw_shape_penalization.R`
  # @param k candidate value for shape
  # @param nObs number of observations in group
  # @return non-negative penalty value. Big values mean higher penalty (it gets subtracted from the criterion to be maximized)
  penF <- function(k, nObs = 1) {
    if (!isTRUE(control$pen_shape)) {
      return(0)
    }

    pen_shape_shift <- 9.5 #shift parameter of softplus penalty
    pen_shape_steep <- .91 #steepness of softplus penality

    nObs * log1p(exp(pen_shape_steep * (k - pen_shape_shift)) / pen_shape_steep)
  } #fn penF

  # Calculate objective function to be maximized based on the log-likelihood
  #
  # Log-likelihood based value to be maximized, either naive, weighted or in
  # corrected form It is calculated for a single group. A two-group setting will
  # call this function twice, once for each group. What precisely is calculated
  # depends on its surrounding closure (see variable `method` but also the
  # profiled-flag).
  # For MLEw, we use a value that comes from 1st deriv of log-likelihood.
  # Penalty term is subtracted here, see `penF`
  # @param pars complete vector of parameters (can refer to two groups)
  # @param group which group?
  # @param isOrig Is the parameter vector already on original scale?
  # @param methodSelected character Which likelihood method to use?
  # @param isCrit Was a criterion explicitly requested? (which would mean: no penalization)
  # @return log-likelihood (certain flavour or related like negative L2-norm of gradient of log-likelihood) for specified group
  getLogLik <- function(
    pars,
    group,
    isOrig = FALSE,
    methodSelected,
    isCrit = FALSE
  ) {
    # Old idea: change signature to be with pars.gr and obs for both getLogLik and getCumDiffs
    #+But what are the benefits?

    # access observations of group
    obs <- if (group == "y") y else x #direct access by name
    #obs <- rlang::env_get(env = rlang::env_parent(rlang::current_env(), n=1L), nm = group, inherit = FALSE)

    # extract parameters for specified group on original scale (for CDF)
    pars.gr <- extractPars(
      pars,
      group = group,
      isOpt = !isOrig,
      transform = !isOrig
    )

    #Calculate the objective function to be maximized which depends on
    #+methodSelected
    #+profiled
    nObs <- length(obs)
    stopifnot(nObs > 1L)
    # shape parameter (candidate), set to 1 if not applicable
    k <- if (distO$hasShape) pars.gr[[2L]] else 1L

    # return value (to be maximized)
    rVal <- switch(
      EXPR = methodSelected,
      MLEn = {
        # objective fn to be maximized:
        # log-likelihood

        if (!(profiled && distO$dist == "weibull")) {
          # all cases except profiled Weibull
          # directly give log-likelihood (iterative approach)

          if (!isSurv) {
            # numeric, non-Surv
            sum(rlang::exec(
              distO$pdf,
              !!!c(list(x = obs, log = TRUE), pars.gr)
            ))
          } else {
            # Surv-response
            switch(
              attr(obs, which = "type", exact = TRUE),
              right = {
                sum(
                  rlang::exec(
                    distO$pdf,
                    !!!c(
                      list(x = obs[cens$ind[[group]]$obs, 1L], log = TRUE),
                      pars.gr
                    )
                  ),
                  rlang::exec(
                    distO$cdf,
                    !!!c(
                      list(
                        q = obs[cens$ind[[group]]$right, 1L],
                        lower.tail = FALSE,
                        log.p = TRUE
                      ),
                      pars.gr
                    )
                  )
                )
              },
              stop("This Surv-type is not supported!", call. = FALSE)
            )
          } #esle !isSurv
        } else {
          # Weibull, scale parameter profiled out:
          # scale as function of shape and delay (via 1st derivative)
          stopifnot(profiled, distO$dist == 'weibull')

          if (!isSurv) {
            # numeric response, non-Surv
            obs_c <- obs - pars.gr[[1L]]
            #cat("\nDelay a: ", pars.gr[["delay1"]], "Shape k: ", k, " (", pars[2], ")\n") #DDD debug

            # return early when we have too high delay parameter
            if (obs_c[[1L]] < 0) {
              return(NA_real_)
            }

            # objective function to maximize:
            # use log-likelihood function directly for delay and shape
            # the scale parameter is profiled out (using 1st derivative)
            # we use log(mean(obs_c^k)) = log(sum(obs_c^k)) - log(nObs)
            # and log(n) + log(k) = log(n*k)
            nObs *
              ((k - 1) *
                mean(log(obs_c)) -
                log(sum(obs_c^k)) +
                log(nObs * k) -
                1)

            # alternative:
            #indirect way: !profiled_llik_directly
            #consider min(f'^2) to hunt for *local* extremum as these local extrema have f'^2 == 0 as necessary condition
            #We would need to check that we have indeed an local **maximum** for the log-likelihood (as we have only found candidate values by looking for roots of f')
            #   - (1/k + mean(log(obs_c)) - sum(log(obs_c) * obs_c^k) / sum(obs_c^k))^2 +
            #     # 1st factor is inverse of harmonic mean
            #     -(mean(1/obs_c) * sum(obs_c^k)/sum(obs_c^(k-1)) - k/(k-1))^2 +
            #     # optional penalization term
            #     -penF(k, nObs = nObs)
          } else {
            # Surv
            switch(
              attr(obs, which = "type", exact = TRUE),
              right = {
                # all observations (event or right-censored), centred
                #+then, pick all observed events
                obs_c <- obs[, 1L] - pars.gr[[1L]]
                obs_evc <- obs_c[cens$ind[[group]]$obs] #ind refers to full vector obs_c

                # for later calculations (=> log)
                #+restrict obs_c to positive entries
                obs_c <- obs_c[which(obs_c > 0)]

                # nbr of observed events
                nObs_e <- nObs - cens$n[[group]][["right"]]

                # objective fn to maximize: log-likelihood
                # we used "partial derivative = 0" equation to profile out scale parameter,
                #+but otherwise, use log-likelihood function directly on delay and shape
                nObs_e *
                  ((k - 1) *
                    mean(log(obs_evc)) -
                    log(sum(obs_c^k)) +
                    log(nObs_e * k) -
                    1)
              },
              stop("This Surv-type is not supported!", call. = FALSE)
            ) #hctiws
          } #esle (isSurv)
        } #esle (profiled)
      }, #MLEn

      # weighted MLE
      MLEw = {
        # objective fn to be maximized:
        # neg. squared L2-norm of (score equations + weights)

        stopifnot(profiled)
        stopifnot(distO$hasDelay, distO$dist != "normal")

        # W3 function
        w3F <- weights$W3[[group]]

        # return value (to be maximized)
        rVal <- if (!isSurv) {
          # numeric response, non-Surv
          obs_c <- obs - pars.gr[[1L]]

          # consider length of 1st derivative vector:
          #+it vanishes for any local extremum (necessary condition)
          #+hence, neg of squared summands are maximized to come close to 0 (could also be abs() iso/ square)
          # from equation for shape k
          -(weights$W2[[group]] /
            k -
            sum(log(obs_c) * obs_c^k) / sum(obs_c^k) +
            mean(log(obs_c)))^2 +
            # from equation for delay
            # mean(1/obs_evc) is inverse of harmonic mean
            -(w3F(k) - mean(1 / obs_c) * sum(obs_c^k) / sum(obs_c^(k - 1)))^2
        } else {
          # Surv-response
          switch(
            EXPR = attr(obs, which = "type", exact = TRUE),
            right = {
              # all observations (event or right-censored), centred
              #+then, pick all observed events
              obs_c <- obs[, 1L] - pars.gr[[1L]]
              obs_evc <- obs_c[cens$ind[[group]]$obs] #ind refers to full vector obs_c

              # for later calculations (=> log)
              #+restrict obs_c to positive entries
              obs_c <- obs_c[which(obs_c > 0)]

              # obj fun (to max)
              # from equation for shape k
              -(weights$W2[[group]] /
                k -
                sum(log(obs_c) * obs_c^k) / sum(obs_c^k) +
                mean(log(obs_evc)))^2 +
                # from equation for delay
                # it is the negative of what is in Cousineau, but it is more in line with the likelihood derivation)
                # (for what it's worth, mean(1/obs_evc) is inverse of harmonic mean)
                -(w3F(k) -
                  mean(1 / obs_evc) * sum(obs_c^k) / sum(obs_c^(k - 1)))^2
            },
            stop("This Surv-type is not supported here!", call. = FALSE)
          )
        } #esle !isSurv

        if (verbose > 1L) {
          cat(
            glue(
              "W1 = {round(weights$W1[[group]],2)}, ",
              "W2 = {round(weights$W2[[group]],2)}, ",
              "W3 = {round(w3F(k),4)} for {group}. ",
              "Candidate values: delay {round(pars.gr[[1L]],3)} shape {round(k,3)} ",
              "=> LLval: {round(rVal, 3)}"
            ),
            "\n"
          )
        } #fi

        rVal
      }, #MLEw

      MLEc = {
        # objective function to be maximized:
        # corrected MLE

        stopifnot(nObs >= 2L)
        # contribution of first observation is corrected for: we take first two different values
        ind12 <- indForefront[[group]]

        # MLEc profiling leads to difficult equation for scale
        #+(looks like Lambert W could be necessary but it is even more complicated)
        # add more test routines for MLEc

        if (!isSurv) {
          # numeric response, non-Surv
          length(ind12[["inds_obs1"]]) *
            logspace_sub2_cpp(rlang::exec(
              distO$cdf,
              !!!c(
                list(q = obs[c(1, ind12[["ind_next"]])], log.p = TRUE),
                pars.gr
              )
            )) +
            sum(rlang::exec(
              distO$pdf,
              !!!c(list(x = obs[-ind12[["inds_obs1"]]], log = TRUE), pars.gr)
            ))
        } else {
          switch(
            attr(obs, which = "type", exact = TRUE),
            right = {
              # we need at least two observed event times
              stopifnot(nObs - cens$n[[group]]["any"] >= 2L)

              # first event-time needs correction (even if right censorings are earlier as they do not mandate that delay comes before it)
              length(ind12[["inds_obs1"]]) *
                logspace_sub2_cpp(rlang::exec(
                  distO$cdf,
                  !!!c(
                    list(
                      q = obs[
                        c(ind12[["inds_obs1"]][1L], ind12[["ind_next"]]),
                        1L
                      ],
                      log.p = TRUE
                    ),
                    pars.gr
                  )
                )) +
                # remaining observed event times
                sum(
                  rlang::exec(
                    distO$pdf,
                    !!!c(
                      list(
                        x = obs[
                          setdiff(cens$ind[[group]]$obs, ind12[["inds_obs1"]]),
                          1L
                        ],
                        log = TRUE
                      ),
                      pars.gr
                    )
                  ),
                  # right-censored observations do not need correction
                  #+(as they are tail probabilities that do not peak so drastically as densities do)
                  rlang::exec(
                    distO$cdf,
                    !!!c(
                      list(
                        q = obs[cens$ind[[group]]$right, 1L],
                        lower.tail = FALSE,
                        log.p = TRUE
                      ),
                      pars.gr
                    )
                  )
                )
            },
            stop("This type of censoring is not supported!", call. = FALSE)
          )
        } #esle !isSurv
      },
      stop(
        glue("This calculation method {methodSelected} is not handled here!"),
        call. = FALSE
      )
    ) #hctiws

    # return:
    if (!isCrit) {
      # apply potential penalty term for high shape parameter if no specific criterion was requested
      #XXX What is the right penalty for MLEw (which has a objective function as square length of normal equation [=solution of 1st deriv set to 0])?
      #+ is it first deriv squared of penF wrt shape k?
      rVal - penF(k, nObs = nObs)
    } else {
      rVal
    }
  } #fn getLogLik

  # Log-spacings to be maximized
  #
  # Calculate the differences in EDF (for given parameters in group) of adjacent
  # observations on log scale The higher the sum of these log-spacings the more
  # evenly spread are the observations. These log-spacings are at the heart of
  # the MPSE-criterion which is the negative mean of these log-spacings. Moran's
  # test statistic is the negative sum of these log-spacings.
  # @param pars vector of parameters (by default, on transformed scale, i.e.
  #   when criterion = FALSE)
  # @param isOrig logical. Are the pars on original scale? Or transformed as
  #   used during optimization?
  # @param ties. how to handle ties. By default, use the tie-setting from
  #   objective function call.
  # @return n+1 cumulative diffs on log-scale (or single negative number in
  #   twoPhase when delay2 <= delay in quick fix)
  getCumDiffs <- function(pars, group, isOrig = FALSE, ties. = ties) {
    #access observations of group
    #obs <- rlang::env_get(env = rlang::env_parent(rlang::current_env(), n=1L),
    # nm = group, inherit = FALSE)
    #+or use env = rlang::fn_env(getCumDiffs) # (but requires function obj)
    obs <- if (group == "y") y else x # direct access by name

    # extract parameters for specified group on original scale (for CDF)
    pars.gr <- extractPars(
      pars,
      group = group,
      isOpt = !isOrig,
      transform = !isOrig
    )

    if (verbose > 1L) {
      cat(
        glue(
          "Parameter vector for group {group} on non-transformed scale: ",
          "{paste(round(pars.gr, 2), collapse = ', ')}"
        ),
        "\n"
      )
    }

    # calculate spacings
    # contract: data is sorted!
    cumDiffs <- if (!isSurv) {
      # numeric response (non-Surv)
      diff(c(0L, rlang::exec(distO$cdf, !!!c(list(q = obs), pars.gr)), 1L))
    } else {
      # Surv-response
      ind_evKM <- which(kmFit$n.event > 0.99) #at least one event (type="interval" makes that we get fractional numbers here [but 0 is 0 also for interval!?])

      # get the right subset of indices for specified group (when having two groups)
      if (twoGroup) {
        # Cave: works only for two groups (x or y) as I only use the strata[[1L]] as cutpoint
        ind_evKM <- if (group == "x") {
          ind_evKM[ind_evKM <= kmFit$strata[[1L]]]
        } else {
          ind_evKM[ind_evKM > kmFit$strata[[1L]]]
        }
      }
      # h: return object
      h <- rep_len(-1, length.out = length(obs))

      # n.event is generally not integer for type=interval/left.
      #+It is increased by a fraction (depending on number of events) and sums to nbr of events+1 (per group)
      # floor(n.event + n.censor) == n
      stopifnot(
        sum(
          as.integer(kmFit$n.event[ind_evKM]),
          if (twoGroup) {
            kmFit$n.censor[
              c(-1, 1)[[1L + (group == "x")]] * seq_len(kmFit$strata[[1L]])
            ]
          } else {
            kmFit$n.censor
          }
        ) ==
          kmFit$n[[if (group == "x") 1L else 2L]]
      )
      # use CDF for all observed event times of right-censored outcome variable
      h[obs[, "status"] == 1] <- rep.int(
        rlang::exec(distO$cdf, !!!c(list(q = kmFit$time[ind_evKM]), pars.gr)) *
          (cens$rcens$surv[ind_evKM]) +
          (1 - cens$rcens$surv[ind_evKM]),
        # n.event is not always integer for Surv-type=interval/left. rep.int truncates floats & it should always work.
        times = kmFit$n.event[ind_evKM]
      )
      # censored observations get interpolated values
      ind_hrcens <- which(h < 0)
      if (length(ind_hrcens)) {
        ind_hobs <- which(h > 0)
        # interpolate values for all censored observations
        h[ind_hrcens] <- stats::approx(
          x = c(0L, ind_hobs, length(obs) + 1L),
          y = c(0L, h[ind_hobs], 1L),
          method = "linear",
          # x-values are already ordered!
          ties = "ordered",
          yleft = NA,
          yright = NA,
          # values where to interpolate
          xout = ind_hrcens
        )$y
      } #fi hrcens

      diff(c(0L, h, 1L))
    } #esle !isSurv

    # check for ties to fix cumDiffs for observed event times
    # tig: tie info group
    tig <- tieInfo[[group]]
    nTigs <- NROW(tig[["tieGrp"]])

    if (nTigs > 0) {
      # all spacings for tied observed event times are 0
      stopifnot(all(cumDiffs[tig[["cumDiffInd"]]] == 0))

      obsVals <- if (!isSurv) {
        obs[tig$tieGrp[, "startInd"]]
      } else {
        obs[tig$tieGrp[, "startInd"], 1L]
      }

      cumDiffs[tig[["cumDiffInd"]]] <- switch(
        ties.,
        density = {
          # use density instead of diff of CDF for tied observation pairs
          rep.int(
            # take first observation per tie-group
            rlang::exec(distO$pdf, !!!c(list(x = obsVals), pars.gr)),
            #tig$tieGrp[, "len"]-1L # number of repeats per tie group
            times = tig$tieGrp[, "len"] - 1L
          )
        },
        # "equispaced" for CDF-backtransformed using given (fitted!) parameters,
        #+then equispaced spacings across tie groups
        # we keep using a standard density strategy for fitting,
        # but can request another tie-strategy for evaluating the MPSE-criterion
        # e.g., for Moran's test
        equispaced = {
          # per tie group, assume tied observations are maximally spread (within rounding radius).
          # Two reasons why this leads to bigger cumDiffs (=smaller criterion/Moran's test statistic = conservative)
          # 1/ for adjacent spacings (involving obs directly before and after tie) we have assumed the original tied observation
          # 2/ we use equal spacings in transformed space for all tied observation (within tie group)
          rep.int(
            diff(rlang::exec(
              distO$cdf,
              !!!c(
                list(
                  q = rep(obsVals, each = 2L) +
                    c(-1, 1) * tig$numPrecision[["rRad"]]
                ),
                pars.gr
              )
            ))[seq.int(from = 1, by = 2, length.out = nTigs)] /
              (tig$tieGrp[, "len"] - 1L),
            times = tig$tieGrp[, "len"] - 1L
          )
        },
        # for what it's worth: tie-strategy "error" should have already quit
        error = {
          stop("getCumDiffs: ties are not allowed!", call. = FALSE)
        },
        # handle exception
        stop(
          glue("Unknown strategy {ties.} to handle ties here."),
          call. = FALSE
        )
      )
    } #fi

    #XXX numerical stability: is diff() accurate? does log1p on 1-xx help?
    #think of log1p: log1p(cumDiffs-1) = log1p(-(1-cumDiffs))
    #and consider .Machine$double.neg.eps first:
    #cumDiffs[which(cumDiffs < .Machine$double.neg.eps)] <- .Machine$double.neg.eps
    #but log1p(-(1-.Machine$double.neg.eps)) = -36.7 while log(.Machine$double.xmin) = -708
    # respect the machine's numerical lower limit
    cumDiffs[which(cumDiffs < .Machine$double.xmin)] <- .Machine$double.xmin

    log(cumDiffs)
  } #fn getCumDiffs

  # Objective function to be minimized
  #
  # Depending on selected method, it is negative mean log-spacings for MPSE or negative log-likelihood for MLEn.
  # For MLEw, the optimization function is based on the squared length of gradient (=vector of partial 1st derivatives).
  # A minimal value 0 corresponds to candidate values for a local extremum.
  # One can estimate parameters by minimizing this objective function.
  #
  # @param `pars` the vector of parameters. transformed when criterion=FALSE and not transformed when criterion=TRUE
  # @param `isOrig` Are parameters on original scale? Or transformed for optimization?
  # @param criterion character. Which log-lik criterion or `NULL` (default) for calculation of optimization function with optional penalization for given method. (This argument is ignored for MPSE.)
  # @param `aggregated` logical. For two group case, `aggregated=FALSE` returns values per group, like mean log cum-diffs per group.
  # @param `maximum` logical. Should the objective function be maximized? Default value is `FALSE`: we minimize the objective function.
  # @param `ties.` How to handle ties for the MPSE-function? Default value is 'density'.
  # @return value of objective function (by default, to be minimized, like neg. log-likelihood)
  objFun <- function(
    pars,
    isOrig = FALSE,
    criterion = NULL,
    aggregated = TRUE,
    maximum = FALSE,
    ties. = ties
  ) {
    maximum <- isTRUE(maximum[1])

    # request a specific criterion?
    isCrit <- !is.null(criterion) &&
      is.character(criterion) &&
      nzchar(criterion[[1]])
    methodSelected <- if (isCrit) {
      # MLEw has no own likelihood function: use MLEc instead
      if (criterion[[1]] == "MLEw") "MLEc" else criterion[[1]]
    } else {
      # if no criterion is requested use optimization for inherent method
      method
    }

    if (verbose > 1) {
      cat("pars:", pars, "\n")
    }

    valToMax <- switch(
      methodSelected,
      MPSE = {
        if (!twoGroup) {
          mean(getCumDiffs(pars, group = "x", isOrig = isOrig, ties. = ties.))
        } else {
          local({
            #twoGroup:
            #the approach to first merge x and y and then do the cumDiffs, log and mean does *not* work out
            #because the parameters should be optimized within group.
            #merged data lead to frequent non-convergence or visually bad fits
            res0 <- c(
              mean(getCumDiffs(
                pars,
                group = "x",
                isOrig = isOrig,
                ties. = ties.
              )),
              mean(getCumDiffs(
                pars,
                group = "y",
                isOrig = isOrig,
                ties. = ties.
              ))
            )

            if (aggregated) {
              stats::weighted.mean(res0, w = c(length(x), length(y)))
            } else {
              res0
            }
          })
        }
      },
      MLEn = ,
      MLEw = ,
      MLEc = {
        stopifnot(!twoPhase) #XXX not implemented yet!

        if (!twoGroup) {
          getLogLik(
            pars,
            group = "x",
            isOrig = isOrig,
            methodSelected = methodSelected,
            isCrit = isCrit
          )
        } else {
          local({
            res0 <- c(
              getLogLik(
                pars,
                group = "x",
                isOrig = isOrig,
                methodSelected = methodSelected,
                isCrit = isCrit
              ),
              getLogLik(
                pars,
                group = "y",
                isOrig = isOrig,
                methodSelected = methodSelected,
                isCrit = isCrit
              )
            )

            #XXX think here: can we use sum of log-lik from two groups in case of derivative-based solutions (MLEw, min)
            if (aggregated) sum(res0) else res0
          })
        }
      },
      # default
      stop(
        glue(
          "Objective function for method {methodSelected} is not implemented!"
        ),
        call. = FALSE
      )
    ) #hctiws

    # switch sign !maximum => -valToMax
    rVal <- (-1 + 2 * maximum) * valToMax
    if (verbose > 2) {
      cat(
        "Objfun value ",
        if (maximum) "(to be maximized):" else ":",
        rVal,
        "\n"
      )
    }

    # return
    if (isCrit) {
      # criterion gets named ##&& aggregated only if single number??
      rlang::set_names(rVal, nm = paste0(if (!maximum) "neg. ", methodSelected))
    } else {
      rVal
    }
  } #fn objFun

  if (
    method == 'MLEn' &&
      !twoGroup &&
      !twoPhase &&
      distO$dist == "exponential" &&
      !isSurv
  ) {
    # attach analytical solution for MLE as attribute "opt"
    attr(objFun, which = "opt") <- local({
      par_analytic <- c(delay1 = x[[1L]], rate1 = 1L / (mean(x) - x[[1L]]))
      list(
        par_orig = par_analytic,
        #transformed parameters
        par = extractPars(
          par_analytic,
          isOpt = FALSE,
          transform = TRUE,
          named = TRUE
        ),
        value = length(x) * (log(mean(x) - x[[1L]]) + 1L),
        methodOpt = "analytic",
        convergence = 0L,
        message = "analytic minimizer solution for naive MLE ('MLEn')",
        counts = 0L
      )
    })
  } #fi

  objFun
} #fn objFunFactory


#' Fit optimal parameters according to the objective function (either MPSE or MLE-based)
#'
#' The objective function carries the given data in its environment and it is to be minimized.
#' R's standard routine `stats::optim` does the numerical optimization, using numerical derivatives.
#'
#' When the analytical solution is available (as attribute `opt` to the objective function), it is returned directly.
#' @param objFun objective function to be minimized
#' @param optim_args list of own arguments for optimization. If `NULL` it uses the default optim arguments associated to the objective function.
#' @param verbose integer that indicates the level of verboseness. Default 0 is quiet.
#' @return optimization object including a named parameter vector or `NULL` in case of errors during optimization
delay_fit <- function(objFun, optim_args = NULL, verbose = 0) {
  if (is.null(objFun)) {
    return(invisible(NULL))
  }
  stopifnot(is.function(objFun))
  objFunEnv <- rlang::fn_env(objFun)

  # gather information from objective function environment
  objFunObjs <- rlang::env_get_list(
    env = objFunEnv,
    nms = c(
      "bind",
      "method",
      "optim_args",
      "control",
      "trNamesFull",
      "profiled",
      "twoGroup",
      "twoPhase",
      "x",
      "y",
      "extractPars",
      "oNames"
    )
  )

  # check if there is already a solution provided by the objective function
  optObj <- attr(objFun, which = "opt", exact = TRUE)

  if (
    is.list(optObj) &&
      all(c("par", "par_orig", "value", "convergence") %in% names(optObj))
  ) {
    if (verbose > 0L) {
      message("Using provided (analytical) solution to objective function.")
    }
  } else {
    optObj <- NULL #start from scratch
    # numeric optimization
    if (verbose > 0L) {
      message("Start with numeric optimiziation of objective function.")
    }

    # ensure optim-args is set
    optim_args <- optim_args %||% objFunObjs[["optim_args"]]

    stopifnot(is.list(optim_args))
    stopifnot(
      "par" %in% names(optim_args),
      is.numeric(optim_args$par),
      length(optim_args$par) == length(objFunObjs$trNamesFull)
    )
    # ensure that transformed parameters are named
    if (!rlang::is_named(optim_args$par)) {
      rlang::names2(optim_args$par) <- objFunObjs$trNamesFull
    }
    stopifnot(identical(names(optim_args$par), objFunObjs$trNamesFull))

    # set objective function (overwrite entry 'fn' if it is already present)
    optim_args[["fn"]] <- objFun

    # optimization: first attempt ----

    # initial start values for optimization
    par0 <- optim_args$par

    try(
      {
        optObj <- rlang::exec(stats::optim, !!!optim_args)
        optObj$methodOpt <- optim_args$method
      },
      silent = TRUE
    )

    #XXX continue here: MLEw: where to do check of 2nd deriv

    if (is.null(optObj)) {
      if (verbose > 0L) {
        warning(
          glue("{objFunObjs$method}-optimization failed during model fit!"),
          call. = FALSE
        )
      } #fi
    } else if (isTRUE(optObj$convergence > 0L)) {
      # do a 2nd attempt of optim in case it did not converge in the first place
      if (verbose > 1L) {
        message(
          "No proper convergence during 1st optimization in delay fit. Re-try with different parameter scaling."
        )
      } #fi

      # Use parameter values of non-converged fit as new start values (and adapt parscale accordingly)
      #+The objFun is to be minimized, smaller is better!
      if (
        isTRUE(
          is.numeric(optObj$par) &&
            all(is.finite(optObj$par)) &&
            optObj$value < objFun(par0)
        )
      ) {
        if (verbose > 1L) {
          cat("Set new start values for 2nd attempt\n")
        }
        optim_args[["par"]] <- optObj$par # purrr::assign_in(where = "par", value = optObj$par)

        if ("parscale" %in% names(optim_args[["control"]])) {
          optim_args[["control"]][["parscale"]] <- scalePars(optim_args[[
            "par"
          ]])
        } #fi

        # optim: 2nd attempt --
        optObj <- NULL
        if (verbose > 1L) {
          message(
            "Do 2nd attempt with renewed start values and parameter scaling"
          )
        } #fi

        try(
          {
            optObj <- rlang::exec(stats::optim, !!!optim_args)
            optObj$methodOpt <- optim_args$method
          },
          silent = TRUE
        )

        if (
          verbose > 0L && (is.null(optObj) || isTRUE(optObj$convergence > 0L))
        ) {
          warning("No proper convergence after re-try.", call. = FALSE)
        }
      } ## fi rescaling for 2nd attempt
    } ## fi 2nd attempt necessary?

    # optimization: last attempt (alternative method) ----

    if (is.null(optObj) || optObj$convergence > 0L) {
      if (verbose > 0L) {
        cat("Do another final attempt with alternative optimizer.\n")
      } #fi

      # choose best start values for alternative:
      # if there are shape parameters, go for start value that is reasonably small
      par1 <- local({
        shapeInd <- which(startsWith(names(par0), prefix = "shape"))
        keep0 <- length(shapeInd) &&
          sum(pmax.int(par0[shapeInd] - 2, 0)^2) <
            sum(pmax.int(optim_args$par[shapeInd] - 2, 0)^2)
        if (keep0) {
          if (verbose > 1) {
            cat("Keep initial start parameters for final optimizer attempt.\n")
          } #fi
          par0
        } else {
          if (verbose > 1) {
            cat("Use updated start parameters for final optimizer attempt.\n")
          } #fi
          optim_args$par
        } #esle
      })

      optim_args$par <- par1 #update optim_args
      optObj <- minObjFunAlt(
        objFun = objFun,
        start = optim_args$par,
        lower = optim_args$lower,
        upper = optim_args$upper,
        verbose = verbose,
        method = "bobyqa"
      )
    } #fi

    # post-process optObj -----

    # set names to parameter vector
    if (!is.null(optObj)) {
      stopifnot("par" %in% names(optObj))
      stopifnot(
        is.numeric(optObj$par),
        length(optObj$par) == length(objFunObjs$trNamesFull)
      )
      if (!rlang::is_named(optObj$par)) {
        names(optObj$par) <- objFunObjs$trNamesFull
      }

      # save optim_args in optimization object (but w/o objective function)
      optim_args$fn <- NULL
      optObj <- append(optObj, values = list(optim_args = optim_args))
    } #fi

    # add par_orig
    optObj <- append(
      optObj,
      values = list(
        par_orig = objFunObjs$extractPars(
          parV = optObj$par,
          group = NULL,
          isOpt = TRUE,
          transform = TRUE,
          named = TRUE
        )
      )
    )
  } #esle numeric optimization

  optObj
}


#' Build control list for [delay_model()] and [objFunFactory()]
#'
#' Default values are set as default argument values in this constructor.
#'
#' This is an internal function. The function might change without precautionary measures taken.
#' @param verbose numeric. Degree of verbosity. Default value 0 means no extra output.
#' @param profiled logical. Use objective function based on parameters that are profiled out?
#' @param pen_shape logical. Should we penalize high shape parameters?
#' @param MLEw_weight character. How to derive the weights?
#' @param MLEw_optim character. How to find the optimum? Currently only "min" for minimization is supported.
#' @param ties character. How to handle tied observations?
#' @returns list. Control settings for fitting routine `delay_model`
buildControl <- function(
  verbose = 0,
  profiled,
  pen_shape = FALSE,
  MLEw_weight = "sdist_median",
  MLEw_optim = "min", #c("min", "root"),
  ties = "density"
) {
  #MLEw_weight used to depend on surv-type of data, but this is not known here nor in delay_model (only within objFunFactory)
  #+was before: MLEw_weight = if (isSurv) "sample" else "sdist_median"

  #MLEw_optim <- match.arg(MLEw_optim)

  # default control-settings
  list(
    verbose = verbose[1],
    profiled = profiled[1],
    #pen_shape = FALSE,
    pen_shape = pen_shape[1],
    MLEw_weight = MLEw_weight[1],
    MLEw_optim = MLEw_optim[1],
    ties = ties[1]
  )
} #fn buildControl


#' Fit a delayed Exponential or Weibull model to one or two given sample(s)
#'
#' Maximum product of spacings estimation is used by default to fit the
#' parameters. Estimation via naive maximum likelihood (`method = "MLEn"`) is
#' available, too, but MLEn yields severely biased estimates for small samples.
#' MLEc is a corrected version of MLEn due to Cheng and Iles (1987).
#'
#' @details The parameter `control=` allows to set specifics of the optimization
#' process of the delay model fit. Possible list entries are
#'
#' * `verbose` level of verboseness. Default 0 is quiet
#' * `ties` character. Strategy to handle ties for `method = "MPSE"`. Either 'density' (default), 'equispaced' or 'error'.
#' * `profiled` aim to profile out a parameter
#' * `pen_shape` logical. Should high values of shape (for Weibull distribution) be penalized? Default is FALSE.
#' * `MLEw_weight` character. Name of method to build weights for MLEw-method.
#' * `MLEw_optim` character. Name for strategy to find extremum: either minimization of L2-norm of 1st partial derivatives (as stated in Cousineau, 2009) or using root-finding
#'
#' Numerical minimization is normally done by `stats::optim`. If this
#' minimization attempt fails `minqa::bobyqa` is used as fall-back. For MLEw, we
#' can also use root finding of gradient function instead of minimization of
#' L2-norm of gradient of MLEw-objective function.
#'
#' @param x numeric. observations of 1st group. Can also be a list of data from
#'   two groups.
#' @param y numeric. observations from 2nd group
#' @param distribution Which delayed distribution is assumed? Exponential or
#'   Weibull. Can be given as character or as distribution list-object.
#' @param twoPhase logical. Allow for two phases?
#' @param bind character. parameter names that are bound together in 2-group
#'   situation.
#' @param method character. Which method to fit the model? 'MPSE' = maximum
#'   product of spacings estimation *or* 'MLEn' = naive maximum likelihood
#'   estimation *or* 'MLEw' = weighted MLE' *or* MLEc' = corrected MLE
#' @param control list. Details that control the optimization. E.g., profiling,
#'   penalization.
#' @returns `incubate_fit` the delay-model fit object with criterion to minimize.
#'   Or `NULL` if optimization failed (e.g. too few observations).
#' @references R. C. H. Cheng, T. C. Iles, Corrected Maximum Likelihood in
#'   Non-Regular Problems, Journal of the Royal Statistical Society: Series B
#'   (Methodological), Volume 49, Issue 1, September 1987, pp. 95–101,
#'   \doi{10.1111/j.2517-6161.1987.tb01428.x}
#' @export
delay_model <- function(
  x = stop("Specify observations for first group x=!", call. = FALSE),
  y = NULL,
  distribution = c("exponential", "weibull", "normal"),
  twoPhase = FALSE,
  bind = NULL,
  method = c("MPSE", "MLEn", "MLEw", "MLEc"),
  control = list()
) {
  # setup -------------------------------------------------------------------

  # unpack x if it is a list of two vectors
  if (is.list(x)) {
    if (length(x) != 2L) {
      stop("If x= is given a list it must be of size 2.", call. = FALSE)
    }
    y <- x[[2L]]
    x <- x[[1L]]
  } #fi

  # enforce that the first argument x= is properly instantiated
  stopifnot(!is.null(x), is.numeric(x), length(x) > 0)

  distO <- switch(
    mode(distribution),
    list = {
      stopifnot(all(
        c("dist", "dist_name", "pdf", "random") %in% names(distribution)
      ))
      distribution
    },
    character = {
      distribution <- match.arg(distribution)
      buildDist(distribution)
    },
    stop("Argument to distribuiton= not supported here!", call. = FALSE)
  )

  method <- if (length(method) == 1L && toupper(method) == "MSE") {
    message(
      "The method name 'MPSE' is prefered over the previously used name 'MSE'!"
    )
    "MPSE"
  } else {
    method[[1L]]
  }
  method <- match.arg(method)

  # build up default control-settings for current situation
  # we used to set weighting method depending on isSurv or not! (determined in objFunFactory)
  cntrl <- buildControl(
    profiled = distO$dist != "normal" && method != "MPSE",
    # penalize shape parameter?
    pen_shape = distO$dist == "weibull" && method == "MLEw",
    MLEw_optim = "min"
  )

  # overwrite control settings as given by control=
  stopifnot(is.list(control))
  controlNms <- names(control)

  local({
    if (length(badNms <- controlNms[!controlNms %in% names(cntrl)])) {
      warning(
        "Unknown names in given control list: ",
        paste0(badNms, "=", collapse = ", "),
        call. = FALSE
      )
    }
  })
  cntrl[controlNms] <- control

  # check control arguments ---

  if (is.logical(cntrl$verbose)) {
    cntrl$verbose <- as.numeric(cntrl$verbose)
  }
  if (
    is.null(cntrl$verbose) ||
      !is.numeric(cntrl$verbose) ||
      !is.finite(cntrl$verbose)
  ) {
    cntrl$verbose <- 0L
  }
  cntrl$verbose <- cntrl$verbose[[1L]]
  cntrl$ties <- match.arg(
    cntrl$ties,
    choices = c('density', 'equispaced', 'error')
  )

  if (is.character(bind)) {
    if (any(endsWith(bind, suffix = "_tr"))) {
      stop(
        "Parameter names to bind= refer to the distribution parameters and not to transformed parameters of the objective function.",
        call. = FALSE
      )
    }

    # translate convenience names (for single phase) to canonical names
    if (distO$twoPhaseAllowed) {
      unNmbrdIdx <- !grepl(pattern = "[12]", bind, fixed = FALSE)
      if (any(unNmbrdIdx)) {
        bind[unNmbrdIdx] <- paste0(bind[unNmbrdIdx], "1") #interpret un-numbered parameters as referring to phase 1
        if (cntrl$verbose > 0L) {
          cat(
            "The unnumbered parameter names in bind= are translated to canonical parameter names (=phase 1).\n"
          )
        }
      }
    } #fi twoPhaseAllowed
  } #fi bind=

  # objective function ------------------------------------------------------

  objFun <- objFunFactory(
    x = x,
    y = y,
    distO = distO,
    method = method,
    twoPhase = twoPhase,
    bind = bind,
    control = cntrl
  )
  if (is.null(objFun)) {
    return(invisible(NULL))
  }
  objFunEnv <- rlang::fn_env(objFun)

  # update cntrl settings: profiled was set to FALSE (e.g., when isSurv)
  # [alternative: read out control from objFunEnv and overwrite cntrl]
  cntrl$profiled <- rlang::env_get(env = objFunEnv, nm = "profiled")

  # optimise objective function
  optObj <- delay_fit(objFun, optim_args = NULL, verbose = cntrl$verbose)

  if (is.null(optObj) || is.null(optObj$par_orig)) {
    return(invisible(NULL))
  }

  # return -----
  twoGroup <- rlang::env_get(env = objFunEnv, nm = "twoGroup")
  # overwrite data with  pre-processed data
  x <- rlang::env_get(env = objFunEnv, nm = "x")
  y <- rlang::env_get(env = objFunEnv, nm = "y", default = NULL)

  # /!\ keep in sync with update()!
  structure(
    list(
      data = if (twoGroup) list(x = x, y = y) else x,
      nobs = c(x = NROW(x), y = if (twoGroup) NROW(y) else 0),
      distO = distO,
      twoPhase = twoPhase,
      twoGroup = twoGroup,
      method = method,
      bind = rlang::env_get(env = objFunEnv, nm = "bind"),
      ties = cntrl$ties,
      #isSurv = rlang::env_get(env = objFunEnv, nm = "isSurv"),
      cens = rlang::env_get(env = objFunEnv, nm = "cens", default = 0L), ##if (twoGroup)
      kmFit = rlang::env_get(env = objFunEnv, nm = "kmFit", default = NULL),
      objFun = objFun,
      par = optObj$par_orig,
      criterion = objFun(
        pars = optObj$par_orig,
        isOrig = TRUE,
        criterion = method,
        aggregated = TRUE,
        maximum = FALSE
      ),
      optimizer = purrr::compact(c(
        list(
          parOpt = optObj$par,
          valOpt = optObj$value,
          profiled = cntrl$profiled
        ),
        optObj[c("methodOpt", 'convergence', 'message', 'counts', 'optim_args')]
      ))
    ),
    class = "incubate_fit"
  )
}


#' Refit an `incubate_fit`-object with specified optimization arguments
#'
#' This function is useful when only an optimization argument is to be changed.
#' If more things need to be changed go back to `delay_model` and start from scratch.
#' @param object `incubate_fit`-object
#' @param optim_args optimization arguments
#' @param verbose integer flag. Requested verbosity during `delay_fit`
#' @param ... further arguments, currently not used.
#' @return The updated fitted object of class `incubate_fit` or `NULL` in case of failure.
#' @export
update.incubate_fit <- function(object, optim_args = NULL, verbose = 0, ...) {
  stopifnot(all(
    c(
      "data",
      "distO",
      "method",
      "objFun",
      "twoPhase",
      "twoGroup",
      "par",
      "criterion",
      "optimizer"
    ) %in%
      names(object)
  ))

  ## fit model with given optim_args
  objFun <- object[["objFun"]]
  optObj <- delay_fit(objFun, optim_args = optim_args, verbose = verbose)

  if (is.null(optObj)) {
    return(invisible(NULL))
  }

  # update all relevant fields in the list
  # /!\ keep in sync with delay_model() /!\
  object[c("par", "criterion", "optimizer")] <- list(
    par = optObj$par_orig,
    criterion = objFun(
      pars = optObj$par_orig,
      isOrig = TRUE,
      criterion = object$method,
      aggregated = TRUE,
      maximum = FALSE
    ),
    # drop NULLs from list (e.g. if optim_args is not present)
    optimizer = purrr::compact(c(
      list(
        parOpt = optObj$par,
        valOpt = optObj$value,
        profiled = object$optimizer$profiled
      ),
      optObj[c("methodOpt", "convergence", "message", "counts", "optim_args")]
    ))
  )

  object
}


#' @export
simulate.incubate_fit <- function(object, nsim = 1, seed = NULL, ...) {
  stopifnot(inherits(object, "incubate_fit"))

  ranFun <- object$distO$random

  #XXX add option to mirror cens= setting in observed data?
  # arguments to the random function generation
  ranFunArgsX <- as.list(c(n = object$nobs[[1L]], coef(object, group = "x")))
  ranFunArgsY <- if (object$twoGroup) {
    as.list(c(n = object$nobs[[2L]], coef(object, group = "y")))
  }

  simExpr <- if (object$twoGroup) {
    expression(list(
      x = rlang::exec(ranFun, !!!ranFunArgsX),
      y = rlang::exec(ranFun, !!!ranFunArgsY)
    ))
  } else {
    expression(rlang::exec(ranFun, !!!ranFunArgsX))
  }

  if (nsim > 1000L) {
    future.apply::future_replicate(
      n = nsim,
      expr = eval(simExpr),
      simplify = FALSE,
      future.seed = TRUE
    )
  } else {
    replicate(n = nsim, expr = eval(simExpr), simplify = FALSE)
  }
} #fn simulate


#' Generate bootstrap distribution of model parameters to a fitted incubate model
#'
#' Given a fitted incubate model, this function generates bootstrap samples to estimate the variability of the model parameters.
#' The bootstrap data is used to make bootstrap inference in the second step.
#' It is an internal function, the main entry point is [confint.incubate_fit()].
#'
#' The bootstrap data can be generated by two methods:
#'
#' 1. parametric bootstrap, new data are generated from the fitted model and then the model is refitted to these data.
#' 2. ordinary bootstrap, new data are generated by resampling with replacement from the original data and then the model is refitted to these data.
#'
#' For the initial delay parameter we allow to smooth the delay estimate in parametric bootstrap. The initial delay parameter is special because this parameter determines when observations can start to occur.
#' Bootstrapping from a delay model will only produce data that starts never before the given delay. Smoothing adds variability here.
#' The default value for smoothing is `smd_factor=1` which means to add normal noise of 1xstd.dev of the first observation to the first observation.
#' Alternatively, when using the objective function to find a region for delay1 (this branch is turned off in the code, cf. USE_OBJFUN hard-coded), the default 1 was an optimal value in a simulation for log-quantile together with log delay-shift = 5.
#'
#' @param object an `incubate_fit`-object
#' @param bs_data character. Which type of bootstrap method to generate data?
#' @param R integer. Number of bootstrapped model coefficient estimates
#' @param useBoot flag. Do you want to use the boot-package? Default value is `FALSE`.
#' @param smd_factor numeric. smooth delay factor for initial delay that influences the amount of smoothing. 0 means no smoothing at all. Default is 1. Higher values mean more variability in the delay parameter during bootstrap data generation.
#' @returns bootstrap data, either as matrix or of class `boot` (depending on the `useBoot`-flag)
bsDataStep <- function(
  object,
  bs_data = c('parametric', 'ordinary'),
  R,
  useBoot = FALSE,
  smd_factor = 1
) {
  bs_data <- match.arg(bs_data)
  stopifnot(is.numeric(R), length(R) == 1L, R > 1L)
  R <- ceiling(R)
  useBoot <- isTRUE(useBoot)
  ranFun <- object$distO$random
  dFun <- object$distO$pdf
  twoGroup <- isTRUE(object$twoGroup)
  nObs <- object$nobs
  # full untransformed parameter vector
  coefVect <- coef.incubate_fit(object, group = NULL, transformed = FALSE)
  ncoef <- length(coefVect)
  stopifnot(ncoef > 0L)
  # indices of coefficients that involve delay1, e.g. 'delay1' or 'delay1.y'
  del1_ind <- grep('delay1', names(coefVect), fixed = TRUE)

  stopifnot(is.numeric(smd_factor), length(smd_factor) == 1L, smd_factor >= 0L)
  smoothDelay <- isTRUE(smd_factor > 0L)

  # delay smoothing only for parametric bootstrap
  if (smoothDelay && bs_data != 'parametric') {
    smoothDelay <- FALSE
    smd_factor <- 0L
  } #fi

  # smooth first delay parameter during parametric bootstrap data generation
  # two approaches to do this:
  # 1/ use the estimated SD for the first observation of the data (as determined by the model parameters. we use order statistics theory) and add normal noise with this sd to the estimated delay parameter.
  # this reflects the variability in the data that directly affects the delay estimate
  # 2/ sample delay values according to objective function (where delay is varied and other parameters are kept fixed) in the vicinity of the estimated first delay
  # This reflects the certainty we have in the initial delay estimation.
  # Low variability in event time data (or high sample size) will lead to a quickly deteriorating objective function.
  # @returns vector of length R with candidate values for initial delay parameter
  getSMDCandidates <- function(group = 'x') {
    # flag: which way to do the smoothing of delay parameter?
    # USE_OBJFUN = TRUE means: use the objective function to weight the region around the delay estimate
    # USE_OBJFUN = FALSE means: add normal noise with estimated SD for minimum observation to delay estimate
    USE_OBJFUN <- FALSE

    obs <- if (twoGroup) object$data[[group]] else object$data

    del_coef <- coef.incubate_fit(
      object,
      transformed = FALSE,
      group = group
    )[[
      'delay1'
    ]]

    retV <- if (!USE_OBJFUN) {
      # add normal noise with estimated SD to delay estimate
      predSD <- smd_factor *
        sqrt(getVariance(object, group = group, type = "min"))
      stopifnot(
        is.numeric(predSD),
        length(predSD) == 1L,
        is.finite(predSD),
        predSD > 0
      )

      # return candidate values for delay1 parameter
      pmax.int(
        0,
        stats::rnorm(n = R, mean = del_coef, sd = predSD)
      )
    } else {
      # use objective function to sample candidate values for delay1 parameter
      stopifnot(
        `Smoothing of delay parameter w/ objective function is currently not implemented for survival data!` = !object$cens$isSurv
      )
      stopifnot(is.numeric(obs), length(obs) > 0L)
      # XXX obs of type Surv breaks here because for Surv, obs is a 2-column matrix
      obs1 <- obs[[1L]]

      # avoid smoothing if 1st observation or estimated delay is too close to zer0
      if (min(obs1, del_coef) < TOL_NUM) {
        return(rep_len(del_coef, length.out = R))
      }

      stopifnot(is.function(object$objFun))

      groupIdx <- 1L + (twoGroup && group == 'y')
      # in case of a delay per group ('delay.x' and 'delay.y') use the correct one
      if (length(del1_ind) > 1L) {
        del1_ind <- del1_ind[[groupIdx]]
      }

      # look at smallest differences within first (at most 23 obs) observations
      obs_d <- diff(obs[seq_len(min(23L, nObs[[groupIdx]]))])
      obs_d <- obs_d[is.finite(obs_d) & obs_d > 0] #get rid of ties
      obs_d <- if (!length(obs_d)) .0001 else min(obs_d)

      # candidate region for delay parameters
      # min(..) ensures that we are not too close at obs1, otherwise for MLE we have only a single point
      # del_coef - (obs1 - del_coef) = 2 * del_coef - obs1
      del_interv <- c(
        low = max(
          0,
          min(
            del_coef - (obs1 - del_coef),
            del_coef - obs_d,
            obs1 - .0001,
            obs1 * .9999,
            na.rm = TRUE
          )
        ),
        high = obs1
      )

      # areas for delay with high values of objective function are more likely to be sampled
      # candidate region: symmetric around coef_del as midpoint, up to smallest observed value
      # candidate region becomes finer sampled the broader the interval is
      # point estimate for delay is part of sample (if lower bound is not cut to be 0, via max in from= argument)
      delayCandDF <- tibble(
        delay1 = seq.int(
          from = del_interv[['low']],
          to = del_interv[['high']],
          # uneven number of grid points (hence, MPSE-estimate for delay will be one of the grid points)
          # grid step width at most 0.005
          length.out = max(
            997L,
            2 * min(ceiling(R / 2), 100 * ceiling(diff(del_interv))) + 1L
          )
        ),
        # fixing all parameter estimates other than delay1
        objVal = purrr::map_dbl(
          .x = .data[["delay1"]],
          # objective function with delay1-entries a little bit varied
          # use `isOrig = TRUE` to operate directly on the original parameters
          # del1_ind: delay1-index within group
          .f = ~ object$objFun(
            pars = replace(coefVect, del1_ind, .x),
            isOrig = TRUE,
            aggregated = FALSE
          )[[groupIdx]]
        )
      )

      # we like to drop last entry (delay = 1st observation) as objective function tends to explode
      # but we have to keep last entry if it corresponds to the delay estimate (e.g. as is the case for MLEn-fitting)
      if (delayCandDF$delay1[NROW(delayCandDF)] > del_coef) {
        delayCandDF <- delayCandDF[-NROW(delayCandDF), , drop = FALSE]
      }
      # relative change to optimal value, will be negative as objective function is minimized
      delayCandDF$objValInv <- (object$criterion - delayCandDF$objVal) /
        (object$criterion + .01)
      # shift upwards into non-negative area
      delayCandDF$objValInv <- delayCandDF$objValInv -
        min(delayCandDF$objValInv, na.rm = TRUE)
      # scale to be between 0 and 1:
      # small smd_factor => high exponent => peaked distribution
      delayCandDF$objValInv <- (delayCandDF$objValInv /
        (max(delayCandDF$objValInv, na.rm = TRUE) + .001))^(1 /
        (smd_factor / 4 + .01))
      delayCandDF$cumSum0 <- cumsum(delayCandDF$objValInv)
      # scale cumSum0 to 1.
      delayCandDF$cumSum <- delayCandDF$cumSum0 / max(delayCandDF$cumSum0)
      # lag-1: have it start with 0 and end with a single 1 (the last cumSum is most often 0 as largest delay value has typically objValInv = 0)
      delayCandDF$cumSum <- c(0L, delayCandDF$cumSum[-NROW(delayCandDF)])

      # draw R delay1-values from delayCandDF according to their objValInv-weights
      # rightmost.closed = TRUE for the unlikely (impossible?!) case that we draw a 1 by runif
      delayCandDF$delay1[findInterval(
        x = stats::runif(R),
        vec = delayCandDF$cumSum,
        rightmost.closed = TRUE
      )]
    }
    retV
  } #fn getSMDCandidates

  delayCandX <- if (smoothDelay) getSMDCandidates(group = 'x')
  delayCandY <- if (smoothDelay && twoGroup) getSMDCandidates(group = 'y')

  if (useBoot) {
    stopifnot(!twoGroup) # for the time being only single group calls are supported!
    boot::boot(
      data = object$data,
      # get coefficients from bootstrapped data
      statistic = function(d, i) {
        coef(
          delay_model(
            x = d[i],
            distribution = object$distO,
            twoPhase = object$twoPhase,
            method = object$method,
            bind = object$bind,
            control = buildControl(
              profiled = object$optimizer$profiled,
              ties = object$ties
            )
          ),
          transformed = FALSE
        )
      },
      sim = bs_data,
      mle = coef(object),
      R = R,
      # generate data from fitted model (only for parametric bootstrap)
      ran.gen = function(d, coe) {
        # ran.gen function is only used for parametric bootstrap
        if (smoothDelay) {
          coe[['delay1']] <- delayCandX[sample.int(n = R, size = 1L)]
        }
        rlang::exec(ranFun, !!!as.list(c(n = nObs[[1L]], coe)))
        #XXX how to handle right-censoring during bootstrap?
      }
    )
  } else {
    # no boot-library
    # own implementation: we inline data generation (simulate) and model fitting in one function
    # get coefficients from bootstrapped data
    #+(either by ordinary bootstrap of data or by parametric bootstrap)
    coefBSFun <- switch(
      bs_data,
      ordinary = function(dummy) {
        # draw bootstrap samples from the data
        x <- (if (twoGroup) object$data$x else object$data)[sample.int(
          n = nObs[[1L]],
          replace = TRUE
        )]
        y <- if (twoGroup) {
          object$data$y[sample.int(n = nObs[[2L]], replace = TRUE)]
        }

        #XXX how to handle right-censoring during ordinary bootstrap?
        retVec <- rep.int(NA_real_, times = ncoef)
        dm <- NULL
        try(
          dm <- suppressWarnings(delay_model(
            x = x,
            y = y,
            distribution = object$distO,
            twoPhase = object$twoPhase,
            method = object$method,
            bind = object$bind,
            control = buildControl(
              profiled = object$optimizer$profiled,
              ties = object$ties
            )
          )),
          silent = TRUE
        )

        if (!is.null(dm) && inherits(dm, "incubate_fit")) {
          retVec <- coef.incubate_fit(dm, transformed = FALSE)
        } #fi

        retVec
      },
      parametric = {
        # generate data from the fitted model
        # for performance reasons, we 'inline' the simulate code, cf. test_diff

        # arguments to the random function generation
        ranFunArgsX <- as.list(c(
          n = nObs[[1L]],
          coef.incubate_fit(object, transformed = FALSE, group = "x")
        ))
        ranFunArgsY <- if (twoGroup) {
          as.list(c(
            n = nObs[[2L]],
            coef.incubate_fit(object, transformed = FALSE, group = "y")
          ))
        }

        function(ind) {
          if (smoothDelay) {
            # smooth delay according to how sure are we about the delay-estimate:
            # the more sure the smaller is the smoothing
            ranFunArgsX[['delay1']] <- delayCandX[ind]
            if (twoGroup) ranFunArgsY[['delay1']] <- delayCandY[ind]
          }

          # cf. simulate (but inlined here for performance reasons)
          x <- rlang::exec(ranFun, !!!ranFunArgsX)
          y <- if (twoGroup) rlang::exec(ranFun, !!!ranFunArgsY)

          # we do not cap bootstrap data at latest observed data because we trust the fitted model
          #XXX random-right censoring: estimate censoring distribution and apply censoring to simulated data (whatever comes first: event or censoring)
          #+ idea: enhance ranFun to optionally accept a censoring (weibull) model, and use this here, censoring dist learned from the sample
          #XXX administrative (type 1) censoring would be simpler: we would use pmin(simulated_data, censoring_time)
          retVec <- rep.int(NA_real_, times = ncoef)
          dm <- NULL
          try(
            dm <- suppressWarnings(delay_model(
              x = x,
              y = y,
              distribution = object$distO,
              twoPhase = object$twoPhase,
              method = object$method,
              bind = object$bind,
              control = buildControl(
                profiled = object$optimizer$profiled,
                ties = object$ties
              )
            )),
            silent = TRUE
          )

          if (!is.null(dm) && inherits(dm, "incubate_fit")) {
            retVec <- coef.incubate_fit(dm, transformed = FALSE)
          } #fi

          retVec
        }
      },
      stop('Unknown bootstrap data generation type!', call. = FALSE)
    )

    # add originally fitted coefficients as first column!
    retM <- cbind(
      coef(object),
      future.apply::future_vapply(
        X = seq_len(R),
        FUN.VALUE = numeric(length = ncoef),
        FUN = coefBSFun,
        future.seed = TRUE
      )
    )

    # drop columns that contain NA-values (as bootstrap coefficient estimates)
    retM <- retM[, !.colSums(!is.finite(retM), m = ncoef, n = R + 1L)]

    # return at most R columns
    retM[, seq_len(min(R, NCOL(retM)))]

    # more clear and shorter but less efficient!
    # future.apply::future_vapply(simulate(object, nsim = R), FUN.VALUE = numeric(length(cf)),
    #  FUN = \(d) coef(delay_model(x=d, distribution = object$distO, control = list(ties = object$ties), method = object$method, bind = object$bind)))
  } # esle !boot
}

#' Confidence intervals for parameters of delay model fit
#'
#' Bias-corrected bootstrap confidence limits (either based on quantile or normal approximation) are generated.
#' Optionally, there are also variants that use a log-transformation first.
#' Ordinary or parametric bootstrap are supported.
#' At least R=1000 bootstrap replications are recommended. Default are quantile-based confidence intervals that internally use a log transformation.
#' @param object object of class `incubate_fit`
#' @param parm character. Which parameters to get confidence interval for?
#' @param level numeric. Which is the requested confidence level for the interval? Default value is 0.95
#' @param R number of bootstrap replications. Used only if not `bs_data`-object is provided.
#' @param bs_data character or bootstrap data object. If character, it specifies which type of bootstrap is requested and the bootstrap data will be generated accordingly. Data can also be provided here directly. If missing it uses parametric bootstrap.
#' @param bs_infer character. Which type of bootstrap inference is requested to generate the confidence interval?
#' @param useBoot logical. Delegate bootstrap confint calculation to the `boot`-package?
#' @param ... further arguments, currently not used.
#' @return A matrix (or vector) with columns giving lower and upper confidence limits for each parameter.
#' @export
confint.incubate_fit <- function(
  object,
  parm,
  level = 0.95,
  R = 199L,
  bs_data,
  bs_infer = c(
    'logquantile',
    'lognormal',
    'quantile',
    'quantile0',
    'normal',
    'normal0'
  ),
  useBoot = FALSE,
  ...
) {
  stopifnot(inherits(object, 'incubate_fit'))
  stopifnot(
    is.numeric(level),
    length(level) == 1L,
    level < 1L,
    level > 0L
  )
  stopifnot(is.numeric(R), length(R) == 1L, R > 0)
  if (missing(bs_data)) {
    bs_data <- 'parametric'
  }
  if (is.vector(bs_data) && is.character(bs_data)) {
    bs_data <- match.arg(bs_data[[1L]], choices = c('parametric', 'ordinary'))
  }
  bs_infer <- match.arg(bs_infer)
  logTransform <- isTRUE(startsWith(bs_infer, 'log'))

  twoGroup <- isTRUE(object$twoGroup)
  nObs <- object$nobs

  useBoot <- isTRUE(useBoot) || inherits(bs_data, 'boot')

  genBootstrapData <- is.character(bs_data) &&
    length(bs_data == 1L) &&
    !is.na(bs_data) &&
    nzchar(bs_data)
  stopifnot(
    genBootstrapData ||
      useBoot && inherits(bs_data, 'boot') ||
      is.matrix(bs_data)
  )

  # check if we can really use boot
  if (
    useBoot &&
      (!requireNamespace("boot", quietly = TRUE) ||
        twoGroup ||
        !bs_infer %in%
          c('normal', 'lognormal', 'quantile', 'logquantile', 'quantile0'))
  ) {
    warning(
      'Using own implementation as package',
      sQuote('boot'),
      'is not available or scenario not implemented.',
      call. = FALSE
    )
    useBoot <- FALSE
  }

  cf <- coef(object)
  pnames <- names(cf)
  stopifnot(
    is.numeric(cf),
    is.character(pnames),
    nzchar(pnames),
    length(cf) == length(pnames)
  )

  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  parm <- intersect(pnames, parm) # in any case

  if (is.null(parm) || !length(parm) || any(!nzchar(parm))) {
    warning('Invalid parameter name given in argument parm=', call. = FALSE)
    return(invisible(NULL))
  }

  stopifnot(is.character(parm), length(parm) >= 1L)

  a <- (1L - level) / 2L
  a <- c(a, 1L - a)

  # if not already provided get bootstrap data (i.e. coefficients) from fitted model to bootstrapped observations
  if (genBootstrapData) {
    bs_data <- bsDataStep(
      object = object,
      bs_data = bs_data,
      R = R,
      useBoot = useBoot
    )
  }
  stopifnot(
    `bootstrap data not attained!` = !is.vector(bs_data),
    `bootstrap data still character!` = !is.character(bs_data)
  )
  # set R according to the provided bs_data (in particular important when both R & bs_data object are given)
  R <- if (useBoot) bs_data[['R']] else NCOL(bs_data)
  if (R < 999) {
    warning(
      glue(
        'Be cautious with the confidence interval(s) because the number of effective bootstrap samples R = {R} < 999 is rather low.'
      ),
      call. = FALSE
    )
  }

  # logShift: needed only when log-transformation is requested.
  # Start with a small standard value for all parameters
  logshift <- rlang::set_names(
    rep_len(.0001, length.out = length(pnames)),
    nm = pnames
  )
  # for delay, the transformation needs to be independent of the scale of delay, so we subtract the minimum and add a shift
  # use fixed logshift_delay = 5 (which performed well in simulation at single group, exponential distribution, together with smd=0.25)
  if (logTransform) {
    LOGSHIFT_DELAY <- 5
    for (i in which(startsWith(pnames, 'delay'))) {
      logshift[i] <- -min(
        if (useBoot) bs_data$t[, i] else bs_data[i, ],
        na.rm = TRUE
      ) +
        LOGSHIFT_DELAY
      # using low quantiles would make it less dependent on R but then we needed to check that x-logshift remains positive (for log)
      #stats::quantile(..i.., probs = c(0, 0.001), na.rm = TRUE, names = FALSE) # catch when diff() > LOGSHIFT_DELAY
    } #rof
  } #fi logTransform

  # do bootstrap inference on bootstrap data
  ci <- if (useBoot) {
    stopifnot(inherits(bs_data, 'boot'))

    # 'perc' just takes the quantiles,
    #+'basic' uses quantiles of the difference to the observed value (bias-correction)
    ci_type <- switch(
      bs_infer,
      quantile0 = 'perc',
      quantile = ,
      logquantile = 'basic',
      normal = ,
      lognormal = 'norm',
      stop(
        'This boot.ci-type from bs_infer= is not supported in boot-package!',
        call. = FALSE
      )
    )

    matrix(
      unlist(
        purrr::map(
          seq_len(length.out = length(coef(object))),
          .f = ~ {
            # the output of boot.ci can have different CIs as named matrix list entries
            ci_bo <- {
              if (logTransform) {
                boot::boot.ci(
                  bs_data,
                  index = .,
                  conf = level,
                  type = ci_type,
                  h = function(t) log(t + logshift[[.]]),
                  hdot = function(t) 1 / (t + logshift[[.]]),
                  hinv = function(t) exp(t) - logshift[[.]]
                )
              } else {
                boot::boot.ci(bs_data, index = ., conf = level, type = ci_type)
              }
            }[[switch(ci_type, norm = 'normal', perc = 'percent', ci_type)]]
            # depending on the CI-type: normal yields 3 columns, perc and others give 5 columns
            stopifnot(is.matrix(ci_bo), NCOL(ci_bo) > 2L)
            # the last two columns are always the lower and upper bound
            ci_bo[, c(NCOL(ci_bo) - 1L, NCOL(ci_bo))]
          }
        )
      ),
      ncol = 2L,
      byrow = TRUE
    )
  } else {
    stopifnot(is.matrix(bs_data))

    # bootstrapped confidence limits
    # bias-correction for parametric bootstrap only!?
    #delayH_mle_bias <- mean(delay_mle_bs) - delayH_mle
    switch(
      bs_infer,
      quantile0 = {
        t(apply(bs_data, 1L, stats::quantile, probs = a, na.rm = TRUE))
      },
      quantile = {
        # bias-corrected quantile-based CI
        # see Davison, p28
        # vector - matrix: vector is expanded column-wise, and the row-dimension fits (=number of coefs)
        2L *
          cf -
          t(apply(bs_data, 1L, stats::quantile, probs = rev(a), na.rm = TRUE))
      },
      logquantile = local({
        # #bs_min <- apply(bs_data, 1L, min) - .15
        # bs_min <- rlang::set_names(rep.int(-.001, length(cf)), nm = names(cf))
        # # for delay, the transformation should be independent of the scale of delay
        # if ('delay' %in% names(bs_min)) bs_min['delay'] <- min(bs_data['delay',], na.rm = TRUE) - .1

        ## bias-corrected normal-based CI after log-transformation
        -logshift +
          exp(
            2L *
              log(cf + logshift) -
              log(
                t(apply(
                  bs_data,
                  1L,
                  stats::quantile,
                  probs = rev(a),
                  na.rm = TRUE
                )) +
                  logshift
              )
          )
      }),
      normal0 = {
        t(
          c(1L, 1L) %o%
            .rowMeans(bs_data, m = length(cf), n = R) +
            stats::qnorm(a) %o% apply(bs_data, 1L, stats::sd)
        )
      },
      normal = {
        ## bias-corrected normal-based CI
        ## ci_delay_mle <- delayH_mle - delayH_mle_bias + c(-1, 1) * qnorm(.975) * delayH_mle_sd
        t(
          c(1L, 1L) %o%
            (2L * cf - .rowMeans(bs_data, m = length(cf), n = R)) +
            stats::qnorm(a) %o% apply(bs_data, 1L, stats::sd)
        )
      },
      lognormal = local({
        # #bs_min <- apply(bs_data, 1L, min) - .15
        # bs_min <- rlang::set_names(rep.int(-.001, length(cf)), nm = names(cf))
        # # for delay, the transformation should be independent of the scale of delay
        # if ('delay' %in% names(bs_min)) bs_min['delay'] <- min(bs_data['delay',], na.rm = TRUE) - .1

        bs_data_h <- log(bs_data + logshift)
        ## bias-corrected normal-based CI after log-transformation
        -logshift +
          exp(
            t(
              c(1L, 1L) %o%
                (2L *
                  log(cf + logshift) -
                  .rowMeans(bs_data_h, m = length(cf), n = R)) +
                stats::qnorm(a) %o% apply(bs_data_h, 1L, stats::sd)
            )
          )
      }),
      stop(
        'This type of bootstrap confidence interval is not supported!',
        call. = FALSE
      )
    )
  } #esle useBoot

  # ensure formatted row and column names
  rownames(ci) <- pnames
  colnames(ci) <- paste0(format(a * 100, trim = TRUE, nsmall = 1L), '%')

  # enforce parameter bounds also for CI
  # all parameters are non-negative!
  ci[which(ci < 0L)] <- 0L

  ci[parm, , drop = FALSE]
}
