# mkuhn, 2021-04-07
# testing the delay estimation,
# in particular parameter estimates, convergence etc. from the model fit object

test_that("Parameter extraction and transformation", {
  #testthat::skip(message = "skip parameter extraction for now")

  distO_e <- buildDist(distribution = "exponential")
  expect_named(
    distO_e,
    expected = c(
      "dist",
      "dist_name",
      "hasDelay",
      "hasShape",
      "negAllowed",
      "twoPhaseAllowed",
      "cdf",
      "pdf",
      "random",
      "param"
    )
  )
  expect_type(distO_e$param, type = "closure")

  # test parameter names of distribution object
  # on transformed scale (for optimization)
  expect_identical(
    distO_e$param(
      twoPhase = FALSE,
      twoGroup = TRUE,
      bind = "rate1",
      profiled = FALSE,
      transformed = TRUE
    ),
    expected = c("rate1_tr", "delay1_tr.x", "delay1_tr.y")
  )
  expect_identical(
    distO_e$param(
      twoPhase = FALSE,
      twoGroup = TRUE,
      bind = "rate1",
      profiled = TRUE,
      transformed = TRUE
    ),
    expected = c("rate1_tr", "delay1_tr.x", "delay1_tr.y")
  )

  distO_w <- buildDist(distribution = "weibull")
  expect_named(
    distO_w,
    expected = c(
      "dist",
      "dist_name",
      "hasDelay",
      "hasShape",
      "negAllowed",
      "twoPhaseAllowed",
      "cdf",
      "pdf",
      "random",
      "param"
    )
  )
  expect_type(distO_w$param, type = "closure")
  expect_identical(
    distO_w$param(
      twoPhase = FALSE,
      twoGroup = TRUE,
      bind = "shape1",
      profiled = FALSE,
      transformed = TRUE
    ),
    expected = c(
      "shape1_tr",
      "delay1_tr.x",
      "scale1_tr.x",
      "delay1_tr.y",
      "scale1_tr.y"
    )
  )
  expect_identical(
    distO_w$param(
      twoPhase = FALSE,
      twoGroup = TRUE,
      bind = "shape1",
      profiled = TRUE,
      transformed = TRUE
    ),
    expected = c("shape1_tr", "delay1_tr.x", "delay1_tr.y")
  )
  # original (=non-transformed) scale
  expect_identical(
    distO_w$param(
      twoPhase = FALSE,
      twoGroup = TRUE,
      bind = "shape1",
      profiled = FALSE,
      transformed = FALSE
    ),
    expected = c("shape1", "delay1.x", "scale1.x", "delay1.y", "scale1.y")
  )
  # param() takes out profiled parameters even when for original (=non-transformed) scale
  #+ this is needed in delay_estimation (at extractParOptInd)
  expect_identical(
    distO_w$param(
      twoPhase = FALSE,
      twoGroup = TRUE,
      bind = "shape1",
      profiled = TRUE,
      transformed = FALSE
    ),
    expected = c("shape1", "delay1.x", "delay1.y")
  )

  #' Slow extract-routine based on R's name-matching capabilities as gold standard function.
  #'
  #' Extract certain parameters from a given named parameter vector. External, slow variant that parses parameter names.
  #'
  #' This routine handles parameter vectors for one or two groups. To this end, it parses the parameter names.
  #' Bound parameters (cf. `bind=`) are deduced from the naming: they lack the .x or .y suffix.
  #' it can also transform the given parameters from the standard form (as used in the delayed distribution functions)
  #' to the internal transformed parametrization as used by the optimization function, and vice versa.
  #'
  #' When parameters for a single group are requested the parameters are always returned in the canonical order of the distribution.
  #' Otherwise, the order of parameters is unchanged.
  #' It parses the parameter names to find out if it is twoPhase, twoGroup and, when `transform=TRUE`, which direction to transform.
  #'
  #' @param parV numeric named parameter vector (for single group or two groups)
  #' @param distribution character name. Exponential or Weibull
  #' @param twoPhase logical. Do we model two phases?
  #' @param group character. Extract only parameters for specified group. Recognizes 'x' and 'y'. Other values are ignored.
  #' @param isTransformed logical. Are the parameters transformed? If `NULL` (default) it is deduced from the parameter names.
  #' @param transform logical. Transform the parameters!?
  #' @return requested parameters as named numeric vector
  extractParsTest <- function(
    parV,
    distribution = c('exponential', 'weibull'),
    twoPhase = NULL,
    group = NULL,
    obs1,
    isTransformed = NULL,
    transform = FALSE
  ) {
    stopifnot(is.numeric(parV), all(nzchar(names(parV))))
    distO <- buildDist(match.arg(distribution))

    # contract: parV is canonically ordered (see below as well)
    # extractPars will not check if it needs first to reorder the parameters given by checking the names.
    pNames <- names(parV)

    # isTransformed when all parameter names contain '_tr'
    if (is.null(isTransformed)) {
      trNames <- grepl(pattern = "_tr", pNames, fixed = TRUE)
      stopifnot(sum(trNames) %in% c(0L, length(parV))) #all or nothing
      isTransformed <- all(trNames)
    }
    if (is.null(twoPhase)) {
      twoPhase <- any(grepl(pattern = "delay2", pNames, fixed = TRUE))
    }
    # parameter names of single group where we start from!
    oNames <- distO$param(
      twoPhase = twoPhase,
      twoGroup = FALSE,
      transformed = isTransformed
    )

    # index of parameters that are *not* group-specific
    idx.nongrp <- which(!grepl(pattern = paste0("[.][xy]$"), pNames))
    #Is there any .x or .y parameter name?
    #+when two groups but all parameters bound then isTwoGroup is FALSE
    isTwoGroup <- length(idx.nongrp) < length(parV)

    # transform parameters if requested
    # if no transformation required, simply extract the relevant parameters
    if (transform) {
      if (distO$dist %in% c("exponential", "weibull") && missing(obs1)) {
        stop("Please specifiy obs1=")
      }
      # transform parameter vector for a single group. Also transforms parameter names.
      # @param parV1: parameter vector for a single group
      # @param obs1: first observation in group
      # @return transformed parameter vector
      transformPars1Test <- function(parV1, obs1) {
        # parameter transformation matrices
        PARAM_TRANSF_M <- switch(
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
            dimnames = list(paste0(
              c("delay1", "shape1", "scale1", "delay2", "shape2", "scale2"),
              "_tr"
            ))
          ),
          stop("Unknown distribution!", call. = FALSE)
        )
        PARAM_TRANSF_Minv <- switch(
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
          stop("Unknown distribution", call. = FALSE)
        )

        PARAM_TRANSF_F <- list(
          exponential = c(stats::qlogis, log, log, log),
          weibull = c(
            stats::qlogis,
            log, #log1p, #identity, #=shape1
            log,
            log,
            log,
            log
          )
        )[[distO$dist]]
        PARAM_TRANSF_Finv <- list(
          exponential = c(stats::plogis, exp, exp, exp),
          weibull = c(
            stats::plogis,
            exp, #expm1, #identity, #=shape1
            exp,
            exp,
            exp,
            exp
          )
        )[[distO$dist]]

        stopifnot(
          length(parV1) <= NCOL(PARAM_TRANSF_M),
          NCOL(PARAM_TRANSF_M) == length(PARAM_TRANSF_F)
        )

        b <- rlang::rep_along(parV1, 1)
        if (distO$dist %in% c("exponential", "weibull")) {
          b[[1]] <- max(obs1, 1)
        }

        if (isTransformed) {
          # param = Ainv %*% (Finv(param') * b)
          rlang::set_names(
            as.numeric(
              PARAM_TRANSF_Minv[seq_along(parV1), seq_along(parV1)] %*%
                (as.numeric(.mapply(
                  FUN = function(f, x) f(x),
                  dots = list(PARAM_TRANSF_Finv[seq_along(parV1)], parV1),
                  MoreArgs = NULL
                )) *
                  b)
            ),
            nm = rownames(PARAM_TRANSF_Minv)[seq_along(parV1)]
          )
        } else {
          # param' = F(A %*% param / b)
          rlang::set_names(
            as.numeric(.mapply(
              FUN = function(f, x) f(x),
              dots = list(
                PARAM_TRANSF_F[seq_along(parV1)],
                as.numeric(
                  PARAM_TRANSF_M[seq_along(parV1), seq_along(parV1)] %*% parV1
                ) /
                  b
              ),
              MoreArgs = NULL
            )),
            nm = rownames(PARAM_TRANSF_M)[seq_along(parV1)]
          )
        }
      } # fn transformPars1Test

      # update parameter vector according to transformation
      parV <- if (!isTwoGroup) {
        transformPars1Test(parV1 = parV, obs1 = obs1[[1]])
      } else {
        # **twoGroup** case (effectively)
        # target parameter names
        pNames_trgt <- if (isTransformed) {
          sub(pattern = "_tr", replacement = "", pNames, fixed = TRUE) # remove '_tr' string
        } else {
          purrr::map_chr(
            .x = strsplit(pNames, split = ".", fixed = TRUE),
            .f = function(s) {
              if (length(s) == 1L) {
                paste0(s, "_tr")
              } else {
                paste0(s[[1L]], "_tr.", s[[2L]])
              }
            }
          )
        }

        stopifnot(length(obs1) == 2)
        # list of transformed parameters, for both groups x and y
        h <- list(
          x = {
            idx.grp <- which(endsWith(x = pNames, suffix = ".x"))
            # drop group naming: last two letters
            pNms_grp <- substr(
              pNames[idx.grp],
              start = 1L,
              stop = nchar(pNames[idx.grp], type = "chars") - 2L
            )
            # apply transform on 1-group parameter vector in canonical order!
            transformPars1Test(
              parV1 = c(
                parV[idx.nongrp],
                rlang::set_names(parV[idx.grp], nm = pNms_grp)
              )[intersect(oNames, c(pNames, pNms_grp))],
              obs1 = obs1[1]
            )
          },
          y = {
            idx.grp <- which(endsWith(x = pNames, suffix = ".y"))
            # drop group naming: last two letters
            pNms_grp <- substr(
              pNames[idx.grp],
              start = 1L,
              stop = nchar(pNames[idx.grp], type = "chars") - 2L
            )
            # apply transform on 1-group parameter vector in canonical order!
            transformPars1Test(
              parV1 = c(
                parV[idx.nongrp],
                rlang::set_names(parV[idx.grp], nm = pNms_grp)
              )[intersect(oNames, c(pNames, pNms_grp))],
              obs1 = obs1[2]
            )
          }
        )

        # parV transformed for effective twoGroup-setting
        rlang::set_names(
          c(
            h[[1L]][pNames_trgt[idx.nongrp]],
            h[[1L]][!names(h[[1L]]) %in% pNames_trgt[idx.nongrp]],
            h[[2L]][!names(h[[2L]]) %in% pNames_trgt[idx.nongrp]]
          ),
          nm = pNames_trgt
        )
      }

      # reflect transformation in meta-data
      isTransformed <- !isTransformed
      pNames <- names(parV)
      oNames <- distO$param(
        twoPhase = twoPhase,
        twoGroup = FALSE,
        transformed = isTransformed
      )
    } # transform

    # return parameter vector
    if (
      !isTwoGroup ||
        is.null(group) ||
        length(group) != 1L ||
        !group %in% c('x', 'y')
    ) {
      parV
    } else {
      # single group extraction
      stopifnot(is.character(group), nzchar(group))

      # extract all group parameters (=contain .x or .y at the end)
      #+and restore original name (=remove ".x" or ".y")
      # contract: parV is canonically ordered
      parV.gr <- parV[grepl(
        pattern = paste0(".", group),
        x = pNames,
        fixed = TRUE
      )]
      names(parV.gr) <- substr(
        names(parV.gr),
        start = 1L,
        stop = nchar(names(parV.gr)) - 2L
      )
      #parV.gr <- rlang::set_names(parV.gr, nm = setdiff(oNames, pNames[idx.nongrp]))

      # restore original order
      # contract: bind is intersected (only valid names, canonical order)
      c(parV.gr, parV[pNames[idx.nongrp]])[intersect(
        oNames,
        c(pNames, names(parV.gr))
      )]
    }
  }

  dummydat <- c(6, 7, 9, 11)
  extPars_exp1NP <- incubate:::objFunFactory(
    x = dummydat,
    distO = distO_e,
    twoPhase = FALSE,
    control = buildControl(profiled = FALSE)
  ) |>
    rlang::fn_env() |>
    rlang::env_get("extractPars")

  par_exp1 <- c(delay1 = 3, rate1 = .8)
  par_tf_exp1 <- c(
    delay1_tr = stats::qlogis(par_exp1[[1L]] / min(dummydat)),
    rate1_tr = log(par_exp1[[2L]])
  )

  expect_identical(
    extPars_exp1NP(par_exp1, isOpt = FALSE, named = TRUE),
    par_exp1
  )
  expect_identical(
    extPars_exp1NP(par_exp1, isOpt = FALSE, named = FALSE),
    as.vector(par_exp1)
  )
  expect_identical(
    extPars_exp1NP(par_exp1, isOpt = FALSE, group = "x", named = TRUE),
    par_exp1
  )
  expect_identical(
    extPars_exp1NP(par_exp1, isOpt = FALSE, group = "y", named = TRUE),
    par_exp1
  ) # group= is ignored for single group!
  expect_identical(
    extPars_exp1NP(par_exp1, isOpt = FALSE, group = "z", named = TRUE),
    par_exp1
  ) # group= is ignored for single group!
  expect_identical(
    extPars_exp1NP(par_exp1, isOpt = FALSE, transform = TRUE, named = TRUE),
    expected = par_tf_exp1
  )
  expect_identical(
    extPars_exp1NP(par_exp1, isOpt = FALSE, transform = TRUE, named = FALSE),
    expected = as.numeric(par_tf_exp1)
  )

  extPars_exp1P <- incubate:::objFunFactory(
    x = dummydat,
    distO = distO_e,
    twoPhase = FALSE,
    control = buildControl(profiled = TRUE)
  ) |>
    rlang::fn_env() |>
    rlang::env_get("extractPars")
  # objective function with profiled=TRUE is different
  expect_identical(
    extPars_exp1P(par_exp1, isOpt = FALSE, named = TRUE),
    par_exp1
  )
  expect_identical(
    extPars_exp1P(par_exp1, isOpt = FALSE, transform = TRUE, named = TRUE),
    expected = par_tf_exp1[1]
  )
  expect_identical(
    extPars_exp1P(par_exp1, isOpt = FALSE, transform = TRUE, named = FALSE),
    expected = par_tf_exp1[[1L]]
  )

  # my original (but slow) implementation
  expect_identical(extractParsTest(parV = par_exp1), par_exp1)
  expect_identical(extractParsTest(parV = par_exp1, group = "x"), par_exp1)
  expect_identical(extractParsTest(parV = par_exp1, group = "y"), par_exp1) # group= is ignored here
  expect_identical(
    extractParsTest(parV = par_exp1, transform = TRUE, obs1 = min(dummydat)),
    expected = par_tf_exp1
  )

  # exponential, two groups, unbound
  objFun_exp2 <- incubate:::objFunFactory(
    x = rexp_delayed(n = 3, delay1 = 5, rate1 = .2),
    y = rexp_delayed(n = 3, delay1 = 3, rate1 = .1),
    distO = distO_e,
    twoPhase = FALSE,
    control = buildControl(profiled = TRUE)
  )
  extPars_exp2 <- rlang::env_get(rlang::fn_env(objFun_exp2), "extractPars")

  par_exp2 <- c(delay1.x = 3.8, rate1.x = .81, delay1.y = 2.4, rate1.y = 1.1)

  expect_identical(
    extPars_exp2(par_exp2, isOpt = FALSE, named = TRUE),
    par_exp2
  )
  expect_identical(
    extPars_exp2(par_exp2, isOpt = FALSE, named = TRUE, group = "z"),
    NULL
  ) # strange groups gives NULL in two group setting
  expect_identical(
    extPars_exp2(par_exp2, isOpt = FALSE, named = TRUE, group = "x"),
    setNames(par_exp2[1:2], c("delay1", "rate1"))
  )
  expect_identical(
    extPars_exp2(par_exp2, isOpt = FALSE, named = TRUE, group = "y"),
    setNames(par_exp2[3:4], c("delay1", "rate1"))
  )

  # my original (but slow) implementation
  expect_identical(extractParsTest(parV = par_exp2), par_exp2)
  expect_identical(extractParsTest(parV = par_exp2, group = "z"), par_exp2) # strange groups are ignored with extractParsTest!
  expect_identical(
    extractParsTest(parV = par_exp2, group = "x"),
    setNames(par_exp2[1:2], c("delay1", "rate1"))
  )
  expect_identical(
    extractParsTest(parV = par_exp2, group = "y"),
    setNames(par_exp2[3:4], c("delay1", "rate1"))
  )

  # exponential, two groups, bound
  local({
    x <- rexp_delayed(n = 3, delay1 = 5, rate1 = .2)
    y <- rexp_delayed(n = 4, delay1 = 3, rate1 = .1)

    objFun_exp2b <- incubate:::objFunFactory(
      x = x,
      y = y,
      distO = distO_e,
      twoPhase = FALSE,
      bind = "rate1",
      control = buildControl(profiled = FALSE)
    )
    extPars_exp2b <- rlang::env_get(rlang::fn_env(objFun_exp2b), "extractPars")

    par_exp2b <- c(rate1 = .23, delay1.x = 4.8, delay1.y = 2.7)
    par_tf_exp2b <- c(
      rate1_tr = log(par_exp2b[["rate1"]]),
      delay1_tr.x = stats::qlogis(par_exp2b[[2]] / min(x)),
      delay1_tr.y = stats::qlogis(par_exp2b[[3]] / min(y))
    )

    expect_identical(
      extPars_exp2b(parV = par_exp2b, isOpt = FALSE, named = TRUE),
      par_exp2b
    )
    expect_identical(
      extPars_exp2b(parV = par_exp2b, isOpt = FALSE, named = TRUE, group = "x"),
      setNames(par_exp2b[c(2, 1)], c("delay1", "rate1"))
    )
    expect_identical(
      extPars_exp2b(parV = par_exp2b, isOpt = FALSE, named = TRUE, group = "y"),
      setNames(par_exp2b[c(3, 1)], c("delay1", "rate1"))
    )
    expect_identical(
      extPars_exp2b(
        parV = par_exp2b,
        isOpt = FALSE,
        named = TRUE,
        transform = TRUE
      ),
      expected = par_tf_exp2b
    )
    expect_identical(
      extPars_exp2b(
        parV = par_exp2b,
        isOpt = FALSE,
        named = TRUE,
        group = "x",
        transform = TRUE
      ),
      expected = purrr::set_names(
        par_tf_exp2b[c(2, 1)],
        c("delay1_tr", "rate1_tr")
      )
    )
    # idem-potent (round-trip)
    expect_equal(
      extPars_exp2b(
        extPars_exp2b(par_exp2b, isOpt = FALSE, transform = TRUE),
        isOpt = TRUE,
        named = TRUE,
        transform = TRUE
      ),
      par_exp2b
    )

    # my original (but slow) implementation
    expect_identical(extractParsTest(parV = par_exp2b), par_exp2b)
    expect_identical(
      extractParsTest(parV = par_exp2b, group = "x"),
      setNames(par_exp2b[c(2, 1)], c("delay1", "rate1"))
    )
    expect_identical(
      extractParsTest(parV = par_exp2b, group = "y"),
      setNames(par_exp2b[c(3, 1)], c("delay1", "rate1"))
    )
    expect_identical(
      extractParsTest(
        parV = par_exp2b,
        transform = TRUE,
        obs1 = c(x = min(x), y = min(y))
      ),
      expected = par_tf_exp2b
    )

    expect_identical(
      extractParsTest(
        parV = par_exp2b,
        group = "x",
        transform = TRUE,
        obs1 = c(x = min(x), y = min(y))
      ),
      expected = purrr::set_names(
        par_tf_exp2b[c(2, 1)],
        c("delay1_tr", "rate1_tr")
      )
    )
    # idem-potent (round-trip)
    expect_equal(
      extractParsTest(
        extractParsTest(
          parV = par_exp2b,
          transform = TRUE,
          obs1 = c(min(x), min(y))
        ),
        transform = TRUE,
        obs1 = c(min(x), min(y))
      ),
      par_exp2b
    )
  })

  # weibull
  local({
    x <- rweib_delayed(n = 3, delay1 = 5, shape1 = 1.2)
    par_weib1 <- c(delay1 = 5, shape1 = .8, scale1 = 1.5)
    # extractParsTest specific: incomplete parameter vector
    par_weib1s <- par_weib1[1:2]
    par_tf_weib1 <- c(
      delay1_tr = stats::qlogis(par_weib1[[1]] / min(x)),
      shape1_tr = log(par_weib1[["shape1"]])
    )

    objFun_weib1 <- incubate:::objFunFactory(
      x = x,
      distO = distO_w,
      twoPhase = FALSE,
      control = buildControl(profiled = TRUE)
    )
    extPars_weib1 <- rlang::env_get(rlang::fn_env(objFun_weib1), "extractPars")

    expect_identical(
      extPars_weib1(par_weib1, isOpt = FALSE, named = TRUE),
      par_weib1
    )
    expect_identical(
      extractParsTest(par_weib1, distribution = "weibull"),
      par_weib1
    )

    expect_identical(
      extPars_weib1(par_weib1s, isOpt = FALSE, named = TRUE),
      c(par_weib1s, scale1 = NA)
    ) #missing parameter gets named NA
    expect_identical(
      extractParsTest(par_weib1s, distribution = "weibull"),
      par_weib1s
    )
    expect_identical(
      extractParsTest(
        par_weib1s,
        distribution = "weibull",
        obs1 = min(x),
        transform = TRUE
      ),
      expected = par_tf_weib1
    )
  })

  # weibull two groups
  local({
    x <- rweib_delayed(n = 3, delay1 = 5, shape1 = 1.2)
    y <- rweib_delayed(n = 4, delay1 = 7, shape1 = 2.8, scale1 = .9)

    objFun_weib2 <- incubate:::objFunFactory(
      x = x,
      y = y,
      distO = distO_w,
      twoPhase = FALSE,
      control = buildControl(profiled = FALSE)
    )
    extPars_weib2 <- rlang::env_get(rlang::fn_env(objFun_weib2), "extractPars")

    par_weib2 <- c(
      delay1.x = 1,
      shape1.x = 1.8,
      scale1.x = 7,
      delay1.y = 4.2,
      shape1.y = 3.4,
      scale1.y = 12
    )
    par_weib2.y <- par_weib2[4:6] |>
      setNames(nm = c("delay1", "shape1", "scale1"))
    par_tf_weib2.y <- c(
      stats::qlogis(par_weib2.y[1] / min(y)),
      log(par_weib2.y[-1])
    ) |>
      setNames(nm = paste0(names(par_weib2.y), "_tr"))

    expect_identical(
      extPars_weib2(par_weib2, isOpt = FALSE, named = TRUE),
      par_weib2
    )
    expect_identical(
      extPars_weib2(par_weib2, isOpt = FALSE, named = TRUE, group = "y"),
      par_weib2.y
    )
    expect_identical(
      extPars_weib2(
        par_weib2,
        isOpt = FALSE,
        named = TRUE,
        group = "y",
        transform = TRUE
      ),
      expected = par_tf_weib2.y
    )

    expect_identical(
      extractParsTest(par_weib2, distribution = "weibull"),
      par_weib2
    )
    expect_identical(
      extractParsTest(par_weib2, distribution = "weibull", group = "y"),
      par_weib2.y
    )
    expect_identical(
      extractParsTest(
        par_weib2,
        distribution = "weibull",
        group = "y",
        obs1 = c(min(x), min(y)),
        transform = TRUE
      ),
      par_tf_weib2.y
    )

    # extractParsTest specific: incomplete parameter vector
    par_weib2s <- par_weib2[-c(3, 6)] # drop scale
    expect_identical(
      extractParsTest(par_weib2s, distribution = "weibull", group = "y"),
      setNames(par_weib2s[c(3, 4)], nm = c("delay1", "shape1"))
    )

    expect_identical(
      extractParsTest(
        par_weib2s,
        distribution = "weibull",
        group = "y",
        obs1 = c(min(x), min(y)),
        transform = TRUE
      ),
      expected = par_tf_weib2.y[1:2]
    )
  })
})


test_that("Handle Surv-objects", {
  expect_true(requireNamespace("survival", quietly = TRUE))

  # numeric, non-Surv (no ties)
  ti_x <- sort(2 + rpois(17, lambda = 5) + rnorm(17, sd = .1))
  ti_y <- sort(5 + rpois(11, lambda = 2.8) + rnorm(11, sd = .1))

  # right-censoring
  ti_x2 <- survival::Surv(ti_x)
  ti_y2 <- Surv(ti_y)
  ti_x2c <- survival::Surv(
    ti_x,
    event = sample(c(1, 1, 0), size = length(ti_x), replace = TRUE)
  )
  ti_y2c <- survival::Surv(
    ti_y,
    event = sample(c(1, 1, 0), size = length(ti_y), replace = TRUE)
  )

  # interval-censoring
  ti_x3 <- survival::Surv(
    ti_x,
    time2 = NA,
    event = rep_len(1, length(ti_x)),
    type = "interval"
  )
  ti_y3 <- survival::Surv(
    ti_y,
    time2 = NA,
    event = rep_len(1, length(ti_y)),
    type = "interval"
  )

  # single group
  fm1 <- delay_model(x = ti_x) # numeric observations
  expect_identical(fm1, delay_model(x = ti_x2)) # right-censoring, no cens
  expect_identical(fm1, delay_model(x = ti_x3)) # interval-censored, no cens

  # two groups
  fm2 <- delay_model(x = ti_x, y = ti_y) # numeric observerations
  expect_identical(fm2, delay_model(x = ti_x2, y = ti_y2)) # right-censoring, no cens
  expect_identical(fm2, delay_model(x = ti_x3, y = ti_y3)) # interval-censored, no cens

  fm2b <- delay_model(x = ti_x, y = ti_y, bind = "rate1")
  expect_identical(fm2b, delay_model(x = ti_x2, y = ti_y2, bind = "rate1")) # right-censoring, no cens
  expect_identical(fm2b, delay_model(x = ti_x3, y = ti_y3, bind = "rate1")) # interval-censored, no cens

  fm3 <- delay_model(x = ti_x2c, y = ti_y, bind = "rate1") # with cens
  expect_identical(fm3, delay_model(x = ti_x2c, y = ti_y2, bind = "rate1"))
  expect_error(
    delay_model(x = ti_x2c, y = ti_y3, bind = "rate1"),
    regexp = "same type"
  )
})


test_that("Censored response", {
  expect_true(requireNamespace("survival", quietly = TRUE))

  local({
    # early right censorings can lead to negative obs_c => which explodes in log()
    # data stem from rweib_delayed(n=17, delay1=5, shape1 = 1.2, scale1=2.3, cens=.35)
    xr <- survival::Surv(
      c(
        5.30,
        5.67,
        5.74,
        5.77,
        5.83,
        6.13,
        6.28,
        6.61,
        6.89,
        7.03,
        7.23,
        7.94,
        8.13,
        9.89,
        10.84,
        11.50,
        15.19
      ),
      event = c(0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
    )

    expect_s3_class(xr, "Surv")
    # no warning about log(neg. value)
    expect_no_warning(
      object = {
        fmr <- delay_model(x = xr, distribution = "weibu", method = "MLEw")
      },
      message = "NaN"
    )
    expect_s3_class(fmr, class = "incubate_fit")
    expect_identical(fmr$nobs, expected = c(x = 17, y = 0))
    # delay estimate is somewhat close to original value of MC-sim
    expect_equal(coef(fmr)[[1]], expected = 5, tolerance = .15)
  })

  local({
    # from MC-study for estimation with MLEw
    # Weibull, delay=50, scale=10, shape=.5, cens=.3 (run='6591')
    # leads to huge scale estimate: is this problematic/a bug?
    xr <- structure(
      c(
        50.01537038,
        50.02035397,
        50.26236779,
        50.76028195,
        51.63941942,
        51.9748065,
        53.18645016,
        54.19293334,
        54.97244851,
        57.83978461,
        59.27425993,
        71.64327435,
        139.5760608,
        166.6492835,
        167.1776227,
        1,
        1,
        0,
        1,
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
        0
      ),
      dim = c(15L, 2L),
      dimnames = list(NULL, c("time", "status")),
      type = "right",
      class = "Surv"
    )
    expect_s3_class(xr, "Surv")
    expect_no_warning(
      object = {
        fmr <- delay_model(x = xr, distribution = "weibu", method = "MLEw")
      },
      message = NULL
    )
    # true delay = 50
    expect_equal(coef(fmr)[[1]], expected = 50, tolerance = 1e2)
  })
})


test_that("Fit delayed Exponentials", {
  # single group ------------------------------------------------------------

  # from a call to rexp_delayed(16, delay = 9, rate = 0.5)
  exp_d9 <- c(
    9.3758422,
    9.436878,
    9.440794,
    9.63324,
    9.6421,
    9.76594,
    9.80527,
    9.907327,
    10.357,
    10.371,
    10.596,
    10.623,
    11.1074,
    11.575,
    11.849,
    16.38
  )

  fd_exp <- delay_model(
    exp_d9,
    distribution = "expon",
    method = "MPSE",
    control = list(profiled = FALSE)
  )
  coef_exp <- coef(fd_exp)

  expect_type(fd_exp$data, type = 'double')
  expect_identical(length(fd_exp$data), expected = 16L)
  expect_identical(fd_exp$method, expected = "MPSE")
  expect_named(coef(fd_exp), expected = c('delay1', 'rate1'))
  expect_equal(coef_exp[["delay1"]], expected = 9, tolerance = .04)
  expect_equal(coef_exp[["rate1"]], expected = 0.5, tolerance = .37)

  expect_named(
    fd_exp$optimizer,
    expected = c(
      'parOpt',
      "valOpt",
      'profiled',
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_false(fd_exp$optimizer$profiled)
  expect_identical(fd_exp$optimizer$convergence, expected = 0L)
  expect_identical(
    fd_exp$optimizer$valOpt,
    fd_exp[["criterion"]],
    ignore_attr = "names"
  ) # same for MSPE
  # optim converges properly for this data vector!
  # * convergence=51 is warning from L-BFGS-B
  # * convergence=52 is error from L-BFGS-B
  fd_exp_oa <- purrr::pluck(fd_exp, 'optimizer', 'optim_args')
  expect_type(fd_exp_oa, type = 'list')
  # update does not change structure
  expect_identical(update(fd_exp, optim_args = fd_exp_oa), expected = fd_exp)
  # effect of worse start values
  fd_exp_updW <- update(
    fd_exp,
    optim_args = purrr::assign_in(fd_exp_oa, 'par', c(1, .1))
  )
  # we need more iterations during optimization
  expect_gt(
    min(fd_exp_updW$optimizer$counts),
    expected = min(fd_exp$optimizer$counts)
  )
  # objective function does not reach a better (=lower) value when starting with worse start value
  # (add a little safety margin as buffer)
  expect_gte(fd_exp_updW$criterion + 1e-05, expected = fd_exp$criterion)

  # duplicated data
  # same data set but with two duplicated smallest observations
  exp_d9dup <- append(exp_d9, rep.int(exp_d9[[1L]], times = 2L), after = 0L)
  fd_exp_dup <- delay_model(
    exp_d9dup,
    distribution = "expon",
    method = "MPSE",
    control = list(profiled = FALSE)
  )
  expect_identical(fd_exp_dup$optimizer$convergence, expected = 0L)
  expect_gt(coef(fd_exp_dup)["delay1"], expected = coef_exp["delay1"]) # delay estimate is closer to first observation
  expect_gt(coef(fd_exp_dup)["rate1"], expected = coef_exp["rate1"]) # rate estimate is also steeper.

  # MPSE fit with profiled=TRUE
  fd_exp_P <- delay_model(
    exp_d9,
    distribution = "expon",
    control = list(profiled = TRUE)
  )
  expect_equal(fd_exp_P$criterion, expected = fd_exp$criterion, tolerance = .01) # similar criterion
  expect_equal(coef(fd_exp_P), coef_exp, tolerance = .02) # similar coefficients
  expect_named(fd_exp_P$optimizer$parOpt, expected = "delay1_tr")

  # another data set, with duplicates at the beginning
  exp_d5 <- c(
    5.05133760899019,
    5.05133760899019,
    5.05133760899019, # three duplicates
    5.21641483083995,
    5.4131497041096,
    5.62188922194764,
    5.75496241915971,
    5.98260715969298,
    6.18675520061515,
    7.41160508326157,
    9.36626494375473,
    10.5935673083787,
    10.8441290040006
  )
  fd_exp1_mpseNP <- delay_model(
    exp_d5,
    method = "MPSE",
    control = buildControl(profiled = FALSE)
  )
  fd_exp1_mpseP <- delay_model(
    exp_d5,
    method = "MPSE",
    control = buildControl(profiled = TRUE)
  )
  expect_identical(fd_exp1_mpseNP$optimizer$convergence, expected = 0L)
  expect_identical(fd_exp1_mpseP$optimizer$convergence, expected = 0L)
  expect_named(
    fd_exp1_mpseNP$optimizer$parOpt,
    expected = c("delay1_tr", "rate1_tr")
  )
  expect_named(fd_exp1_mpseP$optimizer$parOpt, expected = "delay1_tr")
  expect_equal(
    coef(fd_exp1_mpseP),
    expected = coef(fd_exp1_mpseNP),
    tolerance = .02
  )

  # erroneous data input -----
  expect_error(delay_model(x = "bla"))
  expect_error(delay_model(NULL))
  expect_error(
    delay_model(y = 3 + rlnorm(n = 4)),
    regexp = "Specify.*first group",
    ignore.case = TRUE
  )

  # MLE fits -----------------------------------------------------------

  # MLEn = naive MLE
  fd_exp_MLEn_NP <- delay_model(
    exp_d9,
    distribution = 'expon',
    method = 'MLEn',
    control = list(profiled = FALSE)
  )
  fd_exp_MLEn_P <- delay_model(
    exp_d9,
    distribution = 'expon',
    method = 'MLEn',
    control = list(profiled = TRUE)
  )

  expect_type(fd_exp_MLEn_NP$data, type = 'double')
  expect_identical(length(fd_exp_MLEn_NP$data), expected = length(exp_d9))
  expect_identical(fd_exp_MLEn_NP$method, expected = "MLEn")
  expect_identical(length(coef(fd_exp_MLEn_NP)), expected = 2L)
  expect_named(coef(fd_exp_MLEn_NP), expected = c("delay1", "rate1"))
  expect_named(
    fd_exp_MLEn_NP$optimizer$parOpt,
    expected = c("delay1_tr", "rate1_tr")
  )

  # MLEn uses analytical solution when in single group mode
  expect_named(
    fd_exp_MLEn_NP$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      'profiled',
      "methodOpt",
      "convergence",
      "message",
      "counts"
    )
  ) ##analytic with no "optim_args"))
  expect_identical(
    purrr::chuck(fd_exp_MLEn_NP, 'optimizer', 'methodOpt'),
    expected = "analytic"
  )
  expect_identical(
    purrr::chuck(fd_exp_MLEn_NP, 'optimizer', 'convergence'),
    expected = 0L
  )
  local({
    fd_exp_MLEn_NPopt <- attr(
      fd_exp_MLEn_NP$objFun,
      which = 'opt',
      exact = TRUE
    )
    expect_type(fd_exp_MLEn_NPopt, type = 'list')
    expect_named(
      fd_exp_MLEn_NPopt,
      expected = c(
        "par_orig",
        'par',
        'value',
        "methodOpt",
        'convergence',
        'message',
        'counts'
      )
    )
  })
  # check objective function
  # objective function as criterion is for non-transformed parameters:
  purrr::walk2(
    .x = runif(n = 7, min = 0.1, max = 9), #delay1
    .y = runif(n = 7, min = 0.001, max = 2), #rate1
    .f = ~ expect_equal(
      -length(exp_d9) * (log(.y) - .y * (mean(exp_d9) - .x)),
      expected = fd_exp_MLEn_NP$objFun(
        pars = c(delay1 = .x, rate1 = .y),
        isOrig = TRUE
      )
    )
  )

  expect_type(fd_exp_MLEn_P$data, type = 'double')
  expect_identical(length(fd_exp_MLEn_P$data), expected = length(exp_d9))
  expect_identical(fd_exp_MLEn_P$method, expected = "MLEn")
  expect_true(fd_exp_MLEn_P$optimizer$profiled)
  expect_identical(length(coef(fd_exp_MLEn_P)), expected = 2L)
  expect_named(coef(fd_exp_MLEn_P), expected = c("delay1", "rate1"))
  expect_named(fd_exp_MLEn_P$optimizer$parOpt, expected = "delay1_tr")

  # delay1 estimate = min(x)
  expect_identical(coef(fd_exp_MLEn_NP)[['delay1']], expected = exp_d9[[1L]])
  # MLEn's later delay is compensated for by higher estimated rate
  expect_true(all(coef(fd_exp_MLEn_NP) > coef_exp))
  expect_equal(
    coef(fd_exp_MLEn_NP)[['rate1']],
    expected = (mean(exp_d9) - min(exp_d9))**-1
  )

  # MLEc
  fd_exp_MLEc <- delay_model(
    exp_d9,
    distribution = 'expon',
    method = 'MLEc',
    control = list(profiled = FALSE)
  )

  expect_type(fd_exp_MLEc$data, type = 'double')
  expect_identical(length(fd_exp_MLEc$data), expected = length(exp_d9))
  expect_named(coef(fd_exp_MLEc), expected = c("delay1", "rate1"))
  expect_named(
    fd_exp_MLEc$optimizer$parOpt,
    expected = c("delay1_tr", "rate1_tr")
  )

  expect_named(
    fd_exp_MLEc$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      "convergence",
      "message",
      "counts",
      "optim_args"
    )
  )
  expect_identical(
    purrr::chuck(fd_exp_MLEc, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_false(fd_exp_MLEc$optimizer$profiled)
  # no exact solution for MLEc
  expect_null(attr(fd_exp_MLEc$objFun, which = 'opt', exact = TRUE))

  # MLEc profiled
  fd_exp_MLEc_P <- delay_model(
    exp_d9,
    distribution = 'expon',
    method = 'MLEc',
    control = list(profiled = TRUE)
  )
  expect_type(fd_exp_MLEc_P$data, type = 'double')
  expect_identical(length(fd_exp_MLEc_P$data), expected = length(exp_d9))
  expect_named(coef(fd_exp_MLEc_P), expected = c("delay1", "rate1"))
  expect_named(fd_exp_MLEc_P$optimizer$parOpt, expected = "delay1_tr")
  expect_true(fd_exp_MLEc_P$optimizer$profiled)
  # profiled variant is not more difficult optimization than non-profiled
  expect_lte(
    fd_exp_MLEc_P$optimizer$counts[1],
    expected = fd_exp_MLEc$optimizer$counts[1]
  )
  # quite similar coefficients
  expect_equal(
    coef(fd_exp_MLEc_P),
    expected = coef(fd_exp_MLEc),
    tolerance = .01
  )

  # MLEc on duplicated data
  fd_exp_MLEc_dupNP <- delay_model(
    exp_d9dup,
    distribution = 'expon',
    method = 'MLEc'
  )
  expect_named(coef(fd_exp_MLEc_dupNP), expected = c("delay1", "rate1"))
  fd_exp_MLEc_d5_NP <- delay_model(
    exp_d5,
    distribution = "expon",
    method = "MLEc"
  )
  expect_named(coef(fd_exp_MLEc_d5_NP), expected = c("delay1", "rate1"))
  # coefficients are roughly equal (to MPSE-fit)
  expect_equal(
    coef(fd_exp_MLEc_d5_NP),
    expected = coef(fd_exp1_mpseNP),
    tolerance = .1
  )

  # MLEw
  fd_exp_MLEw <- delay_model(exp_d9, distribution = "expon", method = "MLEw")
  expect_identical(fd_exp_MLEw$method, expected = "MLEw")
  expect_true(fd_exp_MLEw$optimizer$profiled)
  expect_type(fd_exp_MLEw$data, type = "double")
  expect_identical(length(fd_exp_MLEw$data), expected = length(exp_d9))
  expect_identical(fd_exp_MLEw$optimizer$convergence, expected = 0L)
  expect_named(coef(fd_exp_MLEw), expected = c("delay1", "rate1"))
  expect_named(fd_exp_MLEw$optimizer$parOpt, expected = "delay1_tr")
  expect_equal(coef(fd_exp_MLEw), coef(fd_exp_MLEc_P), tolerance = .1) # similar coefficients

  # 2nd group ---------------------------------------------------------------

  set.seed(2022 - 04 - 29)
  exp_d10 <- rexp_delayed(27L, delay1 = 10, rate1 = .9)

  fd_exp2 <- delay_model(x = exp_d9, y = exp_d10, distribution = "expon")
  coef_exp2 <- coef(fd_exp2)

  expect_type(fd_exp2$data, type = 'list')
  # data gets sorted
  expect_identical(fd_exp2$data$x, sort(exp_d9))
  expect_identical(fd_exp2$data$y, sort(exp_d10))
  expect_named(
    fd_exp2$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_type(purrr::chuck(fd_exp2, 'optimizer', 'optim_args'), type = 'list')
  # coefficient do not change much when adding a 2nd independent group and no binding
  expect_equal(
    as.numeric(coef_exp2[1:2]),
    expected = as.numeric(coef_exp),
    tolerance = .01
  )

  expect_identical(
    delay_model(x = list(exp_d9, exp_d10), distribution = "expon"),
    expected = fd_exp2
  )
  # x= as list must be of length 2
  expect_error(delay_model(x = list(exp_d9, exp_d10, pi)), regexp = "size 2")
  expect_error(delay_model(x = list(exp_d9, "bla")), regexp = "numeric") # must be numeric

  # no delay in 2nd group
  set.seed(2022 - 11 - 24)
  exp_d0 <- rexp_delayed(13, delay1 = 0, rate1 = 1.3)
  fd_exp2nd <- delay_model(x = exp_d9, y = exp_d0, distribution = "expon") #nd = no delay

  expect_identical(
    purrr::chuck(fd_exp2nd, "optimizer", "convergence"),
    expected = 0L
  )
  expect_lt(coef(fd_exp2nd)[[3]], expected = 0.1) # no substantial delay in 2nd group

  # MLE fits -----
  fd_exp2_MLEn_NP <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    method = "MLEn"
  )
  coef_exp2_MLEn_NP <- coef(fd_exp2_MLEn_NP)
  expect_type(fd_exp2_MLEn_NP$data, type = "list")
  expect_identical(fd_exp2_MLEn_NP$data$y, sort(exp_d10))
  expect_named(
    fd_exp2_MLEn_NP$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2_MLEn_NP, 'optimizer', 'convergence'),
    expected = 0L
  ) # converged
  expect_type(
    purrr::chuck(fd_exp2_MLEn_NP, 'optimizer', 'optim_args'),
    type = 'list'
  )
  # coefficient do not change much when adding a 2nd independent group and no binding
  expect_equal(
    as.numeric(coef_exp2_MLEn_NP[1:2]),
    expected = as.numeric(coef(fd_exp_MLEn_NP)),
    tolerance = .01
  )
  # MLEn fits have larger (=later) delay and (to recover a higher rate)
  purrr::walk(
    .x = seq_along(coef(fd_exp2)),
    .f = ~ expect_gte(coef(fd_exp2_MLEn_NP)[.x], coef(fd_exp2)[.x])
  )

  fd_exp2_MLEn_P <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  coef_exp2_MLEn_P <- coef(fd_exp2_MLEn_P)
  expect_type(fd_exp2_MLEn_P$data, type = "list")
  expect_identical(fd_exp2_MLEn_P$data$y, sort(exp_d10))
  expect_named(
    fd_exp2_MLEn_P$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2_MLEn_P, 'optimizer', 'convergence'),
    expected = 0L
  ) # converged
  expect_type(
    purrr::chuck(fd_exp2_MLEn_P, 'optimizer', 'optim_args'),
    type = 'list'
  )
  expect_equal(coef_exp2_MLEn_P, expected = coef_exp2_MLEn_NP, tolerance = .01) # parameters are quite similar

  fd_exp2_MLEc <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    method = "MLEc"
  )
  expect_type(fd_exp2_MLEc$data, type = "list")
  expect_identical(fd_exp2_MLEc$data$y, sort(exp_d10))
  expect_named(
    fd_exp2_MLEc$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2_MLEc, 'optimizer', 'convergence'),
    expected = 0L
  ) # converged
  expect_type(
    purrr::chuck(fd_exp2_MLEc, 'optimizer', 'optim_args'),
    type = 'list'
  )
  # coefficient do not change much when adding a 2nd independent group and no binding
  expect_equal(
    as.numeric(coef(fd_exp2_MLEc)[1:2]),
    expected = as.numeric(coef(fd_exp_MLEc)),
    tolerance = .001
  )

  fd_exp2_MLEc_P <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    method = "MLEc",
    control = list(profiled = TRUE)
  )
  expect_type(fd_exp2_MLEc_P$data, type = "list")
  expect_true(fd_exp2_MLEc_P$optimizer$profiled)
  expect_identical(fd_exp2_MLEc_P$data$y, sort(exp_d10))
  expect_named(
    fd_exp2_MLEc_P$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2_MLEc_P, 'optimizer', 'convergence'),
    expected = 0L
  ) # converged
  expect_type(
    purrr::chuck(fd_exp2_MLEc_P, 'optimizer', 'optim_args'),
    type = 'list'
  )
  expect_equal(
    coef(fd_exp2_MLEc_P),
    expected = coef(fd_exp2_MLEc),
    tolerance = .01
  )

  # bind delay
  fd_exp2b_NP <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = "delay1"
  )
  coef_exp2b_NP <- coef(fd_exp2b_NP)

  expect_named(coef_exp2b_NP, expected = c("delay1", "rate1.x", "rate1.y"))
  expect_named(
    fd_exp2b_NP$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2b_NP, 'optimizer', 'convergence'),
    expected = 0L
  )
  # the bound delay is near the minimum of the two delay estimates from the individual group fits
  expect_equal(
    coef_exp2b_NP[[1L]],
    expected = min(coef_exp2[grepl(
      pattern = "delay1",
      names(coef_exp2),
      fixed = TRUE
    )]),
    tolerance = .01
  )

  fd_exp2b_P <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = "delay1",
    control = list(profiled = TRUE)
  )
  coef_exp2b_P <- coef(fd_exp2b_P)
  expect_named(coef_exp2b_P, expected = c("delay1", "rate1.x", "rate1.y"))
  expect_identical(
    purrr::chuck(fd_exp2b_P, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_equal(coef_exp2b_P, expected = coef_exp2b_NP, tolerance = .05) # profiled=T/F: parameters are similar

  # bind delay with MLEn
  fd_exp2b_MLEn_NP <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = "delay1",
    method = "MLEn"
  )
  coef_exp2b_MLEn_NP <- coef(fd_exp2b_MLEn_NP)

  expect_named(coef_exp2b_MLEn_NP, expected = c("delay1", "rate1.x", "rate1.y"))
  expect_named(
    fd_exp2b_MLEn_NP$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2b_MLEn_NP, 'optimizer', 'convergence'),
    expected = 0L
  )
  # the bound delay is near the minimum of the two delay estimates from the individual group fits
  expect_equal(
    coef_exp2b_MLEn_NP[[1L]],
    expected = min(coef(fd_exp2_MLEn_NP)[grepl(
      pattern = "delay1",
      names(coef(fd_exp2_MLEn_NP)),
      fixed = TRUE
    )]),
    tolerance = .01
  )

  fd_exp2b_MLEn_P <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = "delay1",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  coef_exp2b_MLEn_P <- coef(fd_exp2b_MLEn_P)
  expect_named(coef_exp2b_MLEn_P, expected = c("delay1", "rate1.x", "rate1.y"))
  expect_named(
    fd_exp2b_MLEn_P$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2b_MLEn_P, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_equal(
    coef_exp2b_MLEn_P,
    expected = coef_exp2b_MLEn_NP,
    tolerance = 1e-3
  ) # profiled=T/F: similar coefficients

  # bind delay with MLEc
  fd_exp2b_MLEc <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = "delay1",
    method = "MLEc"
  )
  coef_exp2b_MLEc <- coef(fd_exp2b_MLEc)

  expect_named(coef_exp2b_MLEc, expected = c("delay1", "rate1.x", "rate1.y"))
  expect_named(
    fd_exp2b_MLEc$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_identical(
    purrr::chuck(fd_exp2b_MLEc, 'optimizer', 'convergence'),
    expected = 0L
  )
  # the bound delay is near the minimum of the two delay estimates from the individual group fits
  expect_equal(
    coef_exp2b_MLEc[[1L]],
    expected = min(coef(fd_exp2_MLEc)[grepl(
      pattern = "delay1",
      names(coef(fd_exp2_MLEc)),
      fixed = TRUE
    )]),
    tolerance = .001
  )

  fd_exp2b_MLEc_P <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = "delay1",
    method = "MLEc",
    control = list(profiled = TRUE)
  )
  coef_exp2b_MLEc_P <- coef(fd_exp2b_MLEc_P)

  expect_named(coef_exp2b_MLEc_P, expected = c("delay1", "rate1.x", "rate1.y"))
  expect_named(
    fd_exp2b_MLEc_P$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  expect_true(fd_exp2b_MLEc_P$optimizer$profiled)
  expect_identical(
    purrr::chuck(fd_exp2b_MLEc_P, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_equal(coef_exp2b_MLEc_P, expected = coef_exp2b_MLEc, tolerance = .005) # profiled=T/F: similar coefficients

  # bind delay + rate
  fd_exp2c_NP <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = c("delay1", "rate1")
  )
  coef_exp2c_NP <- coef(fd_exp2c_NP)
  expect_identical(fd_exp2c_NP$optimizer$convergence, expected = 0L)
  expect_named(coef_exp2c_NP, expected = c("delay1", "rate1"))

  # bind: order of parameters does not matter
  local({
    fd_exp2c_NP2 <- delay_model(
      x = exp_d9,
      y = exp_d10,
      distribution = "expon",
      bind = c("rate1", "delay1")
    )
    expect_identical(coef(fd_exp2c_NP2), expected = coef(fd_exp2c_NP))
    expect_identical(fd_exp2c_NP2$optimizer$convergence, expected = 0L)
    expect_identical(fd_exp2c_NP2$bind, fd_exp2c_NP$bind)
  })

  # profiling and full bind throws a warning:
  #+if rate1 is to be profiled out but later inferred from the delay1-estimate and the observations per group it will not be the same
  fd_exp2c_P <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = c("delay1", "rate1"),
    control = list(profiled = TRUE)
  )
  expect_identical(fd_exp2c_P$optimizer$convergence, expected = 0L)
  expect_true(fd_exp2c_P$optimizer$profiled)
  expect_equal(coef(fd_exp2c_P), coef_exp2c_NP, tolerance = .03) # parameters are similar (but because of bind= & profiled, rate1 is simply averaged)

  # data combined
  fd_exp2comb_NP <- delay_model(x = c(exp_d9, exp_d10), distribution = "expon")
  coef_exp2comb_NP <- coef(fd_exp2comb_NP)
  expect_named(coef_exp2comb_NP, expected = c("delay1", "rate1"))
  expect_identical(length(coef_exp2comb_NP), length(coef_exp2c_NP))

  # similar coefficients (but the criterion is not identical, weighted mean for both groups vs overall mean)
  expect_equal(coef_exp2c_NP[[1L]], coef_exp2comb_NP[[1L]], tolerance = .01)
  #rate1 is actually quite different
  expect_equal(coef_exp2c_NP[[2L]], coef_exp2comb_NP[[2L]], tolerance = .4)

  fd_exp2comb_P <- delay_model(
    x = c(exp_d9, exp_d10),
    distribution = "expon",
    control = list(profiled = TRUE)
  )
  coef_exp2comb_P <- coef(fd_exp2comb_P)
  expect_true(fd_exp2comb_P$optimizer$profiled)

  expect_named(coef_exp2comb_P, expected = c("delay1", "rate1"))
  expect_equal(coef_exp2comb_P, coef_exp2comb_NP, tolerance = .01)

  # bind delay + rate for MLE
  fd_exp2c_MLEn <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = c("delay1", "rate1"),
    method = "MLEn"
  )
  coef_exp2c_MLEn <- coef(fd_exp2c_MLEn)
  fd_exp2comb_MLEn <- delay_model(
    x = c(exp_d9, exp_d10),
    distribution = "expon",
    method = "MLEn"
  )
  coef_exp2comb_MLEn <- coef(fd_exp2comb_MLEn)

  expect_named(coef_exp2c_MLEn, expected = c("delay1", "rate1"))
  expect_identical(length(coef_exp2comb_MLEn), length(coef_exp2c_MLEn))
  expect_equal(coef_exp2c_MLEn[[1]], coef_exp2comb_MLEn[[1]], tolerance = .001)
  # rate quite different!
  expect_equal(coef_exp2c_MLEn[[2]], coef_exp2comb_MLEn[[2]], tolerance = .5)

  # MLEc
  fd_exp2c_MLEc <- delay_model(
    x = exp_d9,
    y = exp_d10,
    distribution = "expon",
    bind = c("delay1", "rate1"),
    method = "MLEc"
  )
  coef_exp2c_MLEc <- coef(fd_exp2c_MLEc)
  fd_exp2comb_MLEc <- delay_model(
    x = c(exp_d9, exp_d10),
    distribution = "expon",
    method = "MLEc"
  )
  coef_exp2comb_MLEc <- coef(fd_exp2comb_MLEc)

  expect_named(coef_exp2c_MLEc, expected = c("delay1", "rate1"))
  expect_identical(length(coef_exp2comb_MLEc), length(coef_exp2c_MLEc))
  expect_equal(coef_exp2c_MLEc[[1]], coef_exp2comb_MLEc[[1]], tolerance = .01)
  expect_equal(coef_exp2c_MLEc[[2]], coef_exp2comb_MLEc[[2]], tolerance = .6) # rate quite different!
})


test_that("MLEw weights", code = {
  #x <- rweib_delayed(n=17, delay1=3, shape1 = 1.8, scale1 = 2)
  x <- c(
    4.45131763598452,
    3.911063873,
    4.73422157,
    3.5065323295733,
    4.50685111984981,
    5.412202336,
    3.80280886136814,
    6.58136664599287,
    4.857188,
    4.73,
    3.37456255179424,
    7.58997290636633,
    4.7848651426,
    4.52811892646,
    4.183,
    3.63462043493189,
    5.631332554
  )
  #y <- rexp_delayed(n=27, delay1 = 5, rate1 = .2)
  y <- c(
    14.925942346,
    5.589837756968,
    7.65,
    8.1183247,
    10.211537615,
    7.2060469,
    7.14108166,
    7.2677,
    6.358104052848,
    15.79,
    10.85657494,
    18.25155829,
    5.02274499745,
    6.5237,
    6.28239889,
    21.79674,
    9.5456935,
    29.344142,
    5,
    7.132323752,
    7.751231,
    5.293446,
    6.7361,
    6.61256539868,
    6.80225579,
    5.8665655646473,
    36.91641441
  )

  # check MLEw-fits in 1- and 2-group setting:
  # depending on slight minute changes in the data the fit can deteriorate
  testMLEwFits <- function(x, y) {
    fmw1x <- delay_model(x = x, distribution = "weibu", method = "MLEw")
    fmw1y <- delay_model(x = y, distribution = "weibu", method = "MLEw")
    fmw2 <- delay_model(x = x, y = y, distribution = "weibu", method = "MLEw")

    # delay estimate quite similar
    expect_equal(
      coef(fmw2, group = "x")[1],
      expected = coef(fmw1x)[1],
      tolerance = .125
    )
    expect_equal(
      coef(fmw2, group = "y")[1],
      expected = coef(fmw1y)[1],
      tolerance = .125
    )

    expect_equal(
      coef(fmw2, group = "x"),
      expected = coef(fmw1x),
      tolerance = .25
    )
    expect_equal(
      coef(fmw2, group = "y"),
      expected = coef(fmw1y),
      tolerance = .25
    )

    # weights function W3 coincides betw. when used alone or when used in two group setting
    purrr::walk(
      .x = c(.1, .5, 1, 1.5, 2, 5),
      .f = ~ expect_equal(
        rlang::env_get(rlang::fn_env(fmw2$objFun), nm = "weights")$W3[["x"]](
          .x
        ),
        expected = rlang::env_get(
          rlang::fn_env(fmw1x$objFun),
          nm = "weights"
        )$W3[["x"]](.x)
      )
    )
    purrr::walk(
      .x = c(.1, .5, 1, 1.5, 2, 5),
      .f = ~ expect_equal(
        rlang::env_get(rlang::fn_env(fmw2$objFun), nm = "weights")$W3[["y"]](
          .x
        ),
        expected = rlang::env_get(
          rlang::fn_env(fmw1y$objFun),
          nm = "weights"
        )$W3[["x"]](.x)
      )
    )
  } #fn

  testMLEwFits(x = x, y = y)
  set.seed(2024 - 05 - 15)
  # non-robust MLEw optimization:
  # tiny noise changes in data can have an impact on the fit under MLEw
  testMLEwFits(x = x, y = y + stats::rnorm(length(y), sd = .005))
  testMLEwFits(x = x, y = y + stats::rnorm(length(y), sd = .01))
})


# censored data no ties
d_cens1 <- local({
  set.seed(2023 - 03 - 24)
  n <- 97L

  datc <- sort(survival::Surv(
    time = 3.5 + stats::rpois(n, lambda = 9.8) + stats::rnorm(n, sd = .1),
    event = sample(c(0, 1, 1, 1, 1), size = n, replace = TRUE)
  ))

  expect_type(datc, type = "double")
  expect_identical(class(datc), expected = "Surv")
  #not all observed times are events
  expect_lt(sum(datc[, 2]), expected = n)

  datc
})


test_that("Censored obs & delayed exponential", {
  fmCR1_mpse <- delay_model(x = d_cens1, distribution = "exponential")
  #plot(fmCR1_mpse)
  fmCR1_mlen <- delay_model(
    x = d_cens1,
    distribution = "exponential",
    method = "MLEn"
  )
  #plot(fmCR1_mlen)
  fmCR1_mlenp <- delay_model(
    x = d_cens1,
    distribution = "exponential",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  # profiling works for MLEn on censored data
  expect_true(fmCR1_mlenp$optimizer$profiled)
  #plot(fmCR1_mlenp)
  fmCR1_mlec <- delay_model(
    x = d_cens1,
    distribution = "exponential",
    method = "MLEc",
    control = list(profiled = FALSE)
  )
  #plot(fmCR1_mlec)
  fmCR1_mlecp <- delay_model(
    x = d_cens1,
    distribution = "exponential",
    method = "MLEc",
    control = list(profiled = TRUE)
  )
  expect_true(fmCR1_mlecp$optimizer$profiled)
  #plot(fmCR1_mlecp)
  fmCR1_mlewp <- delay_model(
    x = d_cens1,
    distribution = "exponential",
    method = "MLEw",
    control = list(profiled = TRUE)
  )
  #plot(fmCR1_mlewp)
  expect_true(fmCR1_mlewp$optimizer$profiled)
  expect_identical(fmCR1_mlewp$optimizer$convergence, 0L)
  expect_equal(coef(fmCR1_mlewp), coef(fmCR1_mlec), tolerance = 1e-2)

  # profiling does not change a lot
  expect_equal(
    coef(fmCR1_mlenp)[1],
    expected = coef(fmCR1_mlen)[1],
    tolerance = 1e-5
  )
  expect_equal(
    coef(fmCR1_mlenp)[2],
    expected = coef(fmCR1_mlen)[2],
    tolerance = 1e-5
  )

  # profiling for MLEc has little impact
  expect_equal(
    coef(fmCR1_mlecp)[1],
    expected = coef(fmCR1_mlec)[1],
    tolerance = 1e-3
  )
  expect_equal(
    coef(fmCR1_mlecp)[2],
    expected = coef(fmCR1_mlec)[2],
    tolerance = 1e-3
  )

  # MLEw similar to MLEc
  #expect_equal(coef(fmCR1_mlewp)[1], expected = coef(fmCR1_mlec)[1], tolerance = 1e-2)
  #expect_equal(coef(fmCR1_mlewp)[2], expected = coef(fmCR1_mlec)[2], tolerance = .15)

  purrr::walk(
    .x = list(fmCR1_mpse, fmCR1_mlec, fmCR1_mlen, fmCR1_mlenp), #fmCR1_mlewp,
    .f = ~ {
      expect_named(
        .x,
        expected = c(
          "data",
          "nobs",
          "distO",
          "twoPhase",
          "twoGroup",
          "method",
          "bind",
          "ties",
          "cens",
          "kmFit",
          "objFun",
          "par",
          "criterion",
          "optimizer"
        )
      )
      expect_named(.x$cens, expected = c("isSurv", "n", "ind", "rcens"))
      expect_true(.x$cens$isSurv)
    }
  )

  # two groups ------------------------------------------------------

  # shorter surv-data that has censorings & no ties
  d_cens2 <- local({
    set.seed(2024 - 06 - 07)
    n <- 19L
    sort(survival::Surv(
      time = 1.5 + rpois(n, lambda = 9.8) + rnorm(n, sd = .1),
      event = sample(c(0, 0, 1, 1, 1), size = n, replace = TRUE)
    ))
  })

  fmcens2_mpse <- delay_model(
    x = d_cens1,
    y = d_cens2,
    distribution = "exponential"
  )
  plot(fmcens2_mpse)

  # tied observations ------

  d_cens3 <- local({
    set.seed(2023 - 05 - 17)
    n <- 17L
    sort(survival::Surv(
      time = 1.5 + rpois(n, lambda = 9.8),
      event = sample(c(0, 0, 1, 1, 1), size = n, replace = TRUE)
    ))[-c(1, 3, 8)]
  })

  fmCR2_mpse <- delay_model(
    x = d_cens3,
    distribution = "expon",
    method = "MPSE"
  )
  expect_identical(fmCR2_mpse$optimizer$convergence, expected = 0L)
  expect_equal(
    fmCR2_mpse$cens$n$x[["right"]],
    expected = sum(d_cens3[, "status"] == 0)
  ) # nbr of right censorings
  # we have three duplicated observed event times
  expect_identical(
    rlang::env_get(rlang::fn_env(fmCR2_mpse$objFun), nm = "tieInfo")$x[[
      "cumDiffInd"
    ]],
    expected = c(7L, 8L, 11L)
  )
})


test_that("Fit delayed Weibull", {
  distO_w <- buildDist(distribution = "weibull")

  # single group ----

  # susquehanna is an example dataset within incubate
  fd_maxFl <- delay_model(susquehanna, distribution = "weib", method = "MPSE")
  coef_maxFl <- coef(fd_maxFl)

  expect_identical(
    purrr::chuck(fd_maxFl, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_lte(fd_maxFl$criterion, expected = 3.0987)
  expect_equal(
    coef_maxFl,
    expected = c(delay1 = 0.244, shape1 = 1.310, scale1 = .202),
    tolerance = .005
  )

  # MLE-based fits to susquehanna --
  fd_maxFl_MLEn <- delay_model(
    susquehanna,
    distribution = "weib",
    method = "MLEn",
    control = list(profiled = FALSE)
  )
  coef_maxFl_MLEn <- coef(fd_maxFl_MLEn)
  expect_false(fd_maxFl_MLEn$optimizer$profiled)
  expect_identical(
    purrr::chuck(fd_maxFl_MLEn, "optimizer", "convergence"),
    expected = 0L
  )
  expect_named(
    fd_maxFl_MLEn$optimizer,
    expected = c(
      "parOpt",
      "valOpt",
      "profiled",
      "methodOpt",
      'convergence',
      'message',
      'counts',
      'optim_args'
    )
  )
  # expect that MLEn gives late delay1 estimate (closer to obs1)
  expect_gte(coef_maxFl_MLEn[["delay1"]], coef_maxFl[["delay1"]])
  expect_lte(coef_maxFl_MLEn[["scale1"]], coef_maxFl[["scale1"]])

  # check the objective function: direct log-likelihodd (criterion=TRUE)
  purrr::pwalk(
    .l = list(
      delay1 = runif(7, max = 0.25),
      shape1 = runif(7, max = 3),
      scale1 = runif(7, max = 5)
    ),
    .f = ~ {
      xc <- susquehanna - ..1
      expect_equal(
        fd_maxFl_MLEn$objFun(
          c(delay1 = ..1, shape1 = ..2, scale1 = ..3),
          isOrig = TRUE
        ),
        expected = -length(susquehanna) *
          (log(..2) -
            ..2 * log(..3) +
            (..2 - 1L) * mean(log(xc)) -
            mean(xc**..2) / ..3**..2)
      )
    }
  )

  fd_maxFl_MLEn_P <- delay_model(
    susquehanna,
    distribution = "weibu",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  expect_true(fd_maxFl_MLEn_P$optimizer$profiled)
  expect_identical(fd_maxFl_MLEn_P$optimizer$convergence, expected = 0L)
  expect_named(
    coef(fd_maxFl_MLEn_P),
    expected = c("delay1", "shape1", "scale1")
  )
  # MLEn profiled vs non-profiled: parameters are close, at least for delay and scale
  expect_equal(
    coef(fd_maxFl_MLEn_P)[c("delay1", "scale1")],
    expected = coef_maxFl_MLEn[c("delay1", "scale1")],
    tolerance = .05
  )
  # similar criterion value in the end
  expect_equal(
    fd_maxFl_MLEn_P$criterion,
    fd_maxFl_MLEn$criterion,
    tolerance = .03
  )
  # Not too many optim-steps necessary:
  expect_lte(mean(fd_maxFl_MLEn_P$optimizer$counts), expected = 50)
  expect_lte(mean(fd_maxFl_MLEn$optimizer$counts), expected = 50)

  # MLEn profiling by hand, using the indirect criterion of min(f')
  llProfObjFun_ind <- function(theta) {
    stopifnot(is.numeric(theta), length(theta) == 2L)
    a <- theta[[1L]] #delay alpha
    k <- theta[[2L]] #shape k
    (1 /
      k +
      mean(log(susquehanna - a)) -
      sum(log(susquehanna - a) * (susquehanna - a)**k) /
        sum((susquehanna - a)**k))^2 +
      # first factor inverse of harmonic mean of (susquehanna - a)
      (mean(1 / (susquehanna - a)) *
        sum((susquehanna - a)**k) /
        sum((susquehanna - a)**(k - 1)) -
        k / (k - 1))^2
  }

  # the indirect objective function variant (using min(f')) is indeed close to 0 at the fit for the coefficients
  expect_equal(
    llProfObjFun_ind(coef(fd_maxFl_MLEn)[1:2]),
    expected = 0,
    tolerance = .001
  )
  #the profiled variant (_P) has coefficients that lead to values that are a little off with drastic consequences
  expect_equal(
    llProfObjFun_ind(coef(fd_maxFl_MLEn_P)[1:2]),
    expected = 0,
    tolerance = .001
  )

  # manual optimization, using the indirect objective function
  local({
    opt_maxFl_MLEn_Pman <- stats::optim(
      par = c(a = 0.255, k = 1.8), #c(a=0.165, k=exp(1.3847)), #c(a=0.25, k=1.5),
      fn = llProfObjFun_ind,
      method = "L-BFGS-B",
      lower = c(0, 1 + 1.49e-8),
      upper = c(min(susquehanna) - 1.49e-8, +Inf)
    )
    expect_equal(opt_maxFl_MLEn_Pman$convergence, 0)
    coef_opt_Pman <- opt_maxFl_MLEn_Pman$par
    coef_maxFl_MLEnp_man <- c(
      coef_opt_Pman,
      mean((susquehanna - coef_opt_Pman["a"])^coef_opt_Pman["k"])^(1 /
        coef_opt_Pman["k"])
    ) |>
      purrr::set_names(nm = c("delay1", "shape1", "scale1"))
    # estimates are only **roughly** equal, at least for delay and scale
    expect_equal(
      coef_maxFl_MLEnp_man[c("delay1", "scale1")],
      expected = coef(fd_maxFl_MLEn_P)[c("delay1", "scale1")],
      tolerance = .3
    )
    # manually reached criterion is similar and a little worse (=higher) than profiling one
    expect_equal(
      fd_maxFl_MLEn_P$objFun(pars = coef_maxFl_MLEnp_man, isOrig = TRUE),
      expected = fd_maxFl_MLEn_P$criterion,
      ignore_attr = "names",
      tolerance = .1
    )
    expect_gte(
      fd_maxFl_MLEn_P$objFun(pars = coef_maxFl_MLEnp_man, isOrig = TRUE),
      expected = fd_maxFl_MLEn_P$criterion
    )
  })

  # MLEw
  fd_maxFl_MLEw <- delay_model(
    x = susquehanna,
    distribution = "weib",
    method = "MLEw"
  )
  expect_identical(fd_maxFl_MLEw$optimizer$convergence, expected = 0L)
  expect_true(fd_maxFl_MLEw$optimizer$profiled)
  expect_named(coef(fd_maxFl_MLEw), expected = c("delay1", "shape1", "scale1"))
  expect_gte(fd_maxFl_MLEw$criterion, fd_maxFl_MLEn$criterion) #MLEn directly optimizes the criterion
  # coefficients are not close, in particular shape can become huge for MLEw
  expect_equal(
    coef(fd_maxFl_MLEw)[-2],
    expected = coef_maxFl[-2],
    tolerance = .33
  )

  #llProfObjFun(coef_maxFl_MLEnp_man[1:2])
  #llProfObjFun(coef(fd_maxFl_MLEnp)[1:2])

  # # visualize the optimization function landscape
  # objFunLS_maxFl_MLEnp <- tidyr::expand_grid(a = seq.int(0, .26, length.out = 17),
  #                                            k = seq.int(1.3, 3, length.out = 17)) |>
  #   #head() |>
  #   rowwise() |>
  #   dplyr::mutate(ll = llProfObjFun(theta = c(a,k))) |>
  #   ungroup()
  #
  # ggplot(objFunLS_maxFl_MLEnp, aes(x = a, y = k, z = log(ll+.1), colour = after_stat(level))) +
  #   geom_contour() +
  #   scale_colour_distiller(palette = "YlGn", direction = -1)

  # pollution is an example dataset within incubate
  fd_poll <- delay_model(pollution, distribution = "weib")
  objFun_poll <- fd_poll$objFun
  coef_poll <- coef(fd_poll)

  expect_identical(
    purrr::chuck(fd_poll, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_identical(length(coef_poll), expected = 3L)
  expect_lt(objFun_poll(coef_poll, isOrig = TRUE), expected = 3.75)
  expect_equal(
    coef_poll,
    expected = c(delay1 = 1085, shape1 = 0.95, scale1 = 6562),
    tolerance = .0001
  )

  # MLE-based fits for pollution
  fd_poll_MLEn <- delay_model(
    pollution,
    distribution = "weibu",
    method = "MLEn",
    control = list(profiled = FALSE)
  )
  coef_poll_MLEn <- coef(fd_poll_MLEn)
  expect_identical(fd_poll_MLEn$optimizer$convergence, expected = 0L)
  expect_named(coef_poll_MLEn, expected = c("delay1", "shape1", "scale1"))
  # naive MLE gives later delay estimates than MPSE, close to minimal observation
  expect_gt(coef_poll_MLEn[["delay1"]], coef_poll[["delay1"]])
  expect_equal(
    coef_poll_MLEn[["delay1"]],
    expected = min(pollution),
    tolerance = 1e-4
  )
  # MLEn allows for shape estimates k below 1
  expect_lt(coef_poll_MLEn[["shape1"]], expected = 1)

  fd_poll_MLEnp <- delay_model(
    pollution,
    distribution = "weibu",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  coef_poll_MLEnp <- coef(fd_poll_MLEnp)
  expect_identical(fd_poll_MLEnp$optimizer$convergence, expected = 0L)
  expect_named(coef_poll_MLEnp, expected = c("delay1", "shape1", "scale1"))
  # MLEn and MLEnp give very similar parameter estimates
  expect_equal(coef_poll_MLEn, expected = coef_poll_MLEnp, tolerance = 1e-2)
  # profiled MLEn allows for shape estimates k below 1
  expect_lt(coef_poll_MLEnp[["shape1"]], expected = 1)

  # MLEw
  fd_poll_MLEw <- delay_model(
    pollution,
    distribution = "weibu",
    method = "MLEw",
    control = list(profiled = TRUE)
  )
  expect_true(fd_poll_MLEw$optimizer$profiled)
  expect_identical(fd_poll_MLEw$optimizer$convergence, expected = 0L)
  expect_named(coef(fd_poll_MLEw), expected = c("delay1", "shape1", "scale1"))

  # Cousineau's numerical example,
  #+originally taken from Weibull with delay = 300, shape k = 2 and scale = 100
  cousPar_true <- c(delay1 = 300, shape1 = 2, scale1 = 100)
  cousEx <- c(310, 342, 353, 365, 383, 393, 403, 412, 451, 456)
  expect_identical(length(cousEx), expected = 10L)
  expect_gte(min(cousEx), expected = cousPar_true[["delay1"]])

  cousPar_mle1 <- structure(
    c(delay1 = 274.8, shape1 = 2.8, scale1 = 126),
    MLE = -51.89
  )
  # Cousineau reports coefs as result from MLE-2step
  #+i.e., indirect optimization, using 1st deriv/gradient to find candidates for local extremum
  cousPar_mle2 <- c(delay1 = 280.9, shape1 = 2.62, scale1 = 119.0)
  cousPar_mlew <- c(delay1 = 283.7, shape1 = 2.29, scale1 = 116.0)

  # naive MLE (MLEn)
  fd_wbc_mlen <- delay_model(x = cousEx, distribution = "weib", method = "MLEn")
  expect_equal(
    coef(fd_wbc_mlen),
    expected = cousPar_mle1,
    tolerance = .0005,
    ignore_attr = "MLE"
  )
  # our criterion is neg. log-likelihood (to be min.)
  expect_equal(
    -fd_wbc_mlen$criterion,
    expected = sum(rlang::exec(
      distO_w$pdf,
      !!!c(coef(fd_wbc_mlen), list(x = cousEx, log = TRUE))
    )) |>
      purrr::set_names(nm = "neg. MLEn"),
    tolerance = .000001
  )
  # Cousineau gives log-lik value (to be max),
  expect_equal(
    logLik(fd_wbc_mlen),
    expected = attr(cousPar_mle1, "MLE") |> purrr::set_names(nm = "MLEn"),
    tolerance = .0005
  )

  # MLEn with profiling
  fd_wbc_mlenp <- delay_model(
    x = cousEx,
    distribution = "weib",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  # MLE with profiling yields same result as MLEn
  expect_equal(
    coef(fd_wbc_mlenp),
    expected = coef(fd_wbc_mlen),
    tolerance = .0005
  )
  # indirect optimization (using 1st deriv) gives similar log-likelihood
  #+BTW, MLEn has slightly better criterion (=smaller neg. log-lik)
  expect_equal(
    fd_wbc_mlen$criterion,
    expected = fd_wbc_mlen$objFun(
      pars = cousPar_mle2,
      isOrig = TRUE,
      criterion = "MLEn"
    ),
    tolerance = .0005
  )

  # MLEw

  # weights from Cousineau (2009)
  fd_wbc_mlew0 <- delay_model(
    x = cousEx,
    distribution = "weib",
    method = "MLEw",
    control = list(profiled = TRUE, MLEw_weight = "cousineau2009")
  )
  expect_identical(fd_wbc_mlew0$optimizer$convergence, expected = 0L)
  expect_type(fd_wbc_mlew0$optimizer$methodOpt, type = "character")
  expect_true(fd_wbc_mlew0$optimizer$profiled)
  expect_equal(coef(fd_wbc_mlew0), expected = cousPar_mlew, tolerance = .15)

  # compare optimization function to be minimized:
  #+the value reached by the incubate package with the weights from Cousineau (2009) is lower than the value from the published fit
  expect_lte(
    fd_wbc_mlew0$optimizer$valOpt,
    expected = fd_wbc_mlew0$objFun(rlang::env_get(
      rlang::fn_env(fd_wbc_mlew0$objFun),
      "extractPars"
    )(parV = cousPar_mlew, isOpt = FALSE, transform = TRUE))
  )
  # but corrected neg. log-likelihood criterion is in fact quite similar
  expect_equal(
    fd_wbc_mlew0$criterion,
    expected = fd_wbc_mlew0$objFun(
      pars = cousPar_mlew,
      isOrig = TRUE,
      criterion = "MLEc"
    ),
    tolerance = 0.05
  )

  # Cousineau GH weights
  fd_wbc_mlew0GH <- delay_model(
    x = cousEx,
    distribution = "weib",
    method = "MLEw",
    control = list(profiled = TRUE, MLEw_weight = "cousineauGH")
  )
  expect_identical(fd_wbc_mlew0GH$optimizer$convergence, expected = 0L)
  expect_type(fd_wbc_mlew0GH$optimizer$methodOpt, type = "character")
  expect_true(fd_wbc_mlew0GH$optimizer$profiled)
  # weights Cousineau 2009 vs Cousineau GH
  #+coefficient estimates differ only little
  expect_equal(
    coef(fd_wbc_mlew0GH),
    expected = coef(fd_wbc_mlew0),
    tolerance = .1
  )

  # weights from our own bigger MC-simulation
  fd_wbc_mlew1 <- delay_model(
    x = cousEx,
    distribution = "weibu",
    method = "MLEw",
    control = list(profiled = TRUE, MLEw_weight = "sdist_median")
  )

  expect_identical(fd_wbc_mlew1$optimizer$convergence, expected = 0L)
  expect_identical(fd_wbc_mlew1$optimizer$methodOpt, expected = "L-BFGS-B")
  expect_true(fd_wbc_mlew1$optimizer$profiled)
  expect_equal(coef(fd_wbc_mlew1), expected = cousPar_mlew, tolerance = .1)
  # our implementation finds a smaller value of our objective function (to be minimized)
  expect_lte(
    fd_wbc_mlew1$optimizer$valOpt,
    expected = fd_wbc_mlew1$objFun(pars = cousPar_mlew, isOrig = TRUE)
  )
  # but neg. log-likelihood criterion is in fact quite similar
  #+actually, Cousineau's solution is slightly better (=smaller) in terms of MLEn (which was not optimized, though)
  expect_equal(
    fd_wbc_mlew1$objFun(
      pars = coef(fd_wbc_mlew1),
      isOrig = TRUE,
      criterion = "MLEn"
    ),
    expected = fd_wbc_mlew1$objFun(
      pars = cousPar_mlew,
      isOrig = TRUE,
      criterion = "MLEn"
    ),
    tolerance = 0.05
  )

  # weights (cousineau2009 or our own MC-sim) matter
  expect_gt(abs(coef(fd_wbc_mlew1)[1] - coef(fd_wbc_mlew0)[1]), expected = 1.5) #delay1 estimate
  expect_gt(abs(coef(fd_wbc_mlew1)[2] - coef(fd_wbc_mlew0)[2]), expected = .025) #shape1 estimate
  expect_gt(abs(coef(fd_wbc_mlew1)[3] - coef(fd_wbc_mlew0)[3]), expected = 1.5) #scale1 estimate

  # wrongly specified control option triggers warning
  expect_warning(
    delay_model(
      x = cousEx,
      distribution = "weibu",
      method = "MLEw",
      control = list(profiled = TRUE, MLEw_weightxx = "cousineau2009")
    ),
    regexp = "Unknown names.+control list"
  )
  # wrongly named arguments are ignored and still give model fit with results!
  expect_identical(
    suppressWarnings(coef(delay_model(
      x = cousEx,
      distribution = "weibu",
      method = "MLEw",
      control = list(profiled = TRUE, MLEw_weightxx = "cousineau2009")
    ))),
    expected = coef(fd_wbc_mlew1)
  )

  # simulate data set with true shape > 1
  set.seed(2021 - 04 - 30)

  # similar parameters
  param_wb_delay1 <- c(x = 7, y = 6)
  stopifnot(stats::var(param_wb_delay1) > 0)
  datw <- list(
    x = rweib_delayed(
      n = 49,
      delay1 = param_wb_delay1[["x"]],
      shape1 = 1.8,
      scale1 = 3
    ),
    y = rweib_delayed(
      n = 51,
      delay1 = param_wb_delay1[["y"]],
      shape1 = 1.2,
      scale1 = 1.5
    )
  )
  attr(datw, which = "param") <- list(
    x = c(delay1 = param_wb_delay1[["x"]], shape1 = 1.8, scale1 = 3),
    y = c(delay1 = param_wb_delay1[["y"]], shape1 = 1.2, scale1 = 1.5)
  )

  param_wb_minDelay1 <- min(param_wb_delay1)
  datw_grpEarlier <- names(which.min(param_wb_delay1)) # which group starts earlier with events (smaller delay)?
  datw_grpLater <- names(which.max(param_wb_delay1)) # which group starts later with events (bigger delay)?

  fd_wb_MLEw_P <- delay_model(
    x = datw[["x"]],
    distribution = "weibu",
    method = "MLEw",
    control = list(profiled = TRUE)
  )
  expect_identical(fd_wb_MLEw_P$optimizer$convergence, expected = 0L)
  # MLEw: check we do not have crazy high shape for group x!
  # the MLEw-fitting for shape is prone to end up with absurd high shape values because shape is in the exponent
  expect_lte(coef(fd_wb_MLEw_P)[["shape1"]], expected = 5)

  # two groups -----

  fd_wb2 <- incubate::delay_model(
    x = susquehanna,
    y = pollution,
    distribution = "weibu"
  )
  coef_wb2 <- coef(fd_wb2)

  expect_identical(
    purrr::chuck(fd_wb2, 'optimizer', 'convergence'),
    expected = 0L
  )
  expect_identical(length(coef_wb2), expected = 2L * 3L)
  expect_named(
    coef_wb2,
    expected = c(
      "delay1.x",
      "shape1.x",
      "scale1.x",
      "delay1.y",
      "shape1.y",
      "scale1.y"
    )
  )
  expect_equal(
    as.numeric(coef_wb2[1:3]),
    expected = as.numeric(coef_maxFl),
    tolerance = .001
  )
  # there might occur quite some differences in the coefficients
  expect_equal(
    as.numeric(coef_wb2[4:6]),
    expected = as.numeric(coef_poll),
    tolerance = .25
  )

  # MLE based fits
  fd_wb2_MLEn <- delay_model(
    x = susquehanna,
    y = pollution,
    distribution = "weib",
    method = "MLEn"
  )
  expect_named(coef(fd_wb2_MLEn), expected = names(coef(fd_wb2)))
  expect_true(fd_wb2_MLEn$optimizer$profiled)
  # MLEn has later delay estimates
  expect_gt(coef(fd_wb2_MLEn)[["delay1.x"]], coef(fd_wb2)[["delay1.x"]])
  expect_gt(coef(fd_wb2_MLEn)[["delay1.y"]], coef(fd_wb2)[["delay1.y"]])

  fd_wb2_MLEn_P <- delay_model(
    x = susquehanna,
    y = pollution,
    distribution = "weibu",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  expect_true(fd_wb2_MLEn_P$optimizer$profiled)
  expect_identical(fd_wb2_MLEn_P$optimizer$convergence, expected = 0L)
  # roughly equal coefficients as compared to single fits
  expect_equal(
    coef(fd_wb2_MLEn_P, group = "x"),
    expected = coef(fd_maxFl_MLEn_P),
    tolerance = .01
  )
  expect_equal(
    coef(fd_wb2_MLEn_P, group = "y"),
    expected = coef(fd_poll_MLEnp),
    tolerance = .01
  )

  # MLEc
  fd_wb2_MLEc_NP <- delay_model(
    x = susquehanna,
    y = pollution,
    distribution = "weibu",
    method = "MLEc",
    control = list(profiled = FALSE)
  )
  expect_named(coef(fd_wb2_MLEc_NP), expected = names(coef(fd_wb2)))
  expect_false(fd_wb2_MLEc_NP$optimizer$profiled)
  expect_equal(coef(fd_wb2_MLEc_NP), expected = coef(fd_wb2), tolerance = .3)
  expect_gt(coef(fd_wb2_MLEc_NP)[["delay1.x"]], coef(fd_wb2)[["delay1.x"]])
  expect_gt(coef(fd_wb2_MLEc_NP)[["delay1.y"]], coef(fd_wb2)[["delay1.y"]])

  fd_wb2_MLEc_P <- delay_model(
    x = susquehanna,
    y = pollution,
    distribution = "weibu",
    method = "MLEc",
    control = list(profiled = TRUE)
  )
  expect_true(fd_wb2_MLEc_P$optimizer$profiled)
  expect_identical(fd_wb2_MLEc_P$optimizer$convergence, expected = 0L)
  # MLEc profiling has little impact
  expect_equal(
    coef(fd_wb2_MLEc_P),
    expected = coef(fd_wb2_MLEc_NP),
    tolerance = .005
  )

  # MLEw with two groups
  fd_wb2_MLEw <- delay_model(
    x = susquehanna,
    y = pollution,
    distribution = "weibu",
    method = "MLEw",
    control = list(profiled = TRUE)
  )
  expect_named(coef(fd_wb2_MLEw), expected = names(coef(fd_wb2)))
  # MLEc-criterion is very similar when compared with fit from individual models
  expect_equal(
    fd_wb2_MLEw$criterion,
    expected = fd_wb2_MLEw$objFun(
      pars = c(coef(fd_maxFl_MLEw), coef(fd_poll_MLEw)),
      isOrig = TRUE,
      criterion = "MLEc"
    ),
    tolerance = .01
  )
  # coefficients might well be quite far off because groups' scales are so different.
  #+It depends on platform
  expect_equal(
    coef(fd_wb2_MLEw, group = "x"),
    expected = coef(fd_maxFl_MLEw),
    tolerance = .25
  )
  expect_equal(
    coef(fd_wb2_MLEw, group = "y"),
    expected = coef(fd_poll_MLEw),
    tolerance = .25
  )

  # two groups, bind delay1 -------------------------------------------------

  stopifnot(is.list(datw), identical(names(datw), c("x", "y")))

  fd_wb2b <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MPSE",
    bind = "delay1"
  )
  coef_wb2b <- coef(fd_wb2b)

  # we can provide a list with two entries as well (instead of x= and y= separately)
  expect_identical(
    delay_model(x = datw$x, y = datw$y, distribution = "weib", bind = "delay1"),
    expected = fd_wb2b
  )
  expect_identical(
    purrr::chuck(fd_wb2b, 'optimizer', 'convergence'),
    expected = 0L
  )
  # fewer parameters because delay is bound
  expect_identical(length(coef_wb2b), expected = 2L * 3L - 1L)
  expect_named(
    coef_wb2b,
    c("delay1", "shape1.x", "scale1.x", "shape1.y", "scale1.y")
  )
  # expect a delay close to the minimum of the two true delay parameters
  expect_equal(coef_wb2b[[1L]], expected = param_wb_minDelay1, tolerance = .05)

  # compare two-group model with individual one-group model fit of earlier group
  expect_equal(
    coef(fd_wb2b, group = datw_grpEarlier),
    expected = coef(delay_model(
      x = datw[[datw_grpEarlier]],
      distribution = "weib"
    )),
    tolerance = .075
  )

  # model fit resembles the true underlying model:

  # estimated delay is close to smallest delay in both groups
  expect_equal(
    coef_wb2b[["delay1"]],
    expected = param_wb_minDelay1,
    tolerance = .05
  )

  # group with shorter delay has a good model fit parameter wise (also for shape and scale)
  expect_equal(
    coef(fd_wb2b, group = datw_grpEarlier),
    expected = purrr::chuck(attr(datw, "param"), datw_grpEarlier),
    tolerance = .15
  )

  # MLE fits
  fd_wb2b_MLEn <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEn",
    bind = "delay1"
  )
  expect_identical(fd_wb2b_MLEn$optimizer$convergence, expected = 0L)
  expect_named(
    coef(fd_wb2b_MLEn),
    c("delay1", "shape1.x", "scale1.x", "shape1.y", "scale1.y")
  )
  expect_equal(coef(fd_wb2b_MLEn), coef_wb2b, tolerance = .05)
  #delay is bound
  expect_identical(length(coef(fd_wb2b_MLEn)), expected = 2L * 3L - 1L)
  expect_named(
    coef(fd_wb2b_MLEn),
    c("delay1", "shape1.x", "scale1.x", "shape1.y", "scale1.y")
  )
  # expect a delay close to the minimum of the two true delay parameters
  expect_equal(
    coef(fd_wb2b_MLEn)[[1L]],
    expected = param_wb_minDelay1,
    tolerance = .03
  )

  fd_wb2b_MLEn_P <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEn",
    bind = "delay1",
    control = list(profiled = TRUE)
  )
  expect_true(fd_wb2b_MLEn_P$optimizer$profiled)
  expect_identical(fd_wb2b_MLEn_P$optimizer$convergence, expected = 0L)
  expect_equal(coef(fd_wb2b_MLEn_P), coef(fd_wb2b_MLEn), tolerance = .01) # profiling has little effect on coefficients

  fd_wb2b_MLEc <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEc",
    bind = "delay1",
    control = list(profiled = FALSE)
  )
  expect_false(fd_wb2b_MLEc$optimizer$profiled)
  expect_identical(fd_wb2b_MLEc$optimizer$convergence, expected = 0L)
  #delay is bound
  expect_identical(length(coef(fd_wb2b_MLEc)), expected = 2L * 3L - 1L)
  expect_named(
    coef(fd_wb2b_MLEc),
    c("delay1", "shape1.x", "scale1.x", "shape1.y", "scale1.y")
  )
  # expect a delay close to the minimum of the two true delay parameters
  expect_equal(
    coef(fd_wb2b_MLEc)[[1L]],
    expected = param_wb_minDelay1,
    tolerance = .03
  )
  # group with shorter delay has a good model fit parameter wise
  expect_equal(
    coef(fd_wb2b_MLEc, group = datw_grpEarlier)[-1L],
    expected = purrr::chuck(attr(datw, "param"), datw_grpEarlier)[-1L],
    tolerance = .15
  )

  fd_wb2b_MLEc_P <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEc",
    control = list(profiled = TRUE),
    bind = "delay1"
  )
  expect_true(fd_wb2b_MLEc_P$optimizer$profiled)
  expect_identical(fd_wb2b_MLEc_P$optimizer$convergence, expected = 0L)
  expect_named(
    coef(fd_wb2b_MLEc_P),
    c("delay1", "shape1.x", "scale1.x", "shape1.y", "scale1.y")
  )
  expect_equal(coef(fd_wb2b_MLEc_P), coef(fd_wb2b_MLEc), tolerance = .001) # profiling has hardly an effect on the coefficients

  # estimated delay is close to smallest delay in both groups
  expect_equal(
    coef(fd_wb2b_MLEc_P)[["delay1"]],
    expected = param_wb_minDelay1,
    tolerance = .03
  )

  # group with shorter delay has a good model fit parameter wise
  expect_equal(
    coef(fd_wb2b_MLEc_P, group = datw_grpEarlier)[-1L],
    expected = purrr::chuck(attr(datw, "param"), datw_grpEarlier)[-1L],
    tolerance = .15
  )

  # MLEw fit with delay1 bound
  fd_wb2b_MLEw_P <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEw",
    bind = "delay1",
    control = list(verbose = 0, profiled = TRUE)
  )
  expect_true(fd_wb2b_MLEw_P$optimizer$profiled)
  expect_identical(fd_wb2b_MLEw_P$optimizer$convergence, expected = 0L)
  #delay is bound
  expect_identical(length(coef(fd_wb2b_MLEw_P)), expected = 2L * 3L - 1L)
  expect_named(
    coef(fd_wb2b_MLEw_P),
    c("delay1", "shape1.x", "scale1.x", "shape1.y", "scale1.y")
  )
  # expect a delay close to the minimum of the two true delay parameters
  expect_equal(
    coef(fd_wb2b_MLEw_P)[["delay1"]],
    expected = param_wb_minDelay1,
    tolerance = .03
  )
  # group with shorter delay has a good model fit parameter wise
  expect_equal(
    coef(fd_wb2b_MLEw_P, group = datw_grpEarlier)[-1L],
    expected = purrr::chuck(datw, purrr::attr_getter("param"), datw_grpEarlier)[
      -1L
    ],
    tolerance = .25
  )
  # MLEw: check we do not have crazy high shape for group x!
  #+ optimization easily goes wrong on shape, because shape occurs in the exponent
  #+ this can also happen for a single group (see above)
  expect_lte(coef(fd_wb2b_MLEw_P, group = "x")[["shape1"]], expected = 10)

  # MLE-weights:
  # weights are unchanged if fit 2-group or 1-group for earlier group

  # single group fit
  fd_wb2_MLEw_1gr <- list(
    x = delay_model(datw$x, distribution = "weib", method = "MLEw"),
    y = delay_model(datw$y, distribution = "weib", method = "MLEw")
  )

  weights_wb2_earlier <- rlang::env_get(
    rlang::fn_env(fd_wb2_MLEw_1gr[[datw_grpEarlier]]$objFun),
    nm = "weights"
  )
  weights_wb2_later <- rlang::env_get(
    rlang::fn_env(fd_wb2_MLEw_1gr[[datw_grpLater]]$objFun),
    nm = "weights"
  )

  weights_wb2b <- rlang::env_get(
    rlang::fn_env(fd_wb2b_MLEw_P$objFun),
    nm = "weights"
  )

  shapeVals <- c(5, 10, 15, 20, 25, 30, 40, 50)
  expect_identical(
    weights_wb2b[["W1"]][[datw_grpEarlier]],
    expected = weights_wb2_earlier[["W1"]][[1L]]
  )
  expect_identical(
    weights_wb2b[["W2"]][[datw_grpEarlier]],
    expected = weights_wb2_earlier[["W2"]][[1L]]
  )
  expect_identical(
    weights_wb2b[["W3"]][[datw_grpEarlier]](k = shapeVals),
    expected = weights_wb2_earlier[["W3"]][[1L]](k = shapeVals)
  )

  expect_identical(
    weights_wb2b[["W1"]][[datw_grpLater]],
    expected = weights_wb2_later[["W1"]][[1L]]
  )
  expect_identical(
    weights_wb2b[["W2"]][[datw_grpLater]],
    expected = weights_wb2_later[["W2"]][[1L]]
  )
  expect_identical(
    weights_wb2b[["W3"]][[datw_grpLater]](k = shapeVals),
    expected = weights_wb2_later[["W3"]][[1L]](k = shapeVals)
  )

  # two groups, bind shape -----

  fd_wb2bb <- delay_model(x = datw, distribution = "weib", bind = "shape1")
  coef_wb2bb <- coef(fd_wb2bb)

  expect_identical(fd_wb2bb$optimizer$convergence, expected = 0L)
  expect_identical(length(coef_wb2bb), expected = 2L * 3L - 1L)
  expect_identical(
    names(coef_wb2bb),
    c("shape1", "delay1.x", "scale1.x", "delay1.y", "scale1.y")
  )

  fd_wb2bb_MLEn_NP <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEn",
    bind = "shape1"
  )
  expect_identical(fd_wb2bb_MLEn_NP$optimizer$convergence, expected = 0L)
  expect_identical(
    names(coef(fd_wb2bb_MLEn_NP)),
    c("shape1", "delay1.x", "scale1.x", "delay1.y", "scale1.y")
  )
  expect_equal(coef(fd_wb2bb_MLEn_NP), coef_wb2bb, tolerance = .05)

  fd_wb2bb_MLEn_P <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEn",
    control = list(profiled = TRUE),
    bind = "shape1"
  )
  expect_identical(fd_wb2bb_MLEn_P$optimizer$convergence, expected = 0L)
  expect_true(fd_wb2bb_MLEn_P$optimizer$profiled)
  expect_identical(
    names(coef(fd_wb2bb_MLEn_P)),
    c("shape1", "delay1.x", "scale1.x", "delay1.y", "scale1.y")
  )
  expect_equal(coef(fd_wb2bb_MLEn_P), coef(fd_wb2bb_MLEn_NP), tolerance = .01) # profiling has hardly an effect

  fd_wb2bb_MLEc <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEc",
    control = list(profiled = FALSE),
    bind = "shape1"
  )
  expect_false(fd_wb2bb_MLEc$optimizer$profiled)
  expect_identical(fd_wb2bb_MLEc$optimizer$convergence, expected = 0L)
  expect_identical(
    names(coef(fd_wb2bb_MLEc)),
    c("shape1", "delay1.x", "scale1.x", "delay1.y", "scale1.y")
  )
  fd_wb2bb_MLEc_P <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEc",
    control = list(profiled = TRUE),
    bind = "shape1"
  )
  expect_true(fd_wb2bb_MLEc_P$optimizer$profiled)
  expect_identical(fd_wb2bb_MLEc_P$optimizer$convergence, expected = 0L)
  expect_equal(coef(fd_wb2bb_MLEc_P), coef(fd_wb2bb_MLEc), tolerance = 1e-3) #profiling has little impact

  fd_wb2bb_MLEw_P <- delay_model(
    x = datw,
    distribution = "weib",
    method = "MLEw",
    control = list(profiled = TRUE),
    bind = "shape1"
  )
  expect_identical(fd_wb2bb_MLEw_P$optimizer$convergence, expected = 0L)
  expect_identical(
    names(coef(fd_wb2bb_MLEw_P)),
    c("shape1", "delay1.x", "scale1.x", "delay1.y", "scale1.y")
  )
  # check that MLEw and MLEc coefficients are "roughly" similar
  # MLEw can be sometimes far off
  expect_equal(
    coef(fd_wb2bb_MLEw_P),
    expected = coef(fd_wb2bb_MLEc_P),
    tolerance = .1
  )
})


test_that("Censored obs & weibull", {
  ## Weibull model fits --
  fmCR1w_mpse <- delay_model(x = d_cens1, distribution = "weibu")
  #plot(fmCR1w_mpse)
  fmCR1w_mlen <- delay_model(
    x = d_cens1,
    distribution = "weibu",
    method = "MLEn"
  )
  #plot(fmCR1w_mlen)
  fmCR1w_mlenp <- delay_model(
    x = d_cens1,
    distribution = "weibu",
    method = "MLEn",
    control = list(profiled = TRUE)
  )
  #plot(fmCR1w_mlenp)
  expect_true(fmCR1w_mlenp$optimizer$profiled)
  fmCR1w_mlec <- delay_model(
    x = d_cens1,
    distribution = "weibu",
    method = "MLEc",
    control = list(profiled = FALSE)
  )
  #plot(fmCR1w_mlec)
  fmCR1w_mlecp <- delay_model(
    x = d_cens1,
    distribution = "weibu",
    method = "MLEc",
    control = list(profiled = TRUE)
  )
  expect_true(fmCR1w_mlecp$optimizer$profiled)
  expect_identical(fmCR1w_mlecp$optimizer$convergence, 0L)
  #plot(fmCR1w_mlecp)
  fmCR1w_mlewp <- delay_model(
    x = d_cens1,
    distribution = "weibu",
    method = "MLEw",
    control = list(profiled = TRUE)
  )
  #plot(fmCR1w_mlewp)
  expect_true(fmCR1w_mlewp$optimizer$profiled)
  expect_identical(fmCR1w_mlewp$optimizer$convergence, 0L)

  # delay and shape parameters do not change much when profiling (in particular, if enough data is available)
  expect_equal(
    coef(fmCR1w_mlenp),
    expected = coef(fmCR1w_mlen),
    tolerance = .05
  )
  # profiling leads to better result upon optimization (because it is easier)
  expect_lte(fmCR1w_mlenp$criterion, expected = fmCR1w_mlen$criterion)

  # MLEw worse criterion (higher) than MLEc (because both use MLEc and MLEc is directly optimized for it)
  expect_gte(fmCR1w_mlewp$criterion, fmCR1w_mlec$criterion)
  #expect_gte(coef(fmCR1w_mlewp)[1], expected = 2)
  #expect_equal(coef(fmCR1w_mlewp), expected = coef(fmCR1w_mpse), tolerance = .45)

  purrr::walk(
    .x = list(
      fmCR1w_mpse,
      fmCR1w_mlen,
      fmCR1w_mlenp,
      fmCR1w_mlec,
      fmCR1w_mlecp
    ), #, fmCR1w_mlewp),))
    .f = ~ {
      expect_named(
        .x,
        expected = c(
          "data",
          "nobs",
          "distO",
          "twoPhase",
          "twoGroup",
          "method",
          "bind",
          "ties",
          "cens",
          "kmFit",
          "objFun",
          "par",
          "criterion",
          "optimizer"
        )
      )
      expect_named(.x$cens, expected = c("isSurv", "n", "ind", "rcens"))
      expect_true(.x$cens$isSurv)
    }
  )
})


test_that("Confidence intervals", code = {
  testthat::skip_on_cran()

  set.seed(1234)

  obs_sim <- rexp_delayed(n = 29L, delay = 5, rate = .3)

  fm <- delay_model(x = obs_sim, distribution = 'expon')

  # use default smd_factor
  bs_data <- incubate:::bsDataStep(fm, R = 1199, useBoot = FALSE)
  # set up identical bs_data from boot-package
  bs_data_bt <- incubate:::bsDataStep(fm, R = 3, useBoot = TRUE)
  bs_data_bt$R <- NCOL(bs_data)
  bs_data_bt$t <- t(bs_data)

  # agreement of own implementation and boot-implementation
  purrr::walk(
    .x = c('normal', 'lognormal', 'quantile', 'logquantile', 'quantile0'),
    .f = ~ {
      ci_own <- confint(fm, bs_data = bs_data, bs_infer = .x)
      ci_boot <- confint(
        fm,
        bs_data = bs_data_bt,
        bs_infer = .x,
        useBoot = TRUE
      )
      expect_equal(ci_own, ci_boot, tolerance = 1e-3)
    }
  )
})


test_that("Fit normal", {
  # MPSE fit
  fm_nrm_grph <- delay_model(
    x = incubate::graphite,
    distribution = "normal",
    method = "MPSE",
    control = list(ties = "density")
  )
  gof_nrm_grph <- test_GOF(fm_nrm_grph)

  # cf Cheng & Stephens (1989), 4.3 Example
  expect_identical(fm_nrm_grph$optimizer$convergence, 0L)
  expect_equal(
    coef(fm_nrm_grph),
    expected = c(mean = 34.072, sd = sqrt(6.874)),
    tolerance = 1e-3
  )
  expect_equal(
    gof_nrm_grph$statistic,
    expected = c(`X^2` = 63.1),
    tolerance = 1e-3
  )
  expect_equal(
    stats::qchisq(p = 0.05, df = gof_nrm_grph$df, lower.tail = FALSE),
    expected = 56.9,
    tolerance = 1e-3
  )

  # MLE fit
  fm_nrm_grph_MLEn <- delay_model(
    x = incubate::graphite,
    distribution = "normal",
    method = "MLEn",
    control = list(ties = "density", profiled = FALSE)
  )
  expect_identical(fm_nrm_grph_MLEn$optimizer$convergence, 0L)
  expect_false(fm_nrm_grph_MLEn$optimizer$profiled)
  expect_equal(
    coef(fm_nrm_grph_MLEn),
    expected = c(
      mean = mean(incubate::graphite),
      sd = sqrt(
        stats::var(incubate::graphite) *
          (length(incubate::graphite) - 1) /
          length(incubate::graphite)
      )
    ),
    tolerance = 1e-5
  )

  # two group
  fm_nrm_2gr <- delay_model(
    x = incubate::graphite,
    y = incubate::fatigue,
    distribution = "normal",
    method = "MPSE",
    control = list(ties = "density")
  )
  expect_identical(fm_nrm_2gr$optimizer$convergence, 0L)
  expect_named(
    coef(fm_nrm_2gr),
    expected = c("mean.x", "sd.x", "mean.y", "sd.y")
  )
  expect_equal(
    coef(fm_nrm_2gr)[1:2],
    expected = coef(fm_nrm_grph),
    tolerance = 1e-5,
    ignore_attr = "names"
  )

  # bind is working
  fm_nrm_2grb <- delay_model(
    x = incubate::graphite,
    y = sqrt(incubate::fatigue),
    distribution = "normal",
    method = "MPSE",
    bind = "sd",
    control = list(ties = "density")
  )
  expect_identical(fm_nrm_2grb$optimizer$convergence, 0L)
  expect_named(coef(fm_nrm_2grb), expected = c("sd", "mean.x", "mean.y"))
})


test_that("Variance of fitted model", {
  set.seed(2026 - 05 - 04)

  # generate some small data set with known parameters
  obs_sim <- rexp_delayed(n = 29L, delay = 7, rate = .3)

  fms <- purrr::map(
    .x = purrr::set_names(c("weibull", "exponential", "normal")),
    .f = function(.x) delay_model(x = obs_sim, distribution = .x)
  )

  expect_equal(length(fms), expected = 3L)
  expect_equal(
    purrr::map_dbl(fms, function(fm) getVariance(fm, type = "distribution")),
    expected = c(
      # variance as function of distribution parameters
      weibull = coef(fms[["weibull"]])[["scale1"]]^2 *
        (gamma(1 + 2 / coef(fms[["weibull"]])[["shape1"]]) -
          gamma(1 + 1 / coef(fms[["weibull"]])[["shape1"]])^2),
      exponential = (1 / coef(fms[["exponential"]])[["rate1"]])^2,
      normal = coef(fms[["normal"]])[["sd"]]^2
    ),
    tolerance = .0001
  )

  # huge data sets: compare empirical variance with modelled variance
  # the fitting method should not matter much
  # weibull data
  weibuPars <- c(delay1 = 9, shape1 = 1.5, scale1 = 3)
  obs_sim_weibu_huge <- rlang::exec(
    .fn = rweib_delayed,
    !!!c(list(n = 2 * 3 * 1e5), weibuPars)
  )

  expect_equal(
    getVariance(
      delay_model(
        x = obs_sim_weibu_huge,
        distribution = "weibull",
        method = "MPSE"
      ),
      type = "distribution"
    ),
    stats::var(obs_sim_weibu_huge),
    tolerance = .01
  )

  # check the formula for CDF of minimum
  purrr::walk2(
    .x = weibuPars[["delay1"]] * c(.95, 1, 1.01, 1.02, 1.05, 1.1),
    .y = c(10, 10, 12, 25, 25, 25),
    .f = function(.q, .n) {
      expect_equal(
        1 -
          rlang::exec(
            .fn = pweib_delayed,
            !!!c(list(q = .q, lower.tail = FALSE), weibuPars)
          )^.n,
        mean(
          apply(
            matrix(obs_sim_weibu_huge, ncol = .n),
            MARGIN = 1,
            min,
            simplify = TRUE
          ) <=
            .q
        ),
        tolerance = .05
      )
    }
  )

  # check the variance of the minimum of n weibull distributed variables with the formula
  purrr::walk(.x = c(10, 15, 20), .f = function(.n) {
    local({
      fm_weibu <- delay_model(
        x = obs_sim_weibu_huge[seq_len(.n)],
        distribution = "weibull",
        method = "MPSE"
      )

      expect_equal(
        object = {
          getVariance(
            fm_weibu,
            type = "minimum"
          )
        },
        expected = stats::var(
          apply(
            matrix(
              rlang::exec(
                .fn = rweib_delayed,
                !!!c(list(n = fm_weibu$nobs[[1L]] * 3e5), coef(fm_weibu))
              ),
              ncol = fm_weibu$nobs[[1L]]
            ),
            MARGIN = 1,
            min,
            simplify = TRUE
          )
        ),
        tolerance = .01
      )
    })
  })

  # exponential data
  obs_sim_expon_huge <- rexp_delayed(n = 2 * 3 * 1e5, delay = 9, rate = .3)
  expect_equal(
    getVariance(
      delay_model(
        x = obs_sim_expon_huge,
        distribution = "exponential",
        method = "MPSE"
      ),
      type = "distribution"
    ),
    stats::var(obs_sim_expon_huge),
    tolerance = .01
  )
})
