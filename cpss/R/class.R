#' cpss: an \proglang{S4} class which collects data and information required for further change-point analyses and summaries
#'
#' @slot dat ANY.
#' @slot mdl character.
#' @slot algo character.
#' @slot algo_param_dim numeric.
#' @slot SC character.
#' @slot ncps integer.
#' @slot pelt_pen numeric.
#' @slot cps numeric.
#' @slot params list.
#' @slot S_vals numeric.
#' @slot SC_vals matrix.
#' @slot call list.
#' @slot update_inputs list.
#'
#' @export
#'
#' @importFrom methods setClass setGeneric setMethod
#' @importFrom stats sigma
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_point geom_line geom_vline geom_tile geom_text aes facet_grid vars labs scale_color_manual scale_fill_gradient2 theme element_blank theme_light
#' @importFrom gridExtra grid.arrange
setClass(
  "cpss",
  slots = c(
    dat = "ANY",
    mdl = "character",
    algo = "character",
    algo_param_dim = "numeric",
    SC = "character",
    ncps = "integer",
    pelt_pen = "numeric",
    cps = "numeric",
    params = "list",
    S_vals = "numeric",
    SC_vals = "matrix",
    call = "list",
    update_inputs = "list"
  )
)

#' Generic functions and methods: dat
#'
#' @param cpss cpss class
#' @param x object from cpss
#' @param value value assigned to x
#' @name dat
NULL
#> NULL

#' @rdname dat
#' @export
setGeneric("dat", function(x)
  standardGeneric("dat"))
#' @rdname dat
#' @export
setGeneric("dat<-", function(x, value)
  standardGeneric("dat<-"))
#' @rdname dat
#' @export
setMethod("dat", "cpss", function(x)
  x@dat)
#' @rdname dat
#' @export
setMethod("dat<-", "cpss", function(x, value) {
  x@dat <- value
  x
})

#' Generic functions and methods: mdl
#'
#' @inheritParams dat
#' @name mdl
NULL
#> NULL

#' @rdname mdl
#' @export
setGeneric("mdl", function(x)
  standardGeneric("mdl"))
#' @rdname mdl
#' @export
setGeneric("mdl<-", function(x, value)
  standardGeneric("mdl<-"))
#' @rdname mdl
#' @export
setMethod("mdl", "cpss", function(x)
  x@mdl)
#' @rdname mdl
#' @export
setMethod("mdl<-", "cpss", function(x, value) {
  x@mdl <- value
  x
})

#' Generic functions and methods: algo
#'
#' @inheritParams dat
#' @name algo
NULL
#> NULL

#' @rdname algo
#' @export
setGeneric("algo", function(x)
  standardGeneric("algo"))
#' @rdname algo
#' @export
setGeneric("algo<-", function(x, value)
  standardGeneric("algo<-"))
#' @rdname algo
#' @export
setMethod("algo", "cpss", function(x)
  x@algo)
#' @rdname algo
#' @export
setMethod("algo<-", "cpss", function(x, value) {
  x@algo <- value
  x
})

#' Generic functions and methods: algo_param_dim
#'
#' @inheritParams dat
#' @name algo_param_dim
NULL
#> NULL

#' @rdname algo_param_dim
#' @export
setGeneric("algo_param_dim", function(x)
  standardGeneric("algo_param_dim"))
#' @rdname algo_param_dim
#' @export
setGeneric("algo_param_dim<-", function(x, value)
  standardGeneric("algo_param_dim<-"))
#' @rdname algo_param_dim
#' @export
setMethod("algo_param_dim", "cpss", function(x)
  x@algo_param_dim)
#' @rdname algo_param_dim
#' @export
setMethod("algo_param_dim<-", "cpss", function(x, value) {
  x@algo_param_dim <- value
  x
})

#' Generic functions and methods: SC
#'
#' @inheritParams dat
#' @name SC
NULL
#> NULL

#' @rdname SC
#' @export
setGeneric("SC", function(x)
  standardGeneric("SC"))
#' @rdname SC
#' @export
setGeneric("SC<-", function(x, value)
  standardGeneric("SC<-"))
#' @rdname SC
#' @export
setMethod("SC", "cpss", function(x)
  x@SC)
#' @rdname SC
#' @export
setMethod("SC<-", "cpss", function(x, value) {
  x@SC <- value
  x
})

#' Generic functions and methods: ncps
#'
#' @inheritParams dat
#' @name ncps
NULL
#> NULL

#' @rdname ncps
#' @export
setGeneric("ncps", function(x)
  standardGeneric("ncps"))
#' @rdname ncps
#' @export
setGeneric("ncps<-", function(x, value)
  standardGeneric("ncps<-"))
#' @rdname ncps
#' @export
setMethod("ncps", "cpss", function(x)
  x@ncps)
#' @rdname ncps
#' @export
setMethod("ncps<-", "cpss", function(x, value) {
  x@ncps <- value
  x
})

#' Generic functions and methods: pelt_pen
#'
#' @inheritParams dat
#' @name pelt_pen
NULL
#> NULL

#' @rdname pelt_pen
#' @export
setGeneric("pelt_pen", function(x)
  standardGeneric("pelt_pen"))
#' @rdname pelt_pen
#' @export
setGeneric("pelt_pen<-", function(x, value)
  standardGeneric("pelt_pen<-"))
#' @rdname pelt_pen
#' @export
setMethod("pelt_pen", "cpss", function(x)
  x@pelt_pen)
#' @rdname pelt_pen
#' @export
setMethod("pelt_pen<-", "cpss", function(x, value) {
  x@pelt_pen <- value
  x
})

#' Generic functions and methods: cps
#'
#' @inheritParams dat
#' @name cps
NULL
#> NULL

#' @rdname cps
#' @export
setGeneric("cps", function(x)
  standardGeneric("cps"))
#' @rdname cps
#' @export
setGeneric("cps<-", function(x, value)
  standardGeneric("cps<-"))
#' @rdname cps
#' @export
setMethod("cps", "cpss", function(x)
  x@cps)
#' @rdname cps
#' @export
setMethod("cps<-", "cpss", function(x, value) {
  x@cps <- value
  x
})

#' Generic functions and methods: params
#'
#' @inheritParams dat
#' @name params
NULL
#> NULL

#' @rdname params
#' @export
setGeneric("params", function(x)
  standardGeneric("params"))
#' @rdname params
#' @export
setGeneric("params<-", function(x, value)
  standardGeneric("params<-"))
#' @rdname params
#' @export
setMethod("params", "cpss", function(x)
  x@params)
#' @rdname params
#' @export
setMethod("params<-", "cpss", function(x, value) {
  x@params <- value
  x
})

#' Generic functions and methods: S_vals
#'
#' @inheritParams dat
#' @name S_vals
NULL
#> NULL

#' @rdname S_vals
#' @export
setGeneric("S_vals", function(x)
  standardGeneric("S_vals"))
#' @rdname S_vals
#' @export
setGeneric("S_vals<-", function(x, value)
  standardGeneric("S_vals<-"))
#' @rdname S_vals
#' @export
setMethod("S_vals", "cpss", function(x)
  x@S_vals)
#' @rdname S_vals
#' @export
setMethod("S_vals<-", "cpss", function(x, value) {
  x@S_vals <- value
  x
})

#' Generic functions and methods: SC_vals
#'
#' @inheritParams dat
#' @name SC_vals
NULL
#> NULL

#' @rdname SC_vals
#' @export
setGeneric("SC_vals", function(x)
  standardGeneric("SC_vals"))
#' @rdname SC_vals
#' @export
setGeneric("SC_vals<-", function(x, value)
  standardGeneric("SC_vals<-"))
#' @rdname SC_vals
#' @export
setMethod("SC_vals", "cpss", function(x)
  x@SC_vals)
#' @rdname SC_vals
#' @export
setMethod("SC_vals<-", "cpss", function(x, value) {
  x@SC_vals <- value
  x
})

#' Generic functions and methods: update_inputs
#'
#' @inheritParams dat
#' @name update_inputs
NULL
#> NULL

#' @rdname update_inputs
#' @export
setGeneric("update_inputs", function(x)
  standardGeneric("update_inputs"))
#' @rdname update_inputs
#' @export
setGeneric("update_inputs<-", function(x, value)
  standardGeneric("update_inputs<-"))
#' @rdname update_inputs
#' @export
setMethod("update_inputs", "cpss", function(x)
  x@update_inputs)
#' @rdname update_inputs
#' @export
setMethod("update_inputs<-", "cpss", function(x, value) {
  x@update_inputs <- value
  x
})

#' summary method
#'
#' @param cpss cpss class
#' @param object object from cpss
#' @export
setMethod("summary", "cpss", function(object) {
  mdl.cat <- switch(
    object@mdl,
    mean = "mean",
    var = "(co)variance",
    meanvar = "both mean and (co)variance",
    glm = "GLMs",
    em = "exponential family",
    custom = "user-specified parameters"
  )
  algo.cat <- switch(
    object@algo,
    SN = "segment neighborhood algorithm",
    BS = "binary segmentation algorithm",
    WBS = "wild binary segmentation algorithm",
    PELT = "pruned exact linear time algorithm"
  )
  SC.cat <- switch(
    object@SC,
    CV = "cross-validation",
    MS = "multiple-splitting"
  )
  if (object@algo %in% c("SN", "BS", "WBS")) {
    cat(
      "Detecting changes in ",
      mdl.cat,
      " (if at least one exists):\n",
      "\t Change searching algorithm: ",
      algo.cat,
      "\n",
      "\t Model selection criterion: ",
      SC.cat,
      "\n\n",
      "Call:\n",
      sep = ""
    )
    print(object@call$call)
    cat(
      "\n",
      object@ncps,
      " change-points are detected at locations:\n",
      "\t",
      paste0(object@cps, "  "),
      sep = ""
    )
  } else if (object@algo == "PELT") {
    cat(
      "Detecting changes in ",
      mdl.cat,
      " (if at least one exists):\n",
      "\t Change searching algorithm: ",
      algo.cat,
      "\n",
      "\t Model selection criterion: ",
      SC.cat,
      "\n\n",
      "Call:\n",
      sep = ""
    )
    print(object@call$call)
    cat(
      "\n",
      object@ncps,
      " change-points are detected (with a penalty constant ",
      object@pelt_pen,
      ") at locations:\n",
      "\t",
      paste0(object@cps, "  "),
      sep = ""
    )
  } else {
    stop("Not yet supported change searching algorithm!")
  }
})

pal.GW <- matrix(
  c(
    "#56c1ff",
    "#72fcea",
    "#88fa4e",
    "#fffc66",
    "#ff968d",
    "#ff8cc6",
    "#03a1ff",
    "#15e7cf",
    "#60d837",
    "#fae231",
    "#ff634e",
    "#ef5fa7",
    "#0276ba",
    "#01a99d",
    "#1db100",
    "#f9ba00",
    "#ee230d",
    "#cb297b",
    "#004d80",
    "#007c76",
    "#017101",
    "#ff9300",
    "#b41700",
    "#99185f",
    "#ffffff",
    "#d6d5d5",
    "#929292",
    "#5e5e5e",
    "#000000",
    NA
  ),
  nrow = 5,
  byrow = TRUE
)

#' plot method
#'
#' @param cpss cpss class
#' @param obj object from cpss
#' @param type type of visualization
#' @param x x
#' @param y y
#' @param ... ...
#' @export
setMethod("plot", "cpss", function(obj,
                                   type,
                                   x = c(),
                                   y = c(),
                                   ...) {
  if (type == "scatter") {

    if (obj@mdl == "mean" & ncol(obj@dat) == 1) {
      colnames(obj@dat) <- "Data"
      temp <- sapply(1:length(obj@params), function(k) {
        obj@params[[k]]$mu
      })
      Fitted <- rep(temp, c(obj@cps, nrow(obj@dat)) - c(0, obj@cps))
      df <- as_tibble(obj@dat) %>%
        mutate(
          Time = 1:nrow(obj@dat),
          Fitted = Fitted
        )
      df %>%
        ggplot() +
        geom_point(aes(.data$Time, .data$Data), color = pal.GW[5, 3], shape = 16, size = 1) +
        geom_line(aes(.data$Time, Fitted), color = pal.GW[2, 5], size = 1) +
        geom_vline(xintercept = obj@cps, color = pal.GW[5, 4], linetype = 2) +
        labs(title = "") +
        theme_light()
    } else {
      stop("Not yet supported data structure! Currently, the plot method with \"type = \'scatter\'\" only works for univariate mean change models.")
    }

  } else if (type == "path") {

    if (nrow(obj@SC_vals) == 2) {
      pal1 <- c(pal.GW[2, 5], pal.GW[3, c(1, 2)])
    } else {
      pal1 <- c(pal.GW[2, 5], rep(pal.GW[5, 3], nrow(obj@SC_vals)))
    }
    temp <- t(rbind(apply(obj@SC_vals, 2, mean), obj@SC_vals))
    colnames(temp) <- paste0("Split_", 0:nrow(obj@SC_vals))
    df <- as_tibble(temp) %>%
      mutate(Dim = obj@S_vals) %>%
      pivot_longer(starts_with("Split"),
                   names_to = "Split",
                   values_to = "Value")
    df %>%
      ggplot() +
      geom_point(aes(.data$Dim, .data$Value, color = .data$Split),
                 shape = 16,
                 na.rm = TRUE) +
      geom_line(aes(
        .data$Dim,
        .data$Value,
        color = .data$Split,
        linetype = (.data$Split != "Split_0")
      ), na.rm = TRUE) +
      scale_color_manual(values = pal1) +
      labs(x = "Model dimension", y = "Value of selection criterion") +
      theme_light() +
      theme(legend.position = "none")

  } else if (type == "coef") {

    if (obj@mdl == "meanvar" & ncol(obj@dat) <= 5) {
      df <- coef(obj)
      df1 <- expand.grid(X = 1, Y = 1:dim(df$Sigma)[1])
      df1 <- do.call("rbind", rep(list(df1), dim(df$Sigma)[3]))
      df1$Z <- c(df$mu)
      df1 <- as_tibble(df1) %>%
        mutate(
          Segment = rep(1:dim(df$Sigma)[3], each = (dim(df$Sigma)[1]))
        )
      p1 <- df1 %>%
        ggplot() +
        geom_tile(aes(.data$X, dim(df$Sigma)[1] + 1 - .data$Y, fill = .data$Z)) +
        geom_text(aes(.data$X, dim(df$Sigma)[1] + 1 - .data$Y, label = round(.data$Z, 2)), size = 3) +
        facet_grid(cols = vars(.data$Segment)) +
        scale_fill_gradient2(
          low = pal.GW[1, 1],
          mid = pal.GW[5, 1],
          high = pal.GW[1, 5]
        ) +
        labs(x = "", y = "", title = "Estimated mean") +
        theme_light() +
        theme(
          legend.position = "none"
        )

      df2 <- expand.grid(X = 1:dim(df$Sigma)[1], Y = 1:dim(df$Sigma)[1])
      df2 <- do.call("rbind", rep(list(df2), dim(df$Sigma)[3]))
      df2$Z <- c(df$Sigma)
      df2 <- as_tibble(df2) %>%
        mutate(
          Segment = rep(1:dim(df$Sigma)[3], each = (dim(df$Sigma)[1])^2)
        )
      p2 <- df2 %>%
        ggplot() +
        geom_tile(aes(.data$X, dim(df$Sigma)[1] + 1 - .data$Y, fill = .data$Z)) +
        geom_text(aes(.data$X, dim(df$Sigma)[1] + 1 - .data$Y, label = round(.data$Z, 2)), size = 3) +
        facet_grid(cols = vars(.data$Segment)) +
        scale_fill_gradient2(
          low = pal.GW[1, 1],
          mid = pal.GW[5, 1],
          high = pal.GW[1, 5]
        ) +
        labs(x = "", y = "", title = "Estimated covariance") +
        theme_light() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
        )
      grid.arrange(
        p1, p2, nrow = 2
      )
    } else {
      stop("Not yet supported data structure! Currently, the plot method with \"type = \'coef\'\" only works for simultaneous mean and covariance change models with data dimension no larger than 5 for a better visualization.")
    }

  } else {
    stop("Not yet supported visualization type!")
  }
})

#' coef method
#'
#' @param cpss cpss class
#' @param object object from cpss
#' @export
setMethod("coef", "cpss", function(object) {
  if (object@mdl == "mean") {
    coefs <- params(object)
    out <- matrix(NA, ncol(object@dat), length(coefs))
    for (k in 1:length(coefs)) {
      out[, k] <- coefs[[k]]$mu
    }
    return(out)
  }
  if (object@mdl == "var") {
    coefs <- params(object)
    out <- array(NA, c(ncol(object@dat), ncol(object@dat), length(coefs)))
    for (k in 1:length(coefs)) {
      out[, , k] <- coefs[[k]]$Sigma
    }
    return(out)
  }
  if (object@mdl == "meanvar") {
    coefs <- params(object)
    mu <- matrix(NA, ncol(object@dat), length(coefs))
    Sigma <- array(NA, c(ncol(object@dat), ncol(object@dat), length(coefs)))
    for (k in 1:length(coefs)) {
      mu[, k] <- coefs[[k]]$mu
      Sigma[, , k] <- coefs[[k]]$Sigma
    }
    return(out = list(mu = mu, Sigma = Sigma))
  }
  if (object@mdl == "glm") {
    coefs <- params(object)
    out <- matrix(NA, length(coefs[[1]]$coefficients), length(coefs))
    for (k in 1:length(coefs)) {
      out[, k] <- coefs[[k]]$coefficients
    }
    return(out)
  }
  if (object@mdl == "lm") {
    coefs <- params(object)
    coefs_ <- matrix(NA, length(coefs[[1]]$coefficients), length(coefs))
    sigma_ <- rep(NA, length(coefs))
    for (k in 1:length(coefs)) {
      coefs_[, k] <- coefs[[k]]$coefficients
      sigma_[k] <- sigma(coefs[[k]])
    }
    return(out = list(coef = coefs_, sigma = sigma_))
  }
  if (object@mdl == "em") {
    coefs <- params(object)
    out <- matrix(NA, length(coefs[[1]]$param), length(coefs))
    for (k in 1:length(coefs)) {
      out[, k] <- coefs[[k]]$param
    }
    return(out)
  }
})

#' update method
#'
#' @param cpss cpss class
#' @param object object from cpss
#' @param dim_update model dimension to update
#' @export
setMethod("update", "cpss", function(object, dim_update) {
  if (object@algo == "PELT") {
    ncps_max <- object@update_inputs$ncps_max
    pelt_pen_val <- dim_update
  } else {
    ncps_max <- dim_update
    pelt_pen_val <- NULL
  }
  res <- EST(object@dat, object@update_inputs$n, object@update_inputs$g_subdat, object@update_inputs$g_param, object@update_inputs$g_cost, object@algo, object@update_inputs$dist_min, ncps_max, pelt_pen_val, object@update_inputs$pelt_K, object@update_inputs$wbs_nintervals, object@mdl, object@update_inputs$g_smry, object@update_inputs$easy_cost, object@update_inputs$param.opt)
  if (object@algo == "PELT") {
    cps_update <- sort(res)
  } else {
    cps_update <- sort(res[ncps_max, 1:ncps_max])
  }
  params_update <- list()
  ID <- rep(1:(length(cps_update) + 1), c(cps_update, object@update_inputs$n) - c(0, cps_update))
  for (j in 1:(length(cps_update) + 1)) {
    subdat_j <- object@update_inputs$g_subdat(object@dat, ID == j)
    params_update[[j]] <- object@update_inputs$g_param(subdat_j, object@update_inputs$param.opt)
  }
  object@cps <- cps_update
  object@ncps <- length(object@cps)
  object@params <- params_update
  object@algo_param_dim <- dim_update
  object@call$call <- "The model has been updated!"
  return(object)
  # params_update <- coef(object)
  # return(out = list(cps_update = cps_update, params_update = params_update))
})
