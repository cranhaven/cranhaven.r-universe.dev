#Functions from the archived package chipPCR

#The functions in this file are from the archived chipPCR package. The function th.cyc and all associated helper functions are in this file. The th.cyc function is used in the MDMAPR to calculate cycle threshold values for qPCR data.

#Citation: Roediger S and Burdukiewicz M (2014). chipPCR: Toolkit of helper functions to pre-process amplification data. R package version 0.0.8-4, <http://CRAN.R-project.org/package=chipPCR>.

#th.cyc() function is from the archieved R package chipPCR.
th.cyc <-
  function(x, y, r = 2500, auto = FALSE, linear = TRUE) {

    # Rearrange data for further processing

    xy <- data.frame(x = x, y = y)
    xy <- xy[!is.na(xy[["x"]]), ]

    # Determine type of threshold calculation
    r <- ifelse(auto, quantile(y[1L:10], 0.1) + 3 * mad(y[1L:10]), r)

    # Before runing the analysis, test if signal is indeed larger than the
    # threshold.

#     if (quantile(xy[, 2], 0.9) <= r) {
#       # TODO: FIX OUTPUT
#       stop("Maximum of signal lower than threshold (r).")
#     } else {
      # Actually used number of neighbours around the threshold value
      n <- seq(2, 8, 1)

      # List of all regression results for all tested regressions with different
      # numbers of neighbours
      res.th.est <- lapply(n, function(n)
        th.est(xy, r = r, n, linear = linear))

      # Results of the selection criterium R squared
      res.r.squ <- sapply(1L:length(n), function(i)
        res.th.est[[i]][[1]][["r.squared"]])

      # Result of the optimal regression
      xy.sum <- res.th.est[[which.max(res.r.squ)]]

      if (linear == FALSE) {
        # Extract the coefficients of the regression.
        a <- xy.sum[[1]][["coefficients"]][3, 1]
        b <- xy.sum[[1]][["coefficients"]][2, 1]
        c <- xy.sum[[1]][["coefficients"]][1, 1]

        # Calculate the exact Ct value at the user defined fluorescence threshold.
        # Use either the linear or quadratic model.

        sign <- inder(xy.sum[["values"]])
        switch.sign <- which.max(sign[, "d1y"])
        sqrt.delta <- sqrt(b^2 - 4*a*(c - r))
        if (sign[switch.sign, "y"] < r) {
          x.cal <- (-b - sqrt.delta)/(2*a)
        } else {
          x.cal <- (-b + sqrt.delta)/(2*a)
        }
      } else {
        m <- xy.sum[[1]][["coefficients"]][1, 1]
        n <- xy.sum[[1]][["coefficients"]][2, 1]
        x.cal <- (r - m) / n
      }

      # Create the output fot the exact Ct value, the regression and the neighbours
      # of the cycle and fluorescence values at the threshold fluorescence.

      res <-matrix(c(x.cal, r), ncol = 2)
      colnames(res) <- c("cyc.th", "atFluo")

      new("th", .Data = res,
          stats = xy.sum[["summary"]],
          input = data.matrix(xy.sum[["values"]]))
#     }
  }


# Helper function to determine the number of neighbours for the regression
th.est <- function(xy, r, n, linear) {
  # Fetch the neighbours of the cycle and fluorescence values at the threshold
  # fluorescence.

  xy.out <- rbind(tail(xy[xy[, 2] <= r, ], n),
                  head(xy[xy[, 2] >= r, ], n)
  )

  # Determine with a quadratic polynomial the equation for the neighbours of the
  # cycle and fluorescence values at the threshold fluorescence.

  xy.lm <- if (linear == TRUE) {
    lm(xy.out[, 2] ~ xy.out[, 1])
  } else {
    lm(xy.out[, 2] ~ xy.out[, 1] + I(xy.out[, 1]^2))
  }


  # summary of statistical values of the fit
  list(summary = summary(xy.lm), values = xy.out)
}


#testxy function
testxy <- function(x, y, txt.x = "Enter abscissa value",
                   txt.y = "Enter ordinate value", both = TRUE, length = TRUE) {

  if (both == TRUE) {
    # Test if x and y exist and have identical lengths.
    if (is.null(x))
      stop(txt.x)
    if (is.null(y))
      stop(txt.y)
    #   if (is.numeric(x) )
    #     stop("Abscissa value must have numeric class")
    #   if (is.numeric(y))
    #     stop("Ordinate value must have numeric class")
    if (length)
      if (length(x) != length(y))
        stop("Use abscissa and ordinate data with same number of
  	elements")
  } else {
    # Test if x and y exist and have identical lengths.
    if (is.null(y))
      stop(txt.y)
  }
}


#Inder function
# Defintion of the inder function
inder <- function(x, y, Nip = 4, logy = FALSE, smooth.method = "spline") {
  # Test validity ot the input data
  testxy(x, y)

  # Test meaningfulness of the spline interpolation and give a warning in case
  # any violation
  if (Nip < 1)
    stop("Use Nip equal or larger to 1")

  if (Nip > 10)
    warning("Nip larger than 10 may case over-fitting")

  # Set the smooth method for inder

  if(is.null(smooth.method)) {
    tmp.xy <- data.frame(x = x, y = y)
    smooth.method <- "no smoothing"
  } else {
    if (smooth.method == "spline") {
      tmp.xy <- spline(x, y, n = Nip * length(x))
    }

    if (smooth.method == "supsmu") {
      tmp.xy <- supsmu(x, y, span = "cv")
    }
  }

  x <- tmp.xy[["x"]]
  y <- tmp.xy[["y"]]

  if (logy == TRUE) y <- log10(tmp.xy[["y"]])

  # calculate h, naive approach
  h <- vapply(2L:length(x), function(i) x[i] - x[i - 1], 0)
  # instead of zero, in statement should be the minina
  if (var(h) > .Machine[["double.eps"]])
    warning("Points are not equidistant. The results of interpolation
	      could be not correct.")

  h <- h[1]
  # calculate midpoints
  first.der <- c(first.beginpoint0(y[1:5], h),
                 first.beginpoint1(y[1:5], h),
                 vapply(3L:(length(y) - 2), function(i) first.midpoint(y[(i - 2):(i + 2)], h), 0),
                 first.endpoint1(y[(length(y) - 5):length(y)], h),
                 first.endpoint0(y[(length(y) - 5):length(y)], h))

  sec.der <- c(sec.beginpoint0(y[1:5], h),
               sec.beginpoint1(y[1:5], h),
               vapply(3L:(length(y) - 2), function(i) sec.midpoint(y[(i - 2):(i + 2)], h), 0),
               sec.endpoint1(y[(length(y) - 5):length(y)], h),
               sec.endpoint0(y[(length(y) - 5):length(y)], h))

  dat <- cbind(x, y, first.der, sec.der)
  colnames(dat) <- c("x", "y", "d1y", "d2y")
  new("der", '.Data' = dat, 'method' = smooth.method)
}

setGeneric("inder")

setMethod("inder", signature(x = "data.frame", y="missing"),
          function(x, y, Nip = 4, logy = FALSE, smooth.method = "spline") {
            if (ncol(x) != 2)
              stop("'x' must have two columns.")
            inder(x[, 1], x[, 2], Nip = Nip, logy = logy,
                  smooth.method = smooth.method)
          })

setMethod("inder", signature(x = "matrix", y = "missing"),
          function(x, y, Nip = 4, logy = FALSE, smooth.method = "spline") {
            if (ncol(x) != 2)
              stop("'x' must have two columns.")
            inder(x[, 1], x[, 2], Nip = Nip, logy = logy,
                  smooth.method = smooth.method)
          })



#th class and method are from the archieved R package chipPCR.
setOldClass("summary.lm")


#th class, th.cyc function -------------------------------
setClass("th", contains = "matrix", representation(.Data = "matrix",
                                                   stats = "summary.lm",
                                                   input = "matrix"))

setMethod("show", signature(object = "th"), function(object) {
  print(slot(object, ".Data"))
})

setMethod("summary", signature(object = "th"), function(object) {
  cat("Cycle threshold: ", slot(object, ".Data")[, "cyc.th"], "\n")
  cat("Fluorescence threshold: ", slot(object, ".Data")[, "atFluo"], "\n")
  print(slot(object, "stats"))
})


