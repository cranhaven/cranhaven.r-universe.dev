#' @rdname icrf
#' @export
"icrf.formula" <-
    function(formula, data = NULL, data.type = c("interval", "right", "currentstatus"),
             interval.label = c("L", "R"), right.label = c("T", "status"),
             currentstatus.label = c("monitor", "status"), ..., na.action = na.fail, epsilon = NULL) {
### formula interface for icrf which is based on randomForest package.
### randomForest.formula code is again taken from svm.formula (package e1071).
###
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    ## Catch xtest and ytest in arguments.
    if (any(c("xtest", "ytest") %in% names(m)))
      stop("xtest/ytest not supported through the formula interface")
    names(m)[2] <- "formula"
    m <- m[!names(m) %in% c("data.type", "interval.label", "right.label", "currentstatus.label")]
    if (is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    # m$data[time.labels] = NULL
    m$... <- NULL
    m$na.action <- na.action
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())

    # getting intervals
    noSurv <- length(formula) < 3
    if (noSurv) {
      time.labels = c()
      if (data.type =="interval") {
        L <- data[, interval.label[1]]
        R <- data[, interval.label[2]]
        time.labels = interval.label
      } else if (data.type =="right") {
        L <- data[, right.label[1]]
        status <- data[, right.label[2]]
        if (!is.logical(status))
          if (any(!unique(status) %in% c(0, 1)))
            stop("The status should be either logical or 0's and 1's")
        R = ifelse(status, L, Inf)
        time.labels = right.label
      } else if (data.type =="currentstatus") {
        monitor <- data[, currentstatus.label[1]]
        status <- data[, currentstatus.label[2]]
        if (!is.logical(status))
          if (any(!unique(status) %in% c(0, 1)))
            stop("The status should be either logical or 0's and 1's")
        L = ifelse(status, 0, monitor)
        R = ifelse(status, monitor, Inf)
        time.labels = currentstatus.label
      } else {
        stop("data.type should be either 'interval', 'right', or 'currentstatus'")
      }
    }

    if (!noSurv) {
      y <- model.response(m, "numeric")
      y <- as.matrix(y)
      rightCens = y[ ,"status"] == 0
      exact     = y[ ,"status"] == 1
      y[rightCens, "time2"] <- Inf
      y[exact, "time2"] <- y[exact, "time1"]

      L <- y[, "time1"]
      R <- y[, "time2"]
    }


    if (sum(L == R)) {
      if (is.null(epsilon)) epsilon <- min(min(diff(sort(unique(c(L, R)))))/10, 1e-10)
      R <- R + ifelse(L == R, epsilon, 0)
    }

    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
	  attr(L, "na.action") <- attr(m, "na.action")
	  attr(R, "na.action") <- attr(m, "na.action")

	## Drop any "negative" terms in the formula.
    ## test with:
    ## icrf(Fertility~.-Catholic+I(Catholic<50),data=swiss,mtry=2)
    m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
                     data.frame(m))
    ## if (!is.null(y)) m <- m[, -1, drop=FALSE]
    for (i in seq(along=m)) {
        if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
    }

    ret <- icrf.default(m, L, R, ...)
    cl <- match.call()
    cl[[1]] <- as.name("icrf")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(L, "na.action"))) {
        attr(ret$predicted, "na.action") <- ret$na.action <- attr(L, "na.action")
    }
    class(ret) <- c("icrf.formula", "icrf")
    return(ret)
}
