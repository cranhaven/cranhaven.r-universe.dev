calcGrpWprime <- function(x, index, method = "chi-squared", quantile = 0.05){
  
  if (!inherits(x, "all_lscps")) {
    stop("'x' must be the output of cal.call.lscps().", call. = FALSE)
  }
  if (is.null(x$dataframes$new_data)) {
    stop("A 'new_data' component must have been present in the kriged_surfaces object passed to cal.call.lscps() to use calcGrpWprime().",
         call. = FALSE)
  }
  
  if (length(method) != 1 || !is.character(method)) {
    stop("'method' must be one of \"chi-squared\", \"quantile\", or \"max\".", call. = FALSE)
  }
  method <- match.arg(method, c("chi-squared", "quantile", "max"))
  
  fn_dataframe <- x$dataframes
  func.names <- names(fn_dataframe$grid)[-(1:2)]
  
  #Check and process index
  n_new_data <- nrow(fn_dataframe$new_data)
  if (missing(index)) {
    index <- seq_len(n_new_data)
  }
  else {
    e <- substitute(index)
    index <- eval(e, fn_dataframe$new_data, parent.frame())
    
    if (is.numeric(index)) {
      if (!all(index %in% seq_len(n_new_data))) {
        stop(sprintf("Some 'index' values are out of bounds; all must be between 1 and %s.",
                     n_new_data), call. = FALSE)
      }
    }
    else if (is.logical(index)) {
      if (length(index) != n_new_data) {
        stop(sprintf("If 'index' is supplied as a logical vector, it must have length equal to %s.",
                     n_new_data), call. = FALSE)
      }
      index <- which(index)
    }
    else if (is.character(index)) {
      if (!all(index %in% rownames(fn_dataframe$new_data))) {
        stop("All values in 'index' must be present in the new_data supplied to krige_surf().",
             call. = FALSE)
      }
      index <- match(index, rownames(fn_dataframe$new_data))
    }
    else {
      stop("'index' must be a numeric, logical, or character vector or NULL.", call. = FALSE)
    }
    
    if (anyNA(index)) {
      stop("No values in 'index' can be NA.", call. = FALSE)
    }
  }
  
  W <- cbind(x[["grid_weights"]], Z = colMeans(x[["wtd_lscps"]][["new_data"]][index,, drop = FALSE]))
  
  W <- W[order(W[, "Z"], decreasing = TRUE),, drop = FALSE]

  if (method == "max") {
    Zprime <- list(wn = W[1,])
  }
  else {
    x.top <- gettop(W, method = method, quantile = quantile,
                    sortby = "Z")
    Zprime <- list(wn = colMeans(x.top),
                   wn.se = apply(x.top, 2, sd)/sqrt(nrow(x.top)),
                   wn.sd = apply(x.top, 2, sd),
                   wn.range = apply(x.top, 2, range))
    attr(method, "quantile") <- quantile
  }
  attr(Zprime, "method") <- method
  
  Wprime <- calc.W.kr(W = Zprime[["wn"]][func.names], fnc_data = fn_dataframe,
                      func.names = func.names)
  
  #Wlist  - For every set of weights, the Z values produced for each sample in new_data
  #         1 entry per weight set, entry contains data frame with 1 row per sample with weights and sample Zs
  #W/tmp  - For every set of weights, the average of the Z values produced; this is used to determine the best
  #         1 row per weight set, weights and average sample Z
  #x.top  - Sets of the top several weights (wrt to the average Z they produce on new_data) along with the average Z each produces
  #         1 row per weight set (only the best), weights and average sample Z (subset of tmp)
  #Zprime - "best" weights, i.e., average of x.top weights, along with the average Z they produce
  #Wprime - surface (grid) and new_data func.chars after weighting by the Zprime weights, along with the
  #         Z they produce
  #         1 row per sample in new_data, columns are coordinates, weighted func.chars, and sample Zs
 
  out <- list(Zprime = Zprime, W = W, Wprime = Wprime)
  class(out) <- "grp_Wprime"
  
  return(out)
}

calcWprimeBy <- function(x, by, method = "chi-squared", quantile = 0.05) {
  if (!inherits(x, "all_lscps")) {
    stop("'x' must be an all_lscps object, the output of cal.call.lscps().", call. = FALSE)
  }
  if (is.null(x$dataframes$new_data)) {
    stop("A 'new_data' component must have been present in the kriged_surfaces object passed to cal.call.lscps() to use calcWprimeBy().",
         call. = FALSE)
  }
  
  by_name <- deparse(substitute(by))
  if (missing(by)) {
    stop("'by' must be specified.", call. = FALSE)
  }
  if (inherits(by, "formula")) {
    by <- delete.response(terms(by, data = x$dataframes$new_data))
    if (length(attr(by, "term.labels")) != 1) {
      stop("Only one 'by' variable is allowed.", call. = FALSE)
    }
    by_mf <- tryCatch(model.frame(by, data = x$dataframes$new_data),
                      warning = function(w) warning(conditionMessage(w), call. = FALSE),
                      error = function(e) {
                        cond <- conditionMessage(e)
                        if (startsWith(cond, "object") && endsWith(cond, "not found")) {
                          stop("The variable named in 'by' is not present in the 'new_data' component present in the kriged_surfaces object passed to cal.call.lscps().", call. = FALSE)
                        }
                        else stop(cond, call. = FALSE)
                      })
    by <- by_mf[[1]]
    by_name <- names(by_mf)[1]
    rm(by_mf)
  }
  else if (is.atomic(by) && is.null(dim(by))) {
    if (length(by) != nrow(x$dataframes$new_data)) {
      stop("'by' must have length equal to the number of rows in the 'new_data' component present in the kriged_surfaces object passed to cal.call.lscps().", call. = FALSE)
    }
  }
  else {
    stop("'by' must be a one-sided formula (e.g., ~x) or a grouping variable.", call. = FALSE)
  }
  if (anyNA(by)) {
    stop("Missing values are not allowed in 'by'.", call. = FALSE)
  }
  
  if (length(method) != 1 || !is.character(method)) {
    stop("'method' must be one of \"chi-squared\", \"quantile\", or \"max\".", call. = FALSE)
  }
  method <- match.arg(method, c("chi-squared", "quantile", "max"))
  
  by <- factor(by)
  
  res <- lapply(levels(by), function(b) {
    index <- by == b
    calcGrpWprime(x, index, method, quantile)
  })
  
  names(res) <- levels(by)
  attr(by, "by_name") <- by_name
  
  out <- list(by = by,
              grp_Wprimes = res)
  class(out) <- "by_Wprime"
  return(out)
}

# Finds the top percentile of a W matrix
gettop <- function(x, method = "chi-squared", quantile = 0.05, sortby = "Z"){
  
  if (length(method) != 1 || !is.character(method)) {
    stop("'method' must be one of \"quantile\" or \"chi-squared\".", call. = FALSE)
  }
  method <- match.arg(method, c("quantile", "chi-squared"))
  
  V <- x[, sortby]
  if (method == "quantile"){
    x.top <- x[V > quantile(V, probs = 1-quantile),, drop = FALSE]
  }
  else if (method == "chi-squared"){
    critval <- qchisq(quantile, 2)
    chi2 <- -2*(V - max(V)) 
    x.top <- x[chi2 < critval,, drop = FALSE]
  }
  
  x.top <- x.top[order(x.top[, sortby], decreasing = TRUE),, drop = FALSE]
  
  return(x.top)
}

print.grp_Wprime <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  
  tables <- make_wprime_tables(x)
  
  method <- {
    if (attr(x[["Zprime"]], "method") == "max") "max"
    else sprintf("%s, quantile = %s", attr(x[["Zprime"]], "method"),
                 attr(attr(x[["Zprime"]], "method"), "quantile"))
  }
  
  cat("Optimal weights:\n")
  print(tables$res, digits = digits, ...)
  cat("\nAverage fitness value at optimal weights:\n")
  print(tables$z, digits = digits, ...)
  cat(sprintf("\n- method: %s\n", method))
  invisible(x)
}

print.by_Wprime <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  by_name <- attr(x[["by"]], "by_name")
  
  for (i in names(x[["grp_Wprimes"]])) {
    if (i != names(x[["grp_Wprimes"]])[1]) {
      cat("-----------------------------------------\n")
    }
    cat(sprintf("- %s == \"%s\"\n\n", by_name, i))
    tables <- make_wprime_tables(x[["grp_Wprimes"]][[i]])
    cat("Optimal weights:\n")
    print(tables$res, digits = digits, ...)
    cat("\nAverage fitness value at optimal weights:\n")
    print(tables$z, digits = digits, ...)
  }
  
  method <- {
    if (attr(x[["grp_Wprimes"]][[1]][["Zprime"]], "method") == "max") "max"
    else sprintf("%s, quantile = %s", attr(x[["grp_Wprimes"]][[1]][["Zprime"]], "method"),
                 attr(attr(x[["grp_Wprimes"]][[1]][["Zprime"]], "method"), "quantile"))
  }
  cat(sprintf("\n- method: %s\n", method))
  invisible(x)
}

make_wprime_tables <- function(x) {
  func.names <- names(x[["Wprime"]][["W"]])
  func.ind <- seq_along(func.names)
  z.ind <- length(func.names) + 1
  
  if (attr(x[["Zprime"]], "method") == "max") {
    res <- matrix(nrow = length(func.names),
                  ncol = 1, dimnames = list(func.names, "Weight"))
    res[,"Weight"] <- x[["Zprime"]][["wn"]][func.ind]
    
    z <- matrix(nrow = 1, ncol = 1, dimnames = list("Z", "Value"))
    z[,"Value"] <- x[["Zprime"]][["wn"]][z.ind]
    method <- "max"
  }
  else {
    res <- matrix(nrow = length(func.names),
                  ncol = 5, dimnames = list(func.names, 
                                            c("Weight", "SE", "SD", "Min.", "Max.")))
    
    res[,"Weight"] <- x[["Zprime"]][["wn"]][func.ind]
    res[,"SE"] <- x[["Zprime"]][["wn.se"]][func.ind]
    res[,"SD"] <- x[["Zprime"]][["wn.sd"]][func.ind]
    res[,"Min."] <- x[["Zprime"]][["wn.range"]][1, func.ind]
    res[,"Max."] <- x[["Zprime"]][["wn.range"]][2, func.ind]
    
    z <- matrix(nrow = 1, ncol = 5,
                dimnames = list("Z", c("Value", "SE", "SD", "Min.", "Max.")))
    
    z[,"Value"] <- x[["Zprime"]][["wn"]][z.ind]
    z[,"SE"] <- x[["Zprime"]][["wn.se"]][z.ind]
    z[,"SD"] <- x[["Zprime"]][["wn.sd"]][z.ind]
    z[,"Min."] <- x[["Zprime"]][["wn.range"]][1, z.ind]
    z[,"Max."] <- x[["Zprime"]][["wn.range"]][2, z.ind]
    method <- sprintf("%s, quantile = %s", attr(x[["Zprime"]], "method"), attr(attr(x[["Zprime"]], "method"), "quantile"))
  }
  list(res = res, z = z)
}

summary.by_Wprime <- function(object, ...) {
  func.names <- names(object[["grp_Wprimes"]][[1]][["Wprime"]][["W"]])
  w.names <- paste0("W_", func.names)

  out <- do.call("rbind", lapply(object[["grp_Wprimes"]], function(x) x[["Zprime"]][["wn"]]))
  rownames(out) <- names(object[["grp_Wprimes"]])
  colnames(out) <- c(w.names, "Z")
  attr(out, "by_name") <- attr(object[["by"]], "by_name")
  
  class(out) <- c("summary.by_Wprime", class(out))
  out
}

print.summary.by_Wprime <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat(sprintf("Optimal weights by %s:\n", attr(x, "by_name")))
  print(as.table(x), digits = digits)
  invisible(x)
}