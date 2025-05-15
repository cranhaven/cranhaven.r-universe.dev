
# Interval ----------------------------------------------------------------

#' Create an symbolic_interval type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new.sym.intreval <- function(min = numeric(), max = numeric()) {
  vctrs::new_vctr(complex(real = min, imaginary = max), class = "symbolic_interval")
}

#' Create an symbolic_interval type object
#'
#' @param x numeric vector
#' @param .min function that will be used to calculate the minimum interval
#' @param .max function that will be used to calculate the maximum interval
#'
#' @return a symbolic interval
#' @export
#'
#' @examples
#' sym.interval(c(1, 2, 4, 5))
#' sym.interval(1:10)
#' @importFrom vctrs vec_cast
#'
sym.interval <- function(x = numeric(), .min = min, .max = max) {
  x <- vctrs::vec_cast(x, double())
  new.sym.intreval(min = .min(x), max = .max(x))
}


#' Symbolic interval
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_vector and FALSE otherwise.
#'
#' @examples
#' x <- sym.interval(1:10)
#' is.sym.interval(x)
#' is.sym.interval("d")
#' @export
is.sym.interval <- function(x) {
  inherits(x, "symbolic_interval")
}

#' abbr for symbolic interval
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_interval <- function(x) {
  "interval"
}

#' full name for symbolic interval
#' @keywords internal
#' @export
vec_ptype_full.symbolic_interval <- function(x) {
  "symbolic_interval"
}

#' Symbolic interval conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#' @importFrom scales comma
#' @importFrom vctrs vec_data
format.symbolic_interval <- function(x, ...) {
  min <- Re(vctrs::vec_data(x))
  max <- Im(vctrs::vec_data(x))
  paste0(
    "[", scales::comma(min, accuracy = 0.01), " : ",
    scales::comma(max, accuracy = 0.01), "]"
  )
}

#' Maxima and Minima
#' @param x symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#' @param name ...
#'
#' @return a new symbolic interval with the minimum of the minima and the minimum of the maxima
#' @export
#' @rdname Maxima_and_Minima
#' @importFrom vctrs vec_data
min.symbolic_interval <- function(x, ...) {
  sapply(x, function(x) Re(vctrs::vec_data(x)))
}

#' @rdname Maxima_and_Minima
#' @export
max.symbolic_interval <- function(x, ...) {
  sapply(x, function(x) Im(vctrs::vec_data(x)))
}

#' @rdname Maxima_and_Minima
#' @export
`$.symbolic_interval` <- function(x, name = c("min", "max", "mean", "median")) {
  switch(name,
    min = min(x),
    max = max(x),
    mean = mean(x),
    median = median(x),
    NULL
  )
}

#' Symbolic mean for intervals
#' @rdname Symbolic_mean
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic mean for intervals
#' @param x A symbolic interval.
#' @param method The method to be use.
#' @param trim As in R mean function.
#' @param na.rm As in R mean function.
#' @param ... As in R mean function.
#'
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic Mean
#' @export
#' @importFrom vctrs vec_data
mean.symbolic_interval <- function(x, method = c("centers", "interval"), trim = 0,
                                   na.rm = F, ...) {
  method <- match.arg(method)
  if (method == "interval") {
    out <- new.sym.intreval(mean(vctrs::vec_data(Re(x))), mean(vctrs::vec_data(Im(x))))
    return(out)
  } else {
    out <- mean((vctrs::vec_data(Re(x)) + vctrs::vec_data(Im(x))) / 2)
    return(out)
  }
}

#' @rdname Symbolic_mean
#' @export
mean.symbolic_tbl <- function(x, ...) map_symbolic_tbl(x, mean, ...)

#' map function over symbolic table
#' @keywords internal
#' @export
map_symbolic_tbl <- function(.x = NULL, .f = NULL, ...) {
  out <- purrr::map(.x, .f, ...)
  if (length(out) == 1) {
    out <- out[[1]]
    out <- unname(out)
  } else {
    out <- dplyr::bind_cols(out)
  }
  return(out)
}

#' Symbolic Median
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the median for symbolic intervals.
#' @param x A symbolic interval.
#' @param na.rm As in R median function.
#' @param method The method to be use.
#' @param ... As in R median function.
#'
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic Median
#' @export
#' @importFrom vctrs vec_data
#' @importFrom stats median
#' @rdname Symbolic_median
#'
median.symbolic_interval <- function(x, na.rm = FALSE, method = c("centers", "interval"), ...) {
  method <- match.arg(method)
  if (method == "interval") {
    out <- new.sym.intreval(
      stats::median(vctrs::vec_data(Re(x)), na.rm = na.rm),
      stats::median(vctrs::vec_data(Im(x)), na.rm = na.rm)
    )
    return(out)
  } else {
    out <- median((vctrs::vec_data(Re(x)) + vctrs::vec_data(Im(x))) / 2, na.rm = na.rm)
    return(out)
  }
}

#' @rdname Symbolic_median
#' @export
median.symbolic_tbl <- function(x, ...) map_symbolic_tbl(x, median, ...)

#' Symbolic Variance
#' @name var
#' @author Oldemar Rodriguez Rojas
#' @description Compute the symbolic variance.
#' @param x A symbolic interval.
#' @param y NULL (default) or a vector, matrix or data frame with
#' compatible dimensions to x. The default is equivalent to y = x (but more efficient).
#' @param use an optional character string giving a method for computing covariances
#' in the presence of missing values. This must be (an abbreviation of) one of the
#' strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#' or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param na.rm logical. Should missing values be removed?
#' @param ... As in R median function.
#'
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic Variance
#' @export
#'
var <- function(x, ...) UseMethod("var")

#' @rdname var
#' @export
var.default <- function(x, y = NULL, na.rm = FALSE, use, ...) stats::var(x, y = y, na.rm = na.rm, use)

#' @rdname var
#' @export
var.symbolic_interval <- function(x, method = c("centers", "interval", "billard"),
                                  na.rm = FALSE, ...) {
  method <- match.arg(method)
  if (method == "interval") {
    out <- new.sym.intreval(var(vctrs::vec_data(Re(x))), var(vctrs::vec_data(Im(x))))
    return(out)
  } else if (method == "centers") {
    out <- var((min(x) + max(x)), na.rm = na.rm) / 2
    return(out)
  } else if (method == "billard") {
    out <- (1 / (3 * length(x))) * sum(min(x)^2 +
      (min(x) * max(x)) + max(x)^2) - (1 / (4 * length(x)^2)) * sum(min(x) + max(x))^2
    return(out)
  }
}
#' @rdname var
#' @name var
#' @export
var.symbolic_tbl <- function(x, ...) map_symbolic_tbl(x, var, ...)


#' Generic function for the correlation
#' @name cor
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic correlation
#' @param x A symbolic variable.
#' @param y A symbolic variable.
#' @param use An optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param ... As in R cor function.
#'
#' @return Return a real number in [-1,1].
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic Correlation
#' @export
cor <- function(x, ...) UseMethod("cor", x)


#' @rdname cor
#' @export
cor.default <- function(x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"), ...) {
  stats::cor(x, y, use, method)
}

#' @rdname cor
#' @export
cor.symbolic_interval <- function(x, y, method = c("centers", "billard"), ...) {
  if (!(is.sym.interval(x) && is.sym.interval(y))) {
    stop("Impossible to compute the Standard Deviation for this type of variable with this method")
  }
  method <- match.arg(method)
  if (method == "centers") {
    out <- stats::cor((min(x) + max(x)) / 2, (min(y) + max(y)) / 2)
    return(out)
  }
  if (method == "billard") {
    out <- cov(x, y, method) / (sd(x, method) * sd(y, method))
    return(out)
  }
}

#' @rdname cor
#' @export
cor.symbolic_tbl <- function(x, ...) map_symbolic_tbl(x, cor, ...)

#' Generic function for the covariance
#' @name cov
#' @aliases cov
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic covariance.
#' @param x First symbolic variables.
#' @param y Second symbolic variables.
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param na.rm As in R cov function.
#' @param ... As in R cov function.
#'
#' @return Return a real number.
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic Covariance
#'
cov <- function(x, ...) {
  UseMethod("cov", x)
}

#' @rdname cov
#' @export
cov.default <- function(x, y = NULL, use = "everything",
                        method = c("pearson", "kendall", "spearman"), ...) {
  stats::cov(x, y, use, method)
}

#' @rdname cov
#' @export
cov.symbolic_interval <- function(x, y, method = c("centers", "billard"),
                                  na.rm = FALSE, ...) {
  Gj <- function(a, b, vmean) {
    if ((a + b) / 2 <= vmean) {
      return(-1)
    } else {
      return(1)
    }
  }
  Qj <- function(a, b, vmean) {
    return((a - vmean)^2 + (a - vmean) * (b - vmean) + (b - vmean)^2)
  }
  method <- match.arg(method)
  if (method == "centers") {
    out <- cov((min(x) + max(x)) / 2, (min(y) + max(y)) / 2)
    return(out)
  }
  if (method == "billard") {
    ss <- 0
    vmean.x <- mean(x, method = "centers")
    vmean.y <- mean(y, method = "centers")

    for (i in seq_len(length(x))) {
      ss <- ss + Gj(min(x[i]), max(x[i]), vmean.x) * Gj(
        min(y[i]),
        max(y[i]), vmean.y
      ) * sqrt(Qj(min(x[i]), max(x[i]), vmean.x) *
        Qj(min(y[i]), max(y[i]), vmean.y))
    }
    return((1 / (3 * length(x))) * ss)
  }
}

#' @rdname cov
#' @export
cov.symbolic_tbl <- function(x, ...) map_symbolic_tbl(x, cov, ...)

#' Generic function for the standard desviation
#' @name sd
#' @author Oldemar Rodriguez Rojas
#' @description Compute the symbolic standard desviation.
#' @param x A symbolic variable.
#' @param method The method to be use.
#' @param na.rm As in R sd function.
#' @param ... As in R sd function.
#'
#' @return return a real number.
#' @references Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic sd
#' @export
#'
sd <- function(x, ...) {
  UseMethod("sd", x)
}

#' @rdname sd
#' @export
sd.default <- function(x, na.rm = FALSE, ...) {
  stats::sd(x, na.rm)
}

#' @rdname sd
#' @export
#' @importFrom stats sd
sd.symbolic_interval <- function(x, method = c("centers", "interval", "billard"),
                                 na.rm = FALSE, ...) {
  method <- match.arg(method)
  if (method == "centers") {
    out <- sd((min(x) + max(x)) / 2)
    return(out)
  }
  if (method == "interval") {
    return(new.sym.intreval(stats::sd(min(x)), stats::sd(max(x))))
  }
  if (method == "billard") {
    out <- sqrt((1 / (3 * length(x))) * sum(min(x)^2 +
      (min(x) * max(x)) + max(x)^2) - (1 / (4 * (length(x))^2)) *
      sum(min(x) + max(x))^2)
    return(out)
  }
}

#' @rdname sd
#' @export
sd.symbolic_tbl <- function(x, ...) map_symbolic_tbl(x, sd, ...)

#' convertir a data.frame
#'
#' @param x a symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#' @export
#'
as.data.frame.symbolic_interval <- function(x, ...) {
  out <- lapply(x, function(x) data.frame(min = min(x), max = max(x)))
  out <- do.call("rbind", out)
  return(out)
}

#' calcula centros
#'
#' @param x tabla simbolica todos intervalos
#' @export
#' @importFrom tibble add_column as_tibble

interval.centers <- function(x) {
  if (!all(sapply(x, function(x) any(class(x) %in% "symbolic_interval")))) {
    stop("All variables have to be intervals")
  }
  out <- purrr::map_df(x, function(x) ((min(x) + max(x)) / 2))
  return(out)
}

#' calcula minimos
#'
#' @param x tabla simbolica todos intervalos
#' @export
#' @importFrom tibble add_column as_tibble

interval.min <- function(x) {
  if (!all(sapply(x, function(x) any(class(x) %in% "symbolic_interval")))) {
    stop("All variables have to be intervals")
  }
  out <- purrr::map_df(x, function(x) min(x))
  return(out)
}

#' calcula maximos
#'
#' @param x tabla simbolica todos intervalos
#' @export
#' @importFrom tibble add_column as_tibble

interval.max <- function(x) {
  if (!all(sapply(x, function(x) any(class(x) %in% "symbolic_interval")))) {
    stop("All variables have to be intervals")
  }
  out <- purrr::map_df(x, function(x) max(x))
  return(out)
}

#' calcula rangos
#'
#' @param x tabla simbolica todos intervalos
#' @export
#' @importFrom tibble add_column as_tibble
interval.ranges <- function(x) {
  if (!all(sapply(x, function(x) any(class(x) %in% "symbolic_interval")))) {
    stop("All variables have to be intervals")
  }
  out <- purrr::map_df(x, function(x) ((max(x) - min(x)) / 2))
  return(out)
}


# Histogram ---------------------------------------------------------------

#' Create an symbolic_histogram type object
#' @importFrom vctrs vec_assert new_vctr
#' @importFrom rlang abort
#' @importFrom dplyr lead lag
#' @keywords internal
#'
new.sym.histogram <- function(x = double(), breaks = NA_real_) {
  vctrs::vec_assert(x, numeric())
  a <- na.omit(dplyr::lead(breaks))
  b <- na.omit(dplyr::lag(breaks))
  out <- list(
    breaks = breaks,
    props = sapply(seq_along(a), function(i) sum(x >= b[i] & x < a[i]) / length(x))
  )
  new_vctr(list(out), class = "symbolic_histogram")
}


#' Create an symbolic_histogram type object
#'
#' @param x character vector
#' @param breaks a vector giving the breakpoints between histogram cells
#'
#' @return a symbolic histogram
#' @export
#'
#' @examples
#' sym.histogram(iris$Sepal.Length)
#' @importFrom vctrs vec_cast
#'
sym.histogram <- function(x = double(), breaks = NA_real_) {
  x <- vec_cast(x, double())
  new.sym.histogram(x, breaks)
}

#' Symbolic histogram
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_histogram and FALSE otherwise.
#'
#' @examples
#' x <- sym.histogram(iris$Sepal.Length)
#' is.sym.histogram(x)
#' @export
is.sym.histogram <- function(x) {
  inherits(x, "symbolic_histogram")
}

#' abbr for symbolic modal
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_histogram <- function(x) {
  "hist"
}

#' full name for symbolic modal
#' @keywords internal
#' @export
vec_ptype_full.symbolic_histogram <- function(x) {
  "symbolic_histogram"
}

#' Symbolic modal conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
format.symbolic_histogram <- function(x, ...) {
  out <- vector(mode = "character", length = length(x))
  for (i in seq_along(x)) {
    # mean. <- sprintf("%.2f",round(x[[i]]$mean,2))
    # sd. <- sprintf("%.2f",round(x[[i]]$sd,2))
    # out[i] <- stringr::str_glue("mean:{mean.} sd:{sd.}")

    # breaks <- x[[i]]$breaks
    # min. <- min(breaks)
    # max. <- max(breaks)
    # out[i] <- stringr::str_glue("breaks:{length(breaks)} min: {min.} max: {max.}")
    out[i] <- "<hist>"
  }
  out
}


#' $ operator for histograms
#'
#' @param x .....
#' @param name ...
#' @export
`$.symbolic_histogram` <- function(x, name) {
  if (length(x) == 1L) {
    return(x[[1]][[name]])
  } else {
    return(lapply(x, function(x) x[[name]]))
  }
}


#' a data.frame
#'
#' @param x .....
#' @param  ... ...
#' @export
#' @importFrom stats na.omit
#' @importFrom dplyr lead lag
#'
as.data.frame.symbolic_histogram <- function(x, ...) {
  df <- do.call("rbind", x$props)
  df <- as.data.frame(df)

  a <- stats::na.omit(dplyr::lead(x$breaks[[1]]))
  b <- stats::na.omit(dplyr::lag(x$breaks[[1]]))

  colnames(df) <- paste0("[", b, " : ", a, "]")
  df
}


# Modal -------------------------------------------------------------------

#' Create an symbolic_modal type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new.sym.modal <- function(x = character()) {
  x <- prop.table(table(x))
  out <- list(
    var = names(x),
    prop = as.numeric(x)
  )
  vctrs::new_vctr(list(out), class = "symbolic_modal")
}

#' Create an symbolic_modal type object
#'
#' @param x character vector
#'
#' @return a symbolic modal
#' @export
#'
#' @examples
#' sym.modal(factor(c("a", "b", "b", "l")))
#' @importFrom vctrs vec_cast
#'
sym.modal <- function(x = character()) {
  if (!any(class(x) %in% c("factor", "integer"))) {
    stop("To create a variable of modal type, the data must be of type factor or integer.")
  }
  new.sym.modal(x)
}

#' Symbolic modal
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_modal and FALSE otherwise.
#'
#' @examples
#' x <- sym.modal(factor(c("a", "b", "b", "l")))
#' is.sym.modal(x)
#' @export
is.sym.modal <- function(x) {
  inherits(x, "symbolic_modal")
}

#' abbr for symbolic modal
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_modal <- function(x) {
  "modal"
}

#' full name for symbolic set
#' @keywords internal
#' @export
vec_ptype_full.symbolic_modal <- function(x) {
  "symbolic_modal"
}

#' Symbolic modal conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#' @importFrom  stringr str_trunc
format.symbolic_modal <- function(x, ...) {
  out <- vector(mode = "character", length = length(x))
  for (i in seq_along(x)) {
    cats <- abbreviate(x[[i]]$var, 3)
    props <- sprintf("%.2f", round(x[[i]]$prop, 2))
    text <- paste0(paste0(cats, ":", props), collapse = " ")
    text <- stringr::str_trunc(text, width = 30, ellipsis = "...")
    out[i] <- text
  }
  out
}


#' Extract categories
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export

get_cats <- function(x, ...) UseMethod("get_cats")

#' @export
#' @rawNamespace S3method(var, default)
get_cats.symbolic_modal <- function(x, ...) {
  if (length(x) == 1) {
    return(x[[1]]$var)
  } else {
    return(lapply(x, function(x) x$var))
  }
}

#' Extract prop
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export

get_props <- function(x, ...) UseMethod("get_props")


#' @rawNamespace S3method(var, default)
#' @export
get_props.symbolic_modal <- function(x, ...) {
  if (length(x) == 1) {
    out <- x[[1]]$prop
    return(out)
  } else {
    return(lapply(x, function(x) x$prop))
  }
}

#' Extract values
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#'
as.data.frame.symbolic_modal <- function(x, ...) {
  out <- do.call("rbind", x$props)
  colnames(out) <- unique(do.call("c", x$cats))
  out <- as.data.frame(out)
  return(out)
}



#' $ operator for modals
#'
#' @param x .....
#' @param name ...
#' @export
`$.symbolic_modal` <- function(x, name = c("cats", "props", "counts")) {
  switch(name,
    cats = get_cats(x),
    props = get_props(x),
    NULL
  )
}


# Set ---------------------------------------------------------------------

#' Create an symbolic_set type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new.sym.set <- function(x = NA) {
  out <- factor(unique(x), levels = levels(x))
  vctrs::new_vctr(list(out), class = "symbolic_set")
}

#' Create an symbolic_set type object
#'
#' @param x character vector
#'
#' @return a symbolic set
#' @export
#'
#' @examples
#' sym.set(factor(c("a", "b", "b", "l")))
#' @importFrom vctrs vec_cast
#'
sym.set <- function(x = NA) {
  if (!any(class(x) %in% c("factor", "integer"))) {
    stop("To create a variable of modal type, the data must be of type factor or integer")
  }
  x <- as.factor(x)
  new.sym.set(x)
}

#' Symbolic set
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_set and FALSE otherwise.
#'
#' @examples
#' x <- sym.set(factor(c("a", "b", "b", "l")))
#' is.sym.set(x)
#' @export
is.sym.set <- function(x) {
  inherits(x, "symbolic_set")
}

#' abbr for symbolic set
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_set <- function(x) {
  "set"
}

#' full name for symbolic set
#' @keywords internal
#' @export
vec_ptype_full.symbolic_set <- function(x) {
  "symbolic_set"
}

#' Symbolic set conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#' @importFrom  stringr str_trunc
format.symbolic_set <- function(x, ...) {
  out <- vector(mode = "character", length = length(x))
  for (i in seq_along(x)) {
    cats <- as.character(x[[i]])
    text <- paste0(cats, collapse = ",")
    text <- stringr::str_trunc(text, width = 30, ellipsis = "...")
    text <- paste0("{", text, "}")
    out[i] <- text
  }
  out
}


#' $ operator for set
#'
#' @param x .....
#' @param name ...
#' @export
`$.symbolic_set` <- function(x, name = c("levels", "values")) {
  switch(name,
    levels = {
      if (length(x) == 1) {
        return(levels(x[[1]]))
      } else {
        return(lapply(x, levels))
      }
    },
    values = {
      if (length(x) == 1) {
        return(table(x[[1]]))
      } else {
        return(lapply(x, table))
      }
    },
    NULL
  )
}

#' convertir a data.frame
#'
#' @param x a symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#' @export
#'
as.data.frame.symbolic_set <- function(x, ...) {
  out <- lapply(x, function(x) table(x))
  out <- do.call("rbind", out)
  return(out)
}
