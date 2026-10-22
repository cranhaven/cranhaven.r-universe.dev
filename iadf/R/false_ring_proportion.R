globalVariables(".")

#frp----------------------------------------------------------------------------
#' @title false ring proportion
#' @description Calculate the false ring proportion of a set of
#'   binary false ring assignments.
#' @param iadf A data frame with numeric columns representing individual series
#'   and years as rownames where years with IADF are marked binary with 1,
#'   those without with 0, years not covered by the series are set to NA.
#' @return a data frame
#' @seealso \code{\link{afrp}}
#' @export
frp <- function(iadf) {

  if (!is.data.frame(iadf) && !is.matrix(iadf)) {
    stop("x has to be a data.frame or matrix")
  }

  if (!all(as.matrix(iadf) %in% c(0, 1, NA))) {
    stop("the data frame must only contain 0, 1, and NA values")
  }

  n <- ncol(iadf) - rowSums(is.na(iadf), na.rm = TRUE)

  if (any(n == 0)) {
    warning("there are no trees in year: ",
            paste(names(n[n == 0]), collapse = ", "))
  }

  iadfp <- rowMeans(iadf, na.rm = TRUE)

  out <- data.frame(year = as.numeric(names(iadfp)), frp = iadfp)
  return(out)
}

# afrp  ------------------------------------------------------------------------
#' @title adjusted false ring proportion
#' @description Calculate the adjusted false ring proportion, as
#'   suggested by Osborn et. al. (1997), of a set of binary false ring
#'   assignments.
#' @param iadf A data frame with numeric columns representing individual series
#'   and years as rownames where years with IADF are marked with 1,
#'   those without with 0, years not covered by the series are set to NA.
#' @references Osborn TJ, Briffa KR and Jones PD (1997) Adjusting variance for
#'   sample-size in tree-ring chronologies and other regional mean time-series.
#'   Dendrochronologia 15, 89-99.
#' @return a data frame
#' @seealso \code{\link{frp}}
#' @export
afrp <- function(iadf) {

  if (!is.data.frame(iadf) && !is.matrix(iadf)) {
    stop("x has to be a data.frame or matrix")
  }

  if (!all(as.matrix(iadf) %in% c(0, 1, NA))) {
    stop("the data frame must only contain 0, 1, and NA values")
  }

  n <- ncol(iadf) - rowSums(is.na(iadf), na.rm = TRUE)

  if (any(n == 0)){
    warning("there are no trees in year: ",
            paste(names(n[n == 0]), collapse = ", "))
  }

  iadfp <- rowMeans(iadf, na.rm = TRUE)

  aiadfp <- iadfp * sqrt(n)

  out <- data.frame(year = as.numeric(names(aiadfp)), afrp = aiadfp)
  return(out)
}

# novak_freq -------------------------------------------------------------------
#' @title iadf frequency per cambial age
#' @description Calculate the frequency per cambial age as suggested
#'  by Novak et al. (2013).
#' @param iadf A data frame with numeric columns representing individual series
#'   and years as rownames where years with IADF are marked binary with 1,
#'   those without with 0, years not covered by the series are set to NA.
#' @param po a data frame with pith offsets with series names in the first and
#'   pith offset as number of rings in the second column
#' @references
#'   Novak, Klemen and Sánchez, Miguel Angel Saz and Čufar, Katarina and
#'   Raventós, Josep and de Luis, Martin. Age, climate and intra-annual density
#'   fluctuations in in Spain, IAWA Journal, 34, 459-474 (2013),
#'   doi:10.1163/22941932-00000037
#' @return a data frame
#' @export
#' @seealso \code{\link{novak_weibull}},
#'   \code{\link{novak_index}}
#' @examples
#' data('example_iadf')
#' model <- novak_weibull(novak_freq(example_iadf), 15)
#' novak_index(example_iadf, model)

novak_freq <- function(iadf, po = NULL){

  if (!is.data.frame(iadf) && !is.matrix(iadf)) {
    stop("iadf has to be a data.frame or matrix")
  }

  if (!is.null(po)) {
    if (!is.data.frame(po) || ncol(po) != 2) {
      stop("po has to be a data.frame with ncol == 2")
    }
  }

  if (!all(as.matrix(iadf) %in% c(0, 1, NA))) {
    stop("iadf must only contain 0, 1, and NA values")
  }

  aligned <- to_cambial_age(iadf, po)
  freq <- rowMeans(aligned, na.rm = TRUE)
  n <- rowSums(!is.na(aligned))
  out <- data.frame(cambial.age = seq_along(freq), freq = freq,
                    sample.depth = n)
  class(out) <- c("data.frame", "novak.freq")
  return(out)
}


# novak weibull find start values ----------------------------------------------
#' @title novak_weibull_find_start
#' @description Find good start values manually in case
#' \code{\link{novak_weibull}}
#'   returns an error caused by insufficient default starting values.
#' @param novak_freq_object A novak_freq_object as obtained from
#'   \code{\link[iadf]{novak_freq}}
#' @param min.n minimum number of samples within each cambial age to be included
#' in model estimation
#' @param max_a maximum value of manipulate slider for parameter a
#' @param max_b maximum value of manipulate slider for parameter b
#' @param max_c maximum value of manipulate slider for parameter c
#'
#' @return a list which can be used as input argument 'start' in
#' \code{\link{novak_weibull}}
#' @export

novak_weibull_find_start <- function(novak_freq_object, min.n = 15, max_a = 10,
                                     max_b = 3, max_c = 30) {
  start <- list(a = 4, b = 0.33, c = 15.5)
  observe <- FALSE

  a0 <- c(); b0 <- c(); c0 <- c(); x <- c(); return_value <- c()

  manipulate::manipulate({
    plot(novak_freq_object[["freq"]] ~ novak_freq_object[["cambial.age"]],
         pch = 16, col = ifelse(novak_freq_object[["sample.depth"]] >= min.n,
                                "black", "gray80"),
         xlab = "cambial age", ylab = "frequency")
    a <- a0;  b <- b0;  c <- c0
    curve(a * b * c * x^2 * exp(-a * x^b), add = TRUE)
    start <<- list(a = a, b = b, c = c)
    if (return_value) {
      observe <<- TRUE
    }
  },
  a0 = manipulate::slider(0, max_a, step = 0.0001, initial = start$a),
  b0 = manipulate::slider(0, max_b, step = 0.0001, initial = start$b),
  c0 = manipulate::slider(0, max_c, step = 0.0001, initial = start$c),
  return_value = manipulate::button("Return value")
  )

  repeat{
    if (observe) {
      return(start)
    }
  }
}

# novak_weibull ----------------------------------------------------------------
#' @title novak_weibull
#' @description Fit a Weibull function for the calculation of
#'   age corrected IADF frequencies according to Novak et al. (2013).
#' @param novak_freq_object A novak_freq_object as obtained from
#'   \code{\link[iadf]{novak_freq}}
#' @param min.n minimum number of samples within each cambial age to be included
#' in model estimation
#' @param start set custom start values - default to
#' \code{list(a = 4, b = 0.33, c = 15.5)}
#' @param max.iter maximum iterations for internally used
#' \code{\link[stats]{nls}}
#' @param make.plot logical
#' @param ... additional plotting arguments
#' @references
#'   Novak, Klemen and Sánchez, Miguel Angel Saz and Čufar, Katarina and
#'   Raventós, Josep and de Luis, Martin. Age, climate and intra-annual density
#'   fluctuations in in Spain, IAWA Journal, 34, 459-474 (2013),
#'   doi:10.1163/22941932-00000037
#' @return a model object of class "nls"
#' @export
#' @seealso \code{\link{novak_freq}},
#'   \code{\link{novak_index}}
#' @examples
#' data('example_iadf')
#' model <- novak_weibull(novak_freq(example_iadf), 15)
#' novak_index(example_iadf, model)
novak_weibull <- function(novak_freq_object, min.n = 15, start = NULL,
                          max.iter = 500, make.plot = TRUE, ...) {

  if (!any(class(novak_freq_object) == "novak.freq")) {
    stop("input must be derived from novak_freq()")
  }

  tmp <- novak_freq_object[novak_freq_object[["sample.depth"]] >= min.n &
                             rowSums(is.na(novak_freq_object)) < 3, ]

  cambial_age <- tmp[["cambial.age"]]
  freq <- tmp[["freq"]]

  if (is.null(start)) {
    start <- list(a = 4, b = 0.33, c = 15.5)
  }

  try({
    wbu <- stats::nls(freq ~ a * b * c * cambial_age^2 * exp(-a * cambial_age^b),
                      start = start, control = list(maxiter = max.iter))

    if (make.plot) {
      ndata <- data.frame(cambial_age = seq(0, max(cambial_age, na.rm = TRUE),
                                            by = 0.1))
      wpred <- predict(wbu, newdata = ndata)
      plot(novak_freq_object[["freq"]] ~ novak_freq_object[["cambial.age"]],
           pch = 16, col = ifelse(novak_freq_object[["sample.depth"]] >= min.n,
                                  "black", "gray80"),
           xlab = "cambial age", ylab = "frequency", ...)
      lines(ndata$cambial_age, wpred, col = "red")
    }

    return(wbu)
  })
}


# novak_index ------------------------------------------------------------------
#' @title novak_index
#' @description  Calculation of age corrected IADF frequencies according
#'   to Novak et al. (2013).
#' @param iadf A data frame with numeric columns representing individual series
#'   and years as rownames where years with IADF are marked binary with 1,
#'   those without with 0, years not covered by the series are set to NA.
#' @param model a model, output of either  \code{\link[iadf]{novak_weibull}}
#' @return a data frame
#' @param po an optional data frame of pith offsets with series names in the
#'   first and pith offsets in the second column
#' @param method method for the RCS detrending, 'quotient' or 'difference'
#' @references
#'   Novak, Klemen and Sánchez, Miguel Angel Saz and Čufar, Katarina and
#'   Raventós, Josep and de Luis, Martin.  Age, climate and intra-annual density
#'   fluctuations in in Spain, IAWA Journal, 34, 459-474 (2013),
#'   doi:10.1163/22941932-00000037
#' @export
#' @seealso \code{\link{novak_freq}}, \code{\link{novak_weibull}}
#' @examples
#' data('example_iadf')
#' model <- novak_weibull(novak_freq(example_iadf), 15)
#' novak_index(example_iadf, model)
novak_index <- function(iadf, model, po = NULL, method = "difference"){

  if (is.null(po)) {
    po <- data.frame(series = names(iadf), po = 1)
  }

  if (!method %in% c("quotient", "difference")) {
    stop("please choose either 'quotient' or 'difference' as method")
  }

  if (!is.data.frame(iadf)) {
    stop("iadf must be of class data.frame")
  }

  if (!(is.data.frame(po))) {
    stop("po must be of class data.frame")
  }

  if (!setequal(po[, 1], names(iadf))) {
    stop("series names in po are not the same as provided in iadf")
  }

  longest <- max(series_length(iadf) + po[, 2])
  rc <- stats::predict(model, newdata = list(cambial_age = seq_len(longest)))
  detrended <- detrend_given_rc(iadf, rc, po, method = method)
  tmp <- rowMeans(detrended, na.rm = TRUE)
  out <- data.frame(year = as.numeric(names(tmp)), index = as.vector(tmp))
  return(out)
}


# campelo_freq -----------------------------------------------------------------
#' @title iadf frequency per ring width class
#' @description Calculate the frequency per ring width class as suggested
#'  by Campelo (2015).
#' @param iadf A data frame with numeric columns representing individual series
#'   and years as rownames where years with IADF are marked binary with 1,
#'   those without with 0, years not covered by the series are set to NA.
#' @param rwl data frame containing ring widths with years in rows and
#'   series in columns
#' @param n number of ring width classes
#' @return a data frame
#' @references
#'   Campelo, F., Vieira, J., Battipaglia, G. et al. Which matters most for the
#'   formation of intra-annual density fluctuations in Pinus pinaster: age or
#'   size? Trees (2015) 29: 237. doi:10.1007/s00468-014-1108-9
#' @export
#' @seealso  \code{\link{campelo_chapman}}, \code{\link{campelo_index}}
#' @importFrom dplyr %>%
#' @examples
#' data('example_iadf')
#' data('example_rwl')
#' model <- campelo_chapman(campelo_freq(example_iadf, example_rwl))
#' campelo_index(example_iadf, example_rwl, model)
campelo_freq <- function(iadf, rwl, n = 20){
  
  year <- NULL # silence R CMD check

  if (!is.data.frame(iadf)) {
    stop("iadf has to be a data.frame")
  }

  if (!is.data.frame(rwl)) {
    stop("rwl has to be a data.frame")
  }

  if (!all(as.matrix(iadf) %in% c(0, 1, NA))) {
    stop("iadf must only contain 0, 1, and NA values")
  }

  nclass <- rlang::quo(n)

  iadf_tidy <- iadf %>%
    tibble::rownames_to_column("year") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = as.integer(year)) %>%
    tidyr::gather("series", "iadf", -year) %>%
    dplyr::filter(!is.na(iadf))

  rwl_tidy <- tidy_rwl(rwl) %>%
    dplyr::mutate(class = cut(rwl, !!nclass))

  tmp <- dplyr::left_join(iadf_tidy, rwl_tidy, by = c("year", "series"))

  out <- tmp %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(freq = mean(iadf, na.rm = TRUE),
                     class.mean.rwl = mean(rwl, na.rm = TRUE),
                     sample.depth = length(iadf)) %>%
    as.data.frame

  class(out) <- c("data.frame", "campelo.freq")

  return(out)
}

# campelo chapman find start values --------------------------------------------
#' @title campelo_chapman_find_start
#' @description Find good start values manually in case
#' \code{\link{campelo_chapman}}
#'   returns an error caused by insufficient default starting values.
#' @param campelo_freq_object a campelo frequency object,
#' output of \code{\link[iadf]{campelo_freq}}
#' @param min.n minimum number of samples within each group to be included in
#'   model estimation
#' @param max_a maximum value of manipulate slider for parameter a
#' @param max_b maximum value of manipulate slider for parameter b
#' @param max_c maximum value of manipulate slider for parameter c
#'
#' @return a list which can be used as input argument 'start' in
#' \code{\link{campelo_chapman}}
#' @export
campelo_chapman_find_start <- function(campelo_freq_object, min.n = 15,
                                       max_a = 3, max_b = 1, max_c = 17) {

  tmp <- campelo_freq_object[campelo_freq_object[["sample.depth"]] >= min.n &
                               rowSums(is.na(campelo_freq_object)) < 3, ]

  ring_width <- tmp[["class.mean.rwl"]]
  freq <- tmp[["freq"]]

  start <- list(a = max_a / 2, b = max_b / 2, c = max_c / 2)
  observe <- FALSE

  a0 <- c(); b0 <- c(); c0 <- c(); x <- c(); return_value <- c()

  manipulate::manipulate({
    plot(campelo_freq_object[["freq"]] ~ campelo_freq_object[["class.mean.rwl"]],
         pch = 16, col = ifelse(campelo_freq_object[["sample.depth"]] >= min.n,
                                "black", "gray80"),
         xlab = "mean class ring width", ylab = "frequency")
    a <- a0;  b <- b0;  c <- c0
    curve(a * (1 - exp(-b * x))^c, add = TRUE)
    start <<- list(a = a, b = b, c = c)
    if (return_value) {
      observe <<- TRUE
    }
  },
  a0 = manipulate::slider(0, max_a, step = 0.0001, initial = start$a),
  b0 = manipulate::slider(0, max_b, step = 0.0001, initial = start$b),
  c0 = manipulate::slider(0, max_c, step = 0.0001, initial = start$c),
  return_value = manipulate::button("Return value")
  )

  repeat{
    if (observe){
      return(start)
    }
  }
}

# campelo_chapman --------------------------------------------------------------
#' @title campelo_chapman
#' @description Chapman model fitting to size classes for the calculation of
#' size corrected IADF frequencies according to Campelo et al. (2015).
#' @param campelo_freq_object a campelo frequency object,
#' output of \code{\link[iadf]{campelo_freq}}
#' @param min.n minimum number of samples within each group to be included in
#'   model estimation
#' @param start set custom start values - default to
#' \code{list(a = 0.8, b = 0.03, c = 12.5)}
#' @param max.iter maximum iterations for internally used
#' \code{\link[stats]{nls}}
#' @param make.plot logical
#' @param ... additional plotting arguments
#' @return a model object of class "nls"
#' @export
#' @seealso \code{\link{campelo_freq}}, \code{\link{campelo_index}}
#' @references
#'   Campelo, F., Vieira, J., Battipaglia, G. et al. Which matters most for the
#'   formation of intra-annual density fluctuations in Pinus pinaster: age or
#'   size? Trees (2015) 29: 237. doi:10.1007/s00468-014-1108-9
#' @examples
#' data('example_iadf')
#' data('example_rwl')
#' model <- campelo_chapman(campelo_freq(example_iadf, example_rwl))
#' campelo_index(example_iadf, example_rwl, model)
campelo_chapman <- function(campelo_freq_object, min.n = 15, start = NULL,
                            make.plot = TRUE, max.iter = 500, ...){

  if (!any(class(campelo_freq_object) == "campelo.freq")) {
    stop("input must be derived from campelo_freq()")
  }

  tmp <- campelo_freq_object[campelo_freq_object[["sample.depth"]] >= min.n &
                               rowSums(is.na(campelo_freq_object)) < 3, ]

  ring_width <- tmp[["class.mean.rwl"]]
  freq <- tmp[["freq"]]

  if (is.null(start)) {
    start <- list(a = 0.8, b = 0.03, c = 12.5)
  }

  try({
    chapm <- stats::nls(freq ~ a * (1 - exp(-b * ring_width))^c, start = start,
                        control = list(maxiter = max.iter))

    if (make.plot){
      ndata <- data.frame(ring_width = seq(0, max(ring_width, na.rm = TRUE),
                                           by = 0.1))
      cpred <- predict(chapm, newdata = ndata)
      plot(campelo_freq_object[["freq"]] ~ campelo_freq_object[["class.mean.rwl"]],
           pch = 16, col = ifelse(campelo_freq_object[["sample.depth"]] >= min.n,
                                  "black", "gray80"),
           xlab = "mean class ring width", ylab = "frequency", ...)
      lines(ndata$ring_width, cpred, col = "red")
    }

    return(chapm)
  })
}


# campelo_index ----------------------------------------------------------------
#' @title campelo_index
#' @description Calculation of size corrected IADF frequencies according
#'   to Campelo et al. (2015)
#' @param iadf A data frame with numeric columns representing individual series
#'   and years as rownames where years with IADF are marked binary with 1,
#'   those without with 0, years not covered by the series are set to NA.
#' @param rwl a rwl/data.frame object
#' @param model a chapman model, output of \code{\link[iadf]{campelo_chapman}}
#' @return a data frame
#' @export
#' @seealso \code{\link{campelo_freq}}, \code{\link{campelo_chapman}}
#' @importFrom dplyr %>%
#' @references
#'   Campelo, F., Vieira, J., Battipaglia, G. et al. Which matters most for the
#'   formation of intra-annual density fluctuations in Pinus pinaster: age or
#'   size? Trees (2015) 29: 237. doi:10.1007/s00468-014-1108-9
#' @examples
#' data('example_iadf')
#' data('example_rwl')
#' model <- campelo_chapman(campelo_freq(example_iadf, example_rwl))
#' campelo_index(example_iadf, example_rwl, model)
campelo_index <- function(iadf, rwl, model){

  # silence R CMD check
  year <- prediction <- index <- NULL

  if (!is.data.frame(iadf)) {
    stop("iadf has to be a data.frame")
  }

  if (!is.data.frame(rwl)) {
    stop("rwl has to be a data.frame")
  }

  if (!all(as.matrix(iadf) %in% c(0, 1, NA))) {
    stop("iadf must only contain 0, 1, and NA values")
  }

  iadf_tidy <- iadf %>%
    tibble::rownames_to_column("year") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    tidyr::gather("series", "iadf", -year) %>%
    dplyr::filter(!is.na(iadf))

  rwl_tidy <- tidy_rwl(rwl)

  tmp <- dplyr::left_join(iadf_tidy, rwl_tidy, by = c("year", "series")) %>%
    dplyr::mutate(prediction =
                    stats::predict(model,
                                    newdata = list(ring_width = rwl))) %>%
    dplyr::mutate(index = iadf - prediction) %>%
    dplyr::select("year", "series", "index") %>%
    tidyr::spread("series", "index")

  mean_index <- cbind(tmp["year"], rowMeans(dplyr::select(tmp, -year),
                                            na.rm = TRUE))
  names(mean_index) <- c("year", "index")

  spline <- dplR::caps(mean_index[["index"]])
  detrend_index <- dplyr::mutate(mean_index,
                                  detrended_index = index - spline)

  return(detrend_index)
}
