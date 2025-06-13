#' An R package for multiscale climate model assessment
#'
#' Contains functions for flexible assessment of climate model bias and changes at multiple time scales. See documentation for \code{\link{decomp}}, \code{\link{compare}} and \code{\link{vcompare}}. In addition, musica provides functions for multiscale transformations of time series (see \code{\link{msTrans_abs}} and \code{\link{msTrans_dif}})
#'
#' @section Package options:
#' Following option(s) are available:
#' \describe{
#'  \item{additive_variables}{At several places the package compares values. The character vector \code{additive_values} specifies for which variables difference should be used for comparison instead of ratio. Defaults to \code{additive_values = "TAS"}. See \code{\link{options}} for setting or examining options.}
#' }
#'
#'
#' @author Martin Hanel \email{hanel@@fzp.czu.cz}
#' @references Hanel, M., Kozin, R. (2016) Bias correction for hydrological modelling, submitted.
#' @docType package
#' @name musica-package
NULL

#' @importFrom lubridate '%m+%' '%m-%' duration
NULL

#' @import qmap
NULL

#' @import data.table

#' @importFrom magrittr "%>%"
NULL

#' @importFrom stats D approxfun cor ks.test lm loess quantile runmed smooth smooth.spline splinefun ts
NULL

#' @importFrom utils combn
NULL

utils::globalVariables(c('.', 'CF', 'COMPARE_TO', 'CORR', 'CURR', 'D', 'D1', 'DTM', 'FROM', 'G1', 'GRP', 'ID', 'MEA', 'MODIF', 'N', 'NEWDATA', 'ORIG', 'REQ', 'RID', 'TO', 'TS', 'TYP', 'V1', 'V2', 'cDTM', 'comp', 'fun', 'iter','p',  'p.value', 'period', 'period_pos', 'prse', 'sFROM', 'sMODIF', 'sNEWDATA', 'sTO', 'sub_period', 'value', 'variable', 'x', 'y', 'Q' ))

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.musica <- list(
    'additive_variables' = 'TAS'
  )
  toset <- !(names(op.musica) %in% names(op))
  if(any(toset)) options(op.musica[toset])

  invisible()
}




#' Functions for evaluating distance between variables
#'
#'
#' @param x,y variables to be compared
#' @param var variable code
#'
#' @return Difference or ratio of \code{x} and \code{y} (for \code{dif}) and sum or product (for \code{rev_dif} and \code{rev_difv}). Distance is measured as difference for variables included in \code{getOption('additive_variables')}, i.e. temperature (\code{TAS}) by default, and as a ratio for other variables.
#'
#' While \code{rev_dif} returns \code{sum(x, y)} or \code{prod(x, y)}, \code{rev_difv} takes single vector \code{x} and returns \code{sum(x)} or \code{prod(x)}.
#'
#' Used mainly in other functions of the package.
#'
#' @examples
#' getOption('additive_variables')
#'
#' # calculate distance of 2 vectors
#' dif(c(10, 20, 30), c(11, 18, 3), 'TAS')
#' dif(c(10, 20, 30), c(11, 18, 3), 'PR')
#'
#' # inverse for 2 vectors
#' rev_dif(c(10, 20, 30), c(11, 18, 3), 'TAS')
#'
#' # inverse for 1 vector
#' rev_difv(c(10, 1.1, .9), 'TAS')
#' @name difs

#' @rdname difs
#' @export
dif = function(x, y, var){
  if (var %in% getOption('additive_variables')) return(x-y)
  r = x/y
  r[y==0] = 0
  return(r)
}


#' @rdname difs
#' @export
rev_dif = function(x, y, var){
  if (var %in% getOption('additive_variables')) return(x+y)
  r = x*y
  return(r)
}

#' @rdname difs
#' @export
rev_difv = function(x, var){
  if (var %in% getOption('additive_variables')) {return(sum(x))} else {return(prod(x))}

}

#' Evaluation of empirical cumulative distribution function
#'
#' @param x vector of values
#'
#' @return value of the empirical distribution function evaluated at \code{x}
#' @export
#'
#' @examples prob(rnorm(10))
prob = function(x){
  (rank(x)-.3) / (length(x)+.4)
}

# vprod = function(x, variable, ...){
#   if (variable %in% getOption('additive_variables')) return(sum(x, ...)) else (return(prod(x, ...)))
# }


#' Decomposition of time-series
#'
#' Calculate series of averages over the periods specified in the \code{period} argument into the inpur data.table.
#'
#' @param x data.table with columns \code{DTM} (date), \code{variable} and \code{value}. Any number of variables are in principle allowed.
#' @param period The periods over which the averages will be calculated, see Details
#' @param agg_by Function for specification of the period (season, month) to be additionaly included in output, see Details
#' @param full_return (logical) Should the average be repeated for each scale along with original time series? Default is FALSE (e.g. for M1 only monthly and not daily time series is returned)
#' @param remove_incomplete Should the incomplete years be removed from results? Default is TRUE
#' @details The original time series in daily time step is decomposed into series of averages ove periods specified in \code{periods} argument using letter codes `D` - day(s), `M` - month(s), `Y` - year(s) followed by number corresponding to number of periods and `G1` the overall mean. The periods must be given in order from longest to shortest, the overall mean is always included (and needs not to be specified in \code{period}). Shorter periods are always identified within the closest longer periods, i.e. each shorter period is included in exactly one longer period. As a result, the averages may be calculated over shorter periods than specified. This is due to varying length of "month" and "year" periods. The actual length used for averaging is included in the output. To make further assessment of the decomposed objects easier, indicator of period within the year (e.g. quarter or month) as specified by \code{agg_by} argument is included in the output.
#'
#'@return data.table with variables:
#'\describe{
#'  \item{variable}{factor indicating the variable}
#'  \item{DTM}{date}
#'  \item{period}{specification of the averaging length with `D` - day(s), `M` - month(s), `Y` - year(s) and `G1` - the overall mean }
#'  \item{value}{value of the variable for given averaging length}
#'  \item{sub_period}{indication of the aggregating scale specified by \code{agg_by} argument}
#'  \item{period_pos}{average date of the interval}
#'  \item{N}{real length of the vectors used for calculating averages}
#'  \item{TS}{averaging length in hours}
#'}
#' @export decomp
#'
#' @examples
#' data(basin_PT)
#' str(basin_PT)
#' basin_PT[['obs_ctrl']]
#' dobs = decomp(basin_PT[['obs_ctrl']], period = c('1 year', '1 month', '1 day'))
decomp = function(x, period = c('Y1', 'M6', 'M3', 'M1', 'D15', 'D1'), agg_by = quarter, full_return = FALSE, remove_incomplete = TRUE){

  periods = code2period(period)
  year_starts = months(0)
  if (!all(grepl('year|month|day', periods))) stop("periods argument must be specified using 'year', 'month', 'day' keywords only. See ?decomp for details.")
  names(periods) = period2code(periods)
  x = copy(x)

  # shift for arbitrary year start (climatological, hydrological etc.)
  x[, cDTM := as.IDate(DTM %m-% year_starts)]

  if (remove_incomplete){
    x[, N:=.N, by = .(year(cDTM))]
    x = x[N>364]
    x[, N:=NULL]
  }

  for (i in 1:length(periods)){
    bby = if (i>1) {names(periods)[1:(i-1)]} else {NULL}
    x[, (names(periods)[i]) := cut(cDTM, breaks = periods[i]), by = bby]
  }

  wh = names(x)[! names(x) %in% c('DTM', 'cDTM', names(periods))]
  x[, G1:= mean(cDTM)]
  mo = melt(x, measure.vars = wh)

  R = list()
  for (i in 0:length(periods)){

    bby = if (i>0) {names(periods)[i]} else {'G1'}
    ab = agg_by
    R[[i+1]] = if (!full_return) {
      mo[,  .(period = bby, value = mean(value), sub_period = unique(ab(cDTM)), period_pos = unique(prse), N = .N), by = .(variable, prse = eval(parse(text = bby)))]
    } else {
      mo[,  .(dDTM = DTM, period = bby, value = mean(value), sub_period = (ab(cDTM)), period_pos = unique(prse), N = .N), by = .(variable, prse = eval(parse(text = bby)))]
    }

    }

  R = do.call(rbind, R)
  R[, TS:= tscale(period)]
  setnames(R, 'prse', 'DTM')
  R[, DTM := as.IDate(DTM %m+% year_starts)]
  R[, period_pos := as.IDate(DTM) + ifelse(period!='G1', tscale(period)/24/2, 0)]

  copy(R)

}

#' Convert averaging length code to hours
#'
#' Period durations are calculated by the \code{\link{lubridate}} package.
#'
#' @param x Vector of the averaging period codes
#' @param nyears Overall number of years - used for conversion of the overall mean
#'
#' @return numerical vector of durations in hours
#' @export tscale
#'
#' @examples
#' tscale('M1')
#' tscale('G1', nyears = 25)
tscale = function(x, nyears = 30){

  num = suppressWarnings(as.integer(gsub("[^\\d]+", "", x, perl = TRUE)))
  xx = gsub('G', 'year ',  x)
  xx = gsub('Y', 'year ', xx)
  xx = gsub('M', 'month ', xx)
  xx = gsub('D', 'day ', xx)
  xx[grepl('G', x)] = gsub(1, nyears, xx[grepl('G', x)])

  xs = strsplit(xx, ' ')
  xs = data.table(x, do.call(rbind, xs))
  xs[, ts:= unclass(duration(as.double(V2[1]), V1[1]))/3600, by = x]
  return(xs$ts)

}

#' Compare decomposed variables
#'
#' The function evaluates distance between statistical characteristics of specified data sets. Distance is measured as difference for variables included in \code{getOption('additive_variables')}, i.e. temperature (\code{TAS}) by default, and as a ratio for other variables.
#'
#' @param x List of decomposed variables to be compared
#' @param compare_to Decomposed variable used as a reference
#' @param fun Function used for comparison
#' @param wet_int_only (logical) Should only the wet intervals be considered?
#' @param wet_int_thr Numeric value specifying the minimum depth to be considered wet
#' @param exclude_below Some of the intervals might not be of required length, e.g. D10 interval may have less than 10 days available. The \code{exclude_below} argument controls the minimum fraction of the interval that has to be available in order to be considered in the summary statistics.
#'
#' @return data.table summarizing the differences with columns:
#'\describe{
#'  \item{variable}{factor indicating the variable}
#'  \item{period}{specification of the averaging length with `D` - day(s), `M` - month(s), `Y` - year(s) and `G1` - the overall mean }
#'  \item{TS}{averaging length in hours}
#'  \item{sub_period}{indication of the aggregating scale specified by \code{agg_by} argument}
#'  \item{comp}{factor indicating the data sets from \code{x} with labels given by \code{names(x)}}
#'  \item{DIF}{distance between data sets from \code{x} and \code{compare_to}. Distance is measured as difference for variables included in \code{getOption('additive_variables')}, i.e. temperature (\code{TAS}) by default, and as a ratio for other variables, see \code{\link{dif}}}
#'}
#' @export compare
#'
#' @examples
#' library(ggplot2)
#' data(basin_PT)
#' \dontrun{
#' dobs = decomp(basin_PT[['obs_ctrl']])
#' dctrl = decomp(basin_PT[['sim_ctrl']])
#' dscen = decomp(basin_PT[['sim_scen']])
#' d = compare(x = list(CTRL = dctrl, SCEN = dscen), compare_to = dobs, fun = max)
#' ggplot(d) +
#'  geom_line(aes(x = TS, y = DIF, col = factor(sub_period))) +
#'  facet_grid(variable ~ comp, scale = 'free') +
#'  scale_x_log10()
#'  }
compare = function(x, compare_to, fun = mean, wet_int_only = TRUE, wet_int_thr = 0.1, exclude_below = 0.9){

  lst = c(x, COMPARE_TO = list(compare_to))
  #lst = rbindlist(lst, idcol = 'ID')
   for (i in 1:length(lst)){
    lst[[i]][, ID:=names(lst)[i]]
  }
  lst = do.call(rbind, lst)
  lst = lst[N >= exclude_below * (TS/24)]
  stat =
    if (wet_int_only){
      lst[!(! variable  %in% getOption('additive_variables') & value<= wet_int_thr) , .(value = fun(value)), by = .(variable, period, TS, sub_period, ID)]
    } else {
      lst[, .(value = fun(value)), by = .(variable, period, TS, sub_period, ID)]
    }
  cstat = dcast.data.table(stat, variable + period + TS + sub_period ~ ID)
  stat = melt(cstat, measure.vars = names(x), variable.name = 'comp')
  stat[, .(DIF = dif(value, COMPARE_TO, variable[1])), by = .(variable, period, TS, sub_period, comp)]

}

#' Assess the relations between two decomposed variables
#'
#' @param x List of decomposed objects
#' @param fun Function to sumarize dependence (like \code{cor}, \code{cov})
#' @param wet_int_only (logical) Should only the wet intervals be considered?
#' @param wet_int_thr Numeric value specifying the minimum depth to be consider wet
#' @param exclude_below Some of the intervals might not be of required length, e.g. D10 interval may have less than 10 days available. The \code{exclude_below} argument controls the minimum fraction of the interval that has to be available in order to be considered in the summary statistics.
#'
#' @details \code{vcompare} compares the relation between all pairs of variables included in \code{x}, typically precipitation and temperature, but other variables may be included also (e.g. runoff).
#'
#' @return data.table summarizing the relation with columns:
#'\describe{
#'  \item{variable}{factor indicating the variable}
#'  \item{period}{specification of the averaging length with `D` - day(s), `M` - month(s), `Y` - year(s) and `G1` - the overall mean }
#'  \item{TS}{averaging length in hours}
#'  \item{sub_period}{indication of the aggregating scale specified by \code{agg_by} argument}
#'  \item{comp}{factor indicating the data sets from \code{x} with labels given by \code{names(x)}}
#'  \item{DIF}{distance between data sets from \code{x} and \code{compare_to}. Distance is measured as difference for variables included in \code{getOption('additive_variables')}, i.e. temperature (\code{TAS}) by default, and as a ratio for other variables, see \code{\link{dif}}}
#'}
#'
#' @export vcompare
#'
#' @examples
#' library(ggplot2)
#' data(basin_PT)
#' \dontrun{
#' dobs = decomp(basin_PT[['obs_ctrl']])
#' dctrl = decomp(basin_PT[['sim_ctrl']])
#' d = vcompare(x = list(OBS = dobs, CTRL = dctrl), fun = cov)
#' ggplot(d[period!='G1']) +
#'  geom_line(aes(x = TS, y = value, col = factor(sub_period))) +
#'  facet_grid(VARS~ID) +
#'  scale_x_log10()
#'  }
vcompare = function(x, fun = cor, wet_int_only = TRUE, wet_int_thr = 0.1, exclude_below = 0.9){

  lst = x
  for (i in 1:length(lst)){
    lst[[i]][, ID:=names(lst)[i]]
  }
  lst = do.call(rbind, lst)

  vars = lst[, unique(variable)]
  cmb = t(combn(vars, 2))
  map = data.table(TYP = rep(names(x),  each = nrow(cmb)), cmb)

  B = list()
  for (i in 1:nrow(map)){
    yy = dcast(lst[ID == map[i, TYP] & N >= exclude_below * (TS/24), ], ... ~ variable, value.var = 'value')
    cy = yy[, .(ID = map[i, TYP], V1 = map[i, V1], V2 = map[i, V2], TS = tscale(period), value = fun(eval(parse(text = map[i, V1])),  eval(parse(text = map[i, V2]))), VARS = map[i, paste(V1, V2, sep = ' x ')]), by = .(period, sub_period) ]
    B[[i]] = cy
  }

  do.call(rbind, B)

}



#' Convenience function for calculation of quantiles
#'
#' The typical use is in \code{\link{compare}} to avoid anonymous functions in specification of its \code{fun} argument.
#'
#' @param p Specification of the quantile
#' @param ... other arguments passed to \code{\link{quantile}}
#'
#' @return function calculating the p-th quantile
#' @export Q
#'
#' @examples
#' q90 = Q(.9)
#' class(q90)
#' q90(rnorm(10))
Q = function(p, ...){
  function(x)quantile(x, p, ...)
}

# #' @export
# run_bil = function(dta, b = NULL, vars = 'RM'){
#   if (is.null(b)) {b = bil.new(type = 'd', data = dta[, .(DTM, P = PR, T = TAS)]) } else {
#     bil.set.values(b, dta[, .(DTM, P = PR, T = TAS)])
#   }
#   bil.pet(b)
#   d = data.frame(data.frame(bil.run(b))[, vars])
#   names(d) = vars
#   d
# }


#' Indication of a season
#'
#' @param dtm a \code{Date} object
#' @param sub_scale integer indicating the season
#' @param year_starts Month object indicating the start of the year
#'
#' @return 3 letter code (as DJF, JJA etc.) specifying the season
#'
#'
#' @examples
#' month2sea(as.Date('2000-01-01') + months(1:10) )
#'
#' sscale2sea(c(1, 1, 2, 2, 2, 3, 3), year_starts = months(-1))
#' @name m2s

#' @export
#' @rdname m2s
month2sea = function(dtm, year_starts = months(0)){
  i = ((1:12-1) + month(as.Date('1970-01-01') + year_starts)) %% 12
  i[i==0] = 12
  id = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N' , 'D')[i]
  id = rep(c(paste0(id[1:3], collapse = ''), paste0(id[1:3+3], collapse = ''), paste0(id[1:3+6], collapse = ''), paste0(id[1:3+9], collapse = '')), each = 3)
  i = month(dtm %m-% year_starts)#((month(dtm)-1) + month(as.Date('1970-01-01') + year_starts)) %% 12
  i[i==0] = 12
  id[i]
}

#' @rdname m2s
#' @export
sscale2sea = function(sub_scale, year_starts = months(0)){

  i = ((1:12-1) + month(as.Date('1970-01-01') + year_starts)) %% 12
  i[i==0] = 12
  if (length(unique(sub_scale)) == 12) return( (c('DJF', 'DJF', 'MAM', 'MAM', 'MAM', 'JJA', 'JJA', 'JJA', 'SON', 'SON', 'SON', 'DJF')[i])[sub_scale]  )

  id = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N' , 'D')[i]
  id = c(paste0(id[1:3], collapse = ''), paste0(id[1:3+3], collapse = ''), paste0(id[1:3+6], collapse = ''), paste0(id[1:3+9], collapse = ''))
  id[sub_scale]
}


#' Conversion between period specification and codes
#'
#' @param periods period specification
#' @param code period code
#'
#' @details Periods are specified using keywords "day", "month", "year" preceded by an integer and a space and optionally followed by "s" (the specification is further passed to \code{cut.Date}, see \code{\link{cut.Date}} for details). To fit in figures and for simplicity, periods can be also specified by codes, i.e. by \code{D}, \code{M}, \code{Y} (for "day", "month" and "year", respectively) and folowed by integer specifying the number of intervals. The functions \code{period2code} and \code{code2period} provide conversion between the two alternatives.
#'
#' @examples
#' period2code(c('1 day', '23 days', '3 month', '2 years'))
#'
#' code2period(c('D1', 'D23', 'M3', 'Y2'))
#' @name codes

#' @export
#' @rdname codes
period2code = function(periods){
  num = suppressWarnings(as.integer(gsub("[^\\d]+", "", periods, perl = TRUE)))
  if (any(is.na(num))) stop("Invalid scales specification - number of intervals must be provided. See ?decomp for details.")
  let = gsub('day|days', 'D', periods) %>% gsub('month|months', 'M', .) %>% gsub('year|years', 'Y', .) %>% gsub("[\\d]+| ", '', ., perl = TRUE)
  paste0(let, num)
}

#' @export
#' @rdname codes
code2period = function(code){

  num = suppressWarnings(as.integer(gsub("[^\\d]+", "", code, perl = TRUE)))
  per = gsub("[\\d]+| ", '', code, perl = TRUE) %>% gsub('Y', 'year', .) %>% gsub('M', 'month', .) %>% gsub('D', 'day', .)

  out = paste(num, per)
  names(out) = code
  out[code %in% c('G0', 'G1')] = '0'
  out
}

cut.Date = function (x, breaks, labels = NULL, start.on.monday = TRUE, right = FALSE, ...){

  if (!inherits(x, "Date"))
    stop("'x' must be a date-time object")
  x <- as.Date(x)

  if (breaks == '0' | breaks == 0){

    return(rep(x[1], length(x)))

  }

  if (inherits(breaks, "Date")) {
    breaks <- sort(as.Date(breaks))
  }

  else if (is.numeric(breaks) && length(breaks) == 1L) {
  }
  else if (is.character(breaks) && length(breaks) == 1L) {
    by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L)
      stop("invalid specification of 'breaks'")
    valid <- pmatch(by2[length(by2)], c("days", "weeks",
                                        "months", "years", "quarters"))
    if (is.na(valid))
      stop("invalid specification of 'breaks'")
    start <- as.POSIXlt(min(x, na.rm = TRUE))
    if (valid == 1L)
      incr <- 1L
    if (valid == 2L) {
      start$mday <- start$mday - start$wday
      if (start.on.monday)
        start$mday <- start$mday + ifelse(start$wday >
                                            0L, 1L, -6L)
      start$isdst <- -1L
      incr <- 7L
    }
    if (valid == 3L) {
      start$mday <- 1L
      start$isdst <- -1L
      end <- as.POSIXlt(max(x, na.rm = TRUE))
      step <- if (length(by2) == 2L)
        as.integer(by2[1L])
      else 1L
      end <- as.POSIXlt(end + (31 * step * 86400))
      end$mday <- 1L
      end$isdst <- -1L
      breaks <- as.Date(seq(start, end, breaks))
    }
    else if (valid == 4L) {
      start$mon <- 0L
      start$mday <- 1L
      start$isdst <- -1L
      end <- as.POSIXlt(max(x, na.rm = TRUE))
      step <- if (length(by2) == 2L)
        as.integer(by2[1L])
      else 1L
      end <- as.POSIXlt(end + (366 * step * 86400))
      end$mon <- 0L
      end$mday <- 1L
      end$isdst <- -1L
      breaks <- as.Date(seq(start, end, breaks))
    }
    else if (valid == 5L) {
      qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
      start$mon <- qtr[start$mon + 1L]
      start$mday <- 1L
      start$isdst <- -1L
      maxx <- max(x, na.rm = TRUE)
      end <- as.POSIXlt(maxx)
      step <- if (length(by2) == 2L)
        as.integer(by2[1L])
      else 1L
      end <- as.POSIXlt(end + (93 * step * 86400))
      end$mon <- qtr[end$mon + 1L]
      end$mday <- 1L
      end$isdst <- -1L
      breaks <- as.Date(seq(start, end, paste(step * 3L,
                                              "months")))
      lb <- length(breaks)
      if (maxx < breaks[lb - 1])
        breaks <- breaks[-lb]
    }
    else {
      start <- as.Date(start)
      if (length(by2) == 2L)
        incr <- incr * as.integer(by2[1L])
      maxx <- max(x, na.rm = TRUE)
      breaks <- seq(start, maxx + incr, breaks)
      breaks <- breaks[seq_len(1L + max(which(breaks <=
                                                maxx)))]
    }
  }
  else stop("invalid specification of 'breaks'")
  res <- cut(unclass(x), unclass(breaks), labels = labels,
             right = right, ...)
  if (is.null(labels)) {
    levels(res) <- as.character(if (is.numeric(breaks)) x[!duplicated(res)] else breaks[-length(breaks)])
  }
  res
}
