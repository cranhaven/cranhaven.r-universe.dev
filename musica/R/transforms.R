# #' @export
ext_cast = function(out, what){
  dcast.data.table(out[RID==what, .(variable, DTM, D1)], DTM ~ variable, value.var = 'D1')
}

# #' @export
approx = function(x, y = NULL, xout, method = "linear", n = 50, yleft, yright, rule = 1, f = 0, ties = mean){

  if (length(unique(x)) == 1)  {
    return(list(x = xout, y = rep(mean(y), length(xout))))
  } else {
    return(
      stats::approx(x, y, xout, method , n , yleft, yright, rule, f, ties)
    )
  }
}


#' Multiscale quantile mapping bias correction
#'
#' Applies standard quantile mapping at custom time scales.
#'
#' @param dta List with components \code{FROM} (simulated data for the control period), \code{TO} (observed data) and \code{NEWDATA} (data to be corrected). Each component is a \code{data.table} with columns \code{DTM} (date) and the climate variables (typically \code{PR} - precipitation and \code{TAS} - temperature)
#' @param agg_by Function for specification of the period (season, month) to be additionaly included in output, see Details
#' @param wet_int_thr Numeric value specifying the minimum depth to be considered wet
#' @param maxiter Maximum number of iterations, see Details
#' @param tol Stoping criterion of the iteration cycle, see Details
#' @param qstep A numeric value between 0 and 1. The quantile mapping is fitted only for the quantiles defined by quantile(0,1,probs=seq(0,1,by=qstep). Passed to \code{\link{doQmapQUANT}}.
#' @param period Specification of the aggregation lengths the correction is applied at with `D` - day(s), `M` - month(s), `Y` - year(s) and `G1` - the overall mean
#'
#' @return data.table with corrected data
#' @export
#'
#' @details The procedure utilizes standard quantile mapping from the \code{\link{qmap}}-package, but at multiple time scales. Since correction at particular temporal scale influences values at other aggregations, the procedure is applied iterativelly until the maximum number of iterations (\code{maxiter}) is reached or the difference between succesive iteration step is smaller than \code{tol}. Differences between corrected and uncorrected variable at longer time scales are used to modify daily values after each iteration step (see e.g. Mehrorta and Sharma, 2016; Pegram et al. 2009). To make further assessment of the decomposed objects easier, indicator of period within the year (e.g. quarter or month) as specified by \code{agg_by} argument is included in the output.
#'
#' @references
#' Hanel, M., Kozin, R., 2016. Bias and projected changes in climate model simulations at multiple time scales: consequences for hydrological impact assessment. Environmental Modelling and Software, submitted.
#'
#' Mehrotra, R., Sharma, A., 2016. A multivariate quantile-matching bias correction approach with auto-and cross-dependence across multiple time scales: Implications for downscaling. Journal of Climate 29, 3519-3539.
#'
#' Pegram, G.G., et al., 2009. A nested multisite daily rainfall stochastic generation model. Journal of Hydrology 371, 142-153.
#' @examples
#' data("basin_PT")
#' scen = basin_PT$sim_scen
#' ctrl = basin_PT$sim_ctrl
#' obs = basin_PT$obs_ctrl
#' dta = list(TO = obs, FROM = ctrl, NEWDATA = scen)
#' \dontrun{
#' msTrans_abs(dta,  maxiter = 10, period = 'D1')
#' }
msTrans_abs = function(dta, agg_by = month, wet_int_thr = 0.1, maxiter = 10, tol = 1e-4, qstep = .001, period = c('G1', 'Y1', 'M3', 'M1', 'D1')){

  dta = copy(dta)
  dta = list(FROM = copy(dta$FROM), TO = copy(dta$TO), NEWDATA = copy(dta$NEWDATA), MODIF = copy(dta$NEWDATA))
  #dta$MODIF = copy(dta$NEWDATA)
  dta = rbindlist(dta, idcol = 'RID')

  report = FALSE
  year_starts = months(0)
  giveAbs = Vectorize(function(i){

    #mdta[, cut := as.IDate(musica:::cut.Date(DTM, breaks = cpr[i])), by = .(RID, variable)]
    mdta[, cut := as.IDate(cut.Date(DTM, breaks = cpr[i])), by = .(RID, variable)]
    mdta[, GRP := month(cut %m-% year_starts)]
    mdta[, pr[i] := mean(ORIG), by = .(RID, variable, GRP, cut)]
    return(NULL)
  })

  pr = period
  cpr = code2period(pr)

  pdif = c(NA, NA)
  cdif = c(NA, NA)
  pp = c(NA, NA)
  idif = pdif - cdif
  ite = 1

  mdta = melt(dta, id.vars = c('RID', 'DTM'), value.name = 'ORIG')


  while ( (ite <= maxiter) & ((max(abs(idif)) > tol) | any(is.na(idif))) ) {

    cat('\n', format(ite, width =3), '(', format(signif(idif, 5), width = 12), ') :', '\t')
    invisible(giveAbs(1:length(pr)))

    for (i in 1:length(pr)){

      cat(names(cpr[i]), '\t')
      invisible(giveAbs(i))

      mdta[, MEA := eval(parse(text = pr[i]))]

      smry = mdta[, .(MEA = MEA[1]), by = .(RID, variable, cut, GRP)]

      f = list()
      for (g in smry[, unique(GRP)]){
        for (v in smry[, unique(variable)]){
          f[[length(f)+1]] =
            smry[GRP == g & variable == v, .(GRP = g, variable = v, fun = .(fitQmap(obs = MEA[RID=='TO'], mod = MEA[RID=='FROM'], method = 'QUANT', wet.day = FALSE, qstep = qstep))) ]

        }
      }

      f = rbindlist(f)

      mdta = f[mdta, on = c('GRP', 'variable')]

      #mdta[RID %in% c('MODIF', 'FROM'), CORR := musica::doQmapQUANT( eval(parse(text = pr[i])), fun[[1]] ), by = .(GRP, variable, RID) ]
      mdta[RID %in% c('MODIF', 'FROM'), CORR := doQmapQUANT( eval(parse(text = pr[i])), fun[[1]] ), by = .(GRP, variable, RID) ]

      mdta[RID %in% c('MODIF', 'FROM'), CF := dif(CORR, eval(parse(text = pr[i])), variable[1]), by = .(GRP, variable, RID) ]
      mdta[RID %in% c('MODIF', 'FROM'), ORIG:= rev_dif(CF, ORIG, variable[1]), by = .(GRP, variable, RID)]
      mdta[, c('GRP', 'fun', 'CORR', 'CF', 'MEA', 'cut'):=NULL]

    }

    giveAbs(1:length(pr))

    err = data.table()
    for (i in 1:length(pr)){
      suppressWarnings({
        err = rbind(err, mdta[, .(id = c('D', 'p.value', 'alternative', 'method', 'call'), scale = pr[i], unlist(ks.test(eval(parse(text = pr[i]))[RID == 'FROM'], eval(parse(text = pr[i]))[RID == 'TO']) )), by = variable ])
      })}

    cerr = dcast(err, variable + scale ~id, value.var = 'V3')
    DD = cerr[, sum(as.double(D)), by = variable][, V1]
    pp = cerr[, sum(as.numeric(p.value) > .1), by = variable][, V1]

    idif = pdif - DD
    pdif = DD
    ite = ite + 1

  }

  cat('\n')
  mdta = mdta[, .(variable, RID, DTM, D1 = ORIG)]

  #list(FROM = ext_cast(mdta, 'FROM'), TO = ext_cast(mdta, 'TO'), NEWDATA = ext_cast(mdta, 'NEWDATA'), TRANSFORMED = ext_cast(mdta, 'MODIF'))

  ext_cast(mdta, 'MODIF')

}


#' Multiscale delta method
#'
#' Transforms observed data such that the changes in summary statistics of variables at custom time scales are similar to those obtained from climate model simulation. Number of functions can be used to summarize the variables.
#'
#' @param dta List with components \code{FROM} (simulated data for the control period), \code{TO} (simulated data for the scenario period) and \code{NEWDATA} (observed data to be transformed). Each component is a \code{data.table} with columns \code{DTM} (date) and the climate variables (typically \code{PR} - precipitation and \code{TAS} - temperature)
#' @param model One of \code{loess}, \code{const}, \code{identity}, \code{lm}, \code{smooth}, \code{runmed}, \code{smooth.spline}. The model is used to provide statistical summary of the empirical cumulative distribution function.
#' @param model_par optional parameters of the \code{model}
#' @param agg_by Function for specification of the period (season, month) to be additionaly included in output, see Details
#' @param wet_int_thr Numeric value specifying the minimum depth to be considered wet
#' @param maxiter Maximum number of iterations, see Details
#' @param tol Stoping criterion of the iteration cycle, see Details
#' @param period Specification of the aggregation lengths the correction is applied at with `D` - day(s), `M` - month(s), `Y` - year(s) and `G1` - the overall mean
#' @param qstep A numeric value between 0 and 1. The ecdf is calculated only for the quantiles defined by quantile(0, 1, probs = seq(0, 1, by = qstep).
#'
#' @return transformed data.table
#' @export
#'
#' @references
#' Hanel, M., Kozin, R., 2016. Bias and projected changes in climate model simulations at multiple time scales: consequences for hydrological impact assessment. Environmental Modelling and Software, submitted.
#'
#' @examples
#' data("basin_PT")
#' scen = basin_PT$sim_scen
#' ctrl = basin_PT$sim_ctrl
#' obs = basin_PT$obs_ctrl
#' dta = list(TO = scen, FROM = ctrl, NEWDATA = obs)
#' \dontrun{
#' msTrans_dif(dta,  maxiter = 10, period = 'D1')
#' }
msTrans_dif = function(dta, model = 'const', model_par = list(NULL), agg_by = month, wet_int_thr = 0.1, maxiter = 10, tol = 1e-4, period = c('G1', 'Y1', 'M1', 'D1'), qstep = .001){

  sfce = if (!is.function(model)){
    switch(model,
           'loess' = function(y, x, v){loess(y ~ x, model_par[[v]])$fitted},
           'lm' = function(y, x, v){lm(y ~ x, model_par[[v]])$fitted},
           'const' = function(y, x, v){ rep_len(mean(y), length(y)) },#lm(y ~ 1, model_par[[v]])$fitted},
           'smooth' = function(y, x, v){c(smooth(y, model_par[[v]]))},
           'runmed' = function(y, x, v){runmed(y, model_par[[v]])},
           'smooth.spline' = function(y, x, v){smooth.spline(x, y, model_par[[v]])$y},
           'identity' = function(y, x, v){y}
    )
  } else {function(...)model(x, y, model_par[[v]])}

  sfce_ = function(y, x, v){

    if (length(x)==1) return(y)
    if (v %in% getOption('additive_variables')) {
      return(sfce(y, x, v))
    } else {
      return((sfce((y), x, v))^1)
    }

  }

  apxfun = function(y){
    function(v) y
  }

  dta = list(FROM = copy(dta$FROM), TO = copy(dta$TO), NEWDATA = copy(dta$NEWDATA), MODIF = copy(dta$NEWDATA))

  dta = rbindlist(dta, idcol = 'RID')
  dta = melt(dta, id.vars = c('RID', 'DTM'), value.name = 'D1')

  year_starts = months(0)
  pr = code2period(period)
  ip = seq(0, 1, by = qstep)

  repor = list()
  pdif = c(NA, NA)
  cdif = c(NA, NA)
  idif = pdif - cdif
  ite = 1

  while ( (ite <= maxiter) & ((max(abs(idif)) > tol) | any(is.na(idif))) ) {

    cat('\n', format(ite, width =3), '(', format(signif(idif, 5), width = 12), ') :', '\t')

    i =1
    for (i in 1:length(pr)){

      cat(names(pr[i]), '\t')
      #dta[, cut := as.IDate(musica:::cut.Date(DTM, breaks = pr[i])), by = .(RID, variable)]
      dta[, cut := as.IDate(cut.Date(DTM, breaks = pr[i])), by = .(RID, variable)]
      dta[, MEA := mean(D1), by = .(RID, variable, cut)]
      dta[, GRP := agg_by(cut %m-% year_starts)]
      dta[, p := NA_real_]
      dta[(variable == 'TAS') | (variable=='PR' & D1 > wet_int_thr), p := prob(MEA), by = .(RID, variable, GRP)]

      smry = dta[!is.na(p), .(MEA = mean(D1), p = p[1]), by = .(RID, variable, cut, GRP)]

      smry = if (pr[i]==0){
        smry[, .(RID, variable, GRP, p, value = MEA)]
      } else {
        smry[, .(p = ip, value = approx(x = p, xout = ip, y = MEA, rule = 2)$y), by = .(RID, variable, GRP)]
      }

      cf = dcast.data.table(smry, variable + GRP + p ~ RID)
      cf[, sFROM := sfce_(y = FROM, x = p, v = as.character(variable[1])), by = .(variable, GRP)]
      cf[, sMODIF := sfce_(y = MODIF, x = p, v = as.character(variable[1])), by = .(variable, GRP)]
      cf[, sNEWDATA := sfce_(y = NEWDATA, x = p, v = as.character(variable[1])), by = .(variable, GRP)]
      cf[, sTO := sfce_(y = TO, x = p, v = as.character(variable[1])), by = .(variable, GRP)]

      cf[, c('REQ', 'CURR') := .(dif(sTO, sFROM, variable[1]), dif(sMODIF, sNEWDATA, variable[1])), by = .(variable)]
      cf[, CF := dif(REQ, CURR, variable[1]), by = variable]

      f = list()
      for (g in cf[, unique(GRP)]){
        for (v in cf[, unique(variable)]){
          f[[length(f)+1]] = if (pr[i]!=0) {
            cf[GRP == g & variable == v, .(GRP = g, variable = v, fun = .(approxfun(x = p, y = CF, rule = 2)))]
          } else {
            cf[GRP == g & variable == v, .(GRP = g, variable = v, fun = .(apxfun( CF )))]
          }

        }
      }

      f = do.call(rbind, f)

      dta = f[dta, on = c('GRP', 'variable')]
      dta[, CF:= fun[1][[1]](p), by = .(variable, GRP) ]
      dta[RID=='MODIF', D1 := rev_dif(D1, CF, variable[1]), by = .(GRP, variable)]
      dta[is.na(D1), D1:=0]
      dta[, c('GRP', 'fun', 'cut', 'MEA', 'p', 'CF') := NULL]

      repor[[length(repor)+1]] = data.table(scale = names(pr)[i], iter = ite, cf)

    }

    r = do.call(rbind, repor)
    cdif = r[iter==ite, ifelse(variable=='TAS', mean(abs(CF)), mean(abs(CF-1))), by = variable][, V1]
    idif = pdif - cdif
    pdif = cdif
    ite = ite + 1
    repor = list()

  }

  cat('\n')

  ext_cast(dta, 'MODIF')

}


# #' @export
doQmapQUANT = function (x, fobj, type = c("linear", "tricub"), ...)
{
  type <- match.arg(type)
  wet <- if (!is.null(fobj$wet.day)) {
    x >= fobj$wet.day
  }
  else {
    rep(TRUE, length(x))
  }
  out <- rep(NA, length.out = length(x))
  if (type == "linear") {
    out[wet] <- approx(x = fobj$par$modq[, 1], y = fobj$par$fitq[, 1], xout = x[wet], method = "linear", rule = 2, ties = mean)$y
    nq <- nrow(fobj$par$modq)
    largex <- x > fobj$par$modq[nq, 1]
    if (any(largex)) {
      max.delta <- fobj$par$modq[nq, 1] - fobj$par$fitq[nq,1]
      out[largex] <- x[largex] - max.delta
    }
  }
  else if (type == "tricub") {
    sfun <- splinefun(x = fobj$par$modq[, 1], y = fobj$par$fitq[, 1], method = "monoH.FC")
    out[wet] <- sfun(x[wet])
  }
  out[!wet] <- 0
  if (!is.null(fobj$wet.day))
    out[out < 0] <- 0
  return(out)
}
