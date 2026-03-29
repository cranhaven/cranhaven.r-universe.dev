## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(error = FALSE)

## -----------------------------------------------------------------------------
library(sovereign)

## ---- message = F-------------------------------------------------------------
# pull data from FRED 
quantmod::getSymbols.FRED(c('GDPC1','GDPPOT','GDPDEF','FEDFUNDS'), env = globalenv())

## ---- message=F---------------------------------------------------------------
# real output gap and inflation
Y = 
  purrr::reduce(
    list(
      data.frame(GDPC1, date = zoo::index(GDPC1)),
      data.frame(GDPPOT, date = zoo::index(GDPPOT)),
      data.frame(GDPDEF, date = zoo::index(GDPDEF))
    ),
    dplyr::inner_join, by = 'date'
  ) %>% 
  dplyr::mutate(
    y = 100*((GDPC1-GDPPOT)/GDPPOT),
    p = 400*(GDPDEF - dplyr::lag(GDPDEF))/dplyr::lag(GDPDEF)) %>%
  dplyr::select(date, y,p)

# federal funds rate
I = data.frame(FEDFUNDS, date = zoo::index(FEDFUNDS)) %>%
  dplyr::group_by(y = lubridate::year(date), q = lubridate::quarter(date)) %>%
  dplyr::summarize(
    date = min(date),
    ff = mean(FEDFUNDS, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, ff) %>%
  data.frame()
  
# merge data
Data = 
  dplyr::inner_join(Y, I, by = 'date') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(lubridate::year(date) < 2020) %>%
  na.omit()

head(Data)

## ---- message = F-------------------------------------------------------------

# pull NBER recessions from FRED 
quantmod::getSymbols.FRED(c('USRECQ'), env = globalenv())

# merge recessions into Data 
Data.rec = Data %>%
  dplyr::left_join(
    data.frame(USRECQ, date = zoo::index(USRECQ)),
    by = 'date') %>%
  dplyr::rename(cycle_position = USRECQ) %>%
  dplyr::mutate(cycle_position = dplyr::case_when(cycle_position == 0 ~ 'expansion',
                                                  cycle_position == 1 ~ 'recession'))

head(Data.rec)
    

## ---- message = F-------------------------------------------------------------

# let sovereign determine economic states via k-means clustering
Data.kmeans = 
  sovereign::regimes(
    data = Data,
    regime.n = 2,
    method = 'kmeans')

head(Data.kmeans)


## ---- message=F, warning=F, fig.show="hold", out.width="50%"------------------

# estimate VAR
var =
    sovereign::VAR(
        data = Data,
        p = 1,
        horizon = 10,
        freq = 'quarter')

# plot forecasting output, one-period ahead horizon
# plot forecasts
sovereign::plot_forecast(var$forecasts[['H_1']])
# plot residuals
sovereign::plot_error(var$residuals[['H_1']])

## ---- message = F, warning=F, fig.align='center'------------------------------

# estimate IRF
var.irf =
    sovereign::var_irf(
        var,
        bootstrap.num = 10,
        CI = c(0.05,0.95))

# plot IRF
sovereign::plot_irf(var.irf)

## ---- message = F, warning=F, fig.align='center'------------------------------

# estimate forecast error variance decomposition
var.fevd =
    sovereign::var_fevd(
        var,
        horizon = 10)

# plot FEVD
sovereign::plot_fevd(var.fevd)

## ---- message = F, warning=F, fig.align='center'------------------------------

# estimate historical decomposition
var.hd = sovereign::var_hd(var)

# plot HD
sovereign::plot_hd(var.hd)

## ---- message=F, warning=F, fig.show="hold", out.width="50%"------------------

# estimate multi-regime VAR
rvar =
    sovereign::RVAR(
        data = Data.rec,
        regime = 'cycle_position',
        p = 1,
        horizon = 10,
        freq = 'quarter')

# plot forecasting output, one-period ahead horizon
# plot forecasts (right panel)
sovereign::plot_forecast(rvar$forecasts[['H_1']])
# plot residuals (left panel)
sovereign::plot_error(rvar$residuals[['H_1']])

## ---- message=F, warning=F, fig.show="hold", out.width="50%"------------------

# estimate IRF
rvar.irf =
    sovereign::rvar_irf(
        rvar,
        horizon = 10,
        bootstrap.num = 10,
        CI = c(0.05,0.95))

# plot IRF
# regime 1: expansion (right panel)
sovereign::plot_irf(rvar.irf[['regime_expansion']])
# regime 2: recession (left panel)
sovereign::plot_irf(rvar.irf[['regime_recession']])


## ---- message=F, warning=F, fig.show="hold", out.width="50%"------------------

# estimate forecast error variance decomposition
rvar.fevd =
    sovereign::rvar_fevd(
        rvar,
        horizon = 10)

# plot FEVD
# regime 1: expansion (right panel)
sovereign::plot_fevd(rvar.fevd[['regime_expansion']])
# regime 2: recession rates (left panel)
sovereign::plot_fevd(rvar.fevd[['regime_recession']])


## ---- message=F, warning=F----------------------------------------------------

# estimate historical decomposition
rvar.hd = sovereign::rvar_hd(rvar)

# plot hd
sovereign::plot_hd(rvar.hd)


## ---- message=F, warning=F, fig.show="hold", out.width="50%"------------------

# estimate single-regime forecasts 
#  (one or multiple horizons may be estimated)
lp = 
    sovereign::LP(
        data = Data,
        p = 1,
        horizon = c(1:10),
        freq = 'quarter')

# plot forecasting output, one-quarter ahead horizon
# plot forecasts (right panel)
sovereign::plot_forecast(lp$forecasts[['H_1']])
# plot residuals (left panel)
sovereign::plot_error(lp$residuals[['H_1']])


## ---- message = F, warning=F, fig.align='center'------------------------------

# estimate single-regime IRF
lp.irf = sovereign::lp_irf(lp)

# plot IRF
sovereign::plot_irf(lp.irf)


## ---- message=F, warning=F, fig.show="hold", out.width="50%"------------------

# estimate multi-regime IRF
rlp = 
    sovereign::RLP(
        data = Data.kmeans,
        regime = 'regime',
        p = 1,
        horizon = c(1:10),
        freq = 'quarter')

# plot forecasting output, one-quarter ahead horizon
# plot forecasts (right panel)
sovereign::plot_forecast(rlp$forecasts[['H_1']])
# plot residuals (left panel)
sovereign::plot_error(rlp$residuals[['H_1']])

## ---- message=F, warning=F, fig.show="hold", out.width="50%"------------------
# estimate multi-regime IRF
rlp.irf = 
  sovereign::rlp_irf(rlp)

# plot IRF
# regime 1: km-eans cluster 0 (right panel)
sovereign::plot_irf(rlp.irf[[1]])
# regime 2: k-means cluster 1 (left panel)
sovereign::plot_irf(rlp.irf[[2]])


