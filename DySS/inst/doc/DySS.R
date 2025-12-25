## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=5,
  fig.height=4)
options(rmarkdown.html_vignette.check_title=FALSE)

## ----setup--------------------------------------------------------------------
library(DySS)

## -----------------------------------------------------------------------------
data("data_example_long_1d")

## -----------------------------------------------------------------------------
nrow(data_example_long_1d$data_matrix_IC)
nrow(data_example_long_1d$data_matrix_OC)

## -----------------------------------------------------------------------------
result_pattern<-estimate_pattern_long_1d(
  data_matrix=data_example_long_1d$data_matrix_IC,
  time_matrix=data_example_long_1d$time_matrix_IC,
  nobs=data_example_long_1d$nobs_IC,
  design_interval=data_example_long_1d$design_interval,
  n_time_units=data_example_long_1d$n_time_units,
  estimation_method="meanvar",
  smoothing_method="local linear",
  bw_mean=0.1,
  bw_var=0.1)

## -----------------------------------------------------------------------------
plot(
  c(data_example_long_1d$time_matrix_IC[1:20,]),
  c(data_example_long_1d$data_matrix_IC[1:20,]),
  xlab="Time",ylab="Data",
  type="p",col="gray",pch=16)
lines(result_pattern$grid,result_pattern$mean_est)
lines(result_pattern$grid,result_pattern$mean_est+qnorm(0.975)*sqrt(result_pattern$var_est))
lines(result_pattern$grid,result_pattern$mean_est-qnorm(0.975)*sqrt(result_pattern$var_est))

## -----------------------------------------------------------------------------
chart_IC_output<-monitor_long_1d(
  data_example_long_1d$data_matrix_IC,
  data_example_long_1d$time_matrix_IC,
  data_example_long_1d$nobs_IC,
  pattern=result_pattern,side="upward",chart="CUSUM",
  method="standard",parameter=0.2)
chart_OC_output<-monitor_long_1d(
  data_example_long_1d$data_matrix_OC,
  data_example_long_1d$time_matrix_OC,
  data_example_long_1d$nobs_OC,
  pattern=result_pattern,side="upward",chart="CUSUM",
  method="standard",parameter=0.2)

## -----------------------------------------------------------------------------
CL<-4

nind_IC<-nrow(data_example_long_1d$data_matrix_IC)
nind_OC<-nrow(data_example_long_1d$data_matrix_OC)

output_signal_times<-
  calculate_signal_times(
    chart_matrix=chart_IC_output$chart,
    time_matrix=data_example_long_1d$time_matrix_IC,
    nobs=data_example_long_1d$nobs_IC,
    starttime=rep(0,nind_IC),
    endtime=rep(1,nind_IC),
    design_interval=data_example_long_1d$design_interval,
    n_time_units=data_example_long_1d$n_time_units,
    CL=CL)

## -----------------------------------------------------------------------------
print(data.frame(
  subject=1:10,
  signal_time=output_signal_times$signal_times,
  signal=output_signal_times$signals)[1:10,])

## -----------------------------------------------------------------------------
CL<-search_CL(
  chart_matrix=chart_IC_output$chart,
  time_matrix=data_example_long_1d$time_matrix_IC,
  nobs=data_example_long_1d$nobs_IC,
  starttime=rep(0,nind_IC),
  endtime=rep(1,nind_IC),
  design_interval=data_example_long_1d$design_interval,
  n_time_units=data_example_long_1d$n_time_units,
  ATS_nominal=200,
  CL_lower=0,
  CL_upper=10,
  CL_step=0.5)

IC_ATS<-
  calculate_ATS(
    chart_matrix=chart_IC_output$chart,
    time_matrix=data_example_long_1d$time_matrix_IC,
    nobs=data_example_long_1d$nobs_IC,
    starttime=rep(0,nind_IC),
    endtime=rep(1,nind_IC),
    design_interval=data_example_long_1d$design_interval,
    n_time_units=data_example_long_1d$n_time_units,
    CL=CL)

OC_ATS<-
  calculate_ATS(
    chart_matrix=chart_OC_output$chart,
    time_matrix=data_example_long_1d$time_matrix_OC,
    nobs=data_example_long_1d$nobs_OC,
    starttime=rep(0,nind_OC),
    endtime=rep(1,nind_OC),
    design_interval=data_example_long_1d$design_interval,
    n_time_units=data_example_long_1d$n_time_units,
    CL=CL)

print(IC_ATS)
print(OC_ATS)

## -----------------------------------------------------------------------------
output_evaluate<-evaluate_control_chart_two_groups(
  chart_matrix_IC=chart_IC_output$chart,
  time_matrix_IC=data_example_long_1d$time_matrix_IC,
  nobs_IC=data_example_long_1d$nobs_IC,
  starttime_IC=rep(0,nind_IC),
  endtime_IC=rep(1,nind_IC),
  chart_matrix_OC=chart_OC_output$chart,
  time_matrix_OC=data_example_long_1d$time_matrix_OC,
  nobs_OC=data_example_long_1d$nobs_OC,
  starttime_OC=rep(0,nind_OC),
  endtime_OC=rep(1,nind_OC),
  design_interval=data_example_long_1d$design_interval,
  n_time_units=data_example_long_1d$n_time_units,
  no_signal_action="maxtime")

## ----fig.width=7--------------------------------------------------------------
plot_evaluation(output_evaluate)

## -----------------------------------------------------------------------------
plot_PMROC(output_evaluate)

## -----------------------------------------------------------------------------
data("data_example_long_md")

## -----------------------------------------------------------------------------
nrow(data_example_long_md$data_array_IC)
nrow(data_example_long_md$data_array_OC)

## -----------------------------------------------------------------------------
result_pattern<-estimate_pattern_long_md(
  data_array=data_example_long_md$data_array_IC,
  time_matrix=data_example_long_md$time_matrix_IC,
  nobs=data_example_long_md$nobs_IC,
  design_interval=data_example_long_md$design_interval,
  n_time_units=data_example_long_md$n_time_units,
  estimation_method="meanvar",
  bw_mean=0.10,
  bw_var=0.10)

## -----------------------------------------------------------------------------
chart_IC_output<-monitor_long_md(
  data_array_new=data_example_long_md$data_array_IC,
  time_matrix_new=data_example_long_md$time_matrix_IC,
  nobs_new=data_example_long_md$nobs_IC,
  pattern=result_pattern,
  side="upward",
  method="multivariate EWMA",
  parameter=0.5,CL=Inf)

chart_OC_output<-monitor_long_md(
  data_array_new=data_example_long_md$data_array_OC,
  time_matrix_new=data_example_long_md$time_matrix_OC,
  nobs_new=data_example_long_md$nobs_OC,
  pattern=result_pattern,
  side="upward",
  method="multivariate EWMA",
  parameter=0.5,CL=Inf)

## -----------------------------------------------------------------------------
output_evaluate<-evaluate_control_chart_two_groups(
  chart_matrix_IC=chart_IC_output$chart,
  time_matrix_IC=data_example_long_md$time_matrix_IC,
  nobs_IC=data_example_long_md$nobs_IC,
  starttime_IC=rep(0,nrow(data_example_long_md$time_matrix_IC)),
  endtime_IC=rep(1,nrow(data_example_long_md$time_matrix_IC)),
  chart_matrix_OC=chart_OC_output$chart,
  time_matrix_OC=data_example_long_md$time_matrix_OC,
  nobs_OC=data_example_long_md$nobs_OC,
  starttime_OC=rep(0,nrow(data_example_long_md$time_matrix_OC)),
  endtime_OC=rep(1,nrow(data_example_long_md$time_matrix_OC)),
  design_interval=data_example_long_md$design_interval,
  n_time_units=data_example_long_md$n_time_units,
  no_signal_action="maxtime")

## ----fig.width=7--------------------------------------------------------------
plot_evaluation(output_evaluate)

## -----------------------------------------------------------------------------
plot_PMROC(output_evaluate)

## -----------------------------------------------------------------------------
data("data_example_long_surv")

## -----------------------------------------------------------------------------
result_pattern<-estimate_pattern_long_surv(
  data_array=data_example_long_surv$data_array_IC,
  time_matrix=data_example_long_surv$time_matrix_IC,
  nobs=data_example_long_surv$nobs_IC,
  starttime=data_example_long_surv$starttime_IC,
  survtime=data_example_long_surv$survtime_IC,
  survevent=data_example_long_surv$survevent_IC,
  design_interval=data_example_long_surv$design_interval,
  n_time_units=data_example_long_surv$n_time_units,
  estimation_method="risk",
  smoothing_method="local linear",
  bw_beta=0.05,
  bw_mean=0.1,
  bw_var=0.1)

## -----------------------------------------------------------------------------
result_monitoring<-monitor_long_surv(
  data_array_new=data_example_long_surv$data_array_IC,
  time_matrix_new=data_example_long_surv$time_matrix_IC,
  nobs_new=data_example_long_surv$nobs_IC,
  pattern=result_pattern,
  method="risk",
  parameter=0.5)

## -----------------------------------------------------------------------------
output_evaluate<-evaluate_control_chart_one_group(
  chart_matrix=result_monitoring$chart,
  time_matrix=data_example_long_surv$time_matrix_IC,
  nobs=data_example_long_surv$nobs_IC,
  starttime=rep(0,nrow(data_example_long_surv$time_matrix_IC)),
  endtime=rep(1,nrow(data_example_long_surv$time_matrix_IC)),
  status=data_example_long_surv$survevent_IC,
  design_interval=data_example_long_surv$design_interval,
  n_time_units=data_example_long_surv$n_time_units,
  no_signal_action="maxtime")

## ----fig.width=7--------------------------------------------------------------
plot_evaluation(output_evaluate)

## -----------------------------------------------------------------------------
plot_PMROC(output_evaluate)

