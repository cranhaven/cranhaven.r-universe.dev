#####################################################################
#' Calibrate GR4J rainfall runoff model parameters
#' 
#' \code{calGR4J} calibrates the GR4J model using the airGR package. The NSE is used as the objective function.  
#' @param dates is a vector of daily dates 
#' @param P is a vector of daily precipitation data (in mm)
#' @param PET is a vector of daily potential evapotranspiration data (in mm)
#' @param Qobs is the observed daily streamflow (in mm)
#' @param plotResults is a logical indicating whether airGR summary plots are to be produced
#' @return A vector with GR4J parameter values
#' @examples
#' # load dates, precip, PET and streamflow data for Scott Creek
#' data('data_A5030502')
#' clim_ref = list(times = data_A5030502$times,P = data_A5030502$P)  
#' data("egScottCreekSimStoch")
#' # observed flow
#' Qobs = data_A5030502$Qobs
#' # observed PET 
#' PET = data_A5030502$PET 
#' dates = as.Date(clim_ref$times)
#' # calibrate GR4J parameters
#' Param = calGR4J(dates = dates,P=clim_ref$P,PET=PET,Qobs=Qobs)
#' #' @import airGR
#' @export
calGR4J = function(dates,P,PET,Qobs,plotResults=F){
  
  o = add_dummy_year(dates,P,PET)
  dates.new = o$dates; P.new = o$P; PET.new = o$PET
  
  ## preparation of InputsModel object
  InputsModel <- airGR::CreateInputsModel(FUN_MOD = airGR::RunModel_GR4J, DatesR = dates.new,
                                   Precip = P.new, PotEvap = PET.new)
  
  ## calibration period selection
  Ind_Run = (length(dates.new)-length(dates)+1):length(dates.new)
  IndPeriod_WarmUp = 1:(length(dates.new)-length(dates))
  
  ## preparation of RunOptions object
  RunOptions <- airGR::CreateRunOptions(FUN_MOD = airGR::RunModel_GR4J, InputsModel = InputsModel,
                                 IndPeriod_Run = Ind_Run,IndPeriod_WarmUp =IndPeriod_WarmUp )
  
  ## calibration criterion: preparation of the InputsCrit object
  InputsCrit <- airGR::CreateInputsCrit(FUN_CRIT = airGR::ErrorCrit_NSE, InputsModel = InputsModel,
                                 RunOptions = RunOptions, Obs = Qobs)
  
  ## preparation of CalibOptions object
  CalibOptions <- airGR::CreateCalibOptions(FUN_MOD = airGR::RunModel_GR4J, FUN_CALIB = airGR::Calibration_Michel)
  
  ## calibration
  OutputsCalib <- airGR::Calibration_Michel(InputsModel = InputsModel, RunOptions = RunOptions,
                                     InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                     FUN_MOD = airGR::RunModel_GR4J)
  
  ## simulation
  Param <- OutputsCalib$ParamFinalR
  OutputsModel <- airGR::RunModel_GR4J(InputsModel = InputsModel,
                                RunOptions = RunOptions, Param = Param)
  
  ## results preview
  if(plotResults){plot(OutputsModel, Qobs = Qobs[Ind_Run])}
  
  return(Param)
  
}

########################
# define year as global variable to avoid year appearing as an 
# undefined variable when performing devtools::check() 
utils::globalVariables("year")
#####################################################################

#####################################################################
#' System model wrapper GR4J 
#' 
#' \code{GR4J_wrapper} runs the GR4J model, using the airGR package, for a given 
#' set of climate inputs and parameter values and produces a set of runoff metrics  
#' @param data list; contains daily precipitation and PET to be used in GR4J, 
#' in a list with entries  \emph{times}, \emph{P} and optionally \emph{PET}.
#' @param systemArgs list; contains \code{Param} which is a vector of GR4J 
#' parameters (obtained from \code{calGR4J}),
#' \code{dates} which is a vector of dates, 
#' and \code{PET} which is a optional vector of potential transpiration 
#' (required if PET not included in \code{data}) 
#' @param metrics a vector of metric names (including 'meanQ' for 
#' mean daily flow, 'P99' and 'P25' for 99th and 25th percentile daily flows, 
#' and 'min3yr' for minimum 3-year total flow)
#' @return A vector of metric values
#' @examples
#' # load dates, precip, PET and streamflow data for Scott Creek
#' data('data_A5030502')
#' 
#' clim_ref = list(times = data_A5030502$times,P = data_A5030502$P)  
#' 
#' # observed flow
#' Qobs = data_A5030502$Qobs
#' # observed PET 
#' PET = data_A5030502$PET 
#' 
#' dates = as.Date(clim_ref$times)
#' 
#' # calibrate GR4J parameters
#' Param = calGR4J(dates = dates,P=clim_ref$P,PET=PET,Qobs=Qobs)
#' 
#' # setup systemArgs and metrics
#' systemArgs = list(dates=dates,Param=Param,PET=PET)
#' metrics = c('meanQ','P99','P25','min3yr')
#' 
#' metricsObs = GR4J_wrapper(data=clim_ref,systemArgs = systemArgs,metrics=metrics)
#' metricsObs
#' @import airGR
#' @export
GR4J_wrapper = function(data,
                        systemArgs,
                        metrics){
  
  if (!is.null(data$P)){
    P = data$P
  } else {
    print('require P in data')
    stop()
  } 
  
  if (!is.null(data$PET)){
    PET = data$PET
  } else if (!is.null(systemArgs$PET)){
    PET = systemArgs$PET
  } else {
    print('require PET in data or systemArgs')
    stop()
  }  
  
  if (!is.null(systemArgs$dates)){
    dates = systemArgs$dates
  } else {
    print('require dates in systemArgs')
    stop()
  }
  
  o = add_dummy_year(dates,P,PET)
  dates.new = o$dates; P.new = o$P; PET.new = o$PET
  
  InputsModel <- airGR::CreateInputsModel(FUN_MOD = airGR::RunModel_GR4J, DatesR = dates.new,
                                   Precip = P.new, PotEvap = PET.new)
  
  ## calibration period selection
  Ind_Run = (length(dates.new)-length(systemArgs$dates)+1):length(dates.new)
  IndPeriod_WarmUp = 1:(length(dates.new)-length(dates))
  
  ## preparation of RunOptions object
  RunOptions <- airGR::CreateRunOptions(FUN_MOD = airGR::RunModel_GR4J, InputsModel = InputsModel,
                                 IndPeriod_Run = Ind_Run,IndPeriod_WarmUp=IndPeriod_WarmUp)
  
  ## simulation
  Param <- systemArgs$Param
  Qsim <- airGR::RunModel_GR4J(InputsModel = InputsModel,
                        RunOptions = RunOptions, Param = Param)$Qsim
  
  metricList = c()
  metricList['meanQ'] = mean(Qsim)
  metricList['P99'] = stats::quantile(Qsim,p=0.99)
  metricList['P25'] = stats::quantile(Qsim,p=0.25)
  
  # Sample daily data
  df <- data.frame(
    date = systemArgs$dates,Qsim = Qsim)  
 
  annual_data <- df %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      annual_total = sum(Qsim, na.rm = TRUE))
  
  #Qsim.ann = annual_data$annual_total
  
  annual_data$rolling_3yr <- zoo::rollapply(
    annual_data$annual_total,
    width = 3,
    FUN = sum,
    align = "left",
    fill = NA
  )
  
  metricList['min3yr'] = min(annual_data$rolling_3yr,na.rm=T)

  metricList = metricList[metrics]
  
  return(metricList)
  
}

#####################################################################

add_dummy_year = function(dates,P,PET,warmupYrs=1){
  year1 = as.integer(format(dates,'%Y'))[1]
  year0 = year1-warmupYrs
  dates.tmp = seq(as.POSIXct(paste0(year0,'/01/01'),tz='UTC'),
                  as.POSIXct(paste0(year0,'/12/31'),tz='UTC'),
                  by='days')
  P.tmp = P[1:length(dates.tmp)]
  PET.tmp = PET[1:length(dates.tmp)]
  
  dates.new = c(dates.tmp,dates)
  P.new = c(P.tmp,P)
  PET.new = c(PET.tmp,PET)
  
  return(list(dates=dates.new,
              P = P.new,
              PET = PET.new))
  
}

#####################################################################

