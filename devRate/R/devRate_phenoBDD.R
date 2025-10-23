#' Forecast ectotherm phenology as a function of temperature and development rate models
#' available in the package database
#'
#' @param tempTS The temperature time series (a vector).
#' @param timeStepTS The time step of the temperature time series (a numeric with 1 = one day).
#' @param eq The name of the equation (e.g., lactin2_95).
#' @param species The species for the model (e.g., "Sesamia nonagrioides").
#' @param lifeStages The life stages available for the species and the model.
#' @param numInd The number of individuals for the simulation (an integer).
#' @param stocha The standard deviation of a Normal distribution centered on
#'    development rate to create stochasticity among individuals (a numeric).
#' @param timeLayEggs The delay between emergence of adults and the time where
#'    females lay eggs in time steps (a numeric).
#' @return A list with three elements: the table of phenology for each individual,
#'    the models used (nls objects), and the time series for temperature.
#' @examples
#' forecastLactin2_95 <- devRateIBMdataBase(
#'   tempTS = rnorm(n = 20, mean = 20, sd = 1),
#'   timeStepTS = 10,
#'   eq = lactin2_95,
#'   species = "Sesamia nonagrioides",
#'   lifeStages = c("eggs", "larva", "pupa"),
#'   numInd = 10,
#'   stocha = 0.015,
#'   timeLayEggs = 1
#' )
#' @export
devRateIBMdataBase <- function(tempTS, timeStepTS, eq, species, lifeStages, numInd = 10, stocha, timeLayEggs = 1){
  models <- lapply(seq_along(lifeStages), function(z){ # get list of parameters for each model
    eq$startVal[ , grepl( "param." , names( eq$startVal ) ) ][(eq$startVal[ , "genSp"] == species & eq$startVal[ , "stage"] == lifeStages[z]), ]
  })
  modelVar <- sapply(models, function(i){ # get parameters' names for each model
    sapply(strsplit(names(i), split = "\\."), "[[", 2)
  })
  for(ind in 1:numInd){
    g <- 0 # generation
    tx <- 0 # time step number in temperature time series
    ratioSupDev <- 0
    vectorGS <- vector()
    while(tx < length(tempTS)){
      g <- g + 1
      for(i in seq_along(models)){
        if(ratioSupDev > 0){

          for(k in 1:nrow(modelVar)){assign(x = modelVar[k, i], value = models[[i]][k])}
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq$eqAlt)))
          add2Dev <- stats::rnorm(n = 1, mean = devRT, sd = stocha) * ratioSupDev * timeStepTS

          if(add2Dev < 0){add2Dev <- 0}
          currentDev <- add2Dev
        } else {
          currentDev <- 0
        }
        while(currentDev < 1){
          tx <- tx + 1
          if(tx > length(tempTS)){break}

          for(k in 1:nrow(modelVar)){assign(x = modelVar[k, i], value = models[[i]][k])}
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq$eqAlt)))
          addDev <- stats::rnorm(n = 1, mean = devRT, sd = stocha) * timeStepTS

          if(addDev < 0){addDev <- 0}
          currentDev <- currentDev + addDev
        }
        ratioSupDev <- 0
        if(currentDev >= 1){
          # print(currentDev)
          supDev <- 0
          if(currentDev > 1){
            supDev <- currentDev - 1
            ratioSupDev <- supDev / addDev
          }
          vectorGS <- c(vectorGS, tx)
        }
      }
      tx <- tx + as.integer(timeLayEggs)
    }
    currentInd <-  vectorGS
    if(exists(x = "communityInd")){
      communityInd <- rbind(communityInd, c(currentInd, rep(NA, ncol(communityInd) - length(currentInd))))
    }else{
      communityInd <- matrix(c(currentInd, rep(NA, 10)), ncol = length(currentInd) + 10) ### assuming 10G dif.
    }
  }
  bdd <- communityInd
  bdd <- unname(bdd[, !is.na(apply(bdd, MARGIN = 2, FUN = mean, na.rm = TRUE))])
  if(!is.matrix(bdd)){bdd <- matrix(bdd)}
  if(ncol(bdd) != 0){
    colnames(bdd) <- paste0(rep(paste0("g", 1:ncol(bdd)), each = length(models)),
                            "s", 1:length(models))[1:ncol(bdd)]
  }
  rm(communityInd)
  return(list(bdd, models, tempTS))
}



#' Forecast ectotherm phenology as a function of temperature and development
#' rate models using known parameters
#'
#' @param tempTS The temperature time series (a vector).
#' @param timeStepTS The time step of the temperature time series (a numeric
#'   with 1 = one day).
#' @param eq The name of the equation provided in the package (e.g., lactin2_95).
#'   For backward compatibility, the name of equation can be used, however, it
#'   is preferable to use a list object containing the names of the various
#'   equations in character format (e.g., list("campbell_74", "lactin2_95").
#'   See examples below.
#' @param myParam The known parameters for the equation (a list of list for
#'   each life stage).
#' @param numInd The number of individuals for the simulation (an integer).
#' @param stocha The standard deviation of a Normal distribution centered on
#'    development rate to create stochasticity among individuals (a numeric).
#' @param timeLayEggs The delay between emergence of adults and the time where
#'    females lay eggs in time steps (a numeric).
#' @param adultLifeStage An integer to specify when the adult life stage is
#'   tacking place so that timeLayEggs is applied. Default to 0 for
#'   backwards compatibility with previous versions of the package.
#' @return A list with three elements: the table of phenology for each individual,
#'    the models used (nls objects), and the time series for temperature.
#' @details Please note that this function is experimental and only works for
#'   the briere2_99 equation.
#' @examples
#' # with only one life stage
#' forecastX <- devRateIBMparam(
#'   tempTS = rnorm(n = 20, mean = 20, sd = 1),
#'   timeStepTS = 10,
#'   eq = briere2_99,
#'   myParam = list(
#'     list(
#'       aa = 0.0002,
#'       Tmin = 10,
#'       Tmax = 36.1,
#'       bb = 2.84)
#'   ),
#'   numInd = 10,
#'   stocha = 0.015,
#'   timeLayEggs = 1
#' )
#' # with two life stages
#' forecastXX <- devRateIBMparam(
#'   tempTS = rnorm(n = 20, mean = 20, sd = 1),
#'   timeStepTS = 10,
#'   eq = briere2_99,
#'   myParam = list(
#'     lifeStage01 = list(
#'       aa = 0.0002,
#'       Tmin = 10,
#'       Tmax = 36.1,
#'       bb = 2.84),
#'     lifeStage02 = list(
#'       aa = 0.0004,
#'       Tmin = 8,
#'       Tmax = 35,
#'       bb = 2.8)
#'   ),
#'   numInd = 10,
#'   stocha = 0.015,
#'   timeLayEggs = 1
#' )
#' # with three life stages, adult stage tacking place after the pupal stage,
#' # so that adultLifeStage = 2. Adult longevity was exacerbated at 15 days
#' # to highlight the impact on function output.
#' forecastXXX <- devRateIBMparam(
#'   tempTS = rnorm(n = 120, mean = 20, sd = 1),
#'   timeStepTS = 1, eq = briere2_99,
#'   myParam = list(
#'    lifeStage_larva = list(
#'      aa = 0.0002,
#'      Tmin = 10,
#'      Tmax = 36.1,
#'      bb = 2.84),
#'    lifeStage_pupa = list(
#'      aa = 0.0004,
#'      Tmin = 8,
#'      Tmax = 35,
#'      bb = 2.8),
#'    lifeStage_egg = list(
#'      aa = 0.0002,
#'      Tmin = 8,
#'      Tmax = 35,
#'      bb = 2.8)
#'   ),
#'   numInd = 5, stocha = 0.015,
#'   timeLayEggs = 15, adultLifeStage = 2
#' )
#' # with three life stages, and a different model equation for each life stage.
#' forecastXXXX <- devRateIBMparam(
#'   tempTS = rnorm(n = 60, mean = 20, sd = 1),
#'   timeStepTS = 1,
#'   eq = list("briere2_99", "lactin2_95", "campbell_74"),
#'   myParam = list(
#'     list(
#'       aa = 0.0002,
#'       Tmin = 10,
#'       Tmax = 36.1,
#'       bb = 2.84
#'     ),
#'     list(
#'       aa = 0.009,
#'       Tmax = 35.299,
#'       deltaT = 0.201,
#'       bb = -1.049
#'     ),
#'     list(
#'       aa = -0.0459,
#'       bb = 0.0044
#'     )
#'   ),
#'   numInd = 10,
#'   stocha = 0.015,
#'   timeLayEggs = 1
#' )
#' @export
devRateIBMparam <- function(
  tempTS,
  timeStepTS,
  eq,
  myParam,
  numInd = 10,
  stocha,
  timeLayEggs = 1,
  adultLifeStage = 0
  ){
  # accepting different input in eq for backward compatibility
  if(typeof(eq)!="list"){
    if(typeof(eq)=="character"){
      eq <- rep(list(get(eq)), length(myParam))
    } else{
      eq <- rep(list(eq), length(myParam))
    }
  } else {
    if(is.null(names(eq)[[1]])){
      eq <- lapply(seq_along(eq), function(k){
        get(eq[[k]])
      })
    } else {
      if(names(eq)[[1]] == "eq"){
        eq <- rep(list(eq), length(myParam))
      }
    }
  }
  # !!! exceptions for equations with positive values for low temp !!!
  exceptDevRate <- function(temp, curr_eq = 1){
    i <- devRT
    if(eq[[curr_eq]]$name == "Briere-2"){
      Tmin <- Tmin
      if(x <= Tmin){
        i <- 0
      }
    }
    return(i)
  }
  models <- lapply(seq_along(myParam), function(z){
    as.data.frame(myParam[[z]])
  })
  modelVar <- lapply(models, function(i){
    names(i)
  })
  for(ind in 1:numInd){
    g <- 0
    tx <- 0
    ratioSupDev <- 0
    vectorGS <- vector()
    while(tx < length(tempTS)){
      g <- g + 1
      for(i in seq_along(models)){
        if(ratioSupDev > 0){
          # for(k in 1:nrow(modelVar)){
          for(k in 1:length(modelVar[[i]])){
            # assign(x = modelVar[k, i], value = models[[i]][k])
            assign(x = modelVar[[i]][k], value = models[[i]][k])
          }
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq[[i]]$eqAlt)))
          devRT <- exceptDevRate(x, curr_eq = i)
          devRT[is.na(devRT)] <- 0
          devRT[devRT<0] <- 0
          add2Dev <- stats::rnorm(n = 1, mean = devRT, sd = devRT*stocha) *
            ratioSupDev * timeStepTS
          if(add2Dev < 0){add2Dev <- 0}
          currentDev <- add2Dev
        } else { currentDev <- 0 }
        while(currentDev < 1){
          tx <- tx + 1
          if(tx > length(tempTS)){break}
          # for(k in 1:nrow(modelVar)){
          for(k in 1:length(modelVar[[i]])){
            # assign(x = modelVar[k, i], value = models[[i]][k])
            assign(x = modelVar[[i]][k], value = models[[i]][k])
          }
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq[[i]]$eqAlt)))
          devRT <- exceptDevRate(x, curr_eq = i)
          devRT[is.na(devRT)] <- 0
          devRT[devRT<0] <- 0
          addDev <- stats::rnorm(n = 1, mean = devRT, sd = devRT*stocha) *
            timeStepTS
          if(addDev < 0){addDev <- 0}
          currentDev <- currentDev + addDev
        }
        ratioSupDev <- 0
        if(currentDev >= 1){
          # print(currentDev)
          supDev <- 0
          if(currentDev > 1){
            supDev <- currentDev - 1
            ratioSupDev <- supDev / addDev
          }
          vectorGS <- c(vectorGS, tx)
          # ---
          if(adultLifeStage != 0){
            if(adultLifeStage == i){
              tx <- tx + as.integer(timeLayEggs)
              if(tx > length(tempTS)){break}
            }
          }
          # ---
        }
      }
      if (adultLifeStage == 0){
        tx <- tx + as.integer(timeLayEggs)
        if(tx > length(tempTS)){break}
      }
    }
    currentInd <-  vectorGS
    if(exists(x = "communityInd") && length(currentInd) < ncol(communityInd)){
        communityInd <- rbind(
          communityInd,
          c(currentInd, rep(NA, ncol(communityInd) - length(currentInd))))
    }else{
      communityInd <- matrix(
        c(currentInd, rep(NA, 10)), ncol = length(currentInd) + 10
      ) ### assuming 10G dif.
    }
  }
  bdd <- communityInd
  bdd <- unname(bdd[, !is.na(apply(bdd, MARGIN = 2, FUN = mean, na.rm = TRUE))])
  if(!is.matrix(bdd)){bdd <- matrix(bdd)}
  if(ncol(bdd) != 0){
    colnames(bdd) <- paste0(
      rep(paste0("g", 1:ncol(bdd)), each = length(models)),
      "s", 1:length(models))[1:ncol(bdd)]
  }
  rm(communityInd)
  return(list(bdd, models, tempTS))
}
