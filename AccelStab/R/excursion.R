#' @title Temperature Excursion
#'
#' @description Predict a temperature excursion for a product.
#'
#' @details Use the output from step1.down to run a temperature excursion prediction.
#'
#' @param step1_down_object The fit object from the step1.down function (required).
#' @param temp_changes A list that represents the order of the temperatures that
#'  the product is subjected to. Must be the same length as time_changes.
#' @param time_changes List that represents the times at which the temperature changes,
#'  Starts from time zero and must be the same length as temp_changes.
#' @param CI Show confidence intervals.
#' @param PI Show prediction intervals.
#' @param draw Number of simulations used to estimate confidence intervals.
#' @param confidence_interval Confidence level for the confidence and prediction intervals
#'  around the predictions (default 0.95).
#' @param intercept Use a forced y-intercept. If null, the fitted value will be used.
#' @param ribbon Add shade to confidence and prediction intervals (optional).
#' @param xname Label for the x-axis (optional).
#' @param yname Label for the y-axis (optional).
#' @param plot_simulations If TRUE, randomly selects 100 of the simulations to
#'  display on the plot.
#'
#' @return An SB class object, a list including the following elements:
#' \itemize{
#'  \item *prediction* - A data frame containing the predictions with the confidence and prediction intervals.
#'  \item *simulations* - Matrix of the simulations.
#'  \item *excursion plot* - A plot with predictions and statistical intervals.
#'  \item *user_parameters* - List of users input parameters which is utilised by other
#'    functions in the package.
#'    }
#'
#' @examples
#' #load antigenicity
#' data(antigenicity)
#'
#' #run step1.down fit
#' fit1 <- step1_down(data = antigenicity, y = "conc", .time = "time",
#'  C = "Celsius", max_time_pred = 3)
#'
#' #run excursion function with fixed intercept.
#' excursion <- excursion(step1_down_object = fit1,
#'                        temp_changes = c(5,15,10),
#'                        time_changes = c(0.5,1.5,3),
#'                        CI = TRUE, PI = TRUE, draw = 10000,
#'                        confidence_interval = 0.95,
#'                        intercept = 80,
#'                        xname = "Time in years", yname = "Concentration",
#'                        ribbon = TRUE, plot_simulations = TRUE)
#'
#' excursion$excursion_plot
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export excursion

excursion <- function(step1_down_object, temp_changes, time_changes, CI = TRUE,
                      PI = TRUE, draw = 10000, confidence_interval = 0.95,
                      intercept = NULL, ribbon = TRUE, xname = NULL, yname = NULL,
                      plot_simulations = FALSE){

  if (length(temp_changes) != length(time_changes))
    stop("temp_changes and time_changes must be the same length.")

  fit_object <- step1_down_object$fit
  dat <- step1_down_object$data
  Kref = mean(dat$K)
  time_lengths <- c(time_changes[1],diff(time_changes)) # Useful later
  k1 = fit_object$par$k1
  k2 = fit_object$par$k2
  k3 = fit_object$par$k3
  c0 = ifelse(is.null(intercept),fit_object$par$c0, intercept) # Making intercept option

  coeffs_fit <- coef(fit_object)

  preds <- data.frame( # Making empty prediction frame
    phase_time = numeric(),
    temps = numeric(),
    conc = numeric(),
    phase = numeric(),
    total_time = numeric())

  for (i in 1:length(temp_changes)){ # Making the prediction frame with 0.01yr intervals
    preds <- rbind(preds, data.frame(
      phase_time = seq(0,time_lengths[i],length.out = 101),
      temps = rep(temp_changes[i],101),
      conc = rep(NA,101),
      degrad = rep(NA,101),
      phase = rep(i,101),
      total_time = seq(ifelse(i==1,0,time_changes[i-1]),time_changes[i],length.out = 101)))
  }

  # Now it splits for each one of the four options
  if(step1_down_object$user_parameters$reparameterisation == T &&
     step1_down_object$user_parameters$zero_order == T){

    for (i in 1:nrow(preds)){ # predictions
      if (preds$phase[i] == 1){ # First phase no initial degradation
        preds$degrad[i] <- preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15)+ k2/Kref)
        preds$conc[i] <- c0 - c0 * preds$degrad[i]
      }else{
        # This finds the degradation from the end of the previous phase
        degrad_tracker <- preds %>% filter(phase == (preds$phase[i] - 1)) %>% select(degrad) %>% max()

        preds$degrad[i] <- (degrad_tracker) + preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15) + k2/Kref)
        preds$conc[i] <- c0 - c0 * preds$degrad[i]
      }
    }
    # Boot counter
    boot_count = 1

    if (CI  | PI  | plot_simulations){ # adding confidence and prediction intervals
      # making the covariance matrix
      SIG = vcov(fit_object)
      sigma = summary(fit_object)$sigma

      # making pred_fct
      pred_fct <- function(parms){

        k1 = parms[1]
        k2 = parms[2]
        c0 = ifelse(is.null(intercept),parms[3], intercept)

        conc_boot <- rep(NA,101 * length(time_changes))
        degrad_boot <- rep(NA,101 * length(time_changes))

        for (i in 1:length(time_changes)){
          if (i == 1){ # First phase no initial degradation
            degrad_boot[1:101] <- preds$phase_time[1:101] * exp(k1 - k2 / (preds$temps[1:101] + 273.15)+ k2/Kref)
            conc_boot[1:101] <- c0 - c0 * degrad_boot[1:101]
          }else{
            # This finds the degradation from the end of the previous phase
            degrad_tracker <- degrad_boot[(i-1)*101]

            degrad_boot[((i-1)*101 +1):(i*101)] <- (degrad_tracker) + preds$phase_time[((i-1)*101 +1):(i*101)] * exp(k1 - k2 / (preds$temps[((i-1)*101 +1):(i*101)] + 273.15)+ k2/Kref)
            conc_boot[((i-1)*101 +1):(i*101)] <- c0 - c0 * degrad_boot[((i-1)*101 +1):(i*101)]


          }
          if (i == length(time_changes)){
            if (boot_count %% 1000 == 0){
              print(paste0("Sample draw progress: ",(boot_count*100)/draw,"%"))
            }
            boot_count <<- boot_count+1
          }
        }

        return(conc_boot)
      }


      # Multi T bootstrap
      rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 3) + matrix(nrow = draw, ncol = 3, byrow = TRUE, coeffs_fit)
      res.boot = matrix(nrow = draw, ncol = nrow(preds), byrow = TRUE, apply(rand.coef, 1, pred_fct))

      CI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
      CI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)

      simulations <- res.boot
      res.boot = res.boot + rnorm(draw*length(preds$total_time), 0, sigma)
      PI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
      PI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)


    }
  }else if(step1_down_object$user_parameters$reparameterisation == F &&
           step1_down_object$user_parameters$zero_order == T){

    for (i in 1:nrow(preds)){ # predictions
      if (preds$phase[i] == 1){ # First phase no initial degradation
        preds$degrad[i] <- preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15))
        preds$conc[i] <- c0 - c0 * preds$degrad[i]
      }else{
        # This finds the degradation from the end of the previous phase
        degrad_tracker <- preds %>% filter(phase == (preds$phase[i] - 1)) %>% select(degrad) %>% max()

        preds$degrad[i] <- (degrad_tracker) + preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15))
        preds$conc[i] <- c0 - c0 * preds$degrad[i]
      }
    }
    # Boot counter
    boot_count = 1

    if (CI  | PI  | plot_simulations){ # adding confidence and prediction intervals
      # making the covariance matrix
      SIG = vcov(fit_object)
      sigma = summary(fit_object)$sigma

      # making pred_fct
      pred_fct <- function(parms){

        k1 = parms[1]
        k2 = parms[2]
        c0 = ifelse(is.null(intercept),parms[3], intercept)

        conc_boot <- rep(NA,101 * length(time_changes))
        degrad_boot <- rep(NA,101 * length(time_changes))

        for (i in 1:length(time_changes)){
          if (i == 1){ # First phase no initial degradation
            degrad_boot[1:101] <- preds$phase_time[1:101] * exp(k1 - k2 / (preds$temps[1:101] + 273.15))
            conc_boot[1:101] <- c0 - c0 * degrad_boot[1:101]
          }else{
            # This finds the degradation from the end of the previous phase
            degrad_tracker <- degrad_boot[(i-1)*101]

            degrad_boot[((i-1)*101 +1):(i*101)] <- (degrad_tracker) + preds$phase_time[((i-1)*101 +1):(i*101)] * exp(k1 - k2 / (preds$temps[((i-1)*101 +1):(i*101)] + 273.15))
            conc_boot[((i-1)*101 +1):(i*101)] <- c0 - c0 * degrad_boot[((i-1)*101 +1):(i*101)]


          }
          if (i == length(time_changes)){
            if (boot_count %% 1000 == 0){
              print(paste0("Sample draw progress: ",(boot_count*100)/draw,"%"))
            }
            boot_count <<- boot_count+1
          }
        }

        return(conc_boot)
      }


      # Multi T bootstrap
      rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 3) + matrix(nrow = draw, ncol = 3, byrow = TRUE, coeffs_fit)
      res.boot = matrix(nrow = draw, ncol = nrow(preds), byrow = TRUE, apply(rand.coef, 1, pred_fct))

      CI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
      CI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)

      simulations <- res.boot
      res.boot = res.boot + rnorm(draw*length(preds$total_time), 0, sigma)
      PI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
      PI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)


    }
  }else if(step1_down_object$user_parameters$reparameterisation == T &&
           step1_down_object$user_parameters$zero_order == F){

    for (i in 1:nrow(preds)){ # predictions
      if (preds$phase[i] == 1){ # First phase no initial degradation
        preds$degrad[i] <- (1 - ((1 - k3) * (1/(1 - k3) - preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15)+ k2/Kref)))^(1/(1-k3)))
        preds$conc[i] <- c0 - c0 * preds$degrad[i]
      }else{
        # This finds the degradation from the end of the previous phase
        degrad_tracker <- preds %>% filter(phase == (preds$phase[i] - 1)) %>% select(degrad) %>% max()

        preds$degrad[i] <- (1 - ((1 - k3) * (((1- degrad_tracker) ^(1 - k3))/(1 - k3) - preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15) + k2/Kref)))^(1/(1-k3)))
        preds$conc[i] <- c0 - c0 * preds$degrad[i]
      }
    }
    # Boot counter
    boot_count = 1

    if (CI  | PI  | plot_simulations){ # adding confidence and prediction intervals
      # making the covariance matrix
      SIG = vcov(fit_object)
      sigma = summary(fit_object)$sigma

      # making pred_fct
      pred_fct <- function(parms){

        k1 = parms[1]
        k2 = parms[2]
        k3 = parms[3]
        c0 = ifelse(is.null(intercept),parms[4], intercept)

        conc_boot <- rep(NA,101 * length(time_changes))
        degrad_boot <- rep(NA,101 * length(time_changes))

        for (i in 1:length(time_changes)){
          if (i == 1){ # First phase no initial degradation
            degrad_boot[1:101] <- (1 - ((1 - k3) * (1/(1 - k3) - preds$phase_time[1:101] * exp(k1 - k2 / (preds$temps[1:101] + 273.15)+ k2/Kref)))^(1/(1-k3)))
            conc_boot[1:101] <- c0 - c0 * degrad_boot[1:101]
          }else{
            # This finds the degradation from the end of the previous phase
            degrad_tracker <- degrad_boot[(i-1)*101]

            degrad_boot[((i-1)*101 +1):(i*101)] <- (1 - ((1 - k3) * (((1- degrad_tracker) ^(1 - k3))/(1 - k3) - preds$phase_time[((i-1)*101 +1):(i*101)] * exp(k1 - k2 / (preds$temps[((i-1)*101 +1):(i*101)] + 273.15)+ k2/Kref)))^(1/(1-k3)))
            conc_boot[((i-1)*101 +1):(i*101)] <- c0 - c0 * degrad_boot[((i-1)*101 +1):(i*101)]


          }
          if (i == length(time_changes)){
            if (boot_count %% 1000 == 0){
              print(paste0("Sample draw progress: ",(boot_count*100)/draw,"%"))
            }
            boot_count <<- boot_count+1
          }
        }

        return(conc_boot)
      }

      # Multi T bootstrap
      rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 4) + matrix(nrow = draw, ncol = 4, byrow = TRUE, coeffs_fit)
      res.boot = matrix(nrow = draw, ncol = nrow(preds), byrow = TRUE, apply(rand.coef, 1, pred_fct))

      CI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
      CI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)

      simulations <- res.boot
      res.boot = res.boot + rnorm(draw*length(preds$total_time), 0, sigma)
      PI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
      PI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)


    }

  }else if(step1_down_object$user_parameters$reparameterisation == F &&
           step1_down_object$user_parameters$zero_order == F){

    for (i in 1:nrow(preds)){ # predictions
    if (preds$phase[i] == 1){ # First phase no initial degradation
      preds$degrad[i] <- (1 - ((1 - k3) * (1/(1 - k3) - preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15))))^(1/(1-k3)))
      preds$conc[i] <- c0 - c0 * preds$degrad[i]
    }else{
      # This finds the degradation from the end of the previous phase
      #browser()
      degrad_tracker <-  preds %>% filter(phase == (preds$phase[i] - 1)) %>% select(degrad) %>% max()

      preds$degrad[i] <- (1 - ((1 - k3) * (((1- degrad_tracker) ^(1 - k3))/(1 - k3) - preds$phase_time[i] * exp(k1 - k2 / (preds$temps[i] + 273.15))))^(1/(1-k3)))
      preds$conc[i] <- c0 - c0 * preds$degrad[i]
    }
  }
  # Boot counter
  boot_count = 1

  if (CI  | PI  | plot_simulations){ # adding confidence and prediction intervals
    # making the covariance matrix
    SIG = vcov(fit_object)
    sigma = summary(fit_object)$sigma

    # making pred_fct
    pred_fct <- function(parms){

      k1 = parms[1]
      k2 = parms[2]
      k3 = parms[3]
      c0 = ifelse(is.null(intercept),parms[4], intercept)

      conc_boot <- rep(NA,101 * length(time_changes))
      degrad_boot <- rep(NA,101 * length(time_changes))

      for (i in 1:length(time_changes)){
        if (i == 1){ # First phase no initial degradation
          degrad_boot[1:101] <- (1 - ((1 - k3) * (1/(1 - k3) - preds$phase_time[1:101] * exp(k1 - k2 / (preds$temps[1:101] + 273.15))))^(1/(1-k3)))
          conc_boot[1:101] <- c0 - c0 * degrad_boot[1:101]
        }else{
          # This finds the degradation from the end of the previous phase
          degrad_tracker <- degrad_boot[(i-1)*101]

          degrad_boot[((i-1)*101 +1):(i*101)] <- (1 - ((1 - k3) * (((1- degrad_tracker) ^(1 - k3))/(1 - k3) - preds$phase_time[((i-1)*101 +1):(i*101)] * exp(k1 - k2 / (preds$temps[((i-1)*101 +1):(i*101)] + 273.15))))^(1/(1-k3)))
          conc_boot[((i-1)*101 +1):(i*101)] <- c0 - c0 * degrad_boot[((i-1)*101 +1):(i*101)]


        }
        if (i == length(time_changes)){
          if (boot_count %% 1000 == 0){
            print(paste0("Sample draw progress: ",(boot_count*100)/draw,"%"))
          }
          boot_count <<- boot_count+1
        }
      }

      return(conc_boot)
    }

    # Multi T bootstrap
    rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 4) + matrix(nrow = draw, ncol = 4, byrow = TRUE, coeffs_fit)
    res.boot = matrix(nrow = draw, ncol = nrow(preds), byrow = TRUE, apply(rand.coef, 1, pred_fct))

    CI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
    CI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)

    simulations <- res.boot
    res.boot = res.boot + rnorm(draw*length(preds$total_time), 0, sigma)
    PI1b = apply(res.boot, 2, quantile, ((1-confidence_interval)/2), na.rm = TRUE)
    PI2b = apply(res.boot, 2, quantile, ((1+confidence_interval)/2), na.rm = TRUE)


  }}

  if(PI | CI | plot_simulations){
  preds <- cbind(preds,CI1b) %>% cbind(CI2b) %>% cbind(PI1b) %>% cbind(PI2b)

  selected_indices <- sample(1:draw, 100)
  selected_rows <- simulations[selected_indices, ]

  simu_df <- data.frame(
    total_time = rep(preds$total_time, 100),
    conc = c(t(selected_rows)),
    simulation_no = rep(selected_indices, each = 101*length(temp_changes))
  )}

  mytheme <- ggplot2::theme(legend.position = "bottom", strip.background = element_rect(fill = "white"),
                            legend.key = element_rect(fill = "white"), legend.key.width = unit(2,"cm"),
                            axis.text = element_text(size = 13), axis.title = element_text(size = 13),
                            strip.text = element_text(size = 13),
                            legend.text = element_text(size = 13),
                            legend.title = element_text(size = 13))

  confidence_i <- paste0(confidence_interval * 100," % CI")
  prediction_i <- paste0(confidence_interval * 100," % PI")

  lines_t <- c("solid","dotted","longdash")
  names(lines_t) <- c("Prediction",confidence_i,prediction_i)

  if (is.null(xname))
    xname = "Time"
  if (is.null(yname))
    yname = "Response Variable"

  plot1 <- preds %>% mutate(phase = as.factor(phase),temps = as.factor(temps)) %>%
    ggplot() +
    {if(plot_simulations)geom_line(data = simu_df,mapping=aes(x= total_time, y = conc, group = simulation_no),alpha = 0.17,color = "grey")} +
    labs( x = xname, y = yname) +
    geom_line(mapping = aes(x = total_time, y = conc, colour = temps, group = phase , linetype = "Prediction")) +
    mytheme +
    {if(CI)geom_line(mapping = aes(x = total_time, y = CI1b, colour = temps,group = phase , linetype = confidence_i))} +
    {if(CI)geom_line(mapping = aes(x = total_time, y = CI2b, colour = temps,group = phase , linetype = confidence_i))} +
    {if(PI)geom_line(mapping = aes(x = total_time, y = PI1b, colour = temps,group = phase , linetype = prediction_i))} +
    {if(PI)geom_line(mapping = aes(x = total_time, y = PI2b, colour = temps,group = phase , linetype = prediction_i))} +
    {if(ribbon && PI)geom_ribbon(aes(x = total_time, ymin=PI1b, ymax=PI2b,group = phase , fill = temps), alpha=0.08, show.legend = FALSE)} +
    {if(ribbon && CI)geom_ribbon(aes(x = total_time, ymin=CI1b, ymax=CI2b,group = phase , fill= temps), alpha=0.13, show.legend = FALSE)} +
    scale_linetype_manual(name = NULL,values = lines_t)+
    scale_color_discrete(name = "Celsius") +
    theme(legend.box = "vertical", legend.spacing = unit(-0.4,"line"))

  if(PI ==F && CI==F && plot_simulations==F){
    simulations = NULL}
  results = list(preds,simulations,plot1,step1_down_object$user_parameters)
  names(results) = c("predictions","simulations","excursion_plot","user_parameters")
  class(results) = "SB"
  return(results)

  }

globalVariables(c('phase','degrad','temps','total_time','conc','simulation_no'))


