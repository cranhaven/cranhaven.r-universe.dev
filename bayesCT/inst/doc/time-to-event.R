## ----setup, echo=FALSE, results="hide"----------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
suppressWarnings(RNGversion("3.5.0"))
set.seed(28999)

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(bayesCT)

## ----opcminimum---------------------------------------------------------------
value <- survival_outcome(hazard_treatment = c(0.012, 0.008), 
                          cutpoint         = 30) %>%
  study_details(total_sample_size     = 200, 
                study_period          = 70,
                interim_look          = NULL,
                prop_loss_to_followup = 0.1) 
 
				
# Simulate 2 trials
output <- value %>%
  simulate(no_of_sim = 2)

# Structure of the simulation output
str(output)

## ----opcinterimlook-----------------------------------------------------------
# Adding interim looks
value <- value %>%
  study_details(total_sample_size     = 200, 
                study_period          = 70,
                interim_look          = 180,
                prop_loss_to_followup = 0.10)

# Simulate 2 trials
output <- value %>% 
  simulate(no_of_sim = 2)

# Structure of the simulation output
str(output)

## ----opcenrollment------------------------------------------------------------
value <- value %>%
  enrollment_rate(lambda = c(0.25, 0.8), 
                  time   = 40)

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opchypo------------------------------------------------------------------
value <- value %>%
  hypothesis(delta                  = 0.50,
             futility_prob          = 0.05,
             prob_accept_ha         = 0.95,
             expected_success_prob  = 0.90,
             alternative            = "less")

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opcimpute----------------------------------------------------------------
value <- value %>%
  impute(no_of_impute = 10, 
         number_mcmc  = 2000)

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opcprior-----------------------------------------------------------------
value <- value %>%
  gamma_prior(a0 = .2, 
             b0 = .2)

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## -----------------------------------------------------------------------------
hist_data <- data.frame(time      = rexp(100, 0.011),
                        event     = rbinom(100, 1, 0.8),
                        treatment = rep(1, 100))

str(hist_data)

## ----opchist------------------------------------------------------------------
value <- value %>%
  historical_survival(time              = hist_data$time, 
                      treatment         = hist_data$treatment,
                      event             = hist_data$event,
                      discount_function = "weibull",
                      alpha_max         = 1, 
                      fix_alpha         = FALSE,
                      weibull_scale     = 0.135, 
                      weibull_shape     = 3,
                      method            = "mc")

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opcoverall---------------------------------------------------------------
value <- survival_outcome(hazard_treatment = c(0.012, 0.008), 
                          cutpoint         = 30) %>%
  enrollment_rate(lambda = c(0.25, 0.8), 
                  time   = 40) %>%
  study_details(total_sample_size     = 200, 
                study_period          = 70,
                interim_look          = 180,
                prop_loss_to_followup = 0.10) %>%
  hypothesis(delta                  = 0.50,
             futility_prob          = 0.05,
             prob_accept_ha         = 0.95,
             expected_success_prob  = 0.90,
             alternative            = "less") %>%
  impute(no_of_impute = 10, 
         number_mcmc  = 2000) %>%
  gamma_prior(a0 = .2,
              b0 = .2) %>%
   historical_survival(time             = hist_data$time, 
                      treatment         = hist_data$treatment,
                      event             = hist_data$event,
                      discount_function = "weibull",
                      alpha_max         = 1, 
                      fix_alpha         = FALSE,
                      weibull_scale     = 0.135, 
                      weibull_shape     = 3,
                      method            = "mc") %>%
  simulate(no_of_sim = 2)

str(value)

## ----twoarmall----------------------------------------------------------------
value <- survival_outcome(hazard_treatment = c(0.01, 0.012),
                          hazard_control   = c(0.015, 0.017),
                          cutpoint         = 25) %>%
  study_details(total_sample_size     = 250, 
                study_period          = 100,
                interim_look          = NULL,
                prop_loss_to_followup = 0.10) %>%
  hypothesis(delta                 = 0,
             futility_prob         = 0,
             prob_accept_ha        = 0.95,
             expected_success_prob = 1,
             alternative           = "less") %>%
  impute(no_of_impute = 25, 
         number_mcmc  = 5000) %>%
  enrollment_rate(lambda = c(0.8), 
                  time = NULL) %>%
  randomize(block_size          = c(4, 6), 
            randomization_ratio = c(1, 1)) %>%
  simulate(no_of_sim = 2)

str(value)

## ----data---------------------------------------------------------------------
data(survivaldata)

head(survivaldata)

## ----analysisdata-------------------------------------------------------------
input <- data_survival(time       = survivaldata$time,
                       treatment  = survivaldata$treatment,
                       event      = survivaldata$event)

out <- input %>%
  analysis(type = "survival")

str(out)

## ----analysisall--------------------------------------------------------------
out <- data_survival(time       = survivaldata$time,
                     treatment  = survivaldata$treatment,
                     event      = survivaldata$event) %>%
  hypothesis(delta                 = 0.02, 
             futility_prob         = 0.05, 
             prob_accept_ha        = 0.95,
             expected_success_prob = 0.90,
             alternative           = "less") %>%
  impute(no_of_impute = 50, 
         number_mcmc  = 10000) %>%
  gamma_prior(a0 = .2, 
              b0 = .2) %>%
  analysis(type = "survival")

str(out)


