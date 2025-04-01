## ----setup, echo=FALSE, results="hide"----------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
suppressWarnings(RNGversion("3.5.0"))
set.seed(28999)

## ---- message = FALSE, echo = FALSE-------------------------------------------
library(bayesCT)

## ----opcminimum---------------------------------------------------------------
value <- normal_outcome(mu_treatment = 120,
                        sd_treatment = 5.5) %>%
  study_details(total_sample_size     = 400, 
                study_period          = 60,
                interim_look          = NULL,
                prop_loss_to_followup = 0.10)
				
# Simulate 2 trials
output <- value %>%
  simulate(no_of_sim = 2)

# Structure of the simulation output
str(output)

## ----opcinterimlook-----------------------------------------------------------
# adding interim look
value <- value %>%
  study_details(total_sample_size     = 400, 
                study_period          = 60,
                interim_look          = c(350, 380),
                prop_loss_to_followup = 0.10)

# Simulate 2 trials
output <- value %>%
  simulate(no_of_sim = 2)

# Structure of the simulation output
str(output)

## ----opcenroll----------------------------------------------------------------
value <- value %>%
  enrollment_rate(lambda = c(0.4, 0.7), 
                  time = 40) 

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opchypo------------------------------------------------------------------
value <- value %>%
   hypothesis(delta                 = -10, 
              futility_prob         = 0.10, 
			  prob_accept_ha        = 0.95,
              expected_success_prob = 0.85, 
			  alternative           = "less")

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opcimpute----------------------------------------------------------------
value <- value %>%
  impute(no_of_impute = 20, 
         number_mcmc  = 2000)

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opcoverall---------------------------------------------------------------
value <- normal_outcome(mu_treatment = 120,
                        sd_treatment = 5.5) %>%
  study_details(total_sample_size     = 400, 
                study_period          = 60,
                interim_look          = c(350, 380),
                prop_loss_to_followup = 0.10) %>%
  hypothesis(delta                 = -10, 
             futility_prob         = 0.10, 
			 prob_accept_ha        = 0.95,
             expected_success_prob = 0.85, 
			 alternative           = "less") %>%
  enrollment_rate(lambda = c(0.4, 0.7), 
                  time   = 4) %>%
  randomize(block_size          = c(10, 20), 
            randomization_ratio = c(1, 1)) %>%
  impute(no_of_impute = 5, 
         number_mcmc  = 2000)  %>%
  simulate(no_of_sim = 2)

str(value)

## ----twoarmall----------------------------------------------------------------
value <- normal_outcome(mu_treatment = 13, 
                        mu_control = 16, 
						sd_treatment = 1.4, 
						sd_control = 1.9) %>%
  study_details(total_sample_size     = 300, 
                study_period          = 50,
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
  historical_normal(mu0_treatment     = 13, 
                    sd0_treatment     = 5, 
					N0_treatment      = 100,
                    mu0_control       = 12, 
					sd0_control       = 3, 
					N0_control        = 120, 
                    discount_function = "scaledweibull", 
                    alpha_max         = FALSE, 
					fix_alpha         = 1,
                    weibull_scale     = 0.135, 
					weibull_shape     = 3,
                      method            = "fixed") %>%
  simulate(no_of_sim = 2)

str(value)

## ----data---------------------------------------------------------------------
data(normaldata)

head(normaldata)

## ----analysisdatainput--------------------------------------------------------
input <- data_normal(treatment = normaldata$treatment, 
                     outcome   = normaldata$outcome, 
                     complete  = normaldata$complete) 

out <- input %>%
  analysis(type = "normal")

str(out)

## ----analysisall--------------------------------------------------------------
out <- data_normal(treatment = normaldata$treatment,
                   outcome   = normaldata$outcome, 
                   complete  = normaldata$complete) %>%
  hypothesis(delta = 0, 
             futility_prob         = 0.05, 
			 prob_accept_ha        = 0.95,
             expected_success_prob = 0.90, 
			 alternative           = "less") %>%
  impute(no_of_impute = 10, 
         number_mcmc  = 8000) %>%
  analysis(type = "normal")

str(out)

