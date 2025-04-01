## ----setup, echo=FALSE, results="hide"----------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
suppressWarnings(RNGversion("3.5.0"))
set.seed(28999)

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(bayesCT)

## ----opcbinomial--------------------------------------------------------------
value <- binomial_outcome(p_treatment = 0.08) %>%
   study_details(total_sample_size     = 900, 
                 study_period          = 50,
                 interim_look          = NULL,
                 prop_loss_to_followup = 0.10)

# Simulate 2 trials
output <- value %>%
  simulate(no_of_sim = 2)

# Structure of the simulation output
str(output)

## ----opcinterimlook-----------------------------------------------------------
# Adding interim looks
value <- value %>%
  study_details(total_sample_size     = 900, 
                study_period          = 50,
                interim_look          = c(600, 700, 800),
                prop_loss_to_followup = 0.10)

# Simulate 2 trials
output <- value %>% 
  simulate(no_of_sim = 2)

# Structure of the simulation output
str(output)

## ----opcenrollment------------------------------------------------------------
value <- value %>%
  enrollment_rate(lambda = c(0.3, 1), 
                  time   = 25)

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opchypo------------------------------------------------------------------
value <- value %>%
  hypothesis(delta                 = -0.03, 
             futility_prob         = 0.05, 
			 prob_accept_ha        = 0.95,
             expected_success_prob = 0.90, 
			 alternative           = "less")

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opcimpute----------------------------------------------------------------
value <- value %>%
  impute(no_of_impute = 5, 
         number_mcmc  = 1000)

output <- value %>%
  simulate(no_of_sim = 10)

str(output)

## ----opcprior-----------------------------------------------------------------
value <- value %>%
  beta_prior(a0 = 5, 
             b0 = 5)

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opchist------------------------------------------------------------------
value <- value %>%
  historical_binomial(y0_treatment      = 5, 
                      N0_treatment      = 55,
                      discount_function = "identity",
                      y0_control        = NULL, 
                      N0_control        = NULL,
                      alpha_max         = 1, 
                      fix_alpha         = FALSE,
                      weibull_scale     = 0.135, 
                      weibull_shape     = 3,
                      method            = "fixed")

output <- value %>%
  simulate(no_of_sim = 2)

str(output)

## ----opcoverall---------------------------------------------------------------
value <- binomial_outcome(p_treatment = 0.08) %>%
  enrollment_rate(lambda = c(0.3, 1), 
                  time   = 25) %>%
  study_details(total_sample_size     = 900, 
                study_period          = 50,
                interim_look          = c(600, 700, 800),
                prop_loss_to_followup = 0.10) %>%
  hypothesis(delta                 = -0.03, 
             futility_prob         = 0.05, 
			 prob_accept_ha        = 0.95,
             expected_success_prob = 0.90, 
			 alternative           = "less") %>%
  impute(no_of_impute = 25, 
         number_mcmc  = 1000) %>%
  beta_prior(a0 = 5, 
             b0 = 5) %>%
  historical_binomial(y0_treatment      = 5, 
                      N0_treatment      = 55,
                      discount_function = "identity",
                      y0_control        = NULL, 
					  N0_control        = NULL,
                      alpha_max         = 1, 
					  fix_alpha         = FALSE,
                      weibull_scale     = 0.135, 
					  weibull_shape     = 3, 
                      method            = "fixed") %>%
  simulate(no_of_sim = 2)

str(value)

## ----twoarmoverall------------------------------------------------------------

value <- binomial_outcome(p_treatment = 0.15, 
                          p_control   = 0.12) %>%
  study_details(total_sample_size     = 400, 
                study_period          = 30,
                interim_look          = 350,
                prop_loss_to_followup = 0.15) %>%
  hypothesis(delta                 = 0, 
             futility_prob         = 0.10, 
			 prob_accept_ha        = 0.975,
             expected_success_prob = 1, 
			 alternative           = "greater") %>%
  randomize(block_size          = 9, 
            randomization_ratio = c(4, 5)) %>%
  impute(no_of_impute = 5, 
         number_mcmc  = 5000) %>%
  beta_prior(a0 = 0, 
             b0 = 0) %>%
  simulate(no_of_sim = 2)

str(value)

## ----data---------------------------------------------------------------------
data(binomialdata)

head(binomialdata)

## ----analysisdata-------------------------------------------------------------
input <- data_binomial(treatment = binomialdata$treatment,
                       outcome   = binomialdata$outcome,
                       complete  = binomialdata$complete)

out <- input %>%
  analysis(type = "binomial")

str(out)

## ----analysisall--------------------------------------------------------------
out <- data_binomial(treatment = binomialdata$treatment,
                     outcome   = binomialdata$outcome,
                     complete  = binomialdata$complete) %>%
  hypothesis(delta                 = 0.02, 
             futility_prob         = 0.05, 
			 prob_accept_ha        = 0.95,
			 expected_success_prob = 0.90, 
			 alternative           = "greater") %>%
  impute(no_of_impute = 50, 
         number_mcmc  = 10000) %>%
  beta_prior(a0 = 3, 
             b0 = 3) %>%
  historical_binomial(y0_treatment      = 12, 
                      N0_treatment      = 100,
                      y0_control        = NULL, 
					  N0_control        = NULL,
                      discount_function = "weibull",
                      alpha_max         = 1, 
					  fix_alpha         = FALSE,
                      weibull_scale     = 0.135, 
					  weibull_shape     = 3, 
                      method            = "fixed") %>%
  analysis(type = "binomial")

str(out)


