## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 6,
  fig.align = 'center'
)

## ----setup--------------------------------------------------------------------
library(NFCP)

# Set seed for reproducibility:
set.seed(412)

## -----------------------------------------------------------------------------
model_parameters_2F <- NFCP_parameters(N_factors = 2,
                                      GBM = TRUE,
                                      initial_states = FALSE,
                                      N_ME = 5)
## Print the vector of parameters of the specified commodity pricing model:
print(model_parameters_2F)

## -----------------------------------------------------------------------------
###Method 1 - Stitch Crude Oil Contracts according to maturity matching:
SS_oil_stitched <- stitch_contracts(futures = SS_oil$contracts,
futures_TTM = c(1, 5, 9, 13, 17)/12, maturity_matrix = SS_oil$contract_maturities,
rollover_frequency = 1/12, verbose = TRUE)

##Plot the Stitched Maturities:
matplot(as.Date(rownames(SS_oil_stitched$maturities)), SS_oil_stitched$maturities, 
        type = 'l', main = "Stitched Contract Maturities", 
        ylab = "Time To Maturity (Years)", xlab = "Date", col = 1)

## -----------------------------------------------------------------------------
print(SS_oil$two_factor)

## -----------------------------------------------------------------------------
##Example 1 - Replicating the Schwartz and Smith (2000)
##Two-Factor Crude Oil commodity pricing model:

Oil_2F <- NFCP_Kalman_filter(
 parameter_values = SS_oil$two_factor,
 parameter_names = names(SS_oil$two_factor),
 log_futures = log(SS_oil$stitched_futures),
 futures_TTM = SS_oil$stitched_TTM,
 dt = SS_oil$dt,
 verbose = TRUE,
 debugging = TRUE)


## -----------------------------------------------------------------------------
Oil_2F_parameters <- SS_oil$two_factor[1:7]
### Assume a constant measurement error in parameters of 1%:
Oil_2F_parameters["ME_1"] <- 0.01

Oil_2F_contracts <- NFCP_Kalman_filter(
 parameter_values = Oil_2F_parameters,
 parameter_names = names(Oil_2F_parameters),
 log_futures = log(SS_oil$contracts),
 futures_TTM = SS_oil$contract_maturities,
 dt = SS_oil$dt,
 verbose = TRUE,
 debugging = TRUE)


## -----------------------------------------------------------------------------
print(SS_oil$two_factor[8:12])

## -----------------------------------------------------------------------------

# Estimate a GBM model:
Oil_1F <- NFCP_MLE(
      ## Arguments
      log_futures = log(SS_oil$stitched_futures),
      dt = SS_oil$dt,
      futures_TTM= SS_oil$stitched_TTM,
      N_factors = 1,
      N_ME = 3,
      ME_TTM = c(0.5, 1, 1.5),
      print.level = 0)

##Print results:
print(round(rbind(`Estimated Parameter` = Oil_1F$estimated_parameters, 
                  `Standard Error` = Oil_1F$standard_errors),4))

## -----------------------------------------------------------------------------
print(matrix(c(Oil_1F$MLE, Oil_2F$`Log-Likelihood`), 
             dimnames = list(c("One-Factor", "Two-Factor"), "Log-Likelihood")))

## -----------------------------------------------------------------------------
print(round(rbind("One-Factor" = Oil_1F$`Information Criteria`, 
                  "Two-Factor" = Oil_2F$`Information Criteria`), 4))

## -----------------------------------------------------------------------------
##Replicate Table 3 of Schwartz and Smith (2000):
print(round(t(Oil_2F[["Term Structure Fit"]]),4))

## -----------------------------------------------------------------------------
CN_table3 <- matrix(nrow = 2, ncol = 2, dimnames = list(c("One-Factor","Two-Factor"), c("RMSE", "Bias")))
CN_table3[,"Bias"] <- c(Oil_1F$`Filtered Error`["Bias"], Oil_2F$`Filtered Error`["Bias"])
CN_table3[,"RMSE"] <- c(Oil_1F$`Filtered Error`["RMSE"], Oil_2F$`Filtered Error`["RMSE"])

print(round(CN_table3, 4))


## ----out.width='.49\\linewidth'-----------------------------------------------
##One Factor
matplot(as.Date(rownames(Oil_1F$V)), Oil_1F$V, type = 'l',
xlab = "", ylab = "Observation Error",
main = "Contract Observation Error: One-Factor Model"); legend("bottomright", 
colnames(Oil_2F$V),col=seq_len(5),cex=0.8,fill=seq_len(5))

##Two-Factor
matplot(as.Date(rownames(Oil_2F$V)), Oil_2F$V, type = 'l',
xlab = "", ylab = "Observation Error", ylim = c(-0.3, 0.2),
main = "Contract Observation Error: Two-Factor Model"); legend("bottomright", 
colnames(Oil_2F$V),col=seq_len(5),cex=0.8,fill=seq_len(5))

## -----------------------------------------------------------------------------
matplot(cbind(Oil_1F$`Term Structure Fit`["RMSE",], Oil_2F$`Term Structure Fit`["RMSE",]), 
     type = 'l', main = "Root Mean Squared Error of Futures Contracts", 
     xlab = "Contract", ylab = "RMSE"); legend("right",c("One-Factor", "Two-Factor"),
                                               col=1:2,cex=0.8,fill=1:2)

## -----------------------------------------------------------------------------
##Replicate Figure 4 of Schwartz and Smith (2000):
SS_figure_4 <- cbind(`Equilibrium Price` =
                     exp(Oil_2F$X[,1]),
                    `Spot Price` =
                     Oil_2F$Y[,"filtered Spot"])

matplot(as.Date(rownames(SS_figure_4)), SS_figure_4, type = 'l',
xlab = "", ylab = "Oil Price ($/bbl, WTI)", col = 1,
 main = "Estimated Spot and Equilibrium Prices for the Futures Data")

## -----------------------------------------------------------------------------
plot(as.Date(rownames(SS_oil$contracts)), sqrt(Oil_2F_contracts$P_t[1,1,]), 
     type = 'l', xlab = "Date", ylab = "Std. Dev.", 
     main = "Time Series of the Std. Dev for State Variable 1")

## -----------------------------------------------------------------------------
##Figure 1 and 2 of Schwartz and Smith (2000) was developed using Enron data
##and an assumption that mu was approximately 3% p.a.:
Enron_values <- c(0.0300875, 0.0161, 0.014, 1.19, 0.115, 0.158, 0.189)
names(Enron_values) <- NFCP_parameters(2, TRUE, FALSE, 0, FALSE, FALSE)


## -----------------------------------------------------------------------------
## Replicate figure 1 of Schwartz and Smith (2000):

SS_expected_spot <- spot_price_forecast(x_0 = c(2.857, 0.119),
                                           parameters = Enron_values,
                                           t = seq(0,9,1/12),
                                           percentiles = c(0.1, 0.9))
##Factor one only:
equilibrium_theta <- Enron_values[!names(Enron_values) %in%
                                 c("kappa_2", "lambda_2", "sigma_2", "rho_1_2")]
SS_expected_equilibrium <- spot_price_forecast(x_0 = c(2.857, 0),
                                                  equilibrium_theta,
                                                  t = seq(0,9,1/12),
                                                  percentiles = c(0.1, 0.9))
SS_figure_1 <- cbind(SS_expected_spot, SS_expected_equilibrium)
matplot(seq(0,9,1/12), SS_figure_1, type = 'l', col = 1, lty = c(rep(1,3), rep(2,3)),
xlab = "Time (Years)", ylab = "Spot Price ($/bbl, WTI)",
main = "Probabilistic Forecasts for Spot and Equilibrium Prices")

## -----------------------------------------------------------------------------
## Replicate Figure 2 of Schwartz and Smith (2000):

#Forecast expected spot prices under the "True" stochastic process:
SS_expected_spot <- spot_price_forecast(x_0 = c(2.857, 0.119),
                                        parameters = Enron_values,
                                        t = seq(0,9,1/12),
                                        percentiles = c(0.1, 0.9))
#Forecast expected futures prices under the Risk-Neutral stochastic process:
SS_futures_curve <- futures_price_forecast(x_0 = c(2.857, 0.119),
                                         parameters = Enron_values,
                                         futures_TTM = seq(0,9,1/12))
SS_figure_2 <- cbind(SS_expected_spot[,2], SS_futures_curve)
matplot(seq(0,9,1/12), log(SS_figure_2), type = 'l', col = 1, 
        xlab = "Time (Years)", ylab = "Log(Price)", 
        main = "Futures Prices and Expected Spot Prices")


## -----------------------------------------------------------------------------
## Maximum Observed Maturity:
max_maturity <- max(tail(SS_oil$contract_maturities,1), na.rm = TRUE)

##Estimated Futures Prices:

### One Factor (GBM):
oil_TS_1F <- futures_price_forecast(x_0 = Oil_1F$x_t,
                                         parameters = Oil_1F$estimated_parameters,
                                         futures_TTM = seq(0,max_maturity,1/12))

### Two Factor:
oil_TS_2F <- futures_price_forecast(x_0 = Oil_2F$x_t,
                                         parameters = SS_oil$two_factor,
                                         futures_TTM = seq(0,max_maturity,1/12))

matplot(seq(0,max_maturity,1/12), cbind(oil_TS_1F, oil_TS_2F), type = 'l', 
        xlab = "Maturity (Years)", ylab = "Futures Price ($)", 
        main = "Estimated and observed oil futures prices on 1995-02-14"); 
points(tail(SS_oil$contract_maturities,1), tail(SS_oil$contracts,1))
legend("bottomleft", c("One-factor", "Two-Factor", "Observed"),
       col=2:4,cex=0.8,fill=c(1,2,0))


## -----------------------------------------------------------------------------
###Test the Volatility Term Structure Fit of the Schwartz-Smith Two-Factor Oil Model:
V_TSFit <- TSfit_volatility(
 parameters = SS_oil$two_factor,
 futures = SS_oil$stitched_futures,
 futures_TTM = SS_oil$stitched_TTM,
 dt = SS_oil$dt)

##Plot Results:
matplot(V_TSFit["Maturity",], cbind(Oil_1F$`Term Structure Volatility Fit`["Theoretical Volatility",], 
                                    V_TSFit["Theoretical Volatility",]), type = 'l',
xlab = "Maturity (Years)", ylab = "Volatility (%)", 
ylim = c(0, 0.5), main = "Volatility Term Structure of Futures Returns"); points(
V_TSFit["Maturity",], V_TSFit["Empirical Volatility",]); legend("bottomleft", 
                  c("Empirical", "One-Factor", "Two-Factor"),col=0:2,cex=0.8,fill=0:2)

## -----------------------------------------------------------------------------
##100 antithetic simulations of one year of monthly observations
simulated_spot_prices <- spot_price_simulate(
 x_0 = Oil_2F$x_t,
 parameters = SS_oil$two_factor,
 t = 1,
 dt = 1/12,
 N_simulations = 1e3,
 antithetic = TRUE,
 verbose = TRUE)

##Plot Price Paths:
matplot(seq(0,1,1/12), simulated_spot_prices$spot_prices, type = 'l', 
        xlab = "Forecasting Horizon (Years)", 
        ylab = "Spot Price ($/bbl, WTI)", main = "Simulated Crude Oil prices")


## -----------------------------------------------------------------------------
##Plot 95% Prediction interval:
prediction_interval <- rbind.data.frame(apply(simulated_spot_prices$spot_prices, 1,
                       FUN = function(x) stats::quantile(x, probs = c(0.025, 0.975))),
                       Mean = rowMeans(simulated_spot_prices$spot_prices))
matplot(seq(0,1,1/12), t(prediction_interval), type = 'l', col = c(2,2,1),
lwd = 2, lty = c(2,2,1), xlab = "Forecasting Horizon (Years)", 
ylab = "Spot Price ($/bbl, WTI)", main = "Simulated Crude Oil 95% Confidence Interval")

## -----------------------------------------------------------------------------
## Simulate Crude Oil Contract Prices under a Two-Factor model

simulated_contracts <- futures_price_simulate(x_0 = c(log(SS_oil$spot[1,1]), 0),
                                            parameters = Oil_2F_parameters,
                                            dt = SS_oil$dt,
                                            N_obs = nrow(SS_oil$contracts),
                                            futures_TTM = SS_oil$contract_maturities)

##Plot Simulated prices:
matplot(as.Date(rownames(SS_oil$contracts)), simulated_contracts$futures_prices, 
        type = 'l', ylab = "Futures Price ($/bbl, WTI)", xlab = "Observations", 
        main = "Simulated Futures Contracts")

## -----------------------------------------------------------------------------
Option_prices <- matrix(rep(0,2), nrow = 2, ncol = 3, dimnames = list(c("One-Factor", "Two-Factor"), c("European", "American", "Early Exercise Value")))

# Strike Price:
strike <- 20
# Annual risk-free interest rate:
risk_free <- 0.05
# Maturity of option and future:
option <- 1
future <- 2
## American option only - number of antithetic simulations, time step (in years):
time_step <- 1/50
monte_carlo <- 1e5

## One-factor European put option value: 
Option_prices[1,1] <- European_option_value(x_0 = Oil_1F$x_t,
                                            parameters = Oil_1F$estimated_parameters,
                                            futures_maturity = future, option_maturity  = option, K = strike, r = risk_free)
## Two-factor European put option value: 
Option_prices[2,1] <- European_option_value(x_0 = Oil_2F$x_t,
                                         parameters = SS_oil$two_factor,
                                         futures_maturity = future, option_maturity = option, K = strike, r = risk_free)

## One-factor American put option value:
Option_prices[1,2] <- American_option_value(x_0 = Oil_1F$x_t,
                                         parameters = Oil_1F$estimated_parameters,
                                         futures_maturity = future, option_maturity = option, K = strike, r = risk_free, 
                                         N_simulations = monte_carlo, dt = time_step)
## Two-factor American put option value:
Option_prices[2,2] <- American_option_value(x_0 = Oil_2F$x_t,
                                         parameters = SS_oil$two_factor,
                                         futures_maturity = future, option_maturity = option, K = strike, r = risk_free, 
                                         N_simulations = monte_carlo, dt = time_step)

Option_prices[,3] <- Option_prices[,2] - Option_prices[,1]

## Print results:
print(round(Option_prices,3))


