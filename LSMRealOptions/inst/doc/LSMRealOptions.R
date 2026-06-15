## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 6,
  fig.align = 'center'
)

## ----setup--------------------------------------------------------------------
library(LSMRealOptions)
set.seed(1)

## -----------------------------------------------------------------------------
# Step 1 - Simulate stock prices:
stock_prices <- GBM_simulate(n = 1e4, t = 1, mu = 0.06, sigma = 0.2, S0 = 36, dt = 1/50)

## -----------------------------------------------------------------------------
# Step 2 - Value American put option:
put_option_value <- LSM_american_option(state_variables = stock_prices,
                                  payoff = stock_prices,
                                  K = 40,
                                  dt = 1/50,
                                  rf = 0.06,
                                  verbose = TRUE)
print(round(unlist(put_option_value[1:5]),4))

## -----------------------------------------------------------------------------
# Step 1 - Simulate asset prices:
asset_prices <- array(dim = c(51, 1e3, 2))
for(i in seq_len(2)) {
  asset_prices[,,i] <- GBM_simulate(n = 1e3, t = 1, mu = 0.06, 
                                  sigma = c(0.2, 0.3)[i], S0 = c(38, 35)[i], dt = 1/50)}

## -----------------------------------------------------------------------------
# Step 2 - Value American-style option:
OptionValue <- LSM_american_option(state_variables = asset_prices,
                                  payoff = pmax(asset_prices[,,1], asset_prices[,,2]),
                                  K = 40,
                                  dt = 1/50,
                                  rf = 0.06,
                                  verbose = TRUE,
                                  cross_product = TRUE,
                                  orthogonal = "Laguerre",
                                  degree = 9)
print(round(unlist(OptionValue[1:5]),4))

## -----------------------------------------------------------------------------
## Exercise opportunities per year:
dt <- 1/50
## strike price :
K <- 40
## short-term interest rate:
rf <- 0.06
## 100,000 simulations (50% antithetic):
n_simulations <- 1e5
## Stock price volatility:
sigma <- rep(c(rep(0.2,2),rep(0.4,2)),5)
## Stock price:
S0 <- sort(rep(seq(36,44,2),4))
## Option maturity:
TTM <- rep(1:2, 10)

LSM_output <- matrix(0, 20, 2, dimnames = list(NULL, c("Simulated American", "(s.e)")))

## Cycle through the rows of the table:
for(i in 1:20){

simulated_values <- GBM_simulate(n = n_simulations, t = TTM[i], 
                                 mu = rf, sigma = sigma[i], S0 = S0[i], dt = dt)

## American option pricing through LSM Simulation
output <- LSM_american_option(state_variables = simulated_values,
                   payoff = simulated_values,
                   call = FALSE,
                   K = K,
                   dt = dt,
                   rf = rf,
                   verbose = TRUE,
                   orthogonal = "Laguerre",
                   degree = 3
                   )
LSM_output[i,1] <- output$Value
LSM_output[i,2]  <- output$`Standard Error`
}

## Compile and print results:
LnS_table1 <- cbind.data.frame(S = S0, sigma = sigma, T = TTM, LSM_output)
print(round(LnS_table1,3))

## -----------------------------------------------------------------------------
# Step 1 - Simulate the underlying asset price:

## Initial underlying price:
initial_price <- 36

## discrete time step:
dt <- 1/12

## Project lifetime (in years):
project_lifetime <- 10
forecasting_periods <- seq(0, project_lifetime, dt)

revenue_prices <- GBM_simulate(n = 2e4, t = project_lifetime, mu = 0.06, 
                              sigma = 0.2, S0 = initial_price, dt = dt)

# Step 2 - Evaluate cash flows:

## Fixed cash flow:
# FCF <- 1e4 * initial_price
FCF <- 1e4 * 36

## Net cash flow is equal to variable cash flows subtract fixed cash flows:
NCF <- (1e4 * revenue_prices - FCF) * dt

## Financial Parameters:
construction <- 0.5 / dt
rf <- 0.05

## Initial capital investment:
learning_rate <- 0.01
CAPEX <- 1e5 * exp(- learning_rate * dt * (1:nrow(revenue_prices)-1))

# Step 3 - Evaluate Project Value through Real Options Analysis:

project_value <- LSM_real_option(state_variables = revenue_prices,
                              NCF = NCF,
                              CAPEX = CAPEX,
                              dt = dt,
                              rf = rf,
                              construction = construction,
                              verbose = TRUE)
print(format(unlist(project_value[1:6]), big.mark = ","))


## -----------------------------------------------------------------------------
## Evaluate Project Value with OF through ROA:
project_value_OF <- LSM_real_option_OF(state_variables = revenue_prices,
                              NCF = NCF,
                              CAPEX = CAPEX,
                              dt = dt,
                              rf = rf,
                              construction = construction,
                              suspend_CAPEX = 0.1 * CAPEX[1],
                              suspend_OPEX = 0.05 * CAPEX[1] * dt,
                              resume_CAPEX = 0.1 * CAPEX[1],
                              abandon_CAPEX = 0.2 * CAPEX[1],
                              save_states = TRUE,
                              verbose = TRUE,
                              debugging = TRUE
                              )

print(format(unlist(project_value_OF[1:7]), big.mark = ","))


## -----------------------------------------------------------------------------
matplot(forecasting_periods, cbind(project_value$`Cumulative Investment Prob`, 
        project_value_OF$`Cumulative Investment Prob`), type = 'l', ylim = c(0,1), 
        xlab = "Forecasting Horizon", ylab = "Cumulative Investment Proportion", 
        main = "Cumulative Investment Prop. over Forecasting Horizon")
legend("right", c("ROV", "ROV + OF"),cex=0.8, col = 1:2, fill = 1:2)

## ---- warning = FALSE---------------------------------------------------------
states_list <- apply(matrix(colnames(project_value_OF$`Project States`)), 1, 
                     FUN = function(x) cbind.data.frame(x, project_value_OF$`Project States`[,x], 
                                                        forecasting_periods))

states_ggplot <- suppressWarnings(dplyr::bind_rows(states_list))
states_ggplot[,1] <- factor(states_ggplot[,1], levels = rev(colnames(project_value_OF$`Project States`)))
colnames(states_ggplot) <- c("state", "count", "time")

library(ggplot2)

ggplot(states_ggplot, aes(x = time, y = count, fill = state)) + 
geom_bar(position = "fill", stat = "identity", width = 1) + 
scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1)) + 
scale_x_continuous(breaks = seq(0, project_lifetime, 1)) + 
ggtitle("Proportion of Project States over Project Planning Horizon") + xlab("Planning Horizon (Years)")

## -----------------------------------------------------------------------------
# Instantiate iterations:
it <- 0
current_price <- initial_price
  
# Begin Investment Trigger Value Calculate:
repeat{
  
  # Step 1: Calculate the ROV using real options analysis
  LSM_results <- LSM_real_option(state_variables = revenue_prices,
                                NCF = NCF,
                                CAPEX = CAPEX,
                                dt = dt,
                                rf = rf,
                                construction = construction)

  NPV <- LSM_results$NPV
  WOV <- LSM_results$WOV

  # Step 2: Evaluate the next initial asset price through the 'secant' method:

  ## For the first iteration, use an arbitrary initial price multiplier of 2
  if(it == 0){
    multiplier = 2
    new_price = current_price * multiplier
  }
  if(it > 0){

    ## NPV - a linear function of initial prices, so we can find it exactly after two iterations:
    NPV_gradient = (NPV - NPV_old) / (current_price - old_price)
    NPV_new_price = current_price + (0 - NPV)/NPV_gradient
    if(it == 2) NPV_crit_value = NPV_new_price     

    ## ROV -  Secant Method:
    new_price = current_price - WOV * ((current_price - old_price) / (WOV - WOV_old))

    ## Which is a multiple of:
    multiplier = new_price / current_price

    ## The WOV does not have to be exactly zero. Having it within a tolerance value 
    ## can be adequate and decrease processing time:
    WOV_tolerance <- abs(WOV) < 100
    ## If the price is identical within one cent, this can be considered the critical value:
    price_tolerance <- round(new_price,2)==round(current_price, 2)
    ## If the underlying asset impacts costs, and the iteration has pushed the price of the asset
    ## below zero, it's never optimal to invest immediately:
    negative_price <- new_price < 0
    ## Recursion break to ensure infinite loop does not occur:
    loop_break <- it > 20
    ##Approximate the root of WOV to 2 significant figures:
    if(price_tolerance || WOV_tolerance || negative_price || loop_break){
      ROV_crit_value = new_price
      break
    } 
    }
  # Step 3: Update values:

  ## Updating simulated prices:
  revenue_prices <- revenue_prices * multiplier
  ## Updating the NCF of each period:
  NCF <- (1e4 * revenue_prices - FCF) * dt
  
  ## Updating values
  old_price <- current_price
  current_price <- new_price
  WOV_old <- WOV
  NPV_old <- NPV

  # Step 4: Re-iterate:
  it <- it + 1
}

print(round(c(NPV = NPV_crit_value, ROV = ROV_crit_value),2))


## -----------------------------------------------------------------------------
print(NFCP::SS_oil$two_factor[2:7])

## -----------------------------------------------------------------------------
# Step 1 - List project parameters:

## Initial Price:
initial_oil_price <- 60
## Initial State vector:
initial_state_vector <- c(log(initial_oil_price), 0)

## discrete time step:
dt <- 1/12

## Project lifetime (in years):
project_lifetime <- 10
forecasting_periods <- seq(0, project_lifetime, dt)

# Financial Parameters:

## Capital investment:
CAPEX <- 1e4

## Fixed cash flow:
FCF <- 5e4 * initial_oil_price

## Construction - 6 months:
construction <- 0.5 / dt

## Risk-free interest rate:
rf <- 0.05

# Step 1 - Simulate spot prices:

## 10,000 antithetic simulations of one year of monthly observations
simulated_oil_prices <- NFCP::spot_price_simulate(
  x_0 = initial_state_vector,
  parameters = NFCP::SS_oil$two_factor,
  t = 10,
  dt = dt,
  N_simulations = 1e4,
  antithetic = TRUE,
  verbose = TRUE)


oil_revenue <- simulated_oil_prices$spot_prices

state_variables = array(dim = c(dim(simulated_oil_prices$spot_prices), 3))
state_variables[,,1:2] = simulated_oil_prices$state_variables
## Include the price as a state variable:
state_variables[,,3] = simulated_oil_prices$spot_prices

## Net cash flow of simulated price paths:
NCF <- (1e5 * oil_revenue - FCF) * dt


## -----------------------------------------------------------------------------
project_value <- LSM_real_option(state_variables = state_variables,
                              NCF = NCF,
                              CAPEX = CAPEX,
                              dt = dt,
                              rf = rf,
                              construction = construction,
                              orthogonal = "Laguerre",
                              degree = 9,
                              verbose = T)
print(format(round(unlist(project_value[1:6]),2), big.mark = ","))

