## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      eval = TRUE, 
                      fig.align = "center",
                      fig.width = 3.5,
                      fig.height = 3
                      )

## ----message = FALSE, warning = FALSE-----------------------------------------
library(geostan)
data(georgia)

## -----------------------------------------------------------------------------
data(georgia)
georgia$insurance <- georgia$insurance / 100
georgia$insurance.se <- georgia$insurance.se / 100

## -----------------------------------------------------------------------------
SE <- data.frame(insurance = georgia$insurance.se)

## -----------------------------------------------------------------------------
C <- shape2mat(georgia, "B", quiet = TRUE)
cars <- prep_car_data(C)

## -----------------------------------------------------------------------------
ME_list <- prep_me_data(se = SE,
                        car_parts = cars,
                        logit = TRUE
                        )

## ----eval = FALSE, message = FALSE--------------------------------------------
#  fit <- stan_car(deaths.male ~ offset(log(pop.at.risk.male)) + insurance,
#                  centerx = TRUE,
#                  data = georgia,
#                  family = poisson(),
#                  car_parts = cars)

## ----eval = TRUE, message = FALSE---------------------------------------------
fit <- stan_car(deaths.male ~ offset(log(pop.at.risk.male)) + insurance,
                centerx = TRUE,
                ME = ME_list,
                family = poisson(),
                data = georgia, 
                car_parts = cars,
                iter = 650, # for demo speed 
                quiet = TRUE, 
                )

print(fit)

## ----eval = FALSE-------------------------------------------------------------
#  fit2 <- stan_car(deaths.male ~ offset(log(pop.at.risk.male)) + insurance,
#                  centerx = TRUE,
#                  ME = ME_list,
#  		drop = 'x_true',
#                  family = poisson(),
#                  data = georgia,
#                  car_parts = cars
#                  )

## ----fig.width = 8------------------------------------------------------------
me_diag(fit, 'insurance', georgia)

## -----------------------------------------------------------------------------
mu.x <- as.matrix(fit, pars = "mu_x_true")
dim(mu.x)
head(mu.x)
mean(mu.x)

## -----------------------------------------------------------------------------
print(fit$stanfit, pars = c("mu_x_true", "car_rho_x_true", "sigma_x_true"))

## -----------------------------------------------------------------------------
x <- as.matrix(fit, pars = "x_true")
dim(x)

## -----------------------------------------------------------------------------
x.mu <- apply(x, 2, mean)
head(x.mu)

## ----fig.width = 4, fig.height = 4--------------------------------------------
res <- resid(fit)$mean
plot(x.mu, res,
     xlab = 'Insurance rate',
     ylab = 'Residual',
     type = 'n',
     bty = 'L')
abline(h = 0)
points(x.mu, res,
       col = 4,
       pch = 20)

## ----eval = FALSE-------------------------------------------------------------
#  ME_nsp <- prep_me_data(
#    se = data.frame(insurance = georgia$insurance.se),
#    logit = TRUE
#  )
#  fit_nsp <- stan_glm(log(rate.male) ~ insurance, data = georgia, ME = ME_nsp)

## ----eval = FALSE-------------------------------------------------------------
#  georgia$college <- georgia$college / 100
#  georgia$college.se <- georgia$college.se / 100
#  
#  se = data.frame(insurance = georgia$insurance.se,
#                  college = georgia$college.se)
#  
#  ME <- prep_me_data(se = se, logit = c(TRUE, TRUE))
#  
#  fit <- stan_glm(deaths.male ~ offset(log(pop.at.risk.male)) + insurance + college,
#                  ME = ME,
#  		re = ~ GEOID,
#                  family = poisson(),
#                  data = georgia,
#                  iter = 700
#                  )		

## ----eval = FALSE-------------------------------------------------------------
#  data(georgia)
#  
#  # income in $1,000s
#  georgia$income <- georgia$income / 1e3
#  georgia$income.se <- georgia$income.se /1e3
#  
#  # create log income
#  georgia$log_income <- log( georgia$income )
#  
#  # create SEs for log income
#  log_income_se <- se_log( georgia$income, georgia$income.se )
#  
#  # prepare spatial CAR data
#  C <- shape2mat(georgia, "B")
#  cars <- prep_car_data(C)
#  
#  # prepare ME data
#  se <- data.frame( log_income = log_income_se )
#  ME <- prep_me_data( se = se, car_parts = cars )
#  
#  # fit model
#  fit <- stan_car(deaths.male ~ offset(log(pop.at.risk.male)) + log_income,
#                  ME = ME,
#  		car_parts = cars,
#  		family = poisson(),
#  		data = georgia,
#  		cores = 4
#                  )
#  
#  # check ME model
#  me_diag(fit, 'log_income', georgia)
#  
#  # check disease model
#  sp_diag(fit, georgia)
#  
#  # coefficient estimates
#  plot(fit)
#  
#  # plot the income-mortality gradient
#  values <- seq( min(georgia$log_income), max(georgia$log_income), length.out = 100 )
#  new_data <- data.frame(pop.at.risk.male = 1,
#                         log_income = values)
#  
#  preds <- predict(fit, new_data, type = 'response')
#  preds$Income <- exp( preds$log_income )
#  preds$Mortality <- preds$mean * 100e3
#  
#  plot(preds$Income, preds$Mortality,
#      type = 'l',
#      xlab = "Income",
#      ylab = "Deaths per 100,000")

