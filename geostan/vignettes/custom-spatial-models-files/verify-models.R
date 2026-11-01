library(rstan)
#library(geostan)
devtools::load_all("~/dev/geostan")
data(georgia)

A <- shape2mat(georgia, "B")
car_list <- prep_car_data(A, style = "WCAR")

# add data
## (centering covariates improves sampling efficiency)
car_list$y <- log(georgia$income / 1e3)
car_list$x <- scale(log(georgia$population / 1e3), center = TRUE, scale = FALSE)
car_list$k <- ncol(car_list$x)

# compile Stan model from file
autonormal_file <- "autonormal.stan"
car_model <- stan_model(autonormal_file)

# sample from model
samples <- sampling(car_model, data = car_list, iter = 1e3)

# should be same as:
A <- shape2mat(georgia, "B")
car_list <- prep_car_data(A, style = "WCAR")
fit <- stan_car(log(income / 1e3) ~ log(population / 1e3),
                data = georgia, car = car_list, centerx = TRUE,
                iter = 1e3)

#### Poisson
A <- shape2mat(georgia, "B")
car_list <- prep_car_data(A, style = "WCAR")

# add data
car_list$y <- georgia$deaths.male
car_list$const_offset <- log(georgia$pop.at.risk.male)
car_list$x <- scale(log(georgia$income / 10e3), center = TRUE, scale = FALSE)
car_list$k <- ncol(car_list$x)

# compile Stan model from file
car_poisson_file <- "car_poisson.stan"
car_poisson <- stan_model(car_poisson_file)

# sample from model
samples <- sampling(car_poisson,
                    data = car_list,
                    iter = 1e3)


# same as:
A <- shape2mat(georgia, "B")
car_list <- prep_car_data(A, style = "WCAR")
fit <- stan_car(deaths.male ~ offset(log(pop.at.risk.male)) + log(income / 1e3),
                data = georgia, 
                car = car_list, 
                centerx = TRUE,
                family = poisson(), 
                iter = 1e3)

################
#### SAR model
#################
W <- shape2mat(georgia, "W")
sar_list <- prep_sar_data(W)

# add data
sar_list$y <- log(georgia$income / 1e3)
sar_list$x <- scale(log(georgia$population / 1e3), center = TRUE, scale = FALSE)
sar_list$k <- ncol(sar_list$x)

# compile Stan model from file
sar_model_file <- "sar_model.stan"
sar_model <- stan_model(sar_model_file)

# sample from model
samples <- sampling(sar_model, data = sar_list, iter = 1e3)

# same as:
W <- shape2mat(georgia, "W")
fit <- stan_sar(log(income / 1e3) ~ log(population / 1e3), 
                data = georgia,
                C = W, 
                centerx = TRUE,
                iter = 1e3)

samples
fit
