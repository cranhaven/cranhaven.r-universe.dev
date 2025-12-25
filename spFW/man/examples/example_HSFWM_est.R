library(spFW)

# load data
data(spFW_example_data)
Y <- spFW_example_data$yield
VAR <- spFW_example_data$geno
ENV <- spFW_example_data$loc
COOR <- spFW_example_data[,c(4,5)]

# run model
fit1 <- HSFWM_est(Y, VAR, ENV, COOR,
                  M_iter = 1000, burn_in = 500, thin = 5)

# plot estimated Y
plot(Y, fit1$yhat)


