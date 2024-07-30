library(netcox)

# weights of each group
eta_g <- rep(1, 5)
# grouping structure
grp <- matrix(c(0, 0, 0, 0, 0,
                0, 0, 0, 0, 0,
                1, 1, 0, 0, 0,
                0, 0, 0, 0, 0,
                0, 1, 0, 1, 0),
              ncol = 5, byrow = TRUE)
grp.var <- matrix(c(1, 0, 0, 0, 0, #A1
                    1, 0, 0, 0, 0, #A2
                    0, 0, 0, 1, 0, #C1
                    0, 0, 0, 1, 0, #C2
                    0, 1, 0, 0, 0, #B
                    0, 0, 1, 0, 0, #A1B
                    0, 0, 1, 0, 0, #A2B
                    0, 0, 0, 0, 1, #C1B
                    0, 0, 0, 0, 1  #C2B
), ncol = 5, byrow = TRUE)

x <- as.matrix(sim[, c("A1","A2","C1","C2","B","A1B","A2B","C1B","C2B")])
lam.seq <- 10^seq(0, -2, by = -0.2)

# fit
fit <- netcox(x = x,
              ID = sim$Id,
              time = sim$Start,
              time2 = sim$Stop,
              event = sim$Event,
              lambda = lam.seq,
              group = grp,
              group_variable = grp.var,
              penalty_weights = eta_g,
              tol = 1e-4,
              maxit = 1e3,
              verbose = FALSE)
plot_netcox_sp(fit)

# cv
cv <- netcox_cv(x = x,
                ID = sim$Id,
                time = sim$Start,
                time2 = sim$Stop,
                event = sim$Event,
                lambda = lam.seq,
                group = grp,
                group_variable = grp.var,
                penalty_weights = eta_g,
                nfolds = 5,
                tol = 1e-4,
                maxit = 1e3,
                verbose = FALSE)
plot_netcox_sp(cv, plot_min = TRUE, plot_1se = TRUE)
plot_netcox_cv(cv)
