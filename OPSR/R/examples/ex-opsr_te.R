sim_dat <- opsr_simulate()
dat <- sim_dat$data
weights <- runif(nrow(dat))
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat = dat, weights = weights,
            printLevel = 0)
te <- opsr_te(fit, type = "response")
print(te)
summary(te)

te_w <- opsr_te(fit, type = "response", weights = rep(1, nrow(dat)))
summary(te_w)

pairs(te)
