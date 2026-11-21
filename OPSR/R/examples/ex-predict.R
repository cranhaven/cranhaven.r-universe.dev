set.seed(123)
sim_dat <- opsr_simulate()
dat <- sim_dat$data
model <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(model, dat)
p <- predict(fit, group = 1, type = "response")

fit_log <- update(fit, . | log(yo) ~ .)
p_unlog <- predict(fit, group = 1, type = "unlog-response")

## newdata
newdat <- dat[1:4, ]
unique(newdat$ys)  # available selection outcomes
predict(fit, newdata = newdat, group = 2)

