### R code from vignette source 'em_intro.Rnw'

###################################################
### code chunk number 1: em_intro.Rnw:47-53
###################################################
suppressWarnings(RNGversion("3.5.0"))
set.seed(1504)
options(width = 70, prompt = "R> ", continue = "+  ", useFancyQuotes = FALSE)
grDevices::ps.options(family = "Times")
library("graphics")
library("em")


###################################################
### code chunk number 2: em_intro.Rnw:147-152
###################################################
library("em")
fit.lm <- lm(yn ~ x, data = simreg)
summary(fit.lm)
results <- em(fit.lm, latent = 2, verbose = F)
summary(results)


###################################################
### code chunk number 3: em_intro.Rnw:158-162
###################################################
fmm_fit <- predict(results)
str(fmm_fit)
fmm_fit_post <- predict(results, prob = "posterior")
str(fmm_fit_post)


###################################################
### code chunk number 4: em_intro.Rnw:184-185
###################################################
plot(results)


###################################################
### code chunk number 5: em_intro.Rnw:194-195
###################################################
plot(results, by = "prob")


###################################################
### code chunk number 6: em_intro.Rnw:209-214
###################################################
formula <- yc ~ x
formula_c <- ~z
lm_fit <- lm(formula, data = simreg)
results <- em(lm_fit, concomitant = list(formula = formula_c, data = simreg))
summary(results)


###################################################
### code chunk number 7: em_intro.Rnw:231-237 (eval = FALSE)
###################################################
## library("em")
## library("survival")
## fmla <- chosen2 ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3 + strata(id)
## cfit <- clogit(fmla, simclogit)
## emfit <- em(cfit, latent = 2, verbose = F, init.method = "kmeans", use.optim = T, optim.start = "sample5", max_iter = 100)
## print(summary(emfit))


