### R code from vignette source 'opsr.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library("OPSR")
library("MASS")
library("texreg")
library("gridExtra")
library("gridGraphics")
library("scales")
library("sampleSelection")
library("mvtnorm")

set.seed(0)

options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE,
        digits = 3)


###################################################
### code chunk number 2: sim-dat
###################################################
sim_dat <- opsr_simulate(seed = 0)
dat <- sim_dat$data
head(dat)


###################################################
### code chunk number 3: opsr
###################################################
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, data = dat,
  printLevel = 0)


###################################################
### code chunk number 4: summary-xinyi
###################################################
summary(fit)


###################################################
### code chunk number 5: null-model
###################################################
fit_null <- opsr_null_model(fit, printLevel = 0)


###################################################
### code chunk number 6: update
###################################################
fit_intercept <- update(fit, . ~ . | 1)


###################################################
### code chunk number 7: anova
###################################################
anova(fit_null, fit_intercept, fit)


###################################################
### code chunk number 8: texreg
###################################################
texreg::screenreg(list(fit_null, fit_intercept, fit),
  include.pseudoR2 = TRUE, include.R2 = TRUE, single.row = TRUE)


###################################################
### code chunk number 9: predict
###################################################
p1 <- predict(fit, group = 1, type = "response")
p2 <- predict(fit, group = 1, counterfact = 2, type = "response")


###################################################
### code chunk number 10: opsr-te
###################################################
print(opsr_te(fit, type = "response"))


###################################################
### code chunk number 11: telework-data
###################################################
data("telework_data", package = "OPSR")


###################################################
### code chunk number 12: hidden
###################################################
start <- c(
  1.2, 2.4,  # kappa 1 & 2
  0.2, 0.4, 0.1, 0.3, 0.3, 0.2, 0.1, 0.1, -0.1, 0.1, 0.1, 0.3, 0.1, 0.1,  # selection
  3.744, -0.208, 0.010, 0.000, -0.392, -0.019, 0.130, 0.010, 0.415, 0.494, 0.437, 0.186, 0.124, -0.240,  # outcome 1
  2.420, 0.224, 0.670, 0.445, 0.219, 0.824, 0.704, 0.164, -0.176, 0.171,  # outcome 2
  2.355, -0.375, 0.476, 0.317, 0.187, 0.290, 0.313, 0.856, 0.248, -0.275,  # outcome 3
  1.193, 1.248, 1.413,  # sigma
  0.068, 0.128, 0.340  # rho
)


###################################################
### code chunk number 13: formula-xinyi
###################################################
f <-
  twing_status | vmd_ln ~
  edu_2 + edu_3 + hhincome_2 + hhincome_3 + flex_work + work_fulltime +
  twing_feasibility + att_proactivemode + att_procarowning + att_wif +
  att_proteamwork + att_tw_effective_teamwork + att_tw_enthusiasm +
  att_tw_location_flex |
  female + age_mean + age_mean_sq + race_black + race_other + vehicle +
  suburban + smalltown + rural + work_fulltime + att_prolargehouse +
  att_procarowning + region_waa |
  edu_2 + edu_3 + suburban + smalltown + rural + work_fulltime +
  att_prolargehouse + att_proactivemode + att_procarowning |
  female + hhincome_2 + hhincome_3 + child + suburban + smalltown +
  rural + att_procarowning + region_waa


###################################################
### code chunk number 14: model-xinyi
###################################################
start_default <- opsr(f, telework_data, .get2step = TRUE)
fit <- opsr(f, telework_data, start = start, method = "NM", iterlim = 50e3,
  printLevel = 0)


###################################################
### code chunk number 15: hidden
###################################################
custom.model.names <- c("Structural", "Selection", "NTWer (535)", "NUTWer (322)", "UTWer (727)")
custom.coef.names <- c(
  "Kappa 1",
  "Kappa 2",
  "Sigma 1",
  "Sigma 2",
  "Sigma 3",
  "Rho 1",
  "Rho 2",
  "Rho 3",
  "Some college",
  "Bachelor's degree or higher",
  "\\$50,000 to \\$99,999",
  "\\$100,000 or more",
  "Flexible work schedule",
  "Full time worker",
  "Teleworking feasibility",
  "Pro-active-mode",
  "Pro-car-owning",
  "Work interferes with family",
  "Pro-teamwork",
  "TW effective teamwork",
  "TW enthusiasm",
  "TW location flexibility",
  "Intercept",
  "Female",
  "Age",
  "Age squared",
  "Black",
  "Other races",
  "Number of vehicles",
  "Suburban",
  "Small town",
  "Rural",
  "Pro-large-house",
  "Region indicator (WAA)",
  "Number of children"
)
groups <- list(
  "Education (ref: high school or less)" = 9:10,
  "Household income (ref: less than \\$50,000)" = 11:12,
  "Attitudes" = 16:22,
  "Race (ref: white)" = 27:28,
  "Residential location (ref: urban)" = 30:32
)
custom.note <- "%stars. We used robust standard errors in this replica, which may result in slight differences from the original standard errors."


###################################################
### code chunk number 16: replica
###################################################
texreg::texreg(
  fit, beside = TRUE, include.R2 = TRUE, include.pseudoR2 = TRUE,
  custom.model.names = custom.model.names, custom.coef.names = custom.coef.names,
  groups = groups, scalebox = 0.76, booktabs = TRUE, dcolumn = TRUE,
  no.margin = TRUE, use.packages = FALSE, float.pos = "htbp", single.row = TRUE,
  caption = "Replica of \\citet{Wang+Mokhtarian:2024}, Table 3.",
  label = "tab:wang-replica",
  custom.note = custom.note
)


###################################################
### code chunk number 17: replica-te
###################################################
te <- opsr_te(fit, type = "unlog-response", weights = telework_data$weight)
print(te)


###################################################
### code chunk number 18: hidden
###################################################
ste <- summary(te)


###################################################
### code chunk number 19: tobit-5-data
###################################################
set.seed(0)
vc <- diag(3)
vc[lower.tri(vc)] <- c(0.9, 0.5, 0.1)
vc[upper.tri(vc)] <- vc[lower.tri(vc)]
eps <- rmvnorm(500, c(0, 0, 0), vc)
xs <- runif(500)
ys <- xs + eps[, 1] > 0
xo1 <- runif(500)
yo1 <- xo1 + eps[, 2]
xo2 <- runif(500)
yo2 <- xo2 + eps[, 3]
yo <- ifelse(ys, yo2, yo1)
ys <- as.numeric(ys) + 1
dat <- data.frame(ys, yo, yo1, yo2, xs, xo1, xo2)
head(dat)


###################################################
### code chunk number 20: tobit-5-ss
###################################################
tobit5_s <- selection(ys ~ xs, list(yo1 ~ xo1, yo2 ~ xo2), data = dat)
summary(tobit5_s)


###################################################
### code chunk number 21: tobit-5-opsr
###################################################
tobit5_o <- opsr(ys | yo ~ xs | xo1 | xo2, data = dat, printLevel = 0)
summary(tobit5_o)


###################################################
### code chunk number 22: timeuse-data
###################################################
data("timeuse_data", package = "OPSR")


###################################################
### code chunk number 23: boxplot
###################################################
plot.it <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(1, 2))
  plot(log_weekly_km ~ factor(wfh), data = timeuse_data, varwidth = TRUE,
       ylab = "Log weekly distance traveled (km)", xlab = "Telework status",
       names = c("NTWers", "NUTWers", "UTWers"), main = "Weekly distance traveled",
       col = "white")
  plot(log_commute_km ~ factor(wfh), data = timeuse_data, varwidth = TRUE,
       ylab = "Log one-way commute distance (km)", xlab = "Telework status",
       names = c("NTWers", "NUTWers", "UTWers"), main = "Commute distance",
       col = "white")
}

plot.it()


###################################################
### code chunk number 24: fit-polr
###################################################
drop <- c("id", "weekly_km", "log_weekly_km", "commute_km", "log_commute_km",
  "wfh_days")
dat_polr <- subset(timeuse_data, select = !(names(timeuse_data) %in% drop))
dat_polr$wfh <- factor(dat_polr$wfh)
fit_polr <- MASS::polr(wfh ~ ., dat_polr, method = "probit")


###################################################
### code chunk number 25: step
###################################################
fit_step <- MASS::stepAIC(fit_polr, trace = FALSE)
fit_step$anova


###################################################
### code chunk number 26: hidden
###################################################
f_full <- wfh | log_weekly_km ~
  age + educ_higher + hh_income + young_kids + workload + fixed_workplace +
  shift_work + permanent_employed + isco_craft + isco_tech + isco_clerical +
  isco_elementary + car_access + parking_home + freq_onl_order +
  grocery_shopper |
  sex_male + age + educ_higher + swiss + married + res_loc + dogs + hh_size +
  young_kids + n_children + workload + fixed_workplace + permanent_employed +
  driverlicense + car_access + parking_home + parking_work + rents_home +
  freq_onl_order + vacation + grocery_shopper |
  sex_male + age + educ_higher + swiss + married + res_loc + dogs + hh_size +
  young_kids + n_children + workload + fixed_workplace + permanent_employed +
  driverlicense + car_access + parking_home + parking_work + rents_home +
  freq_onl_order + vacation + grocery_shopper |
  sex_male + age + educ_higher + swiss + married + res_loc + dogs + hh_size +
  young_kids + n_children + workload + fixed_workplace + permanent_employed +
  driverlicense + car_access + parking_work + rents_home +
  freq_onl_order + vacation + grocery_shopper


###################################################
### code chunk number 27: full-reduced
###################################################
fit_full <- opsr(f_full, timeuse_data, printLevel = 0)
f_red <- wfh | log_weekly_km ~
  age + educ_higher + hh_income + young_kids + workload + fixed_workplace +
  shift_work + permanent_employed + isco_craft + isco_tech + isco_clerical +
  isco_elementary + car_access + parking_home + freq_onl_order +
  grocery_shopper |
  sex_male + res_loc + workload + permanent_employed + parking_work |
  swiss + res_loc + young_kids + workload + parking_work |
  sex_male + swiss + fixed_workplace + permanent_employed + parking_work

fit_red <- opsr(f_red, timeuse_data, printLevel = 0)
print(anova(fit_red, fit_full), print.formula = FALSE)
summary(fit_red)


###################################################
### code chunk number 28: utils
###################################################
te <- function(fit) {
  te <- summary(opsr_te(fit, type = "unlog-response"))$te
  colnames(te) <- c("NTWers", "NUTWers", "UTWers")
  rownames(te) <- c("NTWing->NUTWing", "NTWing->UTWing", "NUTWing->UTWing")
  te
}

te(fit_red)


###################################################
### code chunk number 29: hidden
###################################################
te.fr <- unclass(te(fit_red))


###################################################
### code chunk number 30: hidden
###################################################
plot.it <- function() {
  x <- aggregate(wfh_days ~ wfh, data = timeuse_data, FUN = mean)$wfh_days
  tw_status <- c("NTWing", "NUTWing", "UTWing")
  xlabs <- paste0(tw_status, "\n(", round(x, 2), " d/week)")
  cond_exp <- opsr_te(fit_red, type = "unlog-response")$ce.by.groups
  mat <- sapply(cond_exp, function(x) {
    apply(x, 2, mean, na.rm = TRUE)
  })
  matplot(x = x, mat, type = "b", xlab = "Telework treatment", ylab = "Weekly distance (km)",
          axes = FALSE, pch = 19, lty = 1, lwd = 2.5, col = c(1, 3, 4))
  axis(1, at = x, labels = xlabs, padj = 0.5)
  axis(2)
  grid()
  legend("topright", legend = c("NTWers", "NUTWers", "UTWers"), pch = 19, col = c(1, 3, 4), lwd = 2.5, bty = "n")
  box()
}
plot.it()


###################################################
### code chunk number 31: pairs
###################################################
plot(fit_red, type = "unlog-response", col = c(1, 3, 4),
  labels.diag = c("NTWing", "NUTWing", "UTWing"),
  labels.reg = c("NTWers", "NUTWers", "UTWers"),
  xlim = c(0, 400), ylim = c(0, 400), cex = 1.5)


###################################################
### code chunk number 32: no-cor
###################################################
start <- coef(fit_red)
fixed <- c("rho1", "rho2", "rho3")
start[fixed] <- 0
fit_nocor <- opsr(f_red, timeuse_data, start = start, fixed = fixed,
  printLevel = 0)


###################################################
### code chunk number 33: opsr.Rnw:767-769
###################################################
te(fit_red)
te(fit_nocor)


###################################################
### code chunk number 34: unit-treatment-effects
###################################################
dat_ute <- subset(timeuse_data, select = c(commute_km, wfh, wfh_days))
dat_ute <- aggregate(cbind(wfh_days, 2 * commute_km) ~ wfh, data = dat_ute,
  FUN = mean)

top <- t(dat_ute[2:3])
colnames(top) <- c("NTWers", "NUTWers", "UTWers")
rownames(top) <- c("WFH (days)", "2-way commute (km)")

i <- "WFH (days)"
twdiff1 <- top[i, "NUTWers"] - top[i, "NTWers"]
twdiff2 <- top[i, "UTWers"] - top[i, "NTWers"]
twdiff3 <- top[i, "UTWers"] - top[i, "NUTWers"]

twdiff <- matrix(c(rep(twdiff1, 3), rep(twdiff2, 3), rep(twdiff3, 3)), nrow = 3)
bottom <- te(fit_red) / twdiff

ute <- rbind(top, bottom)
ute


