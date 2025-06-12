## ----  include = FALSE--------------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>",
  fig.width = 7,
  fig.height = 5
  )

## ----setup--------------------------------------------------------------------
library(natstrat)
data('nh0506')

## ----stratification-----------------------------------------------------------
age_cat <- cut(nh0506$age, 
               breaks = c(19, 39, 50, 85), 
               labels = c('< 40 years', '40 - 50 years', '> 50 years'))
strata <- age_cat : nh0506$sex

## ----stratification hidden, echo = FALSE--------------------------------------
st_tab <- table(strata, nh0506$z)
st_ratios <- data.frame(Stratum = row.names(st_tab),
                        Control = st_tab[, 1],
                        Treated = st_tab[, 2],
                        Ratio = round(st_tab[, 1] / st_tab[, 2], 1),
                        row.names = NULL)
st_ratios[, c('Age', 'Sex')] <- stringr::str_split_fixed(st_ratios$Stratum, ':', 2)
DT::datatable(st_ratios[, c('Stratum', 'Age', 'Sex', 'Control', 'Treated', 'Ratio')], 
              options = list(paging = FALSE, searching = FALSE))


## ----generating constraints---------------------------------------------------
constraints <- generate_constraints(
  balance_formulas = list(age + race + education + povertyr + bmi ~ 1 + strata), 
  z = nh0506$z, 
  data = nh0506)

names(constraints)
dim(constraints$X)
length(constraints$importances)

## ----selecting controls-------------------------------------------------------
results <- optimize_controls(z = nh0506$z, 
                             X = constraints$X, 
                             importances = constraints$importances,
                             st = strata, 
                             ratio = 1)

names(results)

## ----check balance------------------------------------------------------------
cov_data <- nh0506[, c('sex', 'age', 'race', 'education', 'povertyr', 'bmi')]

stand_diffs <- check_balance(z = nh0506$z, 
                             X = cov_data, 
                             st = strata, 
                             selected = results$selected,
                             plot = TRUE)

## -----------------------------------------------------------------------------
stand_diffs$plot_pair

## -----------------------------------------------------------------------------
stand_diffs$plot_strata

## -----------------------------------------------------------------------------
nh0506$group <- factor(nh0506$z, levels = c(0, 1),
                          labels = c("Never smoker", "Daily smoker"))
nh0506$strata <- strata

ggplot(nh0506[results$selected, ], aes(x = group, y = log(homocysteine))) + 
  geom_boxplot() +
  facet_wrap(~ strata)

ggplot(nh0506[results$selected, ], aes(x = group, y = log(homocysteine))) + 
  geom_boxplot()

## -----------------------------------------------------------------------------

att <- mean(log(nh0506$homocysteine[results$selected & nh0506$z == 1])) - 
  mean(log(nh0506$homocysteine[results$selected & nh0506$z == 0]))
cat(paste0("\nThe ATT across the strata is ", round(att, 3), " on the log scale,
    which converted back to a regular scale is ", round(exp(att), 3), " umol/L."))

for (st in levels(nh0506$strata)) {
  att <- mean(log(nh0506$homocysteine[results$selected & nh0506$z == 1 & nh0506$strata == st])) -
    mean(log(nh0506$homocysteine[results$selected & nh0506$z == 0 & nh0506$strata == st]))
  cat(paste0("\nThe ATT for stratum ", st, " is ", round(att, 3), " on the log scale,
      which converted back to a regular scale is ", round(exp(att), 3), " umol/L."))
}

## -----------------------------------------------------------------------------
age_dist <- matrix(data = c(0, 1, 2, 1, 0, 1, 2, 1, 0),
                   nrow = 3, 
                   byrow = TRUE, 
                   dimnames = list(levels(age_cat), levels(age_cat)))
age_dist

sex_dist <- matrix(data = c(0, 1, 1, 0), 
                   nrow = 2, 
                   dimnames = list(levels(nh0506$sex), levels(nh0506$sex)))
sex_dist

strata_dist <- create_dist_matrix(age_dist, sex_dist)
strata_dist

## ---- echo = FALSE------------------------------------------------------------
round(2.5 * st_tab[, 2])

## -----------------------------------------------------------------------------
qs <- generate_qs(z = nh0506$z, 
                  st = strata, 
                  ratio = 2.5,
                  max_ratio = 2.6, 
                  max_extra_s = 0, 
                  strata_dist = strata_dist)
qs[1, ]

## -----------------------------------------------------------------------------
data('nh0506_3groups')
table(nh0506_3groups$z)

## -----------------------------------------------------------------------------
strata2 <- cut(nh0506_3groups$age, 
               breaks = c(19, 39, 50, 85), 
               labels = c('< 40 years', '40 - 50 years', '> 50 years'))

## ---- echo = FALSE------------------------------------------------------------
st_tab2 <- table(strata2, nh0506_3groups$z)
st_ratios2 <- data.frame(Stratum = row.names(st_tab2),
                        'Never smokers' = st_tab2[, 1],
                        'Some smoking' = st_tab2[, 2],
                        'Daily smokers' = st_tab2[, 3],
                        row.names = NULL)
DT::datatable(st_ratios2[, c('Stratum', 'Daily.smokers', 'Some.smoking', 'Never.smokers')],
              colnames = c('Stratum', 'Daily smokers', 'Some smoking', 'Never smokers'), 
              options = list(paging = FALSE, searching = FALSE))

## -----------------------------------------------------------------------------
constraints2 <- generate_constraints(
  balance_formulas = list(age + race + education + povertyr + bmi + sex ~ 1 + strata2), 
  z = nh0506_3groups$z, 
  data = nh0506_3groups,
  treated = 'daily smoker')

names(constraints2)
dim(constraints2$X)
length(constraints2$importances)

## -----------------------------------------------------------------------------
q_star_s <- matrix(c(rep(table(nh0506_3groups$z, strata2)['some smoking', ] - 
                           table(nh0506_3groups$z, strata2)['daily smoker', ], 2), 
                     rep(0, 3)), byrow = TRUE, nrow = 3, 
                   dimnames = list(levels(nh0506_3groups$z), levels(strata2)))
q_star_s

results <- optimize_controls(z = nh0506_3groups$z, 
                             X = constraints2$X, 
                             importances = constraints2$importances,
                             st = strata2, 
                             ratio = 1, 
                             treated = 'daily smoker', 
                             treated_star = 'some smoking',
                             q_star_s = q_star_s,
                             correct_sizes = FALSE)

