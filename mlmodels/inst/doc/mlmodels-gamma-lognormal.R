## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
suppressMessages({
  library(mlmodels)
  library(dplyr)
  library(e1071)
  library(ggplot2)
  library(marginaleffects)
  library(patchwork)
})

## ----density-box, fig.width=10, fig.height=8, dpi=72--------------------------
data(mroz)

# Scale faminc into thousands for smaller values.
mroz$incthou <- mroz$faminc / 1000

# Use ggplot to create both plots

p_density <- ggplot(mroz, aes(x = incthou)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Family Income ($`000s)", x = NULL, y = "Density") +
  theme_minimal(base_size = 15)

p_box <- ggplot(mroz, aes(x = incthou)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, width = 0.4, outlier.shape = 21) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 15)

# Use patchwork to put them together, one on top of the other.
p_density / p_box + plot_layout(heights = c(2, 1))


## ----sumstats-----------------------------------------------------------------
# Store the density to get the mode.
d <- density(mroz$incthou, na.rm = TRUE)

mroz |> 
  summarise(
    Min = min(incthou),
    Q1 = quantile(incthou, probs = 0.25),
    Median = median(incthou),
    Q3 = quantile(incthou, probs = 0.75),
    Max = max(incthou),
    Mean = mean(incthou),
    SD = sd(incthou),
    Mode = d$x[which.max(d$y)],
    Skewness = skewness(incthou, type = 3) # Alternative moments::skewness
  )

## ----estimation---------------------------------------------------------------
# Gamma model
fit_gamma <- ml_gamma(incthou ~ age + I(age^2) + huswage + educ + unem + kidslt6,
                      data = mroz)

# Lognormal model
fit_lognormal <- ml_lm(log(incthou) ~ age + I(age^2) + huswage + educ + unem + kidslt6,
                       data = mroz)

## ----estimates----------------------------------------------------------------
summary(fit_gamma, vcov.type = "robust")

summary(fit_lognormal, vcov.type = "robust")

## ----rsq-ln-------------------------------------------------------------------
# predict the response
resp_ln <- predict(fit_lognormal)

r2_original <- cor(mroz$incthou, resp_ln$fit, use = "complete.obs")^2
round(r2_original, 4)

## ----vuong--------------------------------------------------------------------
vuongtest(fit_gamma, fit_lognormal)

## ----grid---------------------------------------------------------------------
new_data <- datagrid(model = fit_gamma,           # use any model to get the structure
  age = seq(30, 60, length.out = 100),
  FUN = mean)

## ----pred-mean----------------------------------------------------------------
mean_ln <- predictions(fit_lognormal, newdata = new_data, vcov = "robust")
mean_gam <- predictions(fit_gamma, newdata = new_data, vcov = "robust")

## ----pred-var-----------------------------------------------------------------
var_ln <- predictions(fit_lognormal, type = "var_y", newdata = new_data, vcov = "robust")
var_gam <- predictions(fit_gamma, type = "var", newdata = new_data, vcov = "robust")

## ----plot-mean, fig.width=10, fig.height=8, dpi=72----------------------------
means <- bind_rows(
  mean_gam |> mutate(Model = "Gamma"),
  mean_ln  |> mutate(Model = "Lognormal")
)

ggplot(means, aes(x = age, y = estimate, color = Model, fill = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Predicted Mean Family Income",
       subtitle = "Gamma vs Lognormal models",
       x = "Age", 
       y = "Expected Family Income ($ '000s)",
       color = "",
       fill = "") +
  theme_minimal(base_size = 15) + 
  theme(legend.position = "bottom")

## ----plot-var, fig.width=10, fig.height=8, dpi=72-----------------------------
vars <- bind_rows(
  var_gam |> mutate(Model = "Gamma"),
  var_ln  |> mutate(Model = "Lognormal")
)

ggplot(vars, aes(x = age, y = estimate, color = Model, fill = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Predicted Family Income Variance",
       subtitle = "Gamma vs Lognormal models",
       x = "Age", 
       y = "Family Income Variance",
       color = "",
       fill = "") +
  theme_minimal(base_size = 15) + 
  theme(legend.position = "bottom")

