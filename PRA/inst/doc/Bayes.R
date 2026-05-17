## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(42)

## ----setup--------------------------------------------------------------------
library(PRA)

## -----------------------------------------------------------------------------
cause_probs <- c(0.3, 0.2)
risks_given_causes <- c(0.8, 0.6)
risks_given_not_causes <- c(0.2, 0.4)

## ----results='asis'-----------------------------------------------------------
prior_risk <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)
cat("Prior probability of risk event R:", round(prior_risk, 3), "\n")

## -----------------------------------------------------------------------------
risk_probs <- c(0.3, 0.5, 0.2)
means_given_risks <- c(10000, 15000, 5000)
sds_given_risks <- c(2000, 1000, 1000)
base_cost <- 2000

## -----------------------------------------------------------------------------
prior_samples <- cost_pdf(
  num_sims          = 5000,
  risk_probs        = risk_probs,
  means_given_risks = means_given_risks,
  sds_given_risks   = sds_given_risks,
  base_cost         = base_cost
)

## -----------------------------------------------------------------------------
# C1 observed as present; C2 not yet assessed
observed_causes <- c(1, NA)

## ----results='asis'-----------------------------------------------------------
posterior_risk <- risk_post_prob(
  cause_probs, risks_given_causes,
  risks_given_not_causes, observed_causes
)
cat("Posterior probability of risk event R:", round(posterior_risk, 3), "\n")

## -----------------------------------------------------------------------------
prob_data <- data.frame(
  Stage       = c("Prior", "Posterior"),
  Probability = c(prior_risk, posterior_risk)
)

p <- ggplot2::ggplot(prob_data, ggplot2::aes(x = Stage, y = Probability, fill = Stage)) +
  ggplot2::geom_col(width = 0.5, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = round(Probability, 3)),
    vjust = -0.4, size = 4.5
  ) +
  ggplot2::scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "tomato")) +
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2::labs(
    title = "Bayesian Update: Risk Probability",
    x     = NULL,
    y     = "P(Risk Event R)"
  ) +
  ggplot2::theme_minimal(base_size = 13)

print(p)

## -----------------------------------------------------------------------------
observed_risks <- c(1, NA, 1) # Risk 1 and 3 confirmed; Risk 2 not yet assessed

## -----------------------------------------------------------------------------
posterior_samples <- cost_post_pdf(
  num_sims          = 5000,
  observed_risks    = observed_risks,
  means_given_risks = means_given_risks,
  sds_given_risks   = sds_given_risks,
  base_cost         = base_cost
)

## -----------------------------------------------------------------------------
xlim_range <- range(c(prior_samples, posterior_samples))

# Prior cost histogram
hist(prior_samples,
  breaks = 40, freq = FALSE,
  col = rgb(0.27, 0.51, 0.71, 0.5), # steelblue, semi-transparent
  border = "white",
  xlim = xlim_range,
  main = "Prior vs. Posterior Cost Distribution",
  xlab = "Total Cost ($)",
  ylab = "Density"
)

# Posterior cost histogram (overlaid)
hist(posterior_samples,
  breaks = 40, freq = FALSE,
  col = rgb(0.84, 0.24, 0.31, 0.5), # tomato, semi-transparent
  border = "white",
  add = TRUE
)

abline(v = mean(prior_samples), col = "steelblue", lty = 2, lwd = 2)
abline(v = mean(posterior_samples), col = "tomato", lty = 2, lwd = 2)

legend("topright",
  legend = c(
    paste0("Prior  (mean = $", format(round(mean(prior_samples)), big.mark = ",")),
    paste0("Posterior (mean = $", format(round(mean(posterior_samples)), big.mark = ","))
  ),
  fill = c(rgb(0.27, 0.51, 0.71, 0.5), rgb(0.84, 0.24, 0.31, 0.5)),
  bty = "n"
)

