# whatifbandit
<!-- badges: start -->
[![R-CMD-check](https://github.com/Noch05/whatifbandit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Noch05/whatifbandit/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Noch05/whatifbandit/branch/main/graph/badge.svg?token=B51SYMH66I)](https://app.codecov.io/gh/Noch05/whatifbandit)
<!-- badges: end -->


## Overview
`{whatifbandit}` is a package designed to answer: "*What if my experiment was a bandit trial?*"

Traditional RCTs assign participants to treatments with fixed, equal probabilities for the duration
of the trial. However, oftentimes researchers have more complex goals such as testing
many treatment conditions, satisfying a fixed budget, and deploying the most effective treatments
more frequently to optimize effects in the real world, which cannot be accommodated
effectively by RCTs.

Response-adaptive designs address these concerns by updating assignment probabilities as the trial
progresses using the observed outcomes; the most effective treatments can be prioritized. This
maximizes real-world benefits from trials, and potentially increases the power of pairwise tests of
treatment effects, if the original RCT was underpowered. Multi-Arm Bandits provide a framework and
set of algorithms for optimally choosing the adaptive probabilities, balancing the difficult task of
exploring all the treatments, and exploiting the best ones.

`{whatifbandit}` lets researchers explore this alternative experimental design, helping them
determine if it fits for their next field experiment, in two different ways:

-  **Resimulation**: Take data from a trial that has already happened, and resimulate it as if an
 adaptive bandit-based design had been used instead. New outcomes are imputed using the original
 observed results to reconstruct a plausible adaptive* what-if*.
-  **Simulate**: Simulate a bandit trial from scratch, using a set of user-provided population
 parameters. This is useful for designing a future adaptive trial, running power analysis, or
 exploring the behavior of each bandit algorithm before collecting any data.

These ideas are what give the package its name, whatifbandit. bandit for Multi-Arm Bandit, and whatif for the central question
that the package answers.

## Features
Whatifbandit provides robust customization options to match as many experimental designs as possible, but it is only
equipped to handle experiments where success is binary. Functionality for other cases may be introduced in future development.
Some of our major features are:

- 2 bandit algorithms: probability matching via Thompson sampling, and UCB1.
- Variable length and flexible assignment periods, such as individual, batch, and date-based.
- A configurable lookback window (`prior_periods`) and discounting (`discount_rate`) for how much weight past periods carry in each new assignment decision.
- Simulation of perfect and imperfect (`delayed feedback`) information during re-assignment.
- Block randomized and cluster randomized designs.
- Control augmentation and random assignment proportions, to enforce a minimum level of exploration in bandit algorithms.
- Joint hypothesis testing (bootstrap or randomization inference) for whether any treatment effects exist at all.
- Arbitrary pairwise contrasts (vs. control, vs. the best arm, or all pairwise comparisons) between treatment arms.

Additionally, whatifbandit supports parallel processing over multiple simulations
via [future](https://future.futureverse.org/), large data support through
[data.table](https://rdatatable.gitlab.io/data.table/).

## Estimation

Estimation of conditional expectations and treatment effects is powered by the Adaptively Weighted
Augmented Inverse Probability Weighted estimation (AW-AIPW) from [Hadad et.
al(2021)](https://pubmed.ncbi.nlm.nih.gov/33876748/). This estimator is adaptive robust, correcting
for the violations in traditional estimators caused by adaptive probabilities of assignment.

## Installation
```r
# Install the latest stable version from GitHub
remotes::install_github("Noch05/whatifbandit@v1.0.2")

# Install the latest CRAN version (likely behind GitHub)
install.packages("whatifbandit")
```
## Data
Examples below use the `tanf` dataset, bundled with the package containing anonymized
recertification records from [Moore et. al
(2022)](https://www.cambridge.org/core/journals/journal-of-public-policy/article/abs/anchor-management-a-field-experiment-to-encourage-families-to-meet-critical-programme-deadlines/A74D5BF7AF8107861E24D9A420CC026C)


## Usage

### RCT Resimulation

#### Running 1 Trial
```r
sim <- mab_from_rct(
 success ~ condition,
  data = tanf,
  algorithm = "ucb1",
  period_method = "batch",
  period_length = 1000,
  whole_experiment = TRUE
)
```
#### Running multiple trials.
```r
# Setting seed for Reproducible RNG
set.seed(532543)
simulations <- mab_from_rct(
 success ~ condition + block(service_center),
  data = tanf,
  algorithm = "thompson",
  period_method = "date",
  time_unit = "month",
  period_length = 1,
  delayed_feedback = TRUE,
  assignment_date_col = letter_sent_date,
  success_date_col = date_of_recert,
  date_col = appt_date,
  month_col = recert_month,
  whole_experiment = FALSE,
  keep_data = TRUE,
  r = 100
)
```
#### Running in Multiple Trials in Parallel
```r
library(future)

# Set any arbitrary plan
future::plan(plan, workers = availableCores())

set.seed(532543)
simulations <- mab_from_rct(
 success ~ condition + block(service_center),
  data = tanf,
  algorithm = "thompson",
  period_method = "date",
  time_unit = "month",
  period_length = 1,
  delayed_feedback = TRUE,
  assignment_date_col = letter_sent_date,
  success_date_col = date_of_recert,
  date_col = appt_date,
  month_col = recert_month,
  whole_experiment = FALSE,
  keep_data = TRUE,
  r = 100
)
future::plan(sequential)
```
### Simulating a New Adaptive Trial

```r
# Assumed true success probabilities for 3 treatment arms
p <- matrix(c(0.20, 0.35, 0.5), ncol = 1, dimnames = list(c("control", "treatment1", "treatment2"), NULL))

set.seed(123)
sim_from_scratch <- simulate_mab(
  n = 2000,
  t = 20,
  p = p,
  algorithm = "thompson",
  random_assign_prop = 0.3,
)
```

## More Information
For more complete information about the package details, please refer to the full documentation.

If you have any specific questions about the package, feel free to send me an email at <no9857a@american.edu>, and if you encounter
any bugs, please create an issue on [GitHub](https://github.com/Noch05/whatifbandit/issues) with a reproducible example.
