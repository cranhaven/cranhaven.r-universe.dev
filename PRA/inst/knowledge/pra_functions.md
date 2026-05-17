# PRA Package Function Reference

## Monte Carlo Simulation

### mcs(num_sims, task_dists, cor_mat = NULL)
Run a Monte Carlo simulation for total project cost or duration.

**Parameters:**
- `num_sims`: Integer. Number of simulations (typically 10000).
- `task_dists`: List of lists. Each inner list has a `type` field ("normal", "triangular", or "uniform") and distribution parameters:
  - normal: `mean`, `sd`
  - triangular: `a` (min), `b` (max), `c` (mode)
  - uniform: `min`, `max`
- `cor_mat`: Optional numeric matrix. Correlation matrix (dimensions must match number of tasks).

**Returns:** List with `total_mean`, `total_variance`, `total_sd`, `percentiles` (5%, 50%, 95%), `total_distribution` (numeric vector).

**Example task_dists:**
```
list(
  list(type = "normal", mean = 10, sd = 2),
  list(type = "triangular", a = 5, b = 10, c = 15),
  list(type = "uniform", min = 8, max = 12)
)
```

### smm(mean, var, cor_mat = NULL)
Second Moment Method for quick analytical estimate of total project mean and variance.

**Parameters:**
- `mean`: Numeric vector of task means.
- `var`: Numeric vector of task variances.
- `cor_mat`: Optional correlation matrix.

**Returns:** List with `total_mean`, `total_var`, `total_std`.

## Earned Value Management

### pv(bac, schedule, time_period)
Planned Value. `bac` = total budget, `schedule` = vector of cumulative planned completion percentages, `time_period` = current period (integer). Returns numeric scalar.

### ev(bac, actual_per_complete)
Earned Value. `bac` = total budget, `actual_per_complete` = actual percent complete (0-1). Returns numeric scalar.

### ac(actual_costs, time_period, cumulative = TRUE)
Actual Cost. `actual_costs` = vector of costs per period, `time_period` = current period. Returns numeric scalar.

### sv(ev, pv)
Schedule Variance = EV - PV. Returns numeric scalar.

### cv(ev, ac)
Cost Variance = EV - AC. Returns numeric scalar.

### spi(ev, pv)
Schedule Performance Index = EV / PV. Returns numeric scalar.

### cpi(ev, ac)
Cost Performance Index = EV / AC. Returns numeric scalar.

### eac(bac, method = "typical", cpi = NULL, ac = NULL, ev = NULL, spi = NULL)
Estimate at Completion. Methods: "typical" (BAC/CPI), "atypical" (AC + BAC - EV), "combined" (AC + (BAC-EV)/(CPI*SPI)). Returns numeric scalar.

### etc(bac, ev, cpi = NULL)
Estimate to Complete. Returns numeric scalar.

### vac(bac, eac)
Variance at Completion = BAC - EAC. Returns numeric scalar.

### tcpi(bac, ev, ac, target = "bac", eac = NULL)
To-Complete Performance Index. `target` = "bac" or "eac". Returns numeric scalar.

## Post-Simulation Analysis

### contingency(sims, phigh = 0.95, pbase = 0.50)
Calculate contingency reserve. `sims` = mcs result object. Returns numeric scalar (P_high - P_base).

### sensitivity(task_dists, cor_mat = NULL)
Compute variance contribution of each task. Same input format as `mcs()`. Returns numeric vector (one value per task).

## Bayesian Risk Analysis

### risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)
Prior probability of risk event from root causes. All vectors must be same length, values between 0 and 1. Returns numeric scalar.

### risk_post_prob(cause_probs, risks_given_causes, risks_given_not_causes, observed_causes)
Posterior risk probability after observing causes. `observed_causes` = logical vector (TRUE/FALSE/NA). Returns numeric scalar.

### cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost = 0)
Simulate prior cost distribution. Returns numeric vector of length num_sims.

### cost_post_pdf(num_sims, observed_risks, means_given_risks, sds_given_risks, base_cost = 0)
Simulate posterior cost distribution. `observed_risks` = logical vector (TRUE/FALSE/NA). Returns numeric vector.

## Learning Curves

### fit_sigmoidal(data, x_col, y_col, model_type)
Fit a sigmoidal model. `data` = data.frame, `x_col`/`y_col` = column name strings, `model_type` = "pearl", "gompertz", or "logistic". Returns nls model object.

### predict_sigmoidal(fit, x_range, model_type, conf_level = NULL)
Predict from fitted model. `fit` = nls object from fit_sigmoidal, `x_range` = numeric vector of x values. Returns data.frame with columns: x, pred, and optionally lwr, upr.

### plot_sigmoidal(fit, data, x_col, y_col, model_type, conf_level = NULL)
Plot fitted curve with data. Creates a plot as a side effect. Returns data.frame invisibly.

## Dependency Structure Matrices

### parent_dsm(S)
Resource-task dependency matrix. `S` = numeric matrix (resources x tasks). Returns S * t(S).

### grandparent_dsm(S, R)
Risk-based task dependency matrix. `S` = resource-task matrix, `R` = risk-resource matrix. Returns (S * t(R)) * t(S * t(R)).

## Correlation

### cor_matrix(num_samples = 100, num_vars = 5, dists)
Generate a correlation matrix from samples. `dists` = list of distribution functions. Returns numeric correlation matrix.

## Tool Chaining Patterns

Common multi-step workflows:
1. **Schedule/Cost Risk**: mcs() -> contingency() + sensitivity()
2. **EVM Analysis**: pv() + ev() + ac() -> sv() + cv() -> spi() + cpi() -> eac() -> etc() + vac()
3. **Bayesian Prior->Posterior**: risk_prob() -> cost_pdf() -> [observe] -> risk_post_prob() -> cost_post_pdf()
4. **Learning Curve**: fit_sigmoidal() -> predict_sigmoidal() -> plot_sigmoidal()
