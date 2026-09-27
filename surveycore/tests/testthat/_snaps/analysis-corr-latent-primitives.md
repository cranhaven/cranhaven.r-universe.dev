# .corr_canonicalize_polyserial() raises PC-2 for two ordered factors

    Code
      .corr_canonicalize_polyserial("a", "b", d)
    Condition
      Error in `.corr_canonicalize_polyserial()`:
      x `method = "polyserial"` requires exactly one ordinal and one continuous variable per pair.
      i For pair (a, b): classified as ("ordered", "ordered").
      v Supply one <factor>/<ordered> variable and one <numeric> variable, or use `method = "polychoric"` / `method = "pearson"`.

# .corr_canonicalize_polyserial() raises PC-3 for high-cardinality integer vs ordered

    Code
      .corr_canonicalize_polyserial("a", "b", d)
    Condition
      Error in `.corr_canonicalize_polyserial()`:
      x Cannot determine whether a is ordinal or continuous.
      i Integer vectors with more than 10 distinct values, and logical / character vectors, are ambiguous.
      v Coerce to <ordered> for ordinal or <double> for continuous before calling `get_corr()`.

# .corr_estimate_thresholds() raises PC-4 with only one level

    Code
      .corr_estimate_thresholds(x, w, dom, var_name = "my_var")
    Condition
      Error in `.corr_estimate_thresholds()`:
      x Ordinal variable my_var has only 1 observed level in the active domain.
      i Threshold estimation requires at least 2 distinct levels.
      v Filter to a domain with more than one level of my_var.

# .corr_polychoric_mle() raises PC-5 when n_cells_obs < 4

    Code
      .corr_polychoric_mle(ord_x_vec = factor(d$x, ordered = TRUE), ord_y_vec = factor(
        d$y, ordered = TRUE), weights = w, active_domain = dom, x_name = "xvar",
      y_name = "yvar")
    Condition
      Error in `.corr_polychoric_mle()`:
      x Pair (xvar, yvar) has only 3 non-empty cells in the active domain.
      i Polychoric MLE is not identified with fewer than 4 non-empty cells (surveycore guardrail).
      v Collapse levels or choose a different pair.

# .corr_polychoric_mle() raises PC-6 via probe-failure sentinel

    Code
      .corr_polychoric_mle(ord_x_vec = factor(d$x, ordered = TRUE), ord_y_vec = factor(
        d$y, ordered = TRUE), weights = w, active_domain = dom, eps = 1.5, x_name = "xvar",
      y_name = "yvar")
    Condition
      Error in `.corr_polychoric_mle()`:
      x Numerical optimization did not converge for pair (xvar, yvar).
      i Optimizer message: "stats::optimize() returned a non-finite result".
      v Inspect the pair for extreme weight skew, sparse cells, or degenerate ordinal coding.

# .corr_polyserial_mle() raises PC-6 via stats::optimize() failure path

    Code
      .corr_polyserial_mle(ordinal_vec = factor(d$ord, ordered = TRUE),
      continuous_vec = d$cont, weights = w, active_domain = dom, eps = 1.5, ord_name = "ord",
      cont_name = "cont")
    Condition
      Error in `.corr_polyserial_mle()`:
      x Numerical optimization did not converge for pair (ord, cont).
      i Optimizer message: "stats::optimize() returned a non-finite result".
      v Inspect the pair for extreme weight skew, sparse cells, or degenerate ordinal coding.

