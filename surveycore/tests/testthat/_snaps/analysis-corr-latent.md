# single-level ordinal emits PC-4

    Code
      get_corr(d, x = c(o1, o2), method = "polychoric")
    Condition
      Error in `.corr_estimate_thresholds()`:
      x Ordinal variable o1 has only 1 observed level in the active domain.
      i Threshold estimation requires at least 2 distinct levels.
      v Filter to a domain with more than one level of o1.

# integer with 15 distinct values + polyserial raises PC-3

    Code
      get_corr(d, x = c(x1, cont), method = "polyserial")
    Condition
      Error in `.corr_canonicalize_polyserial()`:
      x Cannot determine whether x1 is ordinal or continuous.
      i Integer vectors with more than 10 distinct values, and logical / character vectors, are ambiguous.
      v Coerce to <ordered> for ordinal or <double> for continuous before calling `get_corr()`.

# character column + method = 'polychoric' raises PC-1

    Code
      get_corr(d, x = c(o1, s1), method = "polychoric")
    Condition
      Error in `.corr_latent_pair()`:
      x `method = "polychoric"` requires ordinal variables. Non-ordinal column: s1 (<character>).
      v Coerce to <factor> or <ordered>, or use `method = "pearson"`.

# survey_collection with twophase member + polychoric raises PC-7 (dual)

    Code
      get_corr(coll, x = c(o1, o2), method = "polychoric")
    Condition
      Error in `.corr_latent_pair()`:
      x `method = "polychoric"` is not supported for <surveycore::survey_twophase> designs.
      v Use `method = "pearson"`, or call `get_corr()` on a <survey_taylor> or <survey_replicate> design.

# PC-13 (unordered factor) surfaces via the public API (dual)

    Code
      invisible(withCallingHandlers(get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_unordered_factor = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      }, warning = function(w) invokeRestart("muffleWarning")))
    Message
      ! Variable o1 is an unordered <factor>; using `levels()` order for thresholds.
      v Coerce to <ordered> to make the level order explicit.

# PC-2 (polyserial with two ordered factors) at public API (dual)

    Code
      get_corr(d, x = c(o1, o2), method = "polyserial")
    Condition
      Error in `.corr_canonicalize_polyserial()`:
      x `method = "polyserial"` requires exactly one ordinal and one continuous variable per pair.
      i For pair (o1, o2): classified as ("ordered", "ordered").
      v Supply one <factor>/<ordered> variable and one <numeric> variable, or use `method = "polychoric"` / `method = "pearson"`.

# PC-5 (polychoric insufficient cells) at public API (dual)

    Code
      get_corr(d, x = c(o1, o2), method = "polychoric")
    Condition
      Error in `.corr_polychoric_mle()`:
      x Pair (o1, o2) has only 3 non-empty cells in the active domain.
      i Polychoric MLE is not identified with fewer than 4 non-empty cells (surveycore guardrail).
      v Collapse levels or choose a different pair.

# PC-6 (polychoric optim_failed) at public API (dual, mocked)

    Code
      get_corr(d, x = c(o1, o2), method = "polychoric")
    Condition
      Error in `.corr_polychoric_mle()`:
      x Numerical optimization did not converge for pair (o1, o2).
      i Optimizer message: simulated failure.
      v Inspect the pair for extreme weight skew or sparse cells.

# PC-7 (twophase, single design) at public API (dual)

    Code
      get_corr(d_tp, x = c(o1, o2), method = "polychoric")
    Condition
      Error in `.corr_latent_pair()`:
      x `method = "polychoric"` is not supported for <surveycore::survey_twophase> designs.
      v Use `method = "pearson"`, or call `get_corr()` on a <survey_taylor> or <survey_replicate> design.

# PC-8 (> 20% replicate failure) at public API (dual)

    Code
      suppressWarnings(get_corr(d, x = c(o1, o2), method = "polychoric"))
    Condition
      Error in `.corr_replicate_variance_latent()`:
      x 29 of 30 replicate fits failed to converge (96.7%).
      i Threshold for hard failure is "20"% of replicates.
      v Inspect the design's replicate weights or reduce the number of ordinal levels.

# PC-9 (near-boundary rho, replicate path) at public API (dual)

    Code
      invisible(withCallingHandlers(get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_boundary_rho = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      }, warning = function(w) invokeRestart("muffleWarning")))
    Message
      ! Estimated correlation for pair (o1, o2) is within "1e-4" of the boundary (0.999917287383696).
      i Standard errors based on the delta method or Fisher-z linearization are unreliable near "-1" and "1".

# PC-10 (zero-count interior level) at public API (dual)

    Code
      invisible(withCallingHandlers(get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_zero_count_level = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      }, warning = function(w) invokeRestart("muffleWarning")))
    Message
      ! Ordinal variable o1 has zero-weight level "3" in the active domain.
      i Dropped level(s) removed; remaining levels renumbered before threshold estimation.

# PC-11 (polychoric sparse cell) at public API (dual, mocked)

    Code
      invisible(withCallingHandlers(get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_sparse_cell = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      }, warning = function(w) invokeRestart("muffleWarning")))
    Message
      ! 1 cell in pair (o1, o2) has modeled probability below "1e-12" at the MLE.
      i Log-likelihood was floored; estimate may be sensitive to small perturbations.

# PC-12 at public API populates meta()$n_failed_replicates_total

    Code
      invisible(withCallingHandlers(get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_replicate_convergence = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      }, warning = function(w) invokeRestart("muffleWarning")))
    Message
      ! 1 of 30 replicate fits failed to converge.
      i Variance is computed over the 29 successful replicate(s). See `meta(result)$n_failed_replicates_total`.

# PC-14 (Taylor near-boundary wide CI) at public API (dual)

    Code
      invisible(withCallingHandlers(get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_taylor_boundary_wide_ci = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      }, warning = function(w) invokeRestart("muffleWarning")))
    Message
      ! Taylor linearization for pair (o1, o2) is near the boundary (0.999917287383696); CI is structurally wide.
      i This is a known limitation of the delta-method / numerical influence-function approach (Mannan 2025).
      v Use a <survey_replicate> design for tighter inference near the boundary.

