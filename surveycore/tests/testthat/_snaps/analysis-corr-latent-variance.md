# .corr_replicate_variance_latent() emits PC-12 when 0 < n_failed <= 0.2*R

    Code
      invisible(.corr_replicate_variance_latent(d, method = "polychoric", vec_a = d@
        data$o1, vec_b = d@data$o2, active_domain = active, rho_hat_full = fit$rho))
    Condition
      Warning:
      ! 1 of 30 replicate fits failed to converge.
      i Variance is computed over the 29 successful replicate(s). See `meta(result)$n_failed_replicates_total`.

# .corr_replicate_variance_latent() raises PC-8 when failure > 20%

    Code
      .corr_replicate_variance_latent(d, method = "polychoric", vec_a = d@data$o1,
      vec_b = d@data$o2, active_domain = active, rho_hat_full = fit$rho)
    Condition
      Error in `.corr_replicate_variance_latent()`:
      x 29 of 30 replicate fits failed to converge (96.7%).
      i Threshold for hard failure is "20"% of replicates.
      v Inspect the design's replicate weights or reduce the number of ordinal levels.

# .corr_latent_pair() raises PC-7 on survey_twophase before any MLE work

    Code
      .corr_latent_pair(d, "o1", "o2", method = "polychoric")
    Condition
      Error in `.corr_latent_pair()`:
      x `method = "polychoric"` is not supported for <surveycore::survey_twophase> designs.
      v Use `method = "pearson"`, or call `get_corr()` on a <survey_taylor> or <survey_replicate> design.

# .corr_latent_pair() raises PC-1 under polychoric with a numeric column

    Code
      .corr_latent_pair(d, "o1", "num", method = "polychoric")
    Condition
      Error in `.corr_latent_pair()`:
      x `method = "polychoric"` requires ordinal variables. Non-ordinal column: num (<numeric>).
      v Coerce to <factor> or <ordered>, or use `method = "pearson"`.

# .corr_latent_pair() emits PC-13 for an unordered factor input

    Code
      invisible(withCallingHandlers(.corr_latent_pair(d, "o1", "o2", method = "polychoric"),
      surveycore_warning_polychoric_unordered_factor = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      }, warning = function(w) invokeRestart("muffleWarning")))
    Message
      ! Variable o1 is an unordered <factor>; using `levels()` order for thresholds.
      v Coerce to <ordered> to make the level order explicit.

