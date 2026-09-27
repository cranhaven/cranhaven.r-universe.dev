# .twophase_mean() errors for method = "full" with no phase 2 design info

    Code
      .twophase_mean(d, "y1")
    Condition
      Error in `.twophasevar()`:
      x Two-phase variance method "full" requires phase 2 design structure.
      i No `ids2`, `strata2`, or `probs2` were specified in `as_survey_twophase()`.
      v Reconstruct with `method = "approx"` or supply phase 2 design variables.

