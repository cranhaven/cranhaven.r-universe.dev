# mutate() warns surveytidy_warning_mutate_weight_col when a weight column is modified

    Code
      invisible(mutate(d, wt = wt * 1.1))
    Condition
      Warning:
      ! mutate() modified weight column wt.
      i Effective sample size may be affected.
      v Use `update_design()` to intentionally change design variables.

