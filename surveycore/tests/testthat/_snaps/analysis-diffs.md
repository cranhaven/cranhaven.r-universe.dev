# get_diffs() rejects non-survey-base objects

    Code
      get_diffs(df, dv, treats)
    Condition
      Error in `.check_unsupported_class()`:
      x `get_diffs()` requires a survey design object.
      i Got <data.frame>.

# get_diffs() rejects x resolving to non-existent column

    Code
      get_diffs(d, nonexistent_col, treats)
    Condition
      Error in `get_diffs()`:
      x `x` must select exactly one column.
      i Column nonexistent_col not found in design data.

# get_diffs() rejects treats resolving to non-existent column

    Code
      get_diffs(d, dv, nonexistent_arm)
    Condition
      Error in `get_diffs()`:
      x `treats` must select exactly one column.
      i Column nonexistent_arm not found in design data.

# get_diffs() rejects non-numeric x

    Code
      get_diffs(d, group, treats)
    Condition
      Error in `get_diffs()`:
      x `x` must be numeric. group is <character>.

# get_diffs() rejects treats with only 1 level

    Code
      get_diffs(d, dv, single)
    Condition
      Error in `get_diffs()`:
      x `treats` must have at least 2 levels. single has only 1.

# get_diffs() rejects invalid ref_level

    Code
      get_diffs(d, dv, treats, ref_level = "nonexistent")
    Condition
      Error in `get_diffs()`:
      x `ref_level` "nonexistent" not found in levels of treats.
      i Available levels: "A", "B", or "Control".

# get_diffs() rejects invalid pval_adj

    Code
      get_diffs(d, dv, treats, pval_adj = "invalid_method")
    Condition
      Error in `get_diffs()`:
      x `pval_adj` must be a valid method for `stats::p.adjust()`.
      i Valid methods: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", or "none".

# get_diffs() rejects non-character covariates

    Code
      get_diffs(d, dv, treats, covariates = 42)
    Condition
      Error in `get_diffs()`:
      x `covariates` must be a character vector of model terms.

# get_diffs() print snapshot

    Code
      print(result)
    Output
      # A survey_diffs result
      # Design: Taylor series | Family: gaussian (identity)
      # DV: dv | Treatment: treats (ref: Control)
      # Method: coefficient / intercept
      # A <survey_diffs> [3 × 8]
      # A tibble: 3 x 8
        treats  estimate  mean     n ci_low ci_high p_value stars
        <fct>      <dbl> <dbl> <int>  <dbl>   <dbl>   <dbl> <chr>
      1 Control    0      50.0    65  NA      NA     NA     ""   
      2 A          0.214  50.2    58  -3.29    3.72   0.898 ""   
      3 B         -0.543  49.4    77  -3.95    2.86   0.737 ""   

# get_diffs() rejects alpha > 1

    Code
      get_diffs(d, dv, treats, alpha = 1.5)
    Condition
      Error in `get_diffs()`:
      x `alpha` must be a single numeric value strictly between 0 and 1. Got 1.5.

# get_diffs() rejects alpha = 0

    Code
      get_diffs(d, dv, treats, alpha = 0)
    Condition
      Error in `get_diffs()`:
      x `alpha` must be a single numeric value strictly between 0 and 1. Got 0.

# get_diffs() rejects alpha = '0.05' (character)

    Code
      get_diffs(d, dv, treats, alpha = "0.05")
    Condition
      Error in `get_diffs()`:
      x `alpha` must be a single numeric value strictly between 0 and 1. Got "0.05".

# get_diffs() rejects alpha = NA

    Code
      get_diffs(d, dv, treats, alpha = NA)
    Condition
      Error in `get_diffs()`:
      x `alpha` must be a single numeric value strictly between 0 and 1. Got NA.

# get_diffs() rejects alpha = Inf

    Code
      get_diffs(d, dv, treats, alpha = Inf)
    Condition
      Error in `get_diffs()`:
      x `alpha` must be a single numeric value strictly between 0 and 1. Got Inf.

# get_diffs() rejects alpha = c(0.05, 0.1) (length > 1)

    Code
      get_diffs(d, dv, treats, alpha = c(0.05, 0.1))
    Condition
      Error in `get_diffs()`:
      x `alpha` must be a single numeric value strictly between 0 and 1. Got 0.05 and 0.1.

