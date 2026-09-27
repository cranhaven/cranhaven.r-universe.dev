# get_t_test() print snapshot

    Code
      print(get_t_test(gss_design, age, by = sex, decimals = 2))
    Output
      # A survey_t_test result
      # Design: Taylor series | N: 3,197
      # DV: age of respondent | By: sex (Male vs. Female)
      # A <tbl_df> [1 × 13]
      # A tibble: 1 x 13
        level_a level_b estimate mean_a mean_b   n_a   n_b ci_low ci_high t_stat    df
        <chr>   <chr>      <dbl>  <dbl>  <dbl> <int> <int>  <dbl>   <dbl>  <dbl> <dbl>
      1 Male    Female      0.26   47.8   48.1  1424  1773  -1.65    2.17   0.27    66
      # i 2 more variables: p_value <dbl>, stars <chr>

# get_t_test() rejects non-survey-base objects

    Code
      get_t_test(df, x, by = by)
    Condition
      Error in `.check_unsupported_class()`:
      x `get_t_test()` requires a survey design object.
      i Got <data.frame>.

# get_t_test() rejects by with 3 active levels (T-1)

    Code
      get_t_test(d, outcome, by = grp3)
    Condition
      Error in `get_t_test()`:
      x `by` (grp3) must produce exactly 2 active groups, not 2. Filter the design to 2 groups with `surveytidy::filter()`, or use `get_pairwise()` for k > 2 groups.

# get_t_test() warns for character by (T-2)

    ! grp_chr coerced to factor. Level order: "A" and "B".

# get_t_test() warns for integer by (T-2)

    ! grp_int coerced to factor. Level order: "1" and "2".

# get_t_test() warns for logical by (T-2)

    ! grp_lgl coerced to factor. Level order: "FALSE" and "TRUE".

# get_t_test() rejects by with empty cell (T-3, no group)

    Code
      get_t_test(d, outcome_na, by = grp2)
    Condition
      Error in `get_t_test()`:
      x Level B of `by` (grp2) has no non-NA observations. Cannot compute t-test.

# get_t_test() rejects empty cell in group stratum (T-3g)

    Code
      get_t_test(d, outcome_partial, by = grp2, group = gender)
    Condition
      Error in `get_t_test()`:
      x In group stratum "gender = M", level B of `by` (grp2) has no non-NA observations.

# get_t_test() rejects non-numeric x

    Code
      get_t_test(d, grp3, by = grp2)
    Condition
      Error in `get_t_test()`:
      x `x` must be numeric, not <factor>. Column grp3 cannot be used with `get_t_test()`.

# get_t_test() rejects invalid conf_level

    Code
      get_t_test(d, outcome, by = grp2, conf_level = 1.5)
    Condition
      Error in `get_t_test()`:
      x `conf_level` must be a single number strictly between 0 and 1.
      i Got 1.5.

# get_t_test() rejects invalid variance arg

    Code
      get_t_test(d, outcome, by = grp2, variance = "sd")
    Condition
      Error in `get_t_test()`:
      x `variance` values must be from "se" or "ci".
      i Unknown value: "sd".

# get_t_test() rejects na.rm not logical

    Code
      get_t_test(d, outcome, by = grp2, na.rm = "yes")
    Condition
      Error in `get_t_test()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got a string.

# get_pairwise() print snapshot

    Code
      print(get_pairwise(gss_design, age, by = sex, pval_adj = "holm", decimals = 2))
    Output
      # A survey_pairwise result
      # Design: Taylor series | N: 3,197
      # DV: age of respondent | By: sex (2 levels, 1 pairs)
      # Adjustment: holm
      # A <tbl_df> [1 × 13]
      # A tibble: 1 x 13
        level_a level_b estimate mean_a mean_b   n_a   n_b ci_low ci_high t_stat    df
        <chr>   <chr>      <dbl>  <dbl>  <dbl> <int> <int>  <dbl>   <dbl>  <dbl> <dbl>
      1 Male    Female      0.26   47.8   48.1  1424  1773  -1.65    2.17   0.27    66
      # i 2 more variables: p_value <dbl>, stars <chr>

# get_pairwise() rejects by with only 1 active level (P-1)

    Code
      get_pairwise(d, outcome, by = grp1)
    Condition
      Error in `get_pairwise()`:
      x `by` (grp1) must have at least 2 active groups, not 1.

# get_pairwise() rejects invalid pval_adj

    Code
      get_pairwise(d, outcome, by = grp3, pval_adj = "invalid_method")
    Condition
      Error in `get_pairwise()`:
      x `pval_adj` must be a valid method for `stats::p.adjust()`.
      i Valid methods: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", or "none".
      i Got "invalid_method".

