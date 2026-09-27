# get_effective_n() EN-1: errors for non-survey object

    Code
      get_effective_n(list(x = 1))
    Condition
      Error in `.check_unsupported_class()`:
      x `get_effective_n()` requires a survey design object.
      i Got <list>.

# get_effective_n() EN-2: errors when method='deff' and x is NULL

    Code
      get_effective_n(d, method = "deff")
    Condition
      Error in `get_effective_n()`:
      x `x` is required when `method` = "\"deff\"".
      i Supply a numeric variable name, or switch to `method = "kish"` which needs no variable.

# get_effective_n() EN-3: errors when x resolves to multiple columns

    Code
      get_effective_n(d, starts_with("y"), method = "deff")
    Condition
      Error in `get_effective_n()`:
      x `x` must select exactly one variable, not 3.
      i Selected: y1, y2, and y3.
      v Pass one variable at a time, or loop over variables.

# get_effective_n() kish: print method snapshot (ungrouped)

    Code
      print(result)
    Output
      # A <survey_effective_n> [1 × 3]  method: kish
      # A <survey_effective_n> [1 × 3]
      # A tibble: 1 x 3
            n n_eff deff_kish
        <int> <dbl>     <dbl>
      1   200  190.      1.05

# get_effective_n() deff: print method snapshot (ungrouped)

    Code
      print(result)
    Output
      # A <survey_effective_n> [1 × 3]  method: deff  x: y1
      # A <survey_effective_n> [1 × 3]
      # A tibble: 1 x 3
            n n_eff  deff
        <int> <dbl> <dbl>
      1   200  179.  1.12

# get_effective_n() kish: print method snapshot (grouped)

    Code
      print(result)
    Output
      # A <survey_effective_n> [3 × 4]  method: kish
      # A <survey_effective_n> [3 × 4]
      # A tibble: 3 x 4
        group     n n_eff deff_kish
        <chr> <int> <dbl>     <dbl>
      1 A        63  56.5      1.11
      2 B        71  63.9      1.11
      3 C        66  58.9      1.12

# get_effective_n() deff: print method snapshot (grouped)

    Code
      print(result)
    Output
      # A <survey_effective_n> [3 × 4]  method: deff  x: y1
      # A <survey_effective_n> [3 × 4]
      # A tibble: 3 x 4
        group     n n_eff  deff
        <chr> <int> <dbl> <dbl>
      1 A        54  92.9 0.581
      2 B        71  79.3 0.896
      3 C        75  64.5 1.16 

