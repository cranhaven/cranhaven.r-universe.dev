# print.survey_diffs() renders expected header

    Code
      print(mock_diffs)
    Output
      # A survey_diffs result
      # Design: Taylor series | Family: gaussian (identity)
      # DV: agree_trope | Treatment: message_arm (ref: Control)
      # Method: coefficient / intercept
      # A <survey_diffs> [3 × 8]
      # A tibble: 3 x 8
        message_arm estimate  mean     n ci_low ci_high p_value stars
        <fct>          <dbl> <dbl> <int>  <dbl>   <dbl>   <dbl> <chr>
      1 Control        0     0.401   752 NA      NA      NA     ""   
      2 Msg A          0.082 0.483   748  0.042   0.122   0.001 "**" 
      3 Msg B          0.103 0.504   751  0.063   0.143   0     "***"

# .extract_clean_estimates() missing intercept error has correct snapshot

    Code
      .extract_clean_estimates(fit_noint, "treats2", 0.95)
    Condition
      Error in `.extract_clean_estimates()`:
      x Reference row not found in model output. Expected exactly one intercept row.

