# print.survey_glm_fit() snapshot matches expected format

    Code
      print(fit)
    Output
      Survey-weighted GLM
      
      Family:  gaussian (identity link)
      Formula: y1 ~ y2 + y3
      Design:  Taylor series
      
      Coefficients:
      (Intercept)          y2          y3 
          51.1868     -0.0624     -2.6042 
      
      Degrees of freedom: 16 (design-based)

# print.survey_glm_summary() snapshot matches expected format

    Code
      print(summary(fit))
    Output
      Survey-weighted GLM
      
      Call:
      survey_glm(design = .glm_taylor(), formula = y1 ~ y2 + y3) 
      
      Deviance Residuals:
           Min       1Q   Median       3Q      Max 
      -93.3797 -18.7253  -0.4633  22.9540  75.4878 
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept) 51.1868   0.7268    70.4262  0.0000  ***
      y2          -0.0624   0.8308    -0.0752  0.9412     
      y3          -2.6042   1.4802    -1.7594  0.1003     
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for gaussian family taken to be 986.9)
      
          Null deviance: 197426 on 199 degrees of freedom
      Residual deviance: 194412 on 197 degrees of freedom
      AIC: 1492
      
      Design df: 16 (taylor)

# summary() with fit_ = NULL errors with surveycore_error_predict_no_fit

    Code
      summary(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# predict() with fit_ = NULL errors with surveycore_error_predict_no_fit

    Code
      predict(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# residuals(type = 'response') with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      residuals(fit_null, type = "response")
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# residuals(type = 'pearson') with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      residuals(fit_null, type = "pearson")
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# residuals(type = 'deviance') with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      residuals(fit_null, type = "deviance")
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# residuals(type = 'partial') with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      residuals(fit_null, type = "partial")
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# confint() with level outside (0,1) errors surveycore_error_invalid_conf_level

    Code
      confint(fit, level = 1.5)
    Condition
      Error in `confint()`:
      x `level` must be a single number strictly between 0 and 1. Got 1.5.

# terms() with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      terms(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# model.matrix() with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      model.matrix(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# model.frame() with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      model.frame(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# hatvalues() with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      hatvalues(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# logLik() with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      logLik(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# AIC() with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      AIC(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# BIC() with fit_ = NULL errors surveycore_error_predict_no_fit

    Code
      BIC(fit_null)
    Condition
      Error in `.glm_check_fit_()`:
      x The internal fit_ slot is NULL. This can happen after serialization.
      v Refit the model to restore prediction support.

# print(clean(fit)) snapshot matches expected format and class is correct

    Code
      print(result)
    Output
      # A <survey_glm_tidy> [2 × 11]
      # A tibble: 2 x 11
        term       variable var_label label reference_row estimate std_error statistic
      * <chr>      <chr>    <chr>     <chr> <lgl>            <dbl>     <dbl>     <dbl>
      1 (Intercep~ (Interc~ <NA>      (Int~ FALSE          5.04e+1     0.633   7.95e+1
      2 y2         y2       y2        y2    FALSE          4.67e-5     0.837   5.59e-5
      # i 3 more variables: p_value <dbl>, conf_low <dbl>, conf_high <dbl>

# confint() invalid level error references 'level' arg name

    Code
      confint(fit, level = 1.5)
    Condition
      Error in `confint()`:
      x `level` must be a single number strictly between 0 and 1. Got 1.5.

# update() with @call = NULL errors with surveycore_error_update_no_call

    Code
      update(fit_no_call)
    Condition
      Error in `update()`:
      x Cannot update <survey_glm_fit>: @call is NULL.
      v Refit the model to restore `update()` support.

