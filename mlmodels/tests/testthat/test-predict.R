# tests/testthat/test-beta.R
library(testthat)
library(marginaleffects)

data("docvis")
data("pw401k")
data("mroz")
data("smoke")

mroz$incthou <- mroz$faminc / 1000 # Scaling for linear models.
smoke$smokes <- smoke$cigs > 0     # Binary for logit / probit


# -- 1. Linear and Loglinear Model (ml_lm) -------------------------------------
test_that("All predictions types for ml_lm work", {
  
  lm_hom <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  lm_het <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
                  scale = ~ educ + exper,
                  data = mroz)
  ln_hom <- ml_lm(log(incthou) ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  ln_het <- ml_lm(log(incthou) ~ age + I(age^2) + huswage + educ + unem,
                  scale = ~ educ + exper,
                  data = mroz)
  
  valid_types <- c("response", "mean", "mu", "median", "fitted",
                   "sigma", "sd", "variance",
                   "sigma_y", "sd_y", "var", "var_y", "variance_y",
                   "link", "zd")
  
  models <- list(
    "Homoskedastic Linear" = lm_hom,
    "Heteroskedastic Linear" = lm_het,
    "Homoskedastic Lognormal" = ln_hom,
    "Heteroskedastic Lognormal" = ln_het
    # add your other models here: logit, probit, poisson, nb1, nb2, gamma, beta, etc.
  )
  
  for(model_name in names(models))
  {
    mod <- models[[model_name]]
    for(typ in valid_types)
    {
      pred <- predict(mod, type = typ, se.fit = TRUE)
      
      expect_type(pred, "list")
      expect_true("fit" %in% names(pred))
      expect_type(pred$fit, "double")
      
      
      # For variance-related types, values should be non-negative
      if (typ %in% c("variance", "var", "var_y", "variance_y")) {
        expect_true(all(pred$fit >= 0, na.rm = TRUE))
      }
      
      expect_true(all(pred$se.fit >= 0, na.rm = TRUE))
    }
  }
})

# -- 2. Logit (ml_logit) -------------------------------------------------------
test_that("All predictions types for ml_logit work", {
  
  suppressMessages({
    log_hom <- ml_logit(smokes ~ cigpric + income + age, data = smoke)
    log_het <- ml_logit(smokes ~ cigpric + income + age,
                        scale = ~ educ,
                        data = smoke)
  })
  
  valid_types <- c("response", "prob", "prob0", "link", "odds", "fitted",
                   "sigma", "variance", "zd", "xb")
  
  var_types <- c("sigma", "variance")
  het_types <- c("zd", "sigma", "variance")
  
  models <- list(
    "Homoskedastic Logit" = log_hom,
    "Heteroskedastic Logit" = log_het
  )
  
  for(model_name in names(models))
  {
    mod <- models[[model_name]]
    for(typ in valid_types)
    {
      suppressWarnings(
        pred <- predict(mod, type = typ, se.fit = TRUE)
      )
      expect_type(pred, "list")
      expect_true("fit" %in% names(pred))
      expect_type(pred$fit, "double")
      
      if(typ %in% var_types)
        expect_true(all(pred$fit >= 0, na.rm = TRUE))
      
      if(model_name == "Homoskedastic Logit" && typ %in% het_types)
        expect_all_true(is.na(pred$se.fit))
      else 
        expect_all_true(pred$se.fit >= 0)
    }
  }
})

# -- 3. Probit (ml_probit) -----------------------------------------------------
test_that("All predictions types for ml_probit work", {
  
  suppressMessages({
    prob_hom <- ml_probit(smokes ~ cigpric + income + age, data = smoke)
    prob_het <- ml_probit(smokes ~ cigpric + income + age,
                          scale = ~ educ,
                          data = smoke)
  })
  
  valid_types <- c("response", "prob", "prob0", "link", "odds", "fitted",
                   "sigma", "variance", "zd", "xb")
  
  var_types <- c("sigma", "variance")
  het_types <- c("zd", "sigma", "variance")
  
  models <- list(
    "Homoskedastic Probit" = prob_hom,
    "Heteroskedastic Probit" = prob_het
  )
  
  for(model_name in names(models))
  {
    mod <- models[[model_name]]
    for(typ in valid_types)
    {
      suppressWarnings(
        pred <- predict(mod, type = typ, se.fit = TRUE)
      )
      expect_type(pred, "list")
      expect_true("fit" %in% names(pred))
      expect_type(pred$fit, "double")
      
      if(typ %in% var_types)
        expect_true(all(pred$fit >= 0, na.rm = TRUE))
      
      if(model_name == "Homoskedastic Probit" && typ %in% het_types)
        expect_all_true(is.na(pred$se.fit))
      else 
        expect_all_true(pred$se.fit >= 0)
    }
  }
})

# -- 4. Negative Binomial ------------------------------------------------------
test_that("All predictions types for ml_nebin work", {
  
  dispersions <- c("NB1", "NB2")
  
  general_types <- c("response", "fitted", "mean", "fitted", "link", "zd",
                     "alpha", "variance", "var", "sd", "sigma")
  
  prob_types <- c("P(3)", "P(1,4)", "P(,4)", "P(1,)")
  
  valid_types <- c(general_types, prob_types)
  
  for(dis in dispersions)
  {
    suppressMessages({
      nb_hom <- ml_negbin(docvis ~ private + medicaid + age + I(age^2) + educyr +
                            actlim + totchr, dispersion = dis, data = docvis)
      nb_het <- ml_negbin(docvis ~ private + medicaid + age + I(age^2) + educyr +
                            actlim + totchr, 
                          scale =  ~ female + bh,
                          dispersion = dis, data = docvis)
    })
    
    models <- list(
      "Homoskedastic Negbin" = nb_hom,
      "Heteroskedastic Negbin" = nb_het
    )
    
    for(model_name in names(models))
    {
      mod <- models[[model_name]]
      for(typ in valid_types)
      {
        suppressWarnings(
          pred <- predict(mod, type = typ, se.fit = TRUE)
        )
        expect_type(pred, "list")
        expect_true("fit" %in% names(pred))
        expect_type(pred$fit, "double")
        
        if(!(typ %in% c("link", "zd")))
          expect_true(all(pred$fit >= 0, na.rm = TRUE))
        
        expect_all_true(pred$se.fit >= 0)
      }
    }
  }
})

# -- 5. Poisson ----------------------------------------------------------------
test_that("All predictions types for ml_poisson work", {
  
  general_types <- c("response", "fitted", "mean", "link")
  
  prob_types <- c("P(3)", "P(1,4)", "P(,4)", "P(1,)")
  
  valid_types <- c(general_types, prob_types)
  
  suppressMessages({
    pois <- ml_poisson(docvis ~ private + medicaid + age + I(age^2) + educyr +
                         actlim + totchr, data = docvis)
  })
  
  for(typ in valid_types)
  {
    suppressWarnings(
      pred <- predict(pois, type = typ, se.fit = TRUE)
    )
    expect_type(pred, "list")
    expect_true("fit" %in% names(pred))
    expect_type(pred$fit, "double")
    
    if(!(typ %in% c("link")))
      expect_true(all(pred$fit >= 0, na.rm = TRUE))
    
    expect_all_true(pred$se.fit >= 0)
  }
})

# -- 6. Gamma ------------------------------------------------------------------
test_that("All predictions types for ml_gamma work", {
  
  suppressMessages({
    gam_hom <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                        data = mroz)
    
    gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                        scale = ~ kidslt6,
                        data = mroz)
  })
  
  valid_types <- c("response", "fitted", "mean", "link", "zd", "nu", "variance", "var", "sd", "sigma")
  
  models <- list(
    "Homoskedastic Gamma" = gam_hom,
    "Heteroskedastic Gamma" = gam_het
  )
  
  for(model_name in names(models))
  {
    mod <- models[[model_name]]
    for(typ in valid_types)
    {
      suppressWarnings(
        pred <- predict(mod, type = typ, se.fit = TRUE)
      )
      expect_type(pred, "list")
      expect_true("fit" %in% names(pred))
      expect_type(pred$fit, "double")
      
      if(!(typ %in% c("link", "zd")))
        expect_true(all(pred$fit >= 0, na.rm = TRUE))
      
      expect_all_true(pred$se.fit >= 0)
    }
  }
})

# -- 7. Beta -------------------------------------------------------------------
test_that("All predictions types for ml_beta work", {
  
  suppressMessages({
    beta_hom <- ml_beta(prate ~ mrate + I(mrate^2) + log(totemp) + I(log(totemp)^2) +
                          age + I(age^2) + sole, data = pw401k,
                        subset = prate < 1)
    beta_het <- ml_beta(prate ~ mrate + I(mrate^2) + log(totemp) + I(log(totemp)^2) +
                          age + I(age^2) + sole, data = pw401k,
                        scale = ~ totemp + sole,
                        subset = prate < 1)
  })
  
  valid_types <- c("response", "fitted", "mean", "odds", "link", "zd", "phi", "shape1", "shape2", "mode", "variance", "var", "sd", "sigma")
  
  models <- list(
    "Homoskedastic Beta" = beta_hom,
    "Heteroskedastic Beta" = beta_het
  )
  
  for(model_name in names(models))
  {
    mod <- models[[model_name]]
    for(typ in valid_types)
    {
      # out of sample predictions to have values in all observations.
      suppressWarnings(
        pred <- predict(mod, type = typ, se.fit = TRUE, newdata = pw401k)
      )
      expect_type(pred, "list")
      expect_true("fit" %in% names(pred))
      expect_type(pred$fit, "double")
      
      if(!(typ %in% c("link", "zd")))
        expect_true(all(pred$fit >= 0, na.rm = TRUE))
      
      # Mode may produce proper NAs, so for it we remove them. For the rest all
      # have to be positive.
      if(typ != "mode")
        expect_all_true(pred$se.fit >= 0)
      else
        expect_true(all(pred$se.fit >= 0, na.rm = TRUE))
    }
  }
})