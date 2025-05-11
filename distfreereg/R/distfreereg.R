distfreereg <-
  function(test_mean,
           ordering = "simplex",
           group = FALSE,
           stat = c("KS", "CvM"),
           B = 1e4,
           control = NULL,
           override = NULL,
           verbose = TRUE,
           ...){
    UseMethod("distfreereg")
  }
