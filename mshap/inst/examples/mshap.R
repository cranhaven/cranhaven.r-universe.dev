if (interactive()) {
  # Simulate shap values
  shap1 <- data.frame(
    age = runif(1000, -5, 5),
    income = runif(1000, -5, 5),
    married = runif(1000, -5, 5),
    sex = runif(1000, -5, 5)
  )
  shap2 <- list(
    data.frame(
      age = runif(1000, -5, 5),
      income = runif(1000, -5, 5),
      married = runif(1000, -5, 5),
      sex = runif(1000, -5, 5)
    ),
    data.frame(
      age = runif(1000, -5, 5),
      income = runif(1000, -5, 5),
      married = runif(1000, -5, 5),
      sex = runif(1000, -5, 5)
    ),
    data.frame(
      age = runif(1000, -5, 5),
      income = runif(1000, -5, 5),
      married = runif(1000, -5, 5),
      sex = runif(1000, -5, 5)
    )
  )
  
  ex1 <- 3
  ex2 <- c(4, 5, 6)
  
  # Case where both models have a single output
  res1 <- mshap(
    shap_1 = shap1,
    shap_2 = shap2[[1]],
    ex_1 = ex1,
    ex_2 = ex2[1]
  )
  View(res1$shap_vals)
  res1$expected_value
  
  # Case where one of your models has multiple outputs that are explained
  res2 <- mshap(
    shap_1 = shap1,
    shap_2 = shap2,
    ex_1 = ex1,
    ex_2 = ex2
  )
  View(res2[[1]]$shap_vals)
  res2[[1]]$expected_value
  
  # Case where the models have different variables
  res3 <- mshap(
    shap_1 = shap1,
    shap_2 = shap2,
    ex_1 = ex1,
    ex_2 = ex2,
    shap_1_names = c("Age", "Income", "Married", "Sex"),
    shap_2_names = c("Age", "Income", "Children", "American")
  )
  # Note how there are now 6 columns of SHAP values, since there are 6
  # distinct variables
  View(res3[[1]]$shap_vals)
  res3[[1]]$expected_value
}