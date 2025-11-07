values <- data.frame(
  age = runif(1000, 18, 60),
  income = runif(1000, 50000, 150000),
  married = as.numeric(runif(1000, 0, 1) > 0.5),
  sex = as.numeric(runif(1000, 0, 1) > 0.5)
)
shap <- data.frame(
  age = runif(1000, -5, 5),
  income = runif(1000, -5, 5),
  married = runif(1000, -5, 5),
  sex = runif(1000, -5, 5)
)

ex <- 3


p1 <- summary_plot(variable_values = values, shap_values = shap)
p2 <- observation_plot(
  variable_values = values[1,],
  shap_values = shap[1,],
  expected_value = ex
)

test_that("Summary Plot Returns ggplot object", {
  expect_type(p1, "list")
  expect_equal(class(p1), c("gg", "ggplot"))
})

test_that("Observation Plot Returns ggplot object", {
  expect_type(p2, "list")
  expect_equal(class(p2), c("gg", "ggplot"))
})

p1_all <- summary_plot(
  variable_values = values, 
  shap_values = shap,
  names = c("Fake Name", "Fake Name 2", "Fake Name 3", "Fake Name 4"),
  colorscale = c("red", "white", "blue"),
  legend.position = "top",
  font_family = NULL,
  title = "Custom Title"
)
p2_all <- observation_plot(
  variable_values = values[1,],
  shap_values = shap[1,],
  expected_value = ex,
  names = c("Fake Name", "Fake Name 2", "Fake Name 3", "Fake Name 4"),
  fill_colors = c("purple", "yellow"),
  connect_color = "red",
  expected_color = "black",
  predicted_color = "blue",
  title = "Custom Title"
)

test_that("Summary Plot Works with all arguments", {
  expect_type(p1_all, "list")
  expect_equal(class(p1_all), c("gg", "ggplot"))
})

test_that("Observation Plot Works with all arguments", {
  expect_type(p2_all, "list")
  expect_equal(class(p2_all), c("gg", "ggplot"))
})


