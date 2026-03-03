library(targets)

list(
  tar_target(
    data,
    iris[iris$Species != "setosa", ]
  ),
  tar_target(
    model,
    lm(Sepal.Width ~ Sepal.Length, data)
  )
)
