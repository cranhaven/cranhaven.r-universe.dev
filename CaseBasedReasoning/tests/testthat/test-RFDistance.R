testthat::context("Proximity & Depth Distance")

testthat::test_that("Terminal Nodes", {
  set.seed((1234))
  rf <- ranger::ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
  # we use different indices in our calculations
  tn1 <- terminalNodes(iris[, -5], rf) - 1
  tn2 <- predict(rf, iris[, -5], type = "terminalNodes")
  testthat::expect_equal(tn1, tn2$predictions)
})


testthat::test_that("Number of Edges between Terminal Nodes", {
  df = tibble::tibble(
    class = as.factor(c(rep(0, 100), rep(1, 100), rep(2, 100))),
    x1 = c(rnorm(100, 0, .1), rnorm(100, 10, .1), rnorm(100, 20, .1)),
    x2 = c(rnorm(100, 10, .1), rnorm(100, 0, .1), rnorm(100, 20, .1))
  )
  
  set.seed((1234))
  rf_fit <- ranger::ranger(class~.,data=df, num.trees=1, mtry=2, min.node.size=0)
  m_forest <- ranger_forests_to_matrix(rf_fit)
  
  # the number of edges can be calculated by looking at the splits
  # 1. terminal node 4 and 5 are neighbours -> number of edges = 2
  # 2. terminal node 2 has the same distance from terminal node 4 and 5; one up and two down -> number of edges = 3
  n_edges_by_hand <- tibble::tibble(
    x = c(2, 2, 4),
    y = c(4, 5, 5),
    tree_1 = c(3, 3, 2)
  )
  
  n_edges_calculated <- edges_between_terminal_nodes(rf_fit)
  testthat::expect_equal(n_edges_by_hand, n_edges_calculated)
})


testthat::test_that("Perfect Separation Test", {
  df = tibble::tibble(
    class = as.factor(c(rep(0, 100), rep(1, 100), rep(2, 100))),
    x1 = c(rnorm(100, 0, .1), rnorm(100, 10, .1), rnorm(100, 20, .1)),
    x2 = c(rnorm(100, 10, .1), rnorm(100, 0, .1), rnorm(100, 20, .1))
  )
  
  set.seed((1234))
  rf_fit <- ranger::ranger(class~.,data=df, num.trees=1, mtry=2, min.node.size=0)
  
  # Proximity
  d <- distanceRandomForest(x = df[, -1], rfObject = rf_fit)
  testthat::expect_equal(sum(diag(table(cutree(hclust(d), k=3), df$class))), 300, info = 'Proximity Distance')
  
  # Depth
  d <- distanceRandomForest(x = df[, -1], rfObject = rf_fit, method = 'Depth')
  testthat::expect_equal(sum(diag(table(cutree(hclust(d), k=3), df$class))), 300, info = 'Depth Distance')
})