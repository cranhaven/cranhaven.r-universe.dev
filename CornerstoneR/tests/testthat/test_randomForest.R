context("Random Forest")

test_that("Classification", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(brush.pred = FALSE, use.rows = "all", num.trees = 500
                    , importance.mode = "permutation", respect.unordered.factors = "ignore"
                    )
  
  # create CS-R output
  createCSEnvir(iris, strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  # run randomForest
  expect_true(randomForest(num.threads = 2))
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(res$statistics[1:8, 2]
               , data.table(Value = c("Classification", "500", "150", "4", "2", "1", "permutation", "gini"))
               )
  expect_numeric(as.double(tail(res$statistics$Value, 2)), lower = 0, any.missing = FALSE)
  expect_data_table(res$importances, any.missing = FALSE, nrows = 4, ncols = 2)
  expect_numeric(res$importances$Importance, lower = 0, upper = 0.5)
  expect_set_equal(res$importances$Variable, c("Petal.Width", "Petal.Length", "Sepal.Length", "Sepal.Width"))
  expect_equal(res$predictions$Species, iris$Species)
  expect_equal(sum(res$predictions$Used.Species), 150)
  expect_equal(sum(res$predictions$Resid.Species), 0)
  expect_equal(res$confusions$Species$N, c(rep(50, 3), rep(0, 6)))
  
  # different script variables
  set.seed(1619)
  createCSEnvir(iris[, c(1:3, 5)], blnBrush = sample(c(!logical(100), logical(50)))
                , strPreds = colnames(iris)[1:3], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = FALSE, use.rows = "non-brushed", num.trees = 200
                                       , importance.mode = scriptvars$importance.mode
                                       , respect.unordered.factors = scriptvars$respect.unordered.factors
                                       )
                , env = env
                )
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(res$statistics[1:8, 2]
               , data.table(Value = c("Classification", "200", "50", "3", "1", "1", "permutation", "gini"))
               )
  expect_numeric(as.double(tail(res$statistics$Value, 2)), lower = 0, any.missing = FALSE)
  
  set.seed(1619)
  createCSEnvir(iris[, c(1:3, 5)], blnBrush = sample(c(!logical(100), logical(50)))
                , strPreds = colnames(iris)[1:3], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = FALSE, use.rows = "brushed", num.trees = 200
                                       , importance.mode = scriptvars$importance.mode
                                       , respect.unordered.factors = scriptvars$respect.unordered.factors
                                       )
                , env = env
                )
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(res$statistics[1:8, 2]
               , data.table(Value = c("Classification", "200", "100", "3", "1", "1", "permutation", "gini"))
               )
  expect_numeric(as.double(tail(res$statistics$Value, 2)), lower = 0, any.missing = FALSE)
  
  set.seed(1619)
  createCSEnvir(iris[, c(1:3, 5)], blnBrush = sample(c(!logical(100), logical(50)))
                , strPreds = colnames(iris)[1:3], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = TRUE, use.rows = "brushed", num.trees = 200
                                       , importance.mode = scriptvars$importance.mode
                                       , respect.unordered.factors = scriptvars$respect.unordered.factors
                                       )
                , env = env
                )
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(res$statistics[1:8, 2]
               , data.table(Value = c("Classification", "200", "150", "4", "2", "1", "permutation", "gini"))
               )
  expect_integer(as.integer(res$statistics$Value[2:6]), lower = 0, any.missing = FALSE)
  expect_numeric(as.double(tail(res$statistics$Value, 2)), lower = 0, any.missing = FALSE)
  
  set.seed(1619)
  createCSEnvir(iris, strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = scriptvars$brush.pred
                                       , use.rows = scriptvars$use.rows
                                       , num.trees = scriptvars$num.trees
                                       , importance.mode = "impurity"
                                       , respect.unordered.factors = "ignore"
                                       )
                , env = env
                )
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(res$statistics$Value[7], "impurity")
  
  set.seed(1619)
  createCSEnvir(iris, strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = scriptvars$brush.pred
                                       , use.rows = scriptvars$use.rows
                                       , num.trees = scriptvars$num.trees
                                       , importance.mode = "impurity_corrected"
                                       , respect.unordered.factors = "order"
                                       )
                , env = env
                )
  expect_warning(randomForest(return.results = TRUE, num.threads = 2))
  res = suppressWarnings(randomForest(return.results = TRUE, num.threads = 2))
  expect_equal(res$statistics$Value[7], "impurity_corrected")
  
  set.seed(1619)
  createCSEnvir(iris, strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = scriptvars$brush.pred
                                       , use.rows = scriptvars$use.rows
                                       , num.trees = scriptvars$num.trees
                                       , importance.mode = scriptvars$importance.mode
                                       , respect.unordered.factors = "partition"
                                       )
                , env = env
                )
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(res$statistics$Value[7], "permutation")
  
  # no response -> brush as response
  set.seed(1619)
  createCSEnvir(iris[, 1:4], blnBrush = sample(c(!logical(80), logical(70)))
                , strPreds = colnames(iris)[1:4]
                , lstScriptVars = scriptvars
                , env = env
                )
  expect_true(randomForest(num.threads = 2))
  
  # predict missing
  set.seed(1619)
  iris.miss.resp = iris
  iris.miss.resp$Species[sample(c(!logical(100), logical(50)))] = NA
  createCSEnvir(iris.miss.resp
                , strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = scriptvars
                , env = env
                )
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(sum(is.na(res$predictions$Resid.Species)), 100)
})

test_that("Regression", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(brush.pred = FALSE, use.rows = "all", num.trees = 500
                    , importance.mode = "permutation", respect.unordered.factors = "ignore"
  )
  
  # create CS-R output
  createCSEnvir(warpbreaks, strPreds = colnames(warpbreaks)[2:3], strResps = colnames(warpbreaks)[1]
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  
  # run randomForest
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(res$statistics[1:8, 2]
               , data.table(Value = c("Regression", "500", "54", "2", "1", "5", "permutation", "variance"))
               )
  expect_integer(as.integer(res$statistics$Value[2:6]), lower = 0, any.missing = FALSE)
  expect_numeric(as.integer(tail(res$statistics$Value, 3)), lower = 0, any.missing = FALSE)
  expect_data_table(res$importances, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_equal(res$importances$Variable, c("tension", "wool"))
  expect_numeric(res$importances$Importance, lower = 15, upper = 65)
  expect_equal(sum(res$predictions$Used.breaks), 54)
  expect_equal(res$predictions$breaks, warpbreaks$breaks)
  expect_list(res$confusions, len = 0)

  # predict missing
  set.seed(1619)
  carstats.miss.resp = carstats
  carstats.miss.resp$Displacement[sample(c(!logical(300), logical(106)))] = NA
  createCSEnvir(carstats.miss.resp[, c("Displacement", "Weight", "Acceleration")]
                , strPreds = c("Weight", "Acceleration")
                , strResps = "Displacement"
                , lstScriptVars = scriptvars, env = env
                )
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_equal(sum(is.na(res$predictions$Resid.Displacement)), 300)
})

test_that("Multi", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(brush.pred = FALSE, use.rows = "all", num.trees = 500
                    , importance.mode = "permutation", respect.unordered.factors = "ignore"
                    )
  
  # create CS-R output
  createCSEnvir(iris, strPreds = colnames(iris)[1:3], strResps = colnames(iris)[4:5]
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  # run randomForest
  res = randomForest(return.results = TRUE, num.threads = 2)
  expect_data_table(res$statistics, nrows = 2, ncols = 13)
  expect_equal(colnames(res$statistics)[1], "Response")
  expect_data_table(res$statistics[, 3:7], types = "integer")
  expect_data_table(res$statistics[, 10:13], types = "numeric")
  expect_data_table(res$importances, nrows = 2, ncols = 4)
  expect_data_table(res$importances[, -1], types = "numeric")
  expect_equal(colnames(res$importances)[1], "Response")
  expect_data_table(res$predictions, nrows = 150, ncols = 8)
  expect_list(res$confusions, len = 1)
})

test_that("Prediction", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(brush.pred = FALSE, use.rows = "all", num.trees = 500
                    , importance.mode = "permutation", respect.unordered.factors = "ignore"
                    )
  
  # create CS-R output
  createCSEnvir(iris, strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  # run randomForest
  res = randomForest(return.results = TRUE, num.threads = 2)
  # create CS-R output
  createCSEnvir(iris[, 1:4], strPreds = colnames(iris)[1:4]
                , lstRobject = res$rgobjects, env = env
                )
  # run predictions
  expect_true(randomForestPredict(num.threads = 2))
  res.predict = randomForestPredict(return.results = TRUE, num.threads = 2)
  expect_data_frame(res.predict$predictions, nrows = 150, ncols = 1)

  # create CS-R output
  createCSEnvir(iris, strPreds = colnames(iris)[1:3], strResps = colnames(iris)[4:5]
                , lstScriptVars = scriptvars, env = env
                )
  # run randomForest
  res = randomForest(return.results = TRUE, num.threads = 2)
  # create CS-R output
  createCSEnvir(iris[, 1:3], strPreds = colnames(iris)[1:3]
                , lstRobject = res$rgobjects, env = env
                )
  # run predictions
  expect_true(randomForestPredict(num.threads = 2))
  res.predict = randomForestPredict(return.results = TRUE, num.threads = 2)
  expect_data_frame(res.predict$predictions, nrows = 150, ncols = 2)
  
  # data does not fit to rf
  # create CS-R output
  createCSEnvir(iris[, c(1, 2, 4)], strPreds = colnames(iris)[c(1, 2, 4)]
                , lstRobject = res$rgobjects, env = env
                )
  # run predictions
  expect_error(randomForestPredict(num.threads = 2))
  
  # many ranger lists
  # create CS-R output
  rgs = rep(res$rgobjects, 3)
  rgs[[3]][[1]] = NULL
  createCSEnvir(iris[, c(1:3)], strPreds = colnames(iris)[c(1:3)]
                , lstRobject = rgs, env = env
                )
  # run predictions
  res.predict = randomForestPredict(return.results = TRUE, num.threads = 2)
  expect_data_frame(res.predict$predictions, nrows = 150, ncols = 5)
})
