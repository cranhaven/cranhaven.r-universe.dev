context("fitFunction")

test_that("Work with Invalid Variable Names", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "", preds.frml = "", resp.frml = "", limits = ""
                    , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
                    )
  # check invalid variable names in backticks (`)
  # Results:
  # - nls: everything is possible
  # - nlsLM: space in name is not possible
  
  # invalid variable names for "User Defined" (1)
  trees1 = trees
  names(trees1) = c(" Gir th", "1 Height", ".Vol ume")
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$math.fun = "User Defined"
  scriptvars1$preds.frml = "a + b*log(` Gir th`) + c*log(`1 Height`)"
  scriptvars1$resp.frml = "log(`.Vol ume`)"
  scriptvars1$start.vals = "a=1, b=1, c=1"
  # create CS-R output
  createCSEnvir(trees1, strPreds = c("1 Height", " Gir th"), strResps = ".Vol ume"
                , lstScriptVars = scriptvars1, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 13, col.names = "named")
  expect_data_table(res$vcov, types = "numeric", any.missing = FALSE, nrows = 3, ncols = 3, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 31, ncols = 3)
  expect_numeric(unlist(res$coeff[, 1]), lower = -7, upper = -6)
  expect_numeric(unlist(res$coeff[, 2:3]), lower = 1, upper = 2)
  expect_numeric(unlist(res$coeff[, 4:8]), lower = 0, upper = 1)
  expect_set_equal(names(res$predictions), c(".Vol ume", "Fitted", "Residuals"))
  
  # invalid variable names for "User Defined" (2)
  trees1 = trees
  names(trees1) = c(" Gir th", "1 Height", "Vol ume")
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$math.fun = "User Defined"
  scriptvars1$preds.frml = "a + b*log(` Gir th`) + c*log(`1 Height`)"
  scriptvars1$resp.frml = "log(`Vol ume`)"
  scriptvars1$start.vals = "a=1, b=1, c=1"
  # create CS-R output
  createCSEnvir(trees1, strPreds = c("1 Height", " Gir th"), strResps = "Vol ume"
                , lstScriptVars = scriptvars1, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 13, col.names = "named")
  expect_data_table(res$vcov, types = "numeric", any.missing = FALSE, nrows = 3, ncols = 3, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 31, ncols = 3)
  expect_numeric(unlist(res$coeff[, 1]), lower = -7, upper = -6)
  expect_numeric(unlist(res$coeff[, 2:3]), lower = 1, upper = 2)
  expect_numeric(unlist(res$coeff[, 4:8]), lower = 0, upper = 1)
  
  # invalid variable names for "Logistic"
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$math.fun = "Logistic"
  # logistic function
  fun = function(x, a, b, c, d, sigma = 1) {
    a+(b-a) / (1+exp(-d*(x-c))) + rnorm(length(x), sd = sigma)
  }
  dt = data.table(  x1 = sample(seq(-10, 10, length.out = 100))
                  , group1 = sample(x = c("A", "B"), replace = TRUE, size = 100)
                  )
  dt[group1 == "A", y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
  dt[group1 == "B", y1 := fun(x1, 8, 2, -1, 0.3, 0.1)]
  # change names
  names(dt) = c(" x 1", "1 grp", ".y 1")
  # create CS-R output
  createCSEnvir(dt, strPreds = names(dt)[1], strResps = names(dt)[3], strGroups = names(dt)[2]
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 2, ncols = 16, col.names = "named")
  expect_data_table(res$vcov, any.missing = FALSE, nrows = 8, ncols = 5, col.names = "named")
  expect_data_table(res$vcov[, 1], types = "character")
  expect_data_table(res$vcov[, -1], types = "numeric")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 100, ncols = 4)
  expect_equal(res$predictions[, 1:2], dt[, 2:3])
  expect_set_equal(colnames(res$predictions), c(names(dt)[2], names(dt)[3], "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 2:4]), data.table(Coeff_a = c(1, 8), Coeff_b = c(10, 2), Coeff_c = c(1, -1)))
  expect_equal(round(res$coeff[, 5], digits = 1), data.table(Coeff_d = c(0.6, 0.3)))
  
  # Date as grouping column
  # linear function
  fun = function(x, a, b, sigma = 1) {
    a+b*x + rnorm(length(x), sd = sigma)
  }
  # two date groups
  time.now = Sys.time()
  time.last = time.now - 60*60
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100))
                  , group1 = sample(x = c(time.now, time.last), replace = TRUE, size = 100)
                  )
  dt[group1 == time.now, y1 := fun(x1, 1, 10, 0.1)]
  dt[group1 == time.last, y1 := fun(x1, 2, 4, 0.1)]
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$math.fun = "User Defined"
  scriptvars1$preds.frml = "a + b*x1"
  scriptvars1$resp.frml = "y1"
  scriptvars1$start.vals = "a=1, b=1"
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = "group1"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 3, upper = 3)
})

test_that("Logistic", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "Logistic", preds.frml = "", resp.frml = "", limits = ""
                    , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
                    )
  
  # logistic function
  fun = function(x, a, b, c, d, sigma = 1) {
    a+(b-a) / (1+exp(-d*(x-c))) + rnorm(length(x), sd = sigma)
  }
  
  # one group
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 15, col.names = "named")
  expect_data_table(res$vcov, types = "numeric", any.missing = FALSE, nrows = 4, ncols = 4, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 100, ncols = 3)
  expect_equal(res$predictions[, 1], dt[, 2])
  expect_set_equal(colnames(res$predictions), c("y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 1:3]), data.table(Coeff_a = 1, Coeff_b = 10, Coeff_c = 1))
  expect_equal(round(res$coeff[, 4], digits = 1), data.table(Coeff_d = 0.6))
  
  # add weighting variable
  dt[, w1 := runif(100)]
  # set weighting
  scriptvars1 = scriptvars
  scriptvars1$weights = "w1"
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strAuxs = "w1"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 15, col.names = "named")
  expect_data_table(res$vcov, types = "numeric", any.missing = FALSE, nrows = 4, ncols = 4, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 100, ncols = 3)
  expect_equal(res$predictions[, 1], dt[, 2])
  expect_set_equal(colnames(res$predictions), c("y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 1:3]), data.table(Coeff_a = 1, Coeff_b = 10, Coeff_c = 1))
  expect_equal(round(res$coeff[, 4], digits = 1), data.table(Coeff_d = 0.6))
  
  # two groups
  dt = data.table(  x1 = sample(seq(-10, 10, length.out = 100))
                  , group1 = sample(x = c("A", "B"), replace = TRUE, size = 100)
                  )
  dt[group1 == "A", y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
  dt[group1 == "B", y1 := fun(x1, 8, 2, -1, 0.3, 0.1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = "group1"
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 2, ncols = 16, col.names = "named")
  expect_data_table(res$vcov, any.missing = FALSE, nrows = 8, ncols = 5, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 100, ncols = 4)
  expect_equal(res$predictions[, 1:2], dt[, 2:3])
  expect_set_equal(colnames(res$predictions), c("group1", "y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 2:4]), data.table(Coeff_a = c(1, 8), Coeff_b = c(10, 2), Coeff_c = c(1, -1)))
  expect_equal(round(res$coeff[, 5], digits = 1), data.table(Coeff_d = c(0.6, 0.3)))
  
  # add weighting variable
  dt[, w1 := runif(100)]
  # set weighting
  scriptvars1 = scriptvars
  scriptvars1$weights = "w1"
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = "group1", strAuxs = "w1"
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 2, ncols = 16, col.names = "named")
  expect_data_table(res$vcov, any.missing = FALSE, nrows = 8, ncols = 5, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 100, ncols = 4)
  expect_equal(res$predictions[, 1:2], dt[, 2:3])
  expect_set_equal(colnames(res$predictions), c("group1", "y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 2:4]), data.table(Coeff_a = c(1, 8), Coeff_b = c(10, 2), Coeff_c = c(1, -1)))
  expect_equal(round(res$coeff[, 5], digits = 1), data.table(Coeff_d = c(0.6, 0.3)))
  
  # no predictor
  createCSEnvir(dt[, "y1"], strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # run fit
  expect_error(fitFunction())
  
  # "Number of iterations has reached" (was singular gradient)
  set.seed(1620)
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  # dt[, y1 := fun(x1, 1, 2, 1, 0.1, 1)]
  dt[, y1 := rnorm(100)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # run fit
  expect_warning(fitFunction(return.results = TRUE))
  res = suppressWarnings(fitFunction(return.results = TRUE))
  expect_integerish(res$coeff$StopCode, lower = -1, upper = -1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 15, col.names = "named")
  expect_data_table(res$vcov, types = "numeric", any.missing = FALSE, nrows = 4, ncols = 4, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 100, ncols = 3)
  expect_equal(dim(res$predictions), c(100, 3))
  expect_false(res$coeff$Converged)
  expect_equal(res$coeff$StopCode, -1)
  
  # "Number of iterations has reached" (was below "minFactor")
  set.seed(1621)
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 2, 1, 0.1, 0.4)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # run fit
  expect_warning(fitFunction(return.results = TRUE))
  res = suppressWarnings(fitFunction(return.results = TRUE))
  expect_integerish(res$coeff$StopCode, lower = -1, upper = -1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 15, col.names = "named")
  expect_data_table(res$vcov, types = "numeric", any.missing = FALSE, nrows = 4, ncols = 4, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 100, ncols = 3)
  expect_equal(dim(res$predictions), c(100, 3))
  expect_false(res$coeff$Converged)
  expect_equal(res$coeff$StopCode, -1)
  
  # increase iterations for problem above
  scriptvars1 = scriptvars
  scriptvars1$max.iter = 55
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  expect_warning(fitFunction(return.results = TRUE))
  res = suppressWarnings(fitFunction(return.results = TRUE))
  expect_integerish(res$coeff$Iterations, lower = 55, upper = 55)
})

test_that("User Definded", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "User Defined", preds.frml = "", resp.frml = "", limits = ""
                    , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
                    )
  
  # first standard test
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a + b*log(Girth) + c*log(Height)"
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$start.vals = "a=1, b=1, c=1"
  # create CS-R output
  createCSEnvir(trees, strPreds = c("Height", "Girth"), strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 13, col.names = "named")
  expect_data_table(res$vcov, types = "numeric", any.missing = FALSE, nrows = 3, ncols = 3, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 31, ncols = 3)
  expect_numeric(unlist(res$coeff[, 1]), lower = -7, upper = -6)
  expect_numeric(unlist(res$coeff[, 2:3]), lower = 1, upper = 2)
  expect_numeric(unlist(res$coeff[, 4:8]), lower = 0, upper = 1)
  
  # two groups
  trees.grp = trees
  trees.grp$grouping = sample(c("A", "B"), size = nrow(trees.grp), replace = TRUE)
  # create CS-R output
  createCSEnvir(trees.grp, strPreds = c("Height", "Girth"), strResps = "Volume", strGroups = "grouping"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 2, ncols = 14, col.names = "named")
  expect_data_table(res$vcov, any.missing = FALSE, nrows = 6, ncols = 4, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 31, ncols = 4)
  
  # weighting
  trees.grp = trees
  trees.grp$Weighting = 1
  scriptvars1$weights = "Weighting"
  # create CS-R output
  createCSEnvir(trees.grp, strPreds = c("Height", "Girth"), strResps = "Volume", strAuxs = "Weighting"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  # Group + Coeff * 2 + 7
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 1, ncols = 13, col.names = "named")
  expect_data_table(res$vcov, any.missing = FALSE, nrows = 3, ncols = 3, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = 31, ncols = 3)
  
  # missing start values
  scriptvars1 = scriptvars
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$preds.frml = "a + b*Girth"
  # create CS-R output
  createCSEnvir(trees[, c("Girth", "Volume")], strResps = "Volume", strPreds = "Girth"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())
  
  # no predictors
  scriptvars1 = scriptvars
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$preds.frml = "a"
  scriptvars1$start.vals = "a = 1"
  # create CS-R output
  createCSEnvir(trees[, "Volume", drop = FALSE], strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())
  
  # malformed start values
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a + b*log(Girth) + c*log(Height)"
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$start.vals = "a=1, =1, c=1"
  # create CS-R output
  createCSEnvir(trees, strPreds = c("Height", "Girth"), strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())

  # malformed start values
  scriptvars1$start.vals = "a=1, b=x, c=1"
  # create CS-R output
  createCSEnvir(trees, strPreds = c("Height", "Girth"), strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())
})

test_that("Date Time Columns", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "User Defined", preds.frml = "", resp.frml = "", limits = ""
                    , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
                    )
  
  # first standard test
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a*time1+b"
  scriptvars1$resp.frml = "y"
  scriptvars1$start.vals = "a=0.5, b=1"
  
  # linear function
  fun = function(x, a, b, sigma = 1) {
    a*x + b + rnorm(length(x), sd = sigma)
  }
  
  # time now
  time.now = Sys.time()
  
  # generate data
  # POSIXct
  dt = data.table(time1 = sample(seq(0, 10, length.out = 100)))
  dt[, y := fun(time1, 0.6, 2, 0.1)]
  dt[, time1 := as.POSIXct(time1 + time.now)]
  
  # create CS-R output
  createCSEnvir(dt, strPreds = "time1", strResps = "y"
                , lstScriptVars = scriptvars1, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  expect_numeric(res$coeff$Coeff_a, lower = 0.55, upper = 0.65)
  expect_numeric(res$coeff$Coeff_b, lower = 1.95, upper = 2.05)

  # difftime
  dt = data.table(time1 = sample(seq(0, 10, length.out = 100)))
  dt[, y := fun(time1, 0.6, 2, 0.1)]
  dt[, time1 := as.difftime(time1 + 11, units = "secs")]
  
  # create CS-R output
  createCSEnvir(dt, strPreds = "time1", strResps = "y"
                , lstScriptVars = scriptvars1, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  expect_numeric(res$coeff$Coeff_a, lower = 0.55, upper = 0.65)
  expect_numeric(res$coeff$Coeff_b, lower = 1.95, upper = 2.05)
})

test_that("Limits", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "User Defined", preds.frml = "", resp.frml = "", limits = ""
                    , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
  )
  
  # limit as variable to fit
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "-c/(b-a)*(x-b)"
  scriptvars1$resp.frml = "y"
  scriptvars1$limits = "min=0, max=c"
  scriptvars1$start.vals = "a=1, b=10, c=5"
  
  # linear function
  fun = function(x, a, b, c, sigma = 1) {
    -c/(b-a)*(x-b) + rnorm(length(x), sd = sigma)
  }
  
  # generate data
  dt = data.table(x = sample(seq(1, 9, length.out = 100)))
  dt[, y := fun(x, 2, 8, 4, 0.1)]
  dt[, y := min(4, y), by = seq_len(nrow(dt))]  # set maximum to data
  dt[, y := max(0, y), by = seq_len(nrow(dt))]  # set minimum to data
  
  # create CS-R output
  createCSEnvir(dt, strPreds = "x", strResps = "y"
                , lstScriptVars = scriptvars1, env = env
  )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  expect_numeric(res$coeff$Coeff_a, lower = 1.9, upper = 2.1)
  expect_numeric(res$coeff$Coeff_b, lower = 7.9, upper = 8.1)
  expect_numeric(res$coeff$Coeff_c, lower = 3.9, upper = 4.1)
  
  # limits with NA
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a*x+b"
  scriptvars1$resp.frml = "y"
  scriptvars1$limits = "min=NA, max=5.5"
  scriptvars1$start.vals = "a=1, b=3"
  
  # linear function
  fun = function(x, a, b, sigma = 1) {
    a*x+b + rnorm(length(x), sd = sigma)
  }
  
  # generate data
  dt = data.table(x = sample(seq(1, 9, length.out = 100)))
  dt[, y := fun(x, 0.5, 2, 0.1)]
  dt[, y := min(5.5, y), by = seq_len(nrow(dt))]  # set maximum to data
  
  # create CS-R output
  createCSEnvir(dt, strPreds = "x", strResps = "y"
                , lstScriptVars = scriptvars1, env = env
  )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 3)
  # residuals over maximum should be 0
  expect_integerish(res$predictions[dt[x >= 7.5, which = TRUE], Residuals], lower = 0, upper = 0)
  
  # malformed limits
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a*x+b"
  scriptvars1$resp.frml = "y"
  scriptvars1$limits = "min=, ="
  scriptvars1$start.vals = "a=1, b=3"
  
  # linear function
  fun = function(x, a, b, sigma = 1) {
    a*x+b + rnorm(length(x), sd = sigma)
  }
  
  # generate data
  dt = data.table(x = sample(seq(1, 9, length.out = 100)))
  dt[, y := fun(x, 0.5, 2, 0.1)]
  
  # create CS-R output
  createCSEnvir(dt, strPreds = "x", strResps = "y"
                , lstScriptVars = scriptvars1, env = env
  )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_error(fitFunction(), "malformed")
  
  # more than one limit
  scriptvars1$limits = "min=1, max=4, min=b"
  
  # create CS-R output
  createCSEnvir(dt, strPreds = "x", strResps = "y"
                , lstScriptVars = scriptvars1, env = env
  )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_error(fitFunction(), "once")
})

test_that("nls error", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "User Defined", preds.frml = "", resp.frml = "", limits = ""
                    , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
  )
  
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a*x+b"
  scriptvars1$resp.frml = "y"
  scriptvars1$limits = "max=c"
  scriptvars1$start.vals = "a=-1, b=10, c=300"
  
  # generate data
  dt = data.table(x = sample(seq(0, 10, length.out = 100)))
  dt[, y := rnorm(100)]
  
  # create CS-R output
  createCSEnvir(dt, strPreds = "x", strResps = "y"
                , lstScriptVars = scriptvars1, env = env
  )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_true(all(is.na(res$coeff[1, 1:11])))
  expect_string(res$coeff$StopMessage[1], pattern = "singular gradient")
  expect_true(all(is.na(res$predictions[, 2:3])))
})

test_that("Multiple Groups", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "Logistic", preds.frml = "", resp.frml = "", limits = ""
                    , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
                    )
  
  # logistic function
  fun = function(x, a, b, c, d, sigma = 1) {
    a+(b-a) / (1+exp(-d*(x-c))) + rnorm(length(x), sd = sigma)
  }
  
  # multiple grouping variables
  n = 400
  dt = data.table(  x1 = sample(seq(-10, 10, length.out = n))
                    , grp1 = sample(x = c("B", "A"), replace = TRUE, size = n)
                    , grp2 = sample(x = c("Z", "Y"), replace = TRUE, size = n)
                    , key = c("grp1", "grp2")
                    )
  dt[.("A", "Y"), y1 := fun(x1, 1, 10, -2, 0.6, 0.1)]
  dt[.("A", "Z"), y1 := fun(x1, 2, 9, -1, 0.5, 0.1)]
  dt[.("B", "Y"), y1 := fun(x1, 3, 8, 0, 0.4, 0.1)]
  dt[.("B", "Z"), y1 := fun(x1, 4, 7, 1, 0.3, 0.1)]
  setkey(dt, NULL)
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = c("grp1", "grp2")
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 1, upper = 1)
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 4, ncols = 17, col.names = "named")
  expect_data_table(res$vcov, any.missing = FALSE, nrows = 16, ncols = 6, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = n, ncols = 5)
  expect_equal(res$predictions[, 1:3], dt[, 2:4])
  expect_set_equal(colnames(res$predictions), c("grp1", "grp2", "y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 3:5]), data.table(Coeff_a = 1:4, Coeff_b = 10:7, Coeff_c = -2:1))
  expect_equal(round(res$coeff[, 6], digits = 1), data.table(Coeff_d = seq(0.6, 0.3, -0.1)))
  
  # user defined function
  fun = function(x, a, b, sigma = 1) {
    a+b*x + rnorm(length(x), sd = sigma)
  }
  
  # multiple grouping variables
  n = 400
  dt = data.table(  x1 = sample(seq(-10, 10, length.out = n))
                    , grp1 = sample(x = c("B", "A"), replace = TRUE, size = n)
                    , grp2 = sample(x = c("Z", "Y"), replace = TRUE, size = n)
                    , key = c("grp1", "grp2")
                    )
  dt[.("A", "Y"), y1 := fun(x1, 1, 10, 0.1)]
  dt[.("A", "Z"), y1 := fun(x1, 2, 9, 0.1)]
  dt[.("B", "Y"), y1 := fun(x1, 3, 8, 0.1)]
  dt[.("B", "Z"), y1 := fun(x1, 4, 7, 0.1)]
  setkey(dt, NULL)
  # update script variables
  scriptvars1 = scriptvars
  scriptvars1$math.fun = "User Defined"
  scriptvars1$preds.frml = "a+b*x1"
  scriptvars1$resp.frml = "y1"
  scriptvars1$start.vals = "a=0, b=1"
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = c("grp1", "grp2")
                , lstScriptVars = scriptvars1, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_integerish(res$coeff$StopCode, lower = 3, upper = 3)
  expect_data_table(res$coeff, any.missing = FALSE, nrows = 4, ncols = 13, col.names = "named")
  expect_data_table(res$vcov, any.missing = FALSE, nrows = 8, ncols = 4, col.names = "named")
  expect_data_table(res$predictions, any.missing = FALSE, nrows = n, ncols = 5)
  expect_equal(res$predictions[, 1:3], dt[, 2:4])
  expect_set_equal(colnames(res$predictions), c("grp1", "grp2", "y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 3:4]), data.table(Coeff_a = 1:4, Coeff_b = 10:7))
})
