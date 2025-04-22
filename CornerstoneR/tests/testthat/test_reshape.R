context("Reshape")

test_that("MeltLong", {
  set.seed(97)
  
  env = globalenv()
  
  dtTest = data.table(
    i_1 = c(1:4, NA, 5)
    , i_2 = c(51, 61, NA , 71, 81, 91)
    , f1 = factor(sample(c(letters[1:3], NA), 6, TRUE))
    , f2 = factor(c("z", "a", "x", "c", "x", "x"), ordered = TRUE)
    , r_1 = rnorm(6)
    , r_2 = rexp(6)
    , r3 = rweibull(6, shape = 2)
  )
  
  # two without splitting
  createCSEnvir(dtTest[, c(1:4)]
                , strPreds = c("i_1", "i_2"), strResps = c("f1", "f2")
                , lstScriptVars = list(split = "")
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 12, ncols = 4)
  expect_equal(colnames(res$reshapeLong), c("i_1", "i_2", "variable", "value"))
  # splitting response names
  createCSEnvir(dtTest[, c(1, 2, 5, 6)]
                , strPreds = c("i_1", "i_2"), strResps = c("r_1", "r_2")
                , lstScriptVars = list(split = "_")
                , env = env
                )
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 12, ncols = 5)
  expect_equal(colnames(res$reshapeLong), c("i_1", "i_2", "variable1", "variable2", "value"))
  # mixed response types
  createCSEnvir(dtTest[, 1:6]
                , strPreds = c("i_1", "i_2"), strResps = colnames(dtTest)[3:6]
                , lstScriptVars = list(split = "_")
                , env = env
                )
  expect_warning(reshapeLong())
  # mixed response names
  createCSEnvir(dtTest[, -c(3:4)]
                , strPreds = c("i_1", "i_2"), strResps = colnames(dtTest)[5:7]
                , lstScriptVars = list(split = "_")
                , env = env
                )
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 18, ncols = 5)
  expect_equal(colnames(res$reshapeLong), c("i_1", "i_2", "variable1", "variable2", "value"))
  # no predictors 1
  createCSEnvir(dtTest[, 1:2]
                , strResps = colnames(dtTest)[1:2]
                , lstScriptVars = list(split = "")
                , env = env
                )
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 12, ncols = 2)
  expect_equal(colnames(res$reshapeLong), c("variable", "value"))
  # no predictors 2
  createCSEnvir(dtTest[, 1:2]
                , strResps = colnames(dtTest)[1:2]
                , lstScriptVars = list(split = "_")
                , env = env
                )
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 12, ncols = 3)
  expect_equal(colnames(res$reshapeLong), c("variable1", "variable2", "value"))
})

test_that("CastWide", {
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(drop = TRUE, aggr.fun = "mean")
  
  # single
  createCSEnvir(Indometh
                , strPreds = colnames(Indometh)[2], strResps = colnames(Indometh)[3]
                , strGroups = colnames(Indometh)[1]
                , lstScriptVars = scriptvars
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  expect_true(reshapeWide())
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 11, ncols = 7)
  expect_equal(colnames(res$reshapeWide)[c(1, 2, 7)], c("time", "conc_mean_1", "conc_mean_3"))
  expect_equal(res$reshapeWide$conc_mean_1, head(Indometh$conc, 11))
  expect_equal(res$reshapeWide$conc_mean_6, tail(Indometh$conc, 11))
  
  # missing aggregation function
  scriptvars1 = scriptvars
  scriptvars1$aggr.fun = ""
  createCSEnvir(Indometh
                , strPreds = colnames(Indometh)[2], strResps = colnames(Indometh)[3]
                , strGroups = colnames(Indometh)[1]
                , lstScriptVars = scriptvars1
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  expect_true(reshapeWide())
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 11, ncols = 7)
  expect_equal(colnames(res$reshapeWide)[c(1, 2, 7)], c("time", "conc_first_1", "conc_first_3"))
  expect_equal(res$reshapeWide$conc_first_1, head(Indometh$conc, 11))
  expect_equal(res$reshapeWide$conc_first_6, tail(Indometh$conc, 11))
  
  # additional response
  IndoExt = cbind(Indometh, rnd = rnorm(66))
  createCSEnvir(IndoExt
                , strPreds = colnames(IndoExt)[2], strResps = colnames(IndoExt)[3:4]
                , strGroups = colnames(IndoExt)[1]
                , lstScriptVars = scriptvars
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 11, ncols = 13)
  expect_equal(colnames(res$reshapeWide)[c(1, 2, 7, 8, 13)]
               , c("time", "conc_mean_1", "conc_mean_3", "rnd_mean_1", "rnd_mean_3")
               )
  
  # multiple groups
  DT = data.table(v1 = rep(1:2, each = 6), v2 = rep(rep(1:3, 2), each = 2), v3 = rep(1:2, 6), v4 = rnorm(6))
  createCSEnvir(DT
                , strPreds = "v2", strResps = "v4"
                , strGroups = c("v1", "v3")
                , lstScriptVars = scriptvars
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 3, ncols = 5)
  expect_equal(colnames(res$reshapeWide)
               , c("v2", "v4_mean_1_1", "v4_mean_1_2", "v4_mean_2_1", "v4_mean_2_2")
               )
  
  # multiple predictors
  createCSEnvir(DT
                , strPreds = c("v2", "v3"), strResps = "v4"
                , strGroups = "v1"
                , lstScriptVars = scriptvars
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 6, ncols = 4)
  expect_equal(colnames(res$reshapeWide), c("v2", "v3", "v4_mean_1", "v4_mean_2"))
  
  # aggreation function working
  # carstats standard
  createCSEnvir(carstats[, .(MPG, Cylinders, Model.Year)]
                , strPreds = "Model.Year", strResps = "MPG", strGroups = "Cylinders"
                , lstScriptVars = scriptvars
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 13, ncols = 6, type = "numeric")
  # carstats more functions
  scriptvars1 = scriptvars
  scriptvars1$aggr.fun = "mean, sd"
  createCSEnvir(carstats[, .(MPG, Cylinders, Model.Year)]
                , strPreds = "Model.Year", strResps = "MPG", strGroups = "Cylinders"
                , lstScriptVars = scriptvars1
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 13, ncols = 11, type = "numeric")
  # carstats wrong function
  scriptvars1 = scriptvars
  scriptvars1$aggr.fun = "mean, notdefinedfunction"
  createCSEnvir(carstats[, .(MPG, Cylinders, Model.Year)]
                , strPreds = "Model.Year", strResps = "MPG", strGroups = "Cylinders"
                , lstScriptVars = scriptvars1
                , env = env
                )
  expect_error(reshapeWide(), "notdefinedfunction")
  # carstats aggregation by other column, check assertion
  scriptvars1 = scriptvars
  scriptvars1$aggr.fun = "mean, minby(Weight), sd, maxby(Blup)"
  createCSEnvir(carstats[, .(Origin, Displacement, Cylinders, Weight)]
                , strPreds = "Origin", strResps = "Displacement", strGroups = "Cylinders", strAuxs = "Weight"
                , lstScriptVars = scriptvars1
                , env = env
                )
  expect_error(reshapeWide())
  # carstats aggregation by other column
  scriptvars1 = scriptvars
  scriptvars1$aggr.fun = "mean, minby(Weight), sd, maxby(Weight)"
  createCSEnvir(carstats[, .(Origin, Displacement, Cylinders, Weight)]
                , strPreds = "Origin", strResps = "Displacement", strGroups = "Cylinders", strAuxs = "Weight"
                , lstScriptVars = scriptvars1
                , env = env
                )
  expect_true(reshapeWide())
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 7, ncols = 5*4+1)
  # carstats aggregation by multiple response columns
  scriptvars1 = scriptvars
  scriptvars1$aggr.fun = "mean, minby(Weight), sd, maxby(Weight)"
  createCSEnvir(carstats[, .(Origin, Displacement, Horsepower, Cylinders, Weight)]
                , strPreds = "Origin", strResps = c("Displacement", "Horsepower")
                , strGroups = "Cylinders", strAuxs = "Weight"
                , lstScriptVars = scriptvars1
                , env = env
                )
  expect_true(reshapeWide())
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 7, ncols = 5*4*2+1)
  # carstats, check results from minby and maxby
  scriptvars1 = scriptvars
  scriptvars1$aggr.fun = "minby(Weight), maxby(Weight)"
  createCSEnvir(carstats[, .(Origin, Displacement, Cylinders, Weight)]
                , strPreds = "Origin", strResps = "Displacement", strGroups = "Cylinders", strAuxs = "Weight"
                , lstScriptVars = scriptvars1
                , env = env
                )
  expect_true(reshapeWide())
  res = reshapeWide(return.results = TRUE)
  # manually proven results
  expect_equal(res$reshapeWide$Displacement_minby_4, c(122, 79, 97, 68, 72, 104, 98))
  expect_equal(res$reshapeWide$Displacement_maxby_4, c(122, 120, 146, 107, 134, 130, 151))
})

test_that("Transpose", {
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(convert.numeric = TRUE)
  
  # standard
  dtTest = data.table(data1 = rnorm(5), data2 = runif(5))
  createCSEnvir(dtTest
                , lstScriptVars = scriptvars
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  expect_true(reshapeTranspose())
  res = reshapeTranspose(return.results = TRUE)
  expect_data_table(res$reshapeTranspose, nrows = 2, ncols = 6, col.names = "named")
  expect_data_table(res$reshapeTranspose[, -1], types = "numeric")
  expect_equal(colnames(res$reshapeTranspose), c("colnames", paste0("V", 1:5)))
  
  # mixed, convert.numeric = FALSE
  scriptvars1 = scriptvars
  scriptvars1$convert.numeric = FALSE
  dtTest = data.table(data1 = rnorm(5), data2 = sample(LETTERS, 5))
  createCSEnvir(dtTest
                , lstScriptVars = scriptvars1
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  expect_true(reshapeTranspose())
  res = reshapeTranspose(return.results = TRUE)
  expect_data_table(res$reshapeTranspose, types = "character", nrows = 2, ncols = 6, col.names = "named")
  expect_equal(colnames(res$reshapeTranspose), c("colnames", paste0("V", 1:5)))
  
  # mixed, convert.numeric = TRUE
  dtTest = data.table(data1 = rnorm(5), data2 = sample(LETTERS, 5))
  createCSEnvir(dtTest
                , lstScriptVars = scriptvars
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  expect_true(reshapeTranspose())
  res = reshapeTranspose(return.results = TRUE)
  expect_data_table(res$reshapeTranspose, nrows = 2, ncols = 6, col.names = "named")
  expect_data_table(res$reshapeTranspose[, -1], types = "numeric")
  
  # column with data for new variable names
  # generate data
  dtTest = data.table(varnames = sample(LETTERS, size = 5), data1 = rnorm(5), data2 = runif(5))
  createCSEnvir(dtTest
                , strGroups = "varnames"
                , lstScriptVars = scriptvars
                , env = env
                )
  expect_true(reshapeTranspose())
  res = reshapeTranspose(return.results = TRUE)
  expect_data_table(res$reshapeTranspose, nrows = 2, ncols = 6, col.names = "named")
  expect_equal(colnames(res$reshapeTranspose), c("colnames", dtTest[[1]]))
  
  # input is numeric as string and should be returned as numeric
  dtTest = data.table(data1 = c("1,2", "2,3", "3,4"), data2 = c("9,87", "7,65", "5,43"))
  createCSEnvir(dtTest
                , lstScriptVars = scriptvars
                , env = env
                )
  expect_true(reshapeTranspose())
  res = reshapeTranspose(return.results = TRUE)
  expect_data_table(res$reshapeTranspose, nrows = 2, ncols = 4, col.names = "named")
  expect_data_table(res$reshapeTranspose[, -1], types = "numeric")
})
