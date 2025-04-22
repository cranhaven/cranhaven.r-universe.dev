context("Redirect Dataset")

test_that("Redirect Dataset", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(remove.pattern = "")
  
  # create CS-R output
  createCSEnvir(carstats
                , strPreds = names(carstats)[1:3], strResps = names(carstats)[4:6]
                , strGroups = names(carstats)[7:8], strAuxs = names(carstats)[9]
                , lstScriptVars = scriptvars
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run redirection
  expect_true(redirectDataset())
  res = redirectDataset(return.results = TRUE)
  expect_list(res, any.missing = FALSE, len = 1)
  expect_list(res$robjects, any.missing = FALSE, len = 5)
  expect_equal(res$robjects$dataset, carstats)
  expect_equal(res$robjects$predictors, names(carstats)[1:3])
  expect_equal(res$robjects$responses, names(carstats)[4:6])
  expect_equal(res$robjects$groups, names(carstats)[7:8])
  expect_equal(res$robjects$auxiliaries, names(carstats)[9])
  expect_class(res$robjects, c("CSR.redirectedDataset", "list"))
  
  # specific remove pattern
  createCSEnvir(carstats
                , strPreds = names(carstats)[1:3], strResps = names(carstats)[4:6]
                , strGroups = names(carstats)[7:8], strAuxs = names(carstats)[9]
                , lstScriptVars = list(remove.pattern = "Mod")
                , env = env
                )
  # run redirection
  expect_true(redirectDataset())
  res = redirectDataset(return.results = TRUE)
  expect_equal(colnames(res$robjects$dataset)[c(1, 9)], c("el", "el.Year"))
  expect_equal(colnames(res$robjects$dataset)[2:8], names(carstats)[2:8])
  expect_equal(res$robjects$predictors, c("el", "Origin", "MPG"))
  expect_equal(res$robjects$responses, names(carstats)[4:6])
  expect_equal(res$robjects$groups, names(carstats)[7:8])
  expect_equal(res$robjects$auxiliaries, "el.Year")
})
