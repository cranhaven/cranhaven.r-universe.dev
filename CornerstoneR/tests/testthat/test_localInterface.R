context("Local Interface")

# test_that("Invoke from R", {
  # has to be invoked as one of the first tests
  # see 'test_aaa.R'
# })

test_that("Invoke from virtual CS", {
  # globalenv() results in an error for R CMD check, environment() solves the issue
  # env = globalenv()
  env = environment()
  # initialise 'cs.session.test' environment before calling createCSFunctions()
  createCSEnvir(carstats[, .(Cylinders, Displacement, Horsepower)]
                , strPreds = "Displacement", strResps = "Horsepower", strGroups = "Cylinders"
                , env = env
                )
  expect_environment(cs.session.test)
  
  # init cs.* functions after create cs.session.test environment
  createCSFunctions(env = env)
  
  # start tests
  expect_false(invokeFromR())
  expect_false(any(grepl("get0", body("cs.in.auxiliaries"))))
  expect_false(any(grepl("get0", body("cs.in.brushed"))))
  expect_false(any(grepl("get0", body("cs.in.dataset"))))
  expect_false(any(grepl("get0", body("cs.in.excluded"))))
  expect_false(any(grepl("get0", body("cs.in.groupvars"))))
  expect_false(any(grepl("get0", body("cs.in.predictors"))))
  expect_false(any(grepl("get0", body("cs.in.responses"))))
  expect_false(any(grepl("get0", body("cs.in.scriptvars"))))
  expect_false(any(grepl("get0", body("cs.in.subsets"))))
  expect_false(any(grepl("get0", body("cs.in.subsets.current"))))
  expect_false(any(grepl("get0", body("cs.quote"))))
  expect_false(any(grepl("get0", body("cs.out.dataset"))))
  expect_false(any(grepl("get0", body("cs.out.emf"))))
  expect_false(any(grepl("get0", body("cs.out.png"))))
})
