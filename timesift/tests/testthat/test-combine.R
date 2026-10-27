# `c()` on a spec is the set of them. A learner and a representation are both lists, so the thing
# under test is that class decides what splices: without the methods `c()` takes each spec apart
# into its own fields and the failure surfaces much later, as a complaint about a `function`.

test_that("c() on learners is a set of them, named by each learner", {
  set <- c(elasticnet(), forest())
  expect_s3_class(set, "timesift_models")
  expect_length(set, 2L)
  expect_named(set, c("elasticnet", "forest"))
  expect_s3_class(set[[1L]], "timesift_learner")
})

test_that("a set handed back to c() splices rather than nests", {
  base <- c(elasticnet(), forest())
  expect_named(c(base, stepwise()), c("elasticnet", "forest", "stepwise"))
  expect_named(c(stepwise(), base), c("stepwise", "elasticnet", "forest"))
  expect_named(c(base, c(stepwise(), mlp())),
               c("elasticnet", "forest", "stepwise", "mlp"))
  expect_length(c(base, base[0L]), 2L)
})

test_that("c() takes the name of a registered learner, as a list does", {
  expect_named(c(elasticnet(), "forest"), c("elasticnet", "forest"))
  expect_s3_class(c(elasticnet(), "forest")[["forest"]], "timesift_learner")
})

test_that("a name given to c() is the name the learner is reported under", {
  expect_named(c(fast = elasticnet(), forest()), c("fast", "forest"))
})

test_that("c() refuses two learners under one name, as a list does", {
  expect_error(c(elasticnet(), elasticnet()), "under the same name")
  expect_error(c(elasticnet(), "elasticnet"), "under the same name")
})

test_that("c() on representations is a sift of them", {
  s <- c(grain("week"), grain("month"))
  expect_s3_class(s, "timesift_sift")
  expect_named(s, c("week", "month"))
  expect_s3_class(s[[1L]], "timesift_representation")
})

test_that("a sift handed back to c() splices, across kinds of representation", {
  s <- c(grains("day", "week"), lookback("30 days"))
  expect_s3_class(s, "timesift_sift")
  expect_named(s, c("day", "week", "30 days"))
  expect_named(c(native(), s), c("native", "day", "week", "30 days"))
  expect_named(c(multigrain(c("month", "year")), grain("day")), c("multigrain", "day"))
})

test_that("an auto sift cannot be combined, since what it stands for is not yet known", {
  expect_error(c(grains("auto"), grain("week")), "cannot be combined")
  expect_error(c(grain("week"), grains("auto")), "cannot be combined")
})

test_that("c() refuses what is not a representation, naming the elements", {
  expect_error(c(grain("week"), list(1, 2)), "not a representation")
  expect_error(c(elasticnet(), 1), "expected a learner")
})

test_that("a set and the list it replaces reach the fitting layer the same way", {
  expect_equal(.learner_list(c(elasticnet(), forest())),
               .learner_list(list(elasticnet(), forest())))
  expect_equal(unclass(c(grain("week"), grain("month"))),
               unclass(as_sift(list(grain("week"), grain("month")))))
})

test_that("a set prints what it holds", {
  expect_output(print(c(elasticnet(), forest())), "2 learners")
  expect_output(print(c(elasticnet(), forest())), "elasticnet")
})
