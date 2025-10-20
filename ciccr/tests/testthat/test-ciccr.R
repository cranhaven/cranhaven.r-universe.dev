context("Testing ciccr")

# code should work whether x has one variable or more

test_that("Case 1 with a scalar x: The default option for avg_RR_logit is 'control'", {

y = ACS_CC$topincome
t = ACS_CC$baplus
x = ACS_CC$age

results_default = avg_RR_logit(y, t, x)
results_control = avg_RR_logit(y, t, x, 'control')

expect_equal( results_default$est, results_control$est)

})

test_that("Case 2 with x and x^2: The default option for avg_RR_logit is 'control'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  x = cbind(x,x^2)

  results_default = avg_RR_logit(y, t, x)
  results_control = avg_RR_logit(y, t, x, 'control')

  expect_equal( results_default$est, results_control$est)

})

test_that("The results for avg_RR_logit should be different between 'case' and 'control'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  results_case = avg_RR_logit(y, t, x, 'case')
  results_control = avg_RR_logit(y, t, x, 'control')

  expect_false( results_case$est == results_control$est)

})

test_that("There should be an error other than 'case' and 'control'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  expect_error(avg_RR_logit(y, t, x, 'ctrl'))

})

test_that("Method 1: Each element of 'y' must be either 0 or 1.", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  y[1] = 2

  expect_error(avg_RR_logit(y, t, x))

})

test_that("Method 2: Each element of 'y' must be either 0 or 1.", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  y[1] = 2

  expect_error(avg_AR_logit(y, t, x))

})

test_that("Method 1: Each element of 't' must be either 0 or 1.", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  t[1] = 2

  expect_error(avg_RR_logit(y, t, x))

})

test_that("Method 2: Each element of 't' must be either 0 or 1.", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  t[1] = 2

  expect_error(avg_AR_logit(y, t, x))

})

test_that("The default sampling option for avg_AR_logit is 'cc'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  results_default = avg_AR_logit(y, t, x)
  results_cc = avg_AR_logit(y, t, x, sampling = 'cc')

  expect_equal( results_default$est, results_cc$est)

})

test_that("The results for cicc_RR should be different between 'cc' and 'cp'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results_cc = cicc_RR(y, t, x, sampling = 'cc')

  y = ACS_CP$topincome
  y = as.integer(is.na(y)==FALSE)
  t = ACS_CP$baplus
  x = ACS_CP$age
  results_cp = cicc_RR(y, t, x, sampling = 'cp')

  expect_false(sum((results_cc$est != results_cp$est))==0)

})

test_that("The default sampling option for cicc_RR is 'cc'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  results_default = cicc_RR(y, t, x)
  results_cc = cicc_RR(y, t, x, sampling = 'cc')

  expect_equal( results_default$est, results_cc$est)

})

test_that("The sampling option for cicc_RR is either 'cc' or 'cp'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  expect_error(cicc_RR(y, t, x, sampling = 'cr'))

})

test_that("The sampling option for avg_AR_logit is either 'cc' or 'cp'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  expect_error(avg_AR_logit(y, t, x, sampling = 'cr'))

})

test_that("The results for avg_AR_logit should be different between 'cc' and 'cp'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results_cc = avg_AR_logit(y, t, x, sampling = 'cc')

  y = ACS_CP$topincome
  y = as.integer(is.na(y)==FALSE)
  t = ACS_CP$baplus
  x = ACS_CP$age
  results_cp = avg_AR_logit(y, t, x, sampling = 'cp')

  expect_false(sum((results_cc$est != results_cp$est))==0)

})

test_that("The results for avg_AR_logit should be different between interaction = TRUE and FALSE", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results1 = avg_AR_logit(y, t, x, interaction = FALSE)
  results2 = avg_AR_logit(y, t, x, interaction = TRUE)

  expect_false(sum((results1$est != results2$est))==0)

})

test_that("There should be an error if interaction != TRUE or FALSE", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  expect_error(avg_AR_logit(y, t, x, interaction = Linear))

})

test_that("The default sampling option for cicc_AR is 'cc'", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  results_default = cicc_AR(y, t, x)
  results_cc = cicc_AR(y, t, x, sampling = 'cc')

  expect_equal( results_default$est, results_cc$est)

})

test_that("Method 1: Checking options for cicc_AR", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  expect_error(cicc_AR(y, t, x, p_upper = 1.1))

})

test_that("Method 2: Checking options for cicc_AR", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  expect_error(cicc_AR(y, t, x, length = 0))

})

test_that("Checking bootstrap works for cicc_AR", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  results1 = cicc_AR(y, t, x, no_boot = 10)
  results2 = cicc_AR(y, t, x, no_boot = 20)

  expect_false(sum((results1$ci != results2$ci))==0)

})


test_that("Checking bootstrap provides warning for abnormal data for cicc_AR", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  x[1] = 1e+10

  results = cicc_AR(y, t, x, no_boot = 10)

  expect_false(sum((results$return_code != "Success: no bootstrap sample is dropped"))==0)

})

test_that("Method 1: Checking cicc_plot options", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  results = cicc_RR(y, t, x)

  expect_error(cicc_plot(results, parameter = 'Relative Risk'))

})

test_that("Method 2: Checking cicc_plot options", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

  results = cicc_RR(y, t, x)

  expect_error(cicc_plot(results, sampling ='cr'))

})

test_that("Checking whether cicc_plot works with default options", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results = cicc_RR(y, t, x)

  expect_type(cicc_plot(results), "NULL")

})

test_that("Checking whether cicc_plot works when confidence intervals include NA", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results = cicc_RR(y, t, x)
  results$ci[1] = NA

  expect_type(cicc_plot(results), "NULL")

})

test_that("Checking whether cicc_plot works with save_plots = TRUE", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results = cicc_RR(y, t, x)

  expect_type(cicc_plot(results, save_plots = TRUE, file_name = "Rplots"), "integer")

})

test_that("Checking whether cicc_plot works for AR", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results = cicc_AR(y, t, x)

  expect_type(cicc_plot(results, parameter = 'AR'), "NULL")

})

test_that("Checking whether cicc_plot works for AR with bootstrap", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results = cicc_AR(y, t, x, no_boot = 50L)

  expect_type(cicc_plot(results, parameter = 'AR'), "NULL")

})

test_that("Checking whether cicc_plot works for AR with sampling cp", {

  y = ACS_CP$topincome
  y = as.integer(is.na(y)==FALSE)
  t = ACS_CP$baplus
  x = ACS_CP$age
  results = cicc_AR(y, t, x, sampling = 'cp')

  expect_type(cicc_plot(results, parameter = 'AR', sampling = 'cp'), "NULL")

})


test_that("Checking whether cicc_plot works for AR with sampling cp and bootstrap", {

  y = ACS_CP$topincome
  y = as.integer(is.na(y)==FALSE)
  t = ACS_CP$baplus
  x = ACS_CP$age
  results = cicc_AR(y, t, x, sampling = 'cp', no_boot = 50L)

  expect_type(cicc_plot(results, parameter = 'AR', sampling = 'cp'), "NULL")

})

# added on 7 Aug 2021

test_that("The results for AAA_DML should be different between 'pro' and 'retro'", {

  y = ciccr::ACS$topincome
  t = ciccr::ACS$baplus
  age = ciccr::ACS$age
  x = splines::bs(age, df=6) # b-splines for age
  results_pro = AAA_DML(y, t, x, 'pro', k=2)
  results_retro = AAA_DML(y, t, x, 'retro', k=2)

  expect_false( results_pro$est == results_retro$est)

})

test_that("AAA_DML: the dimension of x should be greater than 1.", {

  y = ciccr::ACS$topincome
  t = ciccr::ACS$baplus
  x = ciccr::ACS$age

  expect_error(AAA_DML(y, t, x))

})

test_that("AAA_DML: Each element of 'y' must be either 0 or 1.", {

  y = ACS$topincome
  t = ACS$baplus
  age = ciccr::ACS$age
  x = splines::bs(age, df=6) # b-splines for age
  y[1] = 2

  expect_error(AAA_DML(y, t, x))

})


test_that("AAA_DML: Each element of 't' must be either 0 or 1.", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  age = ciccr::ACS$age
  x = splines::bs(age, df=6) # b-splines for age
  t[1] = 2

  expect_error(AAA_DML(y, t, x))

})

test_that("AAA_DML: 'type' should be either 'pro' and 'retro'", {

  y = ciccr::ACS$topincome
  t = ciccr::ACS$baplus
  age = ciccr::ACS$age
  x = splines::bs(age, df=6) # b-splines for age

  expect_error(AAA_DML(y, t, x, type='random', k=2))

})

# added on 13 Feb 2023

test_that("RR: The results should be different between FG and FG_CC", {

  y = FG$flag
  t = FG$smallPractice
  x = FG$experYear
  results_rs = cicc_RR(y, t, x, sampling = 'rs')

  y = FG_CC$flag
  t = FG_CC$smallPractice
  x = FG_CC$experYear
  results_cc = cicc_RR(y, t, x, sampling = 'cc')

  expect_false(sum((results_rs$est != results_cc$est))==0)

})

test_that("AR: The results should be different between FG and FG_CC", {

  y = FG$flag
  t = FG$smallPractice
  x = FG$experYear
  results_rs = avg_AR_logit(y, t, x, sampling = 'rs')

  y = FG_CC$flag
  t = FG_CC$smallPractice
  x = FG_CC$experYear
  results_cc = avg_AR_logit(y, t, x, sampling = 'cc')

  expect_false(sum((results_rs$est != results_cc$est[2]))==0)

})

test_that("The results for avg_AR_logit under random sampling should be different between interaction = TRUE and FALSE", {

  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age
  results1 = avg_AR_logit(y, t, x, sampling = 'rs', interaction = FALSE)
  results2 = avg_AR_logit(y, t, x, sampling = 'rs', interaction = TRUE)

  expect_false(sum((results1$est != results2$est))==0)

})

