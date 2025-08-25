library(seminr)
set.seed(123)
corp_rep <- corp_rep_data

# Create measurement model ----
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Create structural model ----
sm_one <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA")),
  paths(from = c("CUSA"), to = c("CUSL")))

sm_two <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))


model_one <- estimate_pls(
  data = corp_rep,
  measurement_model = corp_rep_mm,
  structural_model  = sm_one,
  missing = mean_replacement,
  missing_value = "-99")

model_two<- estimate_pls(
  data = corp_rep,
  measurement_model = corp_rep_mm,
  structural_model  = sm_two,
  missing = mean_replacement,
  missing_value = "-99")

# Function to compare the Loss of two models
Results1 <- assess_cvpat_compare(established_model = model_one,
                                 alternative_model = model_two,
                                 testtype = "two.sided",
                                 nboot = 2000,
                                 technique = predict_EA,
                                 cores = 1)

# Assess the base model ----
Results2 <- as.data.frame(assess_cvpat(model_one, technique = predict_EA, cores = 1))


## Output originally created using following lines
# write.csv(cbind(Results1,Results2), file = "tests/fixtures/cvpat1.csv")

# Load controls
cvpat_control <- as.matrix(read.csv(file = paste(test_folder,"cvpat1.csv", sep = ""), row.names = 1))

# Testing
test_that("CVPAT compares models correctly\n", {
  expect_equal(as.numeric(unlist(Results1)), as.numeric(cvpat_control[,1:5]), tolerance = 0.1)
})

# test_that("CVPAT assesses model correctly\n", {
#   expect_equal(as.numeric(unlist(Results2)), as.numeric(cvpat_control[,6:15]), tolerance = 0.1)
# })

# Now check that CVPAT can generalize ----
# Higher Composite ----
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  higher_composite("COKE",c("COMP", "LIKE"),two_stage ),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Create structural model ----
sm_one <- relationships(
  paths(from = c("COKE"), to = c("CUSA")),
  paths(from = c("CUSA"), to = c("CUSL")))

sm_two <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))


model_one <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm,
  structural_model  = sm_one,
  missing = mean_replacement,
  missing_value = "-99")

model_two <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm,
  structural_model  = sm_two,
  missing = mean_replacement,
  missing_value = "-99")

# Function to compare the Loss of two models
Results1 <- assess_cvpat_compare(established_model = model_one,
                                 alternative_model = model_two,
                                 testtype = "two.sided",
                                 nboot = 2000,
                                 technique = predict_EA,
                                 cores = 1)

# Assess the base model ----
Results2 <- assess_cvpat(model_one, technique = predict_EA, cores = 1)

test_that("CVPAT_compare can handle higher_composite\n", {
  expect_null(Results1)
})

test_that("CVPAT_assess can handle higher_composite\n", {
  expect_null(Results2)
})
