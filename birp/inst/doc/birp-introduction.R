## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, # merges source code and its printed output into a single block
  comment = ">" # Adds > to every line of printed output; visually distinguishing it from the code that generated it
)

## ----setup--------------------------------------------------------------------
library(birp)

## ----echo=FALSE---------------------------------------------------------------
# Create example data
exampleData <- data.frame(
  location = rep(c("site1", "site2", "site3"), each = 5),
  timepoint = rep(2020:2024, times = 3),
  counts    = c(28, 12, 26, 48, 20, 21, 22, 76, 22, 100, 65, 32, 60, 19, 42),
  effort   = c(2,1,2,3,1,1,1,4,1,4,5,2,5,1,2),
  CI_group    = rep("Group_1", times = 15)
)

## ----echo=FALSE---------------------------------------------------------------
print(exampleData)

## ----eval=FALSE---------------------------------------------------------------
# # Create example data
# exampleData <- data.frame(
#   location = rep(c("site1", "site2", "site3"), each = 5),
#   timepoint = rep(2020:2024, times = 3),
#   counts    = c(28, 12, 26, 48, 20, 21, 22, 76, 22, 100, 65, 32, 60, 19, 42),
#   effort   = c(2,1,2,3,1,1,1,4,1,4,5,2,5,1,2),
#   CI_group    = rep("Group_1", times = 15)
# )

## -----------------------------------------------------------------------------
exampleBirp <- birp_data_from_data_frame(exampleData)
print(exampleBirp)

## ----echo=FALSE---------------------------------------------------------------
est <- birp(exampleBirp, verbose=FALSE)

## ----eval=FALSE---------------------------------------------------------------
# est <- birp(exampleBirp)

## -----------------------------------------------------------------------------
print(est)

## ----echo=FALSE---------------------------------------------------------------
cat("Posterior mean of gamma: ", est$post_gamma$posterior_mean, "\n")
cat("Posterior probability of increasing trend P(gamma > 0): ", est$post_gamma$prob_positive, "\n")

## ----fig.width=6, fig.height=4------------------------------------------------
plot(est)

## ----fig.width=6, fig.height=4------------------------------------------------
plot(est, col="deeppink", legend=NA, ylab = "Density of posterior estimates")

## -----------------------------------------------------------------------------
est <- birp(exampleBirp, verbose = FALSE, timesOfChange = 2023)
print(est)

## ----echo=FALSE---------------------------------------------------------------
cat("Posterior probability of increasing trend P(gamma > 0): ", est$post_gamma$prob_positive, "\n")

## ----echo=TRUE----------------------------------------------------------------
rate_design <- matrix(c("Group_1", 1,2), nrow = 1)
print(rate_design)
step_design <- matrix(c("Group_1", 1,0), nrow = 1)
print(step_design)

## ----echo=TRUE----------------------------------------------------------------
est <- birp(exampleBirp,
            verbose = FALSE,
            timesOfChange = 2023,
            rate_design = rate_design,
            step_design = step_design,
            change = "both")
print(est)

## -----------------------------------------------------------------------------
exampleBACI <- data.frame(
  location = rep(c("ctrl1", "ctrl2", "ctrl3", "int1", "int2", "int3"), each = 10),
  timepoint = rep(c(2015, 2016, 2018:2024, 2026), times = 6),
  counts = c(
    80, 76, 72, 68, 65, 62, 59, 56, 53, 50,
    60, 57, 54, 51, 49, 46, 44, 42, 40, 38,
    70, 66, 63, 60, 57, 54, 51, 49, 46, 44,
    80, 76, 72, 68, 65, 80, 95, 113, 135, 160,
    60, 57, 54, 51, 49, 60, 72,  86, 103, 123,
    70, 66, 63, 60, 57, 70, 84, 100, 120, 143
  ),
  effort = rep(3, times = 60),
  CI_group = rep(c("Control", "Intervention"), each = 30)
)
birpBACI <- birp_data_from_data_frame(exampleBACI)

## -----------------------------------------------------------------------------
rate_design_CI <- matrix(c(
  "Control",      1,
  "Intervention", 2
), nrow = 2, byrow = TRUE)

estCI <- birp(birpBACI,
              change = "rate",
              rate_design = rate_design_CI,
              verbose = FALSE)
print(estCI)

## -----------------------------------------------------------------------------
rate_design_BACI <- matrix(c(
  "Control",      1, 1,
  "Intervention", 1, 2
), nrow = 2, byrow = TRUE)

est_BACI <- birp(birpBACI,
                 change = "rate",
                 rate_design = rate_design_BACI,
                 timesOfChange = 2020,
                 verbose = FALSE)
print(est_BACI)

## -----------------------------------------------------------------------------
plot(est_BACI)

## ----fig.width=6, fig.height=4------------------------------------------------
plot(est_BACI)

## ----fig.width=6, fig.height=4------------------------------------------------
plot_epoch_pair(est_BACI, col="navy")

## -----------------------------------------------------------------------------
# Access the path to the example data provided with the package
pathToFiles <- system.file("extdata", package = "birp")

# Read in both files
data <- birp_data_from_file(filenames = c(
  file.path(pathToFiles, "cameraTrapData.csv"),
  file.path(pathToFiles, "trackData.csv")
))

## ----eval=FALSE---------------------------------------------------------------
# data <- birp_data_from_file(filenames = c(
#   "path/to/your/file/cameraTrapData.csv",
#   "path/to/your/file/trackData.csv"
# ))

## -----------------------------------------------------------------------------
estMultiMethod <- birp(data, verbose=FALSE)
print(estMultiMethod)

## -----------------------------------------------------------------------------
exampleData <- data.frame(
  location = rep(c("site1", "site2", "site3"), each = 5),
  timepoint = rep(2020:2024, times = 3),
  counts    = c(28, 12, 26, 48, 20, 21, 22, 76, 22, 100, 65, 32, 60, 19, 42),
  effort   = c(2,1,2,3,1,1,1,4,1,4,5,2,5,1,2),
  CI_group    = rep("Group_1", times = 15)
)
exampleBirp <- birp_data_from_data_frame(exampleData)
fit_nb <- birp(data = exampleBirp,
               negativeBinomial = TRUE,
               verbose = FALSE)
print(fit_nb)

## -----------------------------------------------------------------------------
exampleBirp <- birp_data_from_data_frame(exampleData)
est <- birp(exampleBirp, negativeBinomial = TRUE, verbose=FALSE)
res_assess <- assess_NB(est, numRep = 100, verbose=FALSE)

## -----------------------------------------------------------------------------
exampleBirp <- birp_data_from_data_frame(exampleData)
fit_stoch <- birp(data = exampleBirp,
                  stochastic = TRUE,
                  verbose = FALSE)

## -----------------------------------------------------------------------------
detectionExample_data <- data.frame(
  location  = rep(c("site1", "site2"), each = 5),
  timepoint = rep(2015:2019, times = 2),
  counts    = sample(10:100, 10, replace = TRUE),
  effort    = sample(1:5, 10, replace = TRUE),
  CI_group  = rep("Group_1", 10),
  covDetection_1 = runif(10, 0, 1)      # random values between 0 and 1
)

## -----------------------------------------------------------------------------
detectionBirp <- birp_data_from_data_frame(detectionExample_data)

## ----fig.width=6, fig.height=4------------------------------------------------
detectionEst <- birp(detectionBirp, assumeTrueDetectionProbability=FALSE, verbose = FALSE)

## ----fig.width=6, fig.height=4------------------------------------------------
plot(detectionEst)

## -----------------------------------------------------------------------------
simData <- simulate_birp(gamma = c(-0.03, 0.03),
                      timepoints = 2000:2020,
                      timesOfChange = 2010,
                      verbose = FALSE)

## -----------------------------------------------------------------------------
est <- birp(simData, verbose=FALSE, timesOfChange = 2010)
print(est)

