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
#  # Create example data
#  exampleData <- data.frame(
#    location = rep(c("site1", "site2", "site3"), each = 5),
#    timepoint = rep(2020:2024, times = 3),
#    counts    = c(28, 12, 26, 48, 20, 21, 22, 76, 22, 100, 65, 32, 60, 19, 42),
#    effort   = c(2,1,2,3,1,1,1,4,1,4,5,2,5,1,2),
#    CI_group    = rep("Group_1", times = 15)
#  )

## -----------------------------------------------------------------------------
exampleBirp <- birp_data_from_data_frame(exampleData)
print(exampleBirp)

## ----echo=FALSE---------------------------------------------------------------
est <- birp(exampleBirp, verbose=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  est <- birp(exampleBirp)

## -----------------------------------------------------------------------------
print(est)

## ----echo=FALSE---------------------------------------------------------------
cat("Posterior probability of increasing trend P(gamma > 0): ", est$prob_gamma_positive, "\n")

## ----fig.width=6, fig.height=4------------------------------------------------
plot(est)

## ----fig.width=6, fig.height=4------------------------------------------------
plot(est, col="deeppink", legend=NA, ylab = "Density of posterior estimates")

## -----------------------------------------------------------------------------
est <- birp(exampleBirp, verbose = FALSE, timesOfChange = 2023)
print(est)

## ----echo=FALSE---------------------------------------------------------------
cat("Posterior probability of increasing trend P(gamma > 0): ", est$prob_gamma_positive, "\n")

## -----------------------------------------------------------------------------
BACI_matrix <- matrix(c(
  "A", "1", "1",
  "B", "1", "2"
), nrow = 2, byrow = TRUE)
print(BACI_matrix)

## -----------------------------------------------------------------------------
set.seed(42)
# Simulate data with 4 locations: 2 Control + 2 Intervention
sim_data <- simulate_birp(timepoints = 1:20,
  timesOfChange = 10,
  gamma = c(-0.05, 0.1),
  numLocations = 4,
  numCIGroups = 2,  # 2 CI groups: Control and Intervention
  BACI = BACI_matrix,
  verbose = FALSE) # set TRUE to see verbal output in the console

## -----------------------------------------------------------------------------
est <- birp(data = sim_data,
  timesOfChange = 10,
  BACI = BACI_matrix,
  verbose=FALSE)
print(est)

## ----fig.width=6, fig.height=4------------------------------------------------
plot(est)

## ----fig.width=6, fig.height=4------------------------------------------------
plot_epoch_pair(est, col="navy")

## -----------------------------------------------------------------------------
# Access the path to the example data provided with the package
pathToFiles <- system.file("extdata", package = "birp")

# Read in both files
data <- birp_data_from_file(filenames = c(
  file.path(pathToFiles, "cameraTrapData.csv"),
  file.path(pathToFiles, "trackData.csv")
))

## ----eval=FALSE---------------------------------------------------------------
#  data <- birp_data_from_file(filenames = c(
#    "path/to/your/file/cameraTrapData.csv",
#    "path/to/your/file/trackData.csv"
#  ))

## -----------------------------------------------------------------------------
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
example_data <- data.frame(
  location  = rep(c("site1", "site2"), each = 5),
  timepoint = rep(2015:2019, times = 2),
  counts    = sample(10:100, 10, replace = TRUE),
  effort    = sample(1:5, 10, replace = TRUE),
  CI_group  = rep("Group_1", 10),
  covDetection_1 = runif(10, 0, 1)      # random values between 0 and 1
)

## -----------------------------------------------------------------------------
dat <- birp_data_from_data_frame(example_data)

## ----fig.width=6, fig.height=4------------------------------------------------
est1 <- birp(dat, assumeTrueDetectionProbability=TRUE, verbose = FALSE)
est2 <- birp(dat, assumeTrueDetectionProbability=FALSE, verbose = FALSE)

## ----fig.width=6, fig.height=4------------------------------------------------
plot(est1)
plot(est2)

## -----------------------------------------------------------------------------
simData <- simulate_birp(gamma = c(-0.03, 0.03),
                      timepoints = 2000:2020,
                      timesOfChange = 2010,
                      verbose = FALSE)

## -----------------------------------------------------------------------------
est <- birp(simData, verbose=FALSE, timesOfChange = 2010)
print(est)

