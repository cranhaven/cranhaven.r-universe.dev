## ----echo=FALSE, results='hide'-----------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.path = "reference/figures/"
)

## ----setup, echo=FALSE, results='hide', message=FALSE-------------------------
library(wordpredictor)

# The level of verbosity in the information messages
ve <- 0

#' @description
#' Used to setup the test environment
#' @param rf The required files.
#' @param ve The verbosity level.
#' @return The list of directories in the test environment
setup_env <- function(rf, ve) {
    # An object of class EnvManager is created
    em <- EnvManager$new(rp = "../", ve = ve)
    # The required files are downloaded
    ed <- em$setup_env(rf)

    return(ed)
}

#' @description
#' Used to clean up the test environment
clean_up <- function(ve) {
    # An object of class EnvManager is created
    em <- EnvManager$new(ve = ve)
    # The test environment is removed
    em$td_env(F)
}

## ----example-prerequisite, echo=TRUE, message=FALSE, results='hide'-----------
library(wordpredictor)

# The level of verbosity in the information messages
ve <- 0

#' @description
#' Used to setup the test environment
#' @param rf The required files.
#' @param ve The verbosity level.
#' @return The list of directories in the test environment
setup_env <- function(rf, ve) {
    # An object of class EnvManager is created
    em <- EnvManager$new(rp = "../", ve = ve)
    # The required files are downloaded
    ed <- em$setup_env(rf)

    return(ed)
}

#' @description
#' Used to clean up the test environment
clean_up <- function(ve) {
    # An object of class EnvManager is created
    em <- EnvManager$new(ve = ve)
    # The test environment is removed
    em$td_env(F)
}

## ----generate-model, results='hide', cache=FALSE------------------------------
# The required files
rf <- c("input.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The following code generates n-gram model using default options for data
# cleaning and tokenization. See the following section on how to customize these
# options. Note that input.txt is the name of the input data file. It should be
# present in the ed directory. The generated model file is also placed in this
# directory.

# ModelGenerator class object is created
mg <- ModelGenerator$new(
    name = "def-model",
    desc = "N-gram model generating using default options",
    fn = "def-model.RDS",
    df = "input.txt",
    n = 4,
    ssize = 0.1,
    dir = ed,
    dc_opts = list(),
    tg_opts = list(),
    ve = ve
)

# Generates n-gram model. The output is the file
# ./data/model/def-model.RDS
mg$generate_model()

# The test environment is cleaned up
clean_up(ve)

## ----model-evaluation-1, cache=FALSE------------------------------------------
# The required files
rf <- c("def-model.RDS", "validate-clean.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The model file name
mfn <- paste0(ed, "/def-model.RDS")
# The path to the cleaned validation file
vfn <- paste0(ed, "/validate-clean.txt")
# ModelEvaluator class object is created
me <- ModelEvaluator$new(mf = mfn, ve = ve)
# The intrinsic evaluation is performed on first 20 lines
stats <- me$intrinsic_evaluation(lc = 20, fn = vfn)

# The test environment is cleaned up
clean_up(ve)

## ----model-evaluation-2, cache=FALSE------------------------------------------
# The required files
rf <- c("def-model.RDS", "validate-clean.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The model file name
mfn <- paste0(ed, "/def-model.RDS")
# The path to the cleaned validation file
vfn <- paste0(ed, "/validate-clean.txt")
# ModelEvaluator class object is created
me <- ModelEvaluator$new(mf = mfn, ve = ve)
# The intrinsic evaluation is performed on first 100 lines
stats <- me$extrinsic_evaluation(lc = 100, fn = vfn)

# The test environment is cleaned up
clean_up(ve)

## ----predict-word, cache=FALSE------------------------------------------------
# The required files
rf <- c("def-model.RDS", "validate-clean.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The model file name
mfn <- paste0(ed, "/def-model.RDS")
# An object of class ModelPredictor is created. The mf parameter is the name of
# the model file that was generated in the previous example.
mp <- ModelPredictor$new(mf = mfn, ve = ve)
# Given the words: "how are", the next word is predicted. The top 3 most likely
# next words are returned along with their respective probabilities.
res <- mp$predict_word(words = "how are", 3)
# The test environment is cleaned up
clean_up(ve)

