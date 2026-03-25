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

## ----data-exploration, cache=FALSE--------------------------------------------
# The required files
rf <- c(
  "test.txt",
  "validate.txt",
  "validate-clean.txt",
  "test-clean.txt"
)
# The test environment is setup
ed <- setup_env(rf, ve)

# The DataAnalyzer object is created
da <- DataAnalyzer$new(ve = ve)
# Information on all text files in the ed folder is returned
fi <- da$get_file_info(ed)
# The file information is printed
print(fi)

# The test environment is cleaned up
clean_up(ve)

## ----data-sampling-1, cache=FALSE---------------------------------------------
# The required files
rf <- c("input.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The sample size as a proportion of the input.txt file
ssize <- 0.1
# The data file path
dfp <- paste0(ed, "/input.txt")

# The object size is formatted
obj_size <- file.size(dfp) / 10^6
# The proportion of data to sample
prop <- (ssize / obj_size)
# An object of class DataSampler is created
ds <- DataSampler$new(dir = ed, ve = ve)
# The sample file is generated.
# The randomized sample is saved to the file train.txt in the ed folder
ds$generate_sample(
    fn =  "input.txt",
    ss = prop,
    ic = F,
    ir = T,
    ofn = "train.txt",
    is = T
)

# The test environment is cleaned up
clean_up(ve)

## ----data-sampling-2, cache=FALSE---------------------------------------------
# The required files
rf <- c("input.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# An object of class DataSampler is created
ds <- DataSampler$new(dir = ed, ve = ve)
# The train, test and validation files are generated
ds$generate_data(
    fn =  "input.txt",
    percs = list(
        "train" = 0.8,
        "test" = 0.1,
        "validate" = 0.1
    )
)

# The test environment is cleaned up
clean_up(ve)

## ----data-cleaning, cache=FALSE-----------------------------------------------
# The required files
rf <- c("input.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The data file path
fn <- paste0(ed, "/input.txt")
# The clean file path
cfn <- paste0(ed, "/input-clean.txt")
# The data cleaning options
dc_opts <- list(
    "min_words" = 2,
    "to_lower" = T,
    "remove_stop" = F,
    "remove_punct" = T,
    "remove_non_dict" = T,
    "remove_non_alpha" = T,
    "remove_extra_space" = T,
    "remove_bad" = F,
    "output_file" = cfn
)
# The data cleaner object is created
dc <- DataCleaner$new(fn, dc_opts, ve = ve)
# The sample file is cleaned and saved as input-clean.txt in the ed dir
dc$clean_file()

# The test environment is cleaned up
clean_up(ve)

## ----tokenization-1, cache=FALSE----------------------------------------------
# The required files
rf <- c("test-clean.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The test file path
fn <- paste0(ed, "/test-clean.txt")
# The n-grams are generated
for (n in 1:4) {
  # The ngram number is set
  tg_opts <- list("n" = n, "save_ngrams" = T, dir = ed)
  # The TokenGenerator object is created
  tg <- TokenGenerator$new(fn, tg_opts, ve = ve)
  # The ngram tokens are generated
  tg$generate_tokens()
}

# The test environment is cleaned up
clean_up(ve)

## ----tokenization-2, cache=FALSE, out.width="70%", out.height="70%"-----------
# The required files
rf <- c("n2.RDS")
# The test environment is setup
ed <- setup_env(rf, ve)

# The ngram file name
fn <- paste0(ed, "/n2.RDS")
# The DataAnalyzer object is created
da <- DataAnalyzer$new(fn, ve = ve)
# The top features plot is checked
df <- da$plot_n_gram_stats(opts = list(
    "type" = "top_features",
    "n" = 10,
    "save_to" = "png",
    "dir" = "./reference/figures"
))

# The output file path
fn <- paste0("./reference/figures/top_features.png")
knitr::include_graphics(fn)

# The test environment is cleaned up
clean_up(ve)

## ----tokenization-3, cache=FALSE, out.width="70%", out.height="70%"-----------
# The required files
rf <- c("n2.RDS")
# The test environment is setup
ed <- setup_env(rf, ve)

# The ngram file name
fn <- paste0(ed, "/n2.RDS")
# The DataAnalyzer object is created
da <- DataAnalyzer$new(fn, ve = ve)
# The top features plot is checked
df <- da$plot_n_gram_stats(opts = list(
    "type" = "coverage",
    "n" = 10,
    "save_to" = "png",
    "dir" = "./reference/figures"
))

# The output file path
fn <- paste0("./reference/figures/coverage.png")
knitr::include_graphics(fn)

# The test environment is cleaned up
clean_up(ve)

## ----tokenization-4, cache=FALSE----------------------------------------------
# The required files
rf <- c("n2.RDS")
# The test environment is setup
ed <- setup_env(rf, ve)

# The ngram file name
fn <- paste0(ed, "/n2.RDS")
# The DataAnalyzer object is created
da <- DataAnalyzer$new(ve = ve)
# Bi-grams starting with "and_" are returned
df <- da$get_ngrams(fn = fn, c = 10, pre = "^and_*")
# The data frame is sorted by frequency
df <- df[order(df$freq, decreasing = T), ]
# The first 10 rows of the data frame are printed
knitr::kable(df[1:10, ], col.names = c("Prefix", "Frequency"))

# The test environment is cleaned up
clean_up(ve)

## ----transition-probabilities, cache=FALSE------------------------------------
# The required files
rf <- c("n1.RDS", "n2.RDS", "n3.RDS", "n4.RDS")
# The test environment is setup
ed <- setup_env(rf, ve)
# The TPGenerator object is created
tp <- TPGenerator$new(opts = list(n = 4, dir = ed), ve = ve)
# The combined transition probabilities are generated
tp$generate_tp()

# The test environment is cleaned up
clean_up(ve)

## ----generate-model, results='hide', cache=FALSE------------------------------
# The required files
rf <- c("input.txt")
# The test environment is setup
ed <- setup_env(rf, ve)

# The following code generates n-gram model using default options for data
# cleaning and tokenization. See the following section on how to customize these
# options. Note that input.txt is the name of the input data file. It should be
# present in the data directory. dir is the directory containing the input and
# output files. It is set to the path of the environment directory, ed.

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

# Generates n-gram model. The output is the file def-model.RDS
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
rf <- c("def-model.RDS")
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

