## ----include = FALSE----------------------------------------------------------
dir <- tempdir()
knitr::opts_knit$set(root.dir = normalizePath(tempdir(), winslash = '/'))

# dyncli is not installed on travis or CRAN, so don't run some parts of the vignette there
NOT_CRAN <- Sys.getenv("NOT_CRAN") == "" || identical(tolower(Sys.getenv("NOT_CRAN")), "true")
NOT_TRAVIS <- !identical(tolower(Sys.getenv("TRAVIS")), "true")

## ----setup--------------------------------------------------------------------
library(dynwrap)

## ----echo = FALSE-------------------------------------------------------------
definition_string <- paste0(readLines(system.file("examples/script/definition.yml", package = "dynwrap")), "\n", collapse = "")

readr::write_file(definition_string, "definition.yml")
knitr::asis_output(paste0("```yaml\n", definition_string, "\n```"))

## ----echo = FALSE-------------------------------------------------------------
run_r_string <- paste0(readLines(system.file("examples/script/run.R", package = "dynwrap")), "\n", collapse = "")

readr::write_file(run_r_string, "run.R")
knitr::asis_output(paste0("```r\n", run_r_string, "\n```"))

## ----echo = FALSE-------------------------------------------------------------
run_py_script <- paste0(readLines(system.file("examples/script/run.py", package = "dynwrap")), "\n", collapse = "")

readr::write_file(run_py_script, "run.py")
knitr::asis_output(paste0("```python\n", run_py_script, "\n```"))

## ----eval=FALSE---------------------------------------------------------------
# method <- create_ti_method_definition("definition.yml", "run.R")
# dataset <- dynwrap::example_dataset
# trajectory <- infer_trajectory(dataset, method(), verbose = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# library(dynplot)
# # for now, install from github using:
# # remotes::install_github("dynverse/dynplot")
# plot_graph(trajectory)
# plot_heatmap(trajectory, expression_source = dataset$expression)

## ----error = TRUE, eval = FALSE-----------------------------------------------
try({
# trajectory <- infer_trajectory(dataset, method(), debug = TRUE)
})

## ----echo = FALSE-------------------------------------------------------------
temp_files <- c("run.R", "run.py", "definition.yml")
for (file in temp_files) {
  if (file.exists(file)) 
    file.remove(file)
}

