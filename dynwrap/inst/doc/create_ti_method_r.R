## ----setup--------------------------------------------------------------------
library(dynwrap)

# dyncli is not installed on travis or CRAN, so don't run some parts of the vignette there
NOT_CRAN <- Sys.getenv("NOT_CRAN") == "" || identical(tolower(Sys.getenv("NOT_CRAN")), "true")
NOT_TRAVIS <- !identical(tolower(Sys.getenv("TRAVIS")), "true")

## -----------------------------------------------------------------------------
definition <- definition(
  method = def_method(
      id = "comp1"
    ),
  parameters = def_parameters(
    dynparam::integer_parameter(
      id = "component",
      default = 1,
      distribution = dynparam::uniform_distribution(1, 10),
      description = "The nth component to use"
    )
  ),
  wrapper = def_wrapper(
    input_required = "expression",
    input_optional = "start_id"
  )
)

## -----------------------------------------------------------------------------
run_fun <- function(expression, priors, parameters, seed, verbose) {
  pca <- prcomp(expression)

  pseudotime <- pca$x[, parameters$component]

  # flip pseudotimes using start_id
  if (!is.null(priors$start_id)) {
    if(mean(pseudotime[start_id]) > 0.5) {
      pseudotime <- 1-pseudotime
    }
  }

  dynwrap::wrap_data(cell_ids = rownames(expression)) %>%
    dynwrap::add_linear_trajectory(pseudotime = pseudotime)
}

## -----------------------------------------------------------------------------
ti_comp1 <- create_ti_method_r(definition, run_fun, package_loaded = "dplyr")

## -----------------------------------------------------------------------------
dataset <- dynwrap::example_dataset
trajectory <- infer_trajectory(dataset, ti_comp1())

## ----eval=FALSE---------------------------------------------------------------
# library(dynplot)
# # for now, install from github using:
# # remotes::install_github("dynverse/dynplot")
# plot_graph(trajectory)
# plot_heatmap(trajectory, expression_source = dataset$expression)

## -----------------------------------------------------------------------------
#' Infer a trajectory from the first principal component
#' 
#' @eval dynwrap::generate_parameter_documentation(definition)
#' 
#' @import dplyr
#' @export
#' 
#' @examples
#' dataset <- dynwrap::example_dataset
#' model <- dynwrap::infer_trajectory(dataset, ti_comp1())
ti_comp1 <- create_ti_method_r(definition, run_fun)

## -----------------------------------------------------------------------------
generate_parameter_documentation(definition)

