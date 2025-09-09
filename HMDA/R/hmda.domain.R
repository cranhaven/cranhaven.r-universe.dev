#' @title compute and plot weighted mean SHAP contributions at group level (factors or domains)
#' @description This function applies different criteria to visualize SHAP contributions
#' @param shapley object of class 'shapley', as returned by the 'shapley' function
#' @param plot character, specifying the type of the plot, which can be either
#'            'bar', 'waffle', or 'shap'. The default is 'bar'.
#' @param domains character list, specifying the domains for grouping the features'
#'                contributions. Domains are clusters of features' names, that
#'                can be used to compute WMSHAP at higher level, along with
#'                their 95% confidence interval. This computation can be used to
#'                better understand how a cluster of features influence the
#'                outcome. Note that either of 'features' or 'domains' arguments
#'                can be specified at the time.
#' @param legendstyle character, specifying the style of the plot legend, which
#'                    can be either 'continuous' (default) or 'discrete'. the
#'                    continuous legend is only applicable to 'shap' plots and
#'                    other plots only use 'discrete' legend.
#' @param scale_colour_gradient character vector for specifying the color gradients
#'                              for the plot.
#' @param print logical. if TRUE, the WMSHAP summary table for the given row is printed
#' @importFrom shapley shapley.domain
#' @importFrom stats na.omit aggregate formula
#' @importFrom h2o h2o.shap_summary_plot h2o.getModel
#' @importFrom ggplot2 scale_colour_gradient2 theme guides guide_legend guide_colourbar
#'             margin element_text theme_classic labs ylab xlab ggtitle
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#'   library(HMDA)
#'   library(h2o)
#'   hmda.init()
#'
#'   # Import a sample binary outcome dataset into H2O
#'   train <- h2o.importFile(
#'   "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_train_10k.csv")
#'   test <- h2o.importFile(
#'   "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_test_5k.csv")
#'
#'   # Identify predictors and response
#'   y <- "response"
#'   x <- setdiff(names(train), y)
#'
#'   # For binary classification, response should be a factor
#'   train[, y] <- as.factor(train[, y])
#'   test[, y] <- as.factor(test[, y])
#'
#'   params <- list(learn_rate = c(0.01, 0.1),
#'                  max_depth = c(3, 5, 9),
#'                  sample_rate = c(0.8, 1.0)
#'   )
#'
#'   # Train and validate a cartesian grid of GBMs
#'   hmda_grid1 <- hmda.grid(algorithm = "gbm", x = x, y = y,
#'                           grid_id = "hmda_grid1",
#'                           training_frame = train,
#'                           nfolds = 10,
#'                           ntrees = 100,
#'                           seed = 1,
#'                           hyper_params = params)
#'
#'   # Assess the performances of the models
#'   grid_performance <- hmda.grid.analysis(hmda_grid1)
#'
#'   # Return the best 2 models according to each metric
#'   hmda.best.models(grid_performance, n_models = 2)
#'
#'   # build an autoEnsemble model & test it with the testing dataset
#'   meta <- hmda.autoEnsemble(models = hmda_grid1, training_frame = train)
#'   print(h2o.performance(model = meta$model, newdata = test))
#'
#'   # compute weighted mean shap values
#'   wmshap <- hmda.wmshap(models = hmda_grid1,
#'                         newdata = test,
#'                         performance_metric = "aucpr",
#'                         standardize_performance_metric = FALSE,
#'                         performance_type = "xval",
#'                         minimum_performance = 0,
#'                         method = "mean",
#'                         cutoff = 0.01,
#'                         plot = TRUE)
#'
#'   # define domains to combine their WMSHAP values
#'   # =============================================
#'   #
#'   # There are different ways to specify a cluster of features or even
#'   # a group of factors that touch on a broader domain. HMDA includes
#'   # exploratory factor analysis procedure to help with this process
#'   # (see ?hmda.efa function). Here, "assuming" that we have good reasons
#'   # to combine some of the features under some clusters:
#'
#'   domains = list(Group1 = c("x22", "x18", "x14", "x1", "x10", "x4"),
#'                  Group2 = c("x25", "x23", "x6", "x27"),
#'                  Group3 = c("x28", "x26"))
#'
#'   hmda.domain(shapley = wmshap,
#'               plot = "bar",
#'               domains = domains,
#'               print = TRUE)
#' }
#' @export
#' @author E. F. Haghish


hmda.domain <- function(shapley,
                        domains,
                        plot = "bar",
                        legendstyle = "continuous",
                        scale_colour_gradient = NULL, #this is a BUG because it is not implemented
                        # COLORCODE IS MISSING :(
                        print = FALSE) {

  return(
    shapley.domain(shapley = shapley,
             domains = domains,
             plot = plot,
             legendstyle = legendstyle,
             scale_colour_gradient = scale_colour_gradient, #this is a BUG because it is not implemented
             # COLORCODE IS MISSING :(
             print = print)
  )
}
