# Generates forest plots for coxph model (more than one if strata isn't null)
plot_forest <-
  function(formula, data, strata = NULL, title, ...) {
    plots <- list()
    
    if (!is.null(strata)) {
      # strategy: remake formula coxph with strata removed (using call and grep, 'optional:(+\w*)?strata\(.+\)'
      # with '') then do several ggforests, using the strata to filter the *data* off
      
      fit_expression <- deparse(formula$call)
      fit_expression <-
        str2expression(gsub("\\+?\\s?(strata\\(.+\\)) ", "", fit_expression))
      
      fit_strataless <- eval(fit_expression, data)
      
      forest_plots <- list()
      class(forest_plots) <- "vsdstrata"
      
      forest_plots$all <-
        survminer::ggforest(fit_strataless, data, main = title, ...)
      
      forest_plots$strata <- list()
      
      for (i in levels(strata)) {
        # does a forest for each strata, separatedly!
        subdata <- data[strata == i, ]
        fit_expression[[1]]$data <- subdata
        
        forest_plots$strata[[i]] <-
          survminer::ggforest(eval(fit_expression),
                              subdata,
                              main = paste(title, i, sep = ", "),
                              ...)
      }
      
      plots$forest <- forest_plots
    } else {
      plots$forest <- survminer::ggforest(formula, data, main = title)
    }
    
    return(plots)
  }
