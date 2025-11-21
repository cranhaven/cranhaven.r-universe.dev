

#' Interactive Probability Plot.
#'
#' This function creates an interactive probability plot for a wblr object.
#' It can include confidence bounds, suspension data, and a results table.
#'
#' @param wblr_obj An object of class 'wblr'. This is a required argument.
#' @param susp An optional numeric vector of suspension data. Default is NULL.
#' @param showConf Show the confidence bounds (TRUE) or not (FALSE). Default is TRUE if
#' confidence bounds are available in the wblr object.
#' @param showSusp Show the suspensions plot (TRUE) or not (FALSE). Default is TRUE
#' if susp is provided.
#' @param showRes Show the results table (TRUE) or not (FALSE). Default is TRUE.
#' @param showGrid Show grid (TRUE) or hide grid (FALSE). Default is TRUE.
#' @param main Main title. Default is 'Probability Plot'.
#' @param xlab X-axis label. Default is 'Time to Failure'.
#' @param ylab Y-axis label. Default is 'Probability'.
#' @param probCol Color of the probability values. Default is 'black'.
#' @param fitCol Color of the model fit. Default is 'black'.
#' @param confCol Color of the confidence bounds. Default is 'black'.
#' @param intCol Color of the intervals for interval censored models. Default is 'black'.
#' @param gridCol Color of the grid. Default is 'lightgray'.
#' @param signif Significant digits of results. Default is 3. Must be a positive integer.
#' @return The function returns no value. It creates an interactive probability plot.
#' @examples
#' library(WeibullR)
#' library(WeibullR.plotly)
#' failures<-c(30, 49, 82, 90, 96)
#' obj<-wblr.conf(wblr.fit(wblr(failures)))
#' plotly_wblr(obj)
#'
#' suspensions<-c(100, 45, 10)
#' obj<-wblr.conf(wblr.fit(wblr(failures, suspensions)))
#' plotly_wblr(obj, suspensions, fitCol = 'blue', confCol
#' = 'blue')
#' inspection_data <- data.frame(left=c(0, 6.12, 19.92, 29.64, 35.4, 39.72, 45.32, 52.32),
#'                            right=c(6.12, 19.92, 29.64, 35.4, 39.72, 45.32, 52.32, 63.48),
#'                            qty=c(5, 16, 12, 18, 18, 2, 6, 17))
#' suspensions <- data.frame(time = 63.48, event = 0, qty = 73)
#' obj <- wblr(suspensions, interval = inspection_data)
#' obj <- wblr.fit(obj, method.fit = "mle")
#' obj <- wblr.conf(obj, method.conf = "fm", lty = 2)
#' suspensions <- as.vector(suspensions$time)
#' plotly_wblr(obj, susp = suspensions, fitCol = 'red', confCol = 'red', intCol = 'blue',
#'         main = 'Parts Cracking Inspection Interval Analysis',
#'         ylab =  'Cumulative % Cracked', xlab='Inspection Time')
#' failures <- c(25, 30, 42, 49, 55, 67, 73, 82, 90, 96, 101, 110, 120, 132, 145)
#' fit <- wblr.conf(wblr.fit(wblr(failures), dist = "weibull3p"))
#' plotly_wblr(fit, fitCol='darkgreen', confCol = 'darkgreen')
#'
#' @import WeibullR
#' @import plotly
#' @importFrom graphics text
#' @importFrom stats runif qnorm
#' @export

plotly_wblr <- function(wblr_obj,
                        susp = NULL,
                        showConf = TRUE,
                        showSusp = TRUE,
                        showRes = TRUE,
                        showGrid = TRUE,
                        main = "Probability Plot",
                        xlab = "Time to Failure",
                        ylab = "Probability",
                        probCol = "black",
                        fitCol = "black",
                        confCol = "black",
                        intCol = "black",
                        gridCol = "lightgray",
                        signif = 3) {

  # Validate inputs
  validate_inputs <- function() {
    if (!identical(class(wblr_obj), "wblr")) {
      stop("Argument 'wblr_obj' is not of class 'wblr'.")
    }
    if (!is.null(susp) && !is.numeric(susp)) {
      stop("Argument 'susp' must be a numeric vector.")
    }
  }

  # Get time & probability data
  get_time_data <- function(obj, signif) {
    if (obj$interval == 0) {
      time <- obj$data$dpoints$time
      ints <- NULL
      prob <- obj$data$dpoints$ppp
    } else {
      t1 <- obj$data$dlines$t1
      t2 <- obj$data$dlines$t2
      time <- (t1 + t2) / 2
      ints <- log(t2 - t1)
      prob <- obj$data$dlines$ppp
    }
    list(
      time = time,
      time_sd = round(time, signif),
      ints = ints,
      prob = prob,
      prob_sd = round(prob, signif)
    )
  }

  # Get confidence‐bound data
  get_conf_data <- function(obj, signif, showConf) {
    fit <- obj$fit[[1]]
    conf <- fit$conf[[1]]$bounds
    if (is.null(fit) || is.null(conf)) {
      # no fit or no conf → everything NULL
      out <- rep(list(NULL), 8)
      names(out) <- c("datum","unrel","lower","upper",
                      "datum_sd","unrel_sd","lower_sd","upper_sd")
      return(out)
    }

    datum <- conf$Datum
    unrel <- conf$unrel

    if (!showConf) {
      lower <- upper <- NULL
    } else {
      lower <- conf$Lower
      upper <- conf$Upper
    }

    list(
      datum = datum,
      unrel = unrel,
      lower = lower,
      upper = upper,
      datum_sd = round(datum, signif),
      unrel_sd = round(unrel, signif),
      lower_sd = if (!is.null(lower)) round(lower, signif),
      upper_sd = if (!is.null(upper)) round(upper, signif)
    )
  }

  # Get distribution params & transforms
  get_dist_params <- function(obj, signif, probability, unrel) {
    fit_opts <- obj$fit[[1]]$options
    vec <- as.numeric(obj$fit[[1]]$fit_vec)

    switch(
      fit_opts$dist,
      lognormal = {
        params <- c("Mulog", "Sigmalog", NULL)
        values <- c(round(vec[1], signif), round(vec[2], signif), NULL)
        prob_tr <- qnorm(probability)
        unrel_tr <- qnorm(unrel)
        list(params = params, values = values,
             prob_trans = prob_tr, unrel_trans = unrel_tr)
      },
      weibull = ,
      weibull3p = {
        # note: for weibull3p, vec[3] is Gamma
        params <- c("Beta", "Eta", if (fit_opts$dist=="weibull3p") "Gamma" else NULL)
        vals <- c(round(vec[2], signif), round(vec[1], signif),
                  if (fit_opts$dist=="weibull3p") round(vec[3], signif) else NULL)
        prob_tr <- log(1/(1 - probability))
        unrel_tr <- log(1/(1 - unrel))
        list(params = params, values = vals,
             prob_trans = prob_tr, unrel_trans = unrel_tr)
      },
      stop("Unsupported distribution: ", fit_opts$dist)
    )
  }

  # Get goodness‐of‐fit metrics
  get_gof_metrics <- function(obj, signif) {
    fit_opts <- obj$fit[[1]]$options
    gof <- obj$fit[[1]]$gof

    if (is.null(fit_opts$method.fit)) return(list(methlab = NULL, methval = NULL))

    if (fit_opts$method.fit == "rr-xony") {
      list(methlab = "R^2", methval = round(gof$r2,        signif))
    } else if (fit_opts$method.fit == "mle") {
      list(methlab = "Loglikelihood", methval = round(gof$loglik, signif))
    } else {
      list(methlab = NULL, methval = NULL)
    }
  }

  # Extract all data from the wblr object
  extract_data <- function(wblr_obj, signif = 3, showConf = TRUE) {
    # times & probs
    tp <- get_time_data(wblr_obj, signif)

    # confidence bounds
    cd <- get_conf_data(wblr_obj, signif, showConf)

    # dist params & transforms
    dp <- if (is.null(wblr_obj$fit)) {
      list(params = NULL, values = NULL,
           prob_trans = NULL, unrel_trans = NULL)
    } else {
      get_dist_params(wblr_obj, signif, tp$prob, cd$unrel)
    }

    # gof
    gm <- if (is.null(wblr_obj$fit)) {
      list(methlab = NULL, methval = NULL)
    } else {
      get_gof_metrics(wblr_obj, signif)
    }

    # combine everything into one list
    c(tp, cd, dp, gm)
  }

  # Create the main probability plot
  plot_prob <- function(data) {
    # Set color for confidence interval fill
    fillcolor <- plotly::toRGB(confCol, 0.2)

    # Set grid visibility
    show_grid <- is.null(showGrid) || isTRUE(showGrid)

    # Y-axis ticks (in %)
    yticks <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.2, 0.5, 1,
                2, 5, 10, 20, 50, 90, 99, 99.999)

    # Transform Y-axis ticks based on distribution type
    dist_type <- wblr_obj$fit[[1]]$options$dist
    if (dist_type == 'lognormal') {
      yticks_trans <- qnorm(yticks / 100)
      yaxis_scale <- 'linear'
    } else {
      yticks_trans <- log(1 / (1 - yticks / 100))
      yaxis_scale <- 'log'
    }

    log_datum <- log10(data$datum)
    log_unrel <- log10(data$unrel)

    xmin <- min(log_datum)
    xmax <- max(log_datum)
    ymin <- min(log10(data$unrel))
    ymax <- max(log10(yticks_trans))

    # Main scatter plot
    prob_plot <- plot_ly(
      x = data$time, y = data$prob_trans, type = 'scatter', mode = 'markers',
      marker = list(color = probCol), showlegend = FALSE,
      error_x = list(array = ~data$ints, color = intCol),
      name = "", text = ~paste0("Probability: (", data$time_sd, ", ", data$prob_sd, ")"),
      hoverinfo = 'text'
    ) %>%

      # Layout setup
      layout(
        title = main,
        xaxis = list(
          type = 'log', title = xlab, showline = TRUE, mirror = 'ticks',
          showgrid = show_grid, gridcolor = gridCol,
          range = list(xmin, xmax)
        ),
        yaxis = list(
          type = yaxis_scale, title = ylab, showline = TRUE, mirror = 'ticks',
          showgrid = show_grid, gridcolor = gridCol, size = text,
          range = list(ymin, ymax),
          tickvals = yticks_trans, ticktext = yticks
        )
      ) %>%

      # Best fit line
      add_trace(
        x = data$datum, y = data$unrel_trans, mode = 'markers+lines',
        marker = list(color = 'transparent'),
        line = list(color = fitCol),
        text = ~paste0("Fit: ", data$datum_sd, ", ", data$unrel_sd, ")"),
        hoverinfo = 'text'
      ) %>%

      # Lower confidence bound (invisible line)
      add_trace(
        x = data$lower, y = data$unrel_trans, mode = 'markers+lines',
        marker = list(color = 'transparent'),
        line = list(color = 'transparent'),
        text = ~paste0("Upper: ", data$lower_sd, ", ", data$unrel_sd, ")"),
        hoverinfo = 'text'
      ) %>%

      # Upper confidence bound with fill
      add_trace(
        x = data$upper, y = data$unrel_trans, mode = 'markers+lines',
        fill = 'tonexty', fillcolor = fillcolor,
        marker = list(color = 'transparent'),
        line = list(color = 'transparent'),
        text = ~paste0("Lower: ", data$upper_sd, ", ", data$unrel_sd, ")"),
        hoverinfo = 'text'
      )

    return(prob_plot)
  }

  # Create the suspensions plot
  plot_susp <- function() {
    if (!showSusp || is.null(susp)) return(NULL)

    log_datum <- log10(data$datum)
    xmin <- min(log_datum)
    xmax <- max(log_datum)

    # Random y-values for visual spacing
    susp_sd <- round(susp, signif)
    rand_y <- stats::runif(length(susp))

    plot_ly(
      x = susp, y = rand_y, type = 'scatter', mode = 'markers',
      marker = list(color = probCol), showlegend = FALSE,
      text = ~paste0("Suspension: ", susp_sd), hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(
          type = 'log', title = '', zeroline = FALSE, showline = TRUE,
          mirror = 'ticks', showticklabels = FALSE, showgrid = FALSE,
          range = list(xmin, xmax)
        ),
        yaxis = list(
          title = '', zeroline = FALSE, showline = TRUE,
          mirror = 'ticks', showticklabels = FALSE, showgrid = FALSE
        )
      )
  }

  # Build results table
  build_table <- function(data) {
    if (!showRes) return(NULL)

    # Define parameter names and their corresponding values
    params <- c(
      'Ranks', 'n', 'Failures', 'Intervals', 'Suspensions', 'Distribution',
      'Method', data$param1, data$param2, data$param3, data$methlab, 'CI', 'Type'
    )

    values <- c(
      wblr_obj$options$pp, wblr_obj$n, wblr_obj$fail, wblr_obj$interval,
      wblr_obj$cens, wblr_obj$options$dist, wblr_obj$options$method.fit,
      data$paramval1, data$paramval2, data$paramval3, data$methval, wblr_obj$options$ci,
      wblr_obj$options$method.conf
    )

    # Create data frame for table
    res <- data.frame(Params = params, Values = values, stringsAsFactors = FALSE)

    # Generate Plotly table
    plot_ly(
      type = 'table',
      domain = list(x = c(0.775, 1)),
      header = list(
        values = c("Params", "Values"),
        align = 'center',
        line = list(width = 1, color = 'black'),
        fill = list(color = "grey"),
        font = list(family = "Arial", color = "white")
      ),
      cells = list(
        values = rbind(res$Params, res$Values),
        align = 'center',
        line = list(color = "black", width = 1),
        font = list(family = "Arial", color = "black")
      )
    )
  }

  # Combine all plots
  combine_plots <- function(prob_plot, susp_plot, results_table) {

    # Helper: Remove problematic plotly layout warnings
    clean_subplot <- function(...) {
      plot <- subplot(...)
      plot$x$layout <- plot$x$layout[grep('NA', names(plot$x$layout), invert = TRUE)]
      plot
    }

    # Case 1: Only prob_plot available
    if (is.null(results_table) && is.null(susp_plot)) {
      return(prob_plot)
    }

    # Case 2: prob_plot + susp_plot only
    if (is.null(results_table) && !is.null(susp_plot)) {
      return(
        clean_subplot(prob_plot, susp_plot, nrows = 2, titleX = TRUE, titleY = TRUE) %>%
          layout(
            xaxis = list(domain = c(0, 1)),
            xaxis2 = list(domain = c(0, 1)),
            yaxis = list(domain = c(0, 0.875)),
            yaxis2 = list(domain = c(0.9, 1))
          )
      )
    }

    # Case 3: prob_plot + results_table only
    if (!is.null(results_table) && is.null(susp_plot)) {
      return(
        clean_subplot(prob_plot, results_table, titleX = TRUE, titleY = TRUE) %>%
          layout(
            xaxis = list(domain = c(0, 0.75)),
            xaxis2 = list(domain = c(0.775, 1)),
            yaxis = list(domain = c(0, 1)),
            yaxis2 = list(domain = c(0, 1))
          )
      )
    }

    # Case 4: all three plots
    clean_subplot(prob_plot, susp_plot, results_table, nrows = 2, titleX = TRUE, titleY = TRUE) %>%
      layout(
        xaxis = list(domain = c(0, 0.75)),
        xaxis2 = list(domain = c(0, 0.75)),
        xaxis3 = list(domain = c(0.775, 1)),
        yaxis = list(domain = c(0, 0.875)),
        yaxis2 = list(domain = c(0.9, 1)),
        yaxis3 = list(domain = c(0, 0.85))
      )
  }

  # Main function calls
  validate_inputs()
  data <- extract_data(wblr_obj, signif, showConf)
  p1 <- plot_prob(data)
  p2 <- plot_susp()
  t1 <- build_table(data)
  combine_plots(p1, p2, t1)

}
