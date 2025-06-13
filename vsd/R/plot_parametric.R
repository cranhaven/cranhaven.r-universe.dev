# Generates parametric graph, with KM on the background
#' @importFrom magrittr %>%
plot_parametric <-
  function(model,
           km_fit,
           strata = NULL,
           data,
           alpha,
           conf.int,
           conf.int.km,
           size,
           ...) {
    if (missing(conf.int)) {
      conf.int <- TRUE
    }

    plots <- list()

    summary <- summary(model)
    if (!is.factor(strata)) {
      plots$parametric <- do.call(survminer::ggflexsurvplot,
                                  append(
                                    list(
                                      model,
                                      data,
                                      size = size,
                                      alpha = alpha,
                                      conf.int = conf.int,
                                      conf.int.km = conf.int.km
                                    ),
                                    list(...)
                                  ))

      # summary <- summary[[1]] %>% dplyr::mutate(strata = "All")
    } else {
      for (level in levels(strata)) {
        summary[[level]] <-
          summary[[level]] %>% dplyr::mutate(strata = level)
      }

      summary <- do.call(rbind, summary)

      plot_fit <-
        do.call(survminer::ggsurvplot, append(
          list(
            km_fit,
            data,
            alpha = alpha / 2,
            size = size,
            conf.int = conf.int.km
          ),
          list(...)
        ))$plot

      plot_parametric <-
        plot_fit + geom_line(aes(.data$time, .data$est, color = .data$strata),
                             data = summary,
                             size = size)

      if (conf.int) {
        plot_parametric <- plot_parametric +
          geom_line(
            aes(.data$time, .data$lcl, color = .data$strata),
            data = summary,
            size = size / 2,
            linetype = "dashed"
          ) +
          geom_line(
            aes(.data$time, .data$ucl, color = .data$strata),
            data = summary,
            size = size / 2,
            linetype = "dashed"
          )
      }
      plots$parametric <- plot_parametric

    }

    return(plots)
  }
