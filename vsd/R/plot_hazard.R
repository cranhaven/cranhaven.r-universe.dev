# Generates hazard plot
plot_hazard <- function(surv, strata = NULL, size, ...) {
  plots <- list()

  if (is.null(strata)) {
    # make a simple muhaz graphic
    hazard <- muhaz::muhaz(surv$time, surv$status)
    hazard_df <-
      data.frame(
        x = hazard$est.grid,
        y = hazard$haz.est,
        strata = factor(rep("All", length(
          hazard$est.grid
        )))
      )
  } else {
    # make several separate hazard maps
    hazard_df <-
      data.frame(x = numeric(),
                 y = numeric(),
                 strata = numeric())

    hazard_count <- table(strata)

    for (i in levels(strata)) {
      # TODO: is it always ten?
      if (hazard_count[[i]] < 10) {
        warning(
          "Level ",
          i,
          " doesn't have enough datapoints to estimate the hazard function",
          call. = FALSE,
          immediate. = TRUE
        )
      } else {
        # creates a sub-table with each muhaz graphic, appends the corresponding strata
        hazard <-
          muhaz::muhaz(surv$time, surv$status, strata == i)
        hazard_df_level <-
          data.frame(
            x = hazard$est.grid,
            y = hazard$haz.est,
            strata = rep(i, length(hazard$est.grid))
          )
        hazard_df <- rbind(hazard_df, hazard_df_level)
      }
    }

    hazard_df$strata <-
      factor(hazard_df$strata, levels(strata))
  }

  plot <-
    ggplot(hazard_df, aes(.data$x, .data$y, color = .data$strata)) +
    geom_line(size = size)

  plots$hazard <- ggpubr::ggpar(plot, ...)

  return(plots)
}
