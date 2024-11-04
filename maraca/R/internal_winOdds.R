# This function does a bit of dirty magic to distribute the values
# onto different "floors", each floor being a numeric offset that is higher
# for each passing tte variable (and highest for the continuous).
# In practice, we are translating the values for each tte variable group.
# Explanation inline
.with_ordered_column <- function(hce_dat) {
  # We create a data frame, grouping according to the outcome,
  # then we get the minimum and maximum values of the value.
  # What we want to know is the "window" where data are for each of the groups
  # We then select the largest window.
  `%>%` <- dplyr::`%>%`

  tmp <- hce_dat %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(min = min(value), max = max(value)) %>%
    dplyr::mutate(separation = max - min) %>%
    dplyr::summarise(max_separation = max(separation)) %>%
    dplyr::ungroup()

  # With the largest window found, we know that if we offset the data at
  # least of this amount, they will never overlap. Bit of clever math here,
  # we use a gap that is larger, amounting to the number of digits, so we
  # have nicer gap value such as 10, 100, or 1000 etc.
  gap <- 10 ^ ceiling(log10(tmp$max_separation)) # nolint

  # apply the gap to all values. outcome is a factor, so we use its numeric
  # value to multiply the offset, and end up having each value "translated up"
  # of the proper amount.
  hce_dat <- hce_dat %>%
    dplyr::mutate(ordered = .env$gap * (as.numeric(outcome) - 1) + value)

  # and now we have a new data set with the column added.
  return(hce_dat)
}

# Computes the win odds from the internal data.
.compute_win_odds <- function(hce_dat, arm_levels,
                              step_outcomes, last_outcome,
                              lowerBetter) {

  `%>%` <- dplyr::`%>%`

  hce_dat <- base::as.data.frame(hce_dat)
  idx_last <- hce_dat$outcome == last_outcome

  # Reversing continous outcome variables if lower is considered better
  if (lowerBetter) {
    hce_dat[idx_last, "value"] <-
      (min(hce_dat[idx_last, "value"], na.rm = TRUE) -
       hce_dat[idx_last, "value"] +
       max(hce_dat[idx_last, "value"], na.rm = TRUE))
  }

  hce_dat <- .with_ordered_column(hce_dat)
  fit <- hce::calcWO(x = hce_dat, AVAL = "ordered",
                     TRTP = "arm",
                     ref = unname(arm_levels["control"]))
  ci <- base::as.numeric(fit[, base::c("WO", "LCL", "UCL")])
  p <- fit$Pvalue
  win_odds <- base::c(ci, p)
  names(win_odds) <- base::c("estimate", "lower", "upper", "p-value")

  win_odds_outcome <- hce::summaryWO(hce_dat, AVAL = "ordered", TRTP = "arm",
                                     ref = unname(arm_levels["control"]),
                                     GROUP = "outcome")

  endpoints <- c(step_outcomes, last_outcome)
  labs <- c(sapply(utils::head(seq_along(endpoints), -1), function(i) {
    paste(endpoints[1:i], collapse = " +\n")
  }), "Overall")

  hce_dat <- hce_dat %>%
    dplyr::mutate_at(dplyr::vars(outcome), factor, levels = c(endpoints, "X"))

  calcs_lst <- lapply(seq_along(endpoints), function(x) {
    idx <- !(hce_dat$outcome %in% endpoints[1:x])
    hce_dat[idx, "outcome"] <- "X"
    hce_dat[idx, "ordered"] <- 1000000
    wins <- hce::calcWINS(hce_dat, AVAL = "ordered", TRTP = "arm",
                          ref = unname(arm_levels["control"]),
                          GROUP = "outcome")
    wo <- hce::summaryWO(hce_dat, AVAL = "ordered", TRTP = "arm",
                         ref = unname(arm_levels["control"]),
                         GROUP = "outcome")
    list("wins" = wins, "wo" = wo)
  })

  wins_forest <- do.call("rbind", lapply(seq_along(calcs_lst), function(i) {
    wins <- calcs_lst[[i]]$wins
    nm <- c("value", "LCL", "UCL", "p value")
    f <- rbind(data.frame(stats::setNames(wins$WO, nm), "method" = "win odds"),
               data.frame(stats::setNames(wins$WR1, nm),
                          "method" = "win ratio"))
    f$GROUP <- labs[i]
    return(f)
  }))

  wo_bar <- do.call("rbind", lapply(seq_along(calcs_lst), function(i) {
    wo <- utils::head(calcs_lst[[i]]$wo$summary, 1)
    wo$outcome <- endpoints[i]
    wo$GROUP <- labs[i]
    wo %>%
      dplyr::rename(dplyr::all_of(c(A_wins = "WIN", P_wins = "LOSS",
                                    Ties = "TIE"))) %>%
      tidyr::pivot_longer(cols = c("A_wins", "P_wins", "Ties"),
                          names_to = "count", values_to = "value")
  }))

  wo_bar <- .label_win_odds_plots(wo_bar, arm_levels)

  wins_forest$GROUP <- factor(wins_forest$GROUP, levels = rev(labs))
  wins_forest$method <- factor(wins_forest$method,
                               levels = c("win odds", "win ratio"))
  wo_bar$GROUP <- factor(wo_bar$GROUP, levels = rev(labs))
  wo_bar$percentage <- 100 * (wo_bar$value / win_odds_outcome$summary$TOTAL[1])

  return(list("win_odds" = win_odds,
              "win_odds_outcome" = win_odds_outcome,
              "wins_forest" = wins_forest,
              "wo_bar" = wo_bar))

}

# Preparing dataset to be used for plotting components
.prep_data_component_plot <- function(win_odds_outcome, endpoints, arms) {

  `%>%` <- dplyr::`%>%`

  # Win odds summary for each outcome from maraca object
  wo_bar_nc <- win_odds_outcome$summary_by_GROUP

  # Add overall numbers
  wo_tot <- win_odds_outcome$summary
  wo_tot <- wo_tot %>%
    dplyr::mutate("GROUP" = "Overall") %>%
    dplyr::select(names(win_odds_outcome$summary_by_GROUP))

  wo_bar_nc <- rbind(wo_tot, wo_bar_nc)

  wo_bar_nc <- wo_bar_nc %>%
    # Order according to outcome
    dplyr::arrange(match(GROUP, endpoints)) %>%
    # Wide format to get 1 line per outcome
    tidyr::pivot_wider(names_from = TRTP,
                       values_from = c(WIN, LOSS, TIE, TOTAL)) %>%
    # Selecting variables of interest and renaming for plotting
    dplyr::select(GROUP, "A_wins" = WIN_A, "P_wins" = WIN_P,
                  "Ties" = TIE_A) %>%
    # Long format for plotting
    tidyr::pivot_longer(cols = c("A_wins", "P_wins", "Ties"),
                        names_to = "count", values_to = "value")

  # Total number of wins/losses/ties to get relative results
  wo_bar_nc$total <- wo_tot$TOTAL[1]

  # Calculate percentage results
  wo_bar_nc$percentage <- 100 * (wo_bar_nc$value / wo_bar_nc$total)

  wo_bar_nc <- .label_win_odds_plots(wo_bar_nc, arms)

  return(wo_bar_nc)
}

.label_win_odds_plots <- function(bar_data, arms) {
  labels <- c(paste(arms["active"], "wins"),
              paste(arms["control"], "wins"),
              "Ties")

  bar_data$count <- ifelse(bar_data$count == "A_wins",
                           labels[1],
                           ifelse(bar_data$count == "P_wins",
                                  labels[2], labels[3]))

  bar_data$count <- factor(bar_data$count, levels = labels)

  return(bar_data)
}

# The main plotting function creating the component plot
.create_component_plot <- function(wo_bar_nc, endpoints, theme) {

  aes <- ggplot2::aes

  wo_bar_nc$GROUP <- factor(wo_bar_nc$GROUP,
                            levels = rev(c("Overall", endpoints)))

  plot <-
    ggplot2::ggplot(data = wo_bar_nc, aes(x = GROUP, y = percentage,
                                          fill = count)) +
    # Bars
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      width = .8) +
    # Flip to show bars horizontally
    ggplot2::coord_flip() +
    # Add wins/losses/ties as labels
    ggplot2::geom_text(aes(label = round(percentage, 1)),
                       position = ggplot2::position_dodge(width = .8),
                       vjust = 0.5, hjust = -0.2)

  plot <- switch(theme,
                 "maraca" = .theme_maraca_cp(plot),
                 "color1" = .theme_color1_cp(plot),
                 "color2" = .theme_color2_cp(plot),
                 "none" = plot,
                 stop("Please provide theme that exists"))

  # Add class to plot - componentPlot
  class(plot) <- c("componentPlot", class(plot))

  return(plot)
}

# Create forest plot part of cumulative plot
.create_forest_plot <- function(wins_forest, theme, include, reverse) {

  xlab <- paste(include, collapse = " / ")
  if (reverse) {
    wins_forest$GROUP <- factor(wins_forest$GROUP,
                                levels = rev(levels(wins_forest$GROUP)))
  }

  plot <- ggplot2::ggplot(data = wins_forest) +
    ggplot2::geom_errorbar(ggplot2::aes(x = GROUP, y = value, ymin = LCL,
                                        ymax = UCL, col = method,
                                        group = method),
                           linewidth = 0.3, width = 0.15,
                           position = ggplot2::position_dodge(width = 0.3)) +
    ggplot2::geom_point(ggplot2::aes(x = GROUP, y = value,
                                     col = method, shape = method),
                        size = 3,
                        position = ggplot2::position_dodge(width = 0.3)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = "#676767") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous() +
    ggplot2::scale_x_discrete(labels = NULL, name = NULL, breaks = NULL)

  if (theme != "none") {
    plot <- plot +
      ggplot2::geom_vline(xintercept =
                            seq(0.5, length(levels(wins_forest$GROUP)) + 1.5,
                                1),
                          linetype = 2, linewidth = 0.3, color = "darkgray") +
      ggplot2::ylab(xlab) +
      ggplot2::theme_bw()

    if (length(include) == 1) {
      plot <- plot +
        ggplot2::scale_color_manual(values = "black") +
        ggplot2::scale_fill_manual(values = "black") +
        ggplot2::guides(shape = "none", color = "none", fill = "none") +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank())
    } else {
      plot <- plot +
        ggplot2::scale_color_manual(values = c("black", "grey50")) +
        ggplot2::scale_fill_manual(values = c("black", "grey50")) +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank())
    }
  }

  return(plot)

}

# Create bar plot part of cumulative plot
.create_bar_plot <- function(wo_bar, theme, reverse) {

  if (reverse) {
    wo_bar$GROUP <- factor(wo_bar$GROUP, levels = rev(levels(wo_bar$GROUP)))
  }

  plot <-  ggplot2::ggplot(data = wo_bar, ggplot2::aes(x = GROUP,
                                                       y = percentage,
                                                       fill = count)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      width = .8) +
    ggplot2::coord_flip() + # make bar plot horizontal
    ggplot2::geom_text(ggplot2::aes(label = round(percentage, 1)),
                       position = ggplot2::position_dodge(width = .8),
                       vjust = 0.5, hjust = -0.2)

  plot <- switch(theme,
                 "maraca" = .theme_maraca_cp(plot),
                 "color1" = .theme_color1_cp(plot),
                 "color2" = .theme_color2_cp(plot),
                 "none" = plot,
                 stop("Please provide theme that exists"))

  return(plot)

}

.add_win_odds_to_plot <- function(p, win_odds, x, y, hjust) {

  p <- p +
    ggplot2::annotate(
      geom = "label",
      x = x,
      y = y,
      label = paste(
        "Win odds: ", round(win_odds[[1]], 2),
        "\n95% CI: ", round(win_odds[[2]], 2), " - ",
        round(win_odds[[3]], 2), "\n",
        "p-value: ", format.pval(win_odds[[4]], digits = 3, eps = 0.001),
        sep = ""
      ),
      hjust = hjust, vjust = 1.4, size = 3
    )

  return(p)
}
