#' Visualize Estimates of Treatment Effects
#'
#' @param est an return object from \code{\link{gcalibrate}} or \code{\link{bcalibrate}}, or
#' \code{data.frame} containing estimates of treatment effects with estimates' type in columns and
#' contrasts of interest in rows.
#'
#' @param show_rv logical. Whether robustness values should be printed in the plot or not?
#' Available only for the "worstcase" calibration.
#' @param order character. The type of order used to plot treatment effects from left to right.
#' Can be one of the following:\cr
#' "naive" - order by the naive estimate from smallest to largest.
#' "worstcase" - place all treatments with negative robust effects on the left, with positive robust
#' effects on the right, and all sensitive ones in the middle. Within the negative robust group, order
#' treatments by the upper bound of the worst-case ignorance region from smallest to largest;
#' within the positive robust group, order treatments by the lower bound of the worst-case ignorance
#' region from smallest to largest; and within the sensitive group, order by  the naive estimate from
#' smallest to largest.
#' @param labels character. Labels of treatments.
#' @param ... further arguments passed to \code{\link{theme}}
#'
#' @return A graph plotting ignorance regions of the causal estimands of interest.
#'
#'
#' @export
#'
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_colour_viridis_d
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 theme
#'
#'
#' @section Note:
#' For examples, please refer to \code{\link{bcalibrate}} or \code{\link{gcalibrate}}

plot_estimates <- function(est, show_rv = TRUE,
                           order = "naive", labels = NULL, ...) {

  x1<-y1<-x2<-y2<-effect<-Type<-NULL

  getstr = function(mystring, initial.character, final.character)
  {
    # check that all inputs are character variables
    if (!is.character(mystring))
    {
      stop('The parent string must be a character variable.')
    }
    if (!is.character(initial.character))
    {
      stop('The initial character must be a character variable.')
    }
    if (!is.character(final.character))
    {
      stop('The final character must be a character variable.')
    }
    # pre-allocate a vector to store the extracted strings
    snippet = rep(0, length(mystring))
    for (i in 1:length(mystring))
    {
      # extract the initial position
      initial.position = gregexpr(initial.character, mystring[i])[[1]][1] + 1

      # extract the final position
      final.position = gregexpr(final.character, mystring[i])[[1]][2] - 1

      # extract the substring between the initial and final positions, inclusively
      snippet[i] = substr(mystring[i], initial.position, final.position)
    }
    return(as.numeric(snippet))
  }


  if(is.list(est) & (!is.data.frame(est))) {
    est_df <- est$est_df
    R2 <- round(c(0, sort(est$R2)), 2)
  } else if (is.data.frame(est)){
    est_df <- est
    R2 <- c(0, getstr(colnames(est)[-1], '_', '_')) %>%
      sort() %>% round(2)
  }


  case <- 1:nrow(est_df)
  if(is.null(labels)) {
    labels <- case
  }

  ## order according to naive estimates
  ord <- case
  if(order == "naive") {
    ord <- order(est_df[, "R2_0"])

  } else if(order == "worstcase") {
    rnk <- 1:length(est$rv)
    rob_neg <- which(is.na(est$rv) & est_df[, "R2_1_upr"] < 0)
    rnk[rob_neg] <- rank(est_df[rob_neg, "R2_1_upr"])

    not_rob <- which(!is.na(est$rv))
    rnk[not_rob] <- rank(est_df[not_rob, "R2_0"]) + length(rob_neg)

    rob_pos <- which(is.na(est$rv) & est_df[, "R2_1_lwr"] > 0)
    rnk[rob_pos] <- rank(est_df[rob_pos, "R2_1_lwr"]) + length(c(rob_neg, not_rob))
    ord <- order(rnk)

  }
  est_df <- est_df[ord, ]
  est$rv <- est$rv[ord]
  labels <- labels[ord]

  bound_df <- data.frame(x1 = case,
                         y1 = apply(est_df, 1, min),
                         x2 = case,
                         y2 = apply(est_df, 1, max))
  if (length(R2) < ncol(est_df)) {
    df_wc <- unlist(cbind(est_df[,1], est_df)) %>%
      matrix(nrow = 2*nrow(est_df))
    colnames(df_wc) <- c(0, est$R2)
    df <- data.frame(df_wc, case = rep(case, 2)) %>%
      gather(key = "Type", value = "effect", - case)
  } else {
    df <- data.frame(est_df, case = case) %>%
      gather(key = "Type", value = "effect", - case)
  }

  df$x1 = df$case -  0.003*length(df$case)
  df$x2 = df$case +  0.003*length(df$case)

  plot <- ggplot(df) +
    geom_segment(aes(x=x1,y=effect,xend=x2,yend=effect, col = Type), size = 1.5) +
    geom_segment(data = bound_df, aes(x=x1,y=y1,xend=x2,yend=y2), size = 0.3) +
    labs(y = "Causal Effect", x = "Treatment Contrast") +
    scale_x_continuous(breaks = unique(case), labels = labels,
                       limits = c(0.5, max(case) + 0.5)) +
    scale_colour_viridis_d(name = expression(R^2~":"),
                           labels = paste0(R2*100, "%"), direction=-1) +
    theme_bw(base_size = 12)

  # add robustness value #
  if(!is.null(est$rv) & show_rv) {
    est$rv[!is.na(est$rv)] <- paste0(round(est$rv[!is.na(est$rv)]), "%")
    est$rv[is.na(est$rv)] <- "R"

    plot + annotate(geom = "text", x = case +0.1 + 0.02*length(case), y = est_df[,'R2_0'],
                    size = 3, label = est$rv, col="dark red") +
      theme(...)
  } else {
    plot
  }
}



