# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Produce a histogram and normal distribution
#'
#' Plots a histogram, a normal distribution with the same standard deviation and mean as well as one with a mean centred around 1
#' @param x numeric vector, column of paygap observations
#' @param label character, prefix for the title
#' @param mu_unbiased numeric, the mean of the unbiased distribution (for paygaps this should be 1)
#' @keywords distribution equity
#' @returns ggplot2 object
#' @export
#' @import ggplot2
#' @examples
#' d <- div_fake_team()
#' pg <- div_paygap(d)
#' div_plot_paygap_distribution(pg$data$paygap)
#'

div_plot_paygap_distribution <- function(
                             x,
                             label       = 'Gender',
                             mu_unbiased = 1
                             ) {
  paygap <- y <- tibble <- NULL  # binding of global variables
  pgs <- x
  pgs <- pgs[complete.cases(pgs)]
  tbl_pgs <- tibble::tibble(paygap = pgs)
  xmin <- min(pgs, 0.9)
  xmax <- max(pgs, 1.1)
  x <- seq(xmin, xmax, length.out = 100)
  sd <- sd(pgs, na.rm = TRUE)
  mu <- mean(pgs, na.rm = TRUE)
  cols <- c("team"="#f8766d","unbiased"="#00bfc4")

  # Transformation function for 2 decimals
  scaleFUN <- function(x) sprintf("%.2f", x)

  p <- ggplot(tbl_pgs) +
    xlim(xmin, xmax) +
#    geom_histogram(aes(x = paygap, y = ..density.., fill = "team", color = "team"),
#                   alpha = 0.2)
  geom_histogram(aes(x = paygap, fill = "team", color = "team"), alpha = 0.2)
  df <- tibble(x = x, y = dnorm(x, mu, sd))
  p <- p + geom_line(data = df, aes(x = x, y = y, colour = "team")) +
    geom_area(data = df, aes(x = x, y = y, color = "team", fill="team"),
              alpha = 0.5)
  df <- tibble(x = x, y = dnorm(x, mu_unbiased, sd))
  p3 <- p + geom_line(data = df, aes(x = x, y = y, color = "unbiased")) +
    geom_area(data = df, aes(x = x, y = y, color = "unbiased", fill="unbiased"),
              alpha = 0.5) +
    #  scale_colour_manual(name = 'Density', values = c('red', 'blue')) +
    theme(legend.position = c(0.85, 0.85)) +
    scale_colour_manual(name="Legend",values=cols,
                        guide = guide_legend(override.aes=aes(fill=NA))) +
    scale_fill_manual(name="Lgend",values=cols, guide="none") +
    xlab(paste(label, "Pay-Gap ratio")) +
    ggtitle(paste(label, "Pay-Gap Distribution")) +
    scale_x_continuous(labels=scaleFUN)
  return(p3)
}
