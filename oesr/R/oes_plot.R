#' Create OES Plot
#'
#' Plot experimental results using OES style
#'
#' oes_plot plots the observed response mean of a control group and the
#' predicted response means of one or more
#' treatment groups based on OES guidance on data reporting and visualization
#' best-practice. Read more about this OES guidance at
#' https://oes.gsa.gov/assets/files/reporting-statistical-results.pdf.
#'
#' @author Miles Williams
#'
#' @param prep A tidy tibble of estimates to plot, such as the output from
#' \code{oes_prep()}.
#'
#' @param font Optional string giving font; defaults to \code{"sans"}.
#'
#' @param device Set device for loading fonts. Default is \code{"pdf"}.
#'
#' @param treatment_fill Bar color for treatment conditions.
#'
#' @param control_fill Bar color for control condition.
#'
#' @param digits Integer representing number of digits after decimal point to
#' report. Defaults to 3.
#'
#' @param report_stars Logical indicating whether to display asterisks for statistical
#' significance. Defaults to \code{TRUE}.
#'
#' @param xlab String providing the \eqn{x}-axis label.
#'
#' @param ylab String providing the \eqn{y}-axis label.
#'
#' @param title String providing the plot title.
#'
#' @param save Logical indicating whether to save the plot. Defaults to
#' \code{FALSE}. If \code{TRUE}, the plot is not returned, but a file is saved using \code{ggsave}.
#'
#' @param name_save File name for saved plot.
#'
#' @param base_size Base font size for plot.
#'
#' @param width Width of saved plot (in inches). Use 6.8 for multiple columns.
#'
#' @param height Height of saved plot (in inches).
#'
#' @param dpi_forplot Resolution of saved plot.
#'
#' @return A plot; if \code{save = TRUE}, a file containing a plot.
#'
#' @import ggplot2 dplyr
#'
#' @examples
#'
#' data(df_oes)
#'
#' # Single binary treatment:
#' fit <- lm(y1 ~ x1, df_oes)
#'
#' # Multiple treatment conditions:
#' fit2 <- lm(y2 ~ x2, df_oes)
#'
#' # Using HC2 SE's from lm_robust():
#' fit_robust <- estimatr::lm_robust(y1 ~ x1, df_oes)
#' fit_robust2 <- estimatr::lm_robust(y2 ~ x2, df_oes)
#'
#' # Using covariates and lm():
#' fit_covars <- lm(y2 ~ x2 + z1 + z2 + z3, df_oes)
#'
#' # Using covariates and lm_robust():
#' fit_covars_robust <- estimatr::lm_robust(y2 ~ x2 + z1 + z2 + z3, df_oes)
#'
#' # Specify treatment_arms:
#' oes_prep(fit, treatment_arms = 1) |> oes_plot()
#'
#' # Specify treatment_vars:
#' fit |>
#'   oes_prep(treatment_vars = "x1") |>
#'   oes_plot()
#'
#' # Specify treatment_arms:
#' oes_prep(fit2, treatment_arms = 3) |>
#'   oes_plot()
#'
#' # Specify treatment_vars:
#' fit2 |>
#'   oes_prep(treatment_vars = c("x21", "x22", "x23")) |>
#'   oes_plot()
#'
#' # Specify custom treatment_labels:
#' prep_w_labels <- oes_prep(fit2, treatment_arms = 3,
#'   treatment_labels = c(
#'     "Email",
#'     "Email +\nReward",
#'     "Email +\nRisk"),
#'   control_label = "Status Quo")
#'
#' oes_plot(prep_w_labels)
#'
#' # Using objects from estimatr::lm_robust():
#' oes_prep(fit_robust, treatment_arms = 1) |> oes_plot()
#'
#' oes_prep(fit_robust2, treatment_arms = 3) |> oes_plot()
#'
#' # Specify covariates with lm:
#' oes_prep(fit_covars, treatment_arms = 3) |> oes_plot()
#'
#' # Specify covariates with lm_robust():
#' oes_prep(fit_covars_robust, treatment_arms = 3) |> oes_plot()
#'
#' # For the Lin estimator, a manual version of lm_lin():
#' m.mat <- cbind(y2 = df_oes$y2, model.matrix(y2 ~ x2 + z1 + z2 + z3, df_oes)[, -1])
#' m.mat <- dplyr::mutate_at(
#'   data.frame(m.mat),
#'   .vars = c('z1', 'z2', 'z31', 'z32', 'z33', 'z34', 'z35'),
#'   function(x) x - mean(x)
#'   )
#'
#' fit_lin <- estimatr::lm_robust(y2 ~ (x21 + x22 + x23) *
#'   (z1 + z2 + z31 + z32 + z33 + z34 + z35), m.mat)
#'
#' oes_prep(fit_lin, treatment_arms = 3) |> oes_plot()
#'
#' @export

oes_plot <- function(prep,
                     font = "sans",
                     device = "pdf",
                     treatment_fill = "#F2C446",
                     control_fill = "#2E9AC4",
                     digits = 3,
                     report_stars = TRUE,
                     xlab,
                     ylab,
                     title = "Outcomes under Treatment",
                     save = FALSE,
                     name_save = "figure1.png",
                     base_size = 12,
                     width = 3.1,
                     height = 4,
                     dpi_forplot = 300
) {

  # Stop and return error message if dependencies are not installed:

  if(!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package 'dplyr' required for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace('tibble', quietly = TRUE)) {
    stop("Package 'tibble' required for this function to work. Please install it.",
         call. = F)
  }
  if(!requireNamespace('ggplot2', quietly = TRUE)) {
    stop("Package 'ggplot2' required for this function to work. Please install it.",
         call. = FALSE)
  }

  # Calculate alpha/confidence labels:

  alpha_level <- prep$alpha.level

  labels <- round(prep$estimate, digits)

  if(report_stars == TRUE) {
    labels <- dplyr::case_when(
      prep$p.value <= alpha_level ~
        paste0(labels, "*"),
      prep$p.value > alpha_level ~
        paste0(labels)
    )
  }

  prep$labels <- labels

  conf_level <- (1 - alpha_level) * 100

  if(missing(ylab)){
    ylab <- paste0("Response\n(",
                   conf_level,
                   "% Confidence Intervals)")
  }

  if(missing(xlab)){
    xlab <- ""
  }

  # Plot results:

  plt <- ggplot(prep) +
    aes(
      x = reorder(.data$term, .data$ord),
      y = .data$estimate,
      ymin = .data$lo.ci,
      ymax = .data$hi.ci,
      fill = .data$response,
      label = .data$labels
    ) +
    geom_col(col = "black") +
    geom_hline(
      aes(yintercept = .data$estimate[1]), lty = 2
    ) +
    geom_errorbar(width = 0.05) +
    scale_fill_manual(values = c(control_fill, treatment_fill)) +
    geom_label(y = prep$hi.ci,
               vjust = -0.25,
               size = base_size * 0.3,
               fill = 'white',
               col = 'white') +
    geom_text(y = prep$hi.ci,
              vjust = -1.1,
              size = base_size * 0.3) +
    ylim(c(min(prep$lo.ci, 0), max(prep$hi.ci) * 1.1)) +
    labs(
      x = xlab,
      y = ylab,
      title = title
    ) +
    theme_oes(base_size = base_size,
              base_family = font,
              device = device) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(fill = "none")
  if(report_stars == TRUE){
    plt <- plt +
      labs(caption = bquote("*p"<=.(alpha_level)))
  }

  if (save == FALSE) {
    return(plt)
  } else {
    ggsave(name_save,
           width = width,
           height = height,
           dpi = dpi_forplot)

  }
}
