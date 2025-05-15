#' @title Plot assignment distributions
#' @description Plot individual assignment probability distributions. 
#' 
#' @param probs matrix or data.frame of individual assignment probabilities. 
#'   Each column represents probability of assignment to that group and rows sum 
#'   to one.
#' @param orig vector of original group assignments
#' @param type either \code{area} for stacked continuous area plot or 
#'   \code{bar} for discrete stacked bar chart. The latter is prefered for small 
#'   numbers of cases. If not specified, a bar chart will be used if all 
#'   classes have <= 30 cases.
#' @param ylab label for y-axis
#' @param freq.sep.line put frequency of original group on second line in facet 
#'   label? If \code{FALSE}, labels are single line. If \code{NULL} frequencies 
#'   will not be included in labels.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' n <- 40
#' probs <- abs(c(rnorm(n, 80, 10), rnorm(n, 20, 10)))
#' probs <- (probs - min(probs)) / max(probs)
#' probs <- cbind(probs, 1 - probs)
#' colnames(probs) <- NULL
#' orig <- rep(c("Group.1", "Group.2"), each = n)
#' 
#' plotAssignments(probs, orig)
#' 
#' n <- 15
#' probs <- abs(c(rnorm(n, 80, 10), rnorm(n, 20, 10)))
#' probs <- (probs - min(probs)) / max(probs)
#' probs <- cbind(probs, 1 - probs)
#' colnames(probs) <- NULL
#' orig <- rep(c("Group.1", "Group.2"), each = n)
#' 
#' plotAssignments(probs, orig)
#' 
#' @export
#'
plotAssignments <- function(
  probs, orig, type = NULL, ylab = NULL, freq.sep.line = TRUE, plot = TRUE
) {
  freq <- table(orig)
  type <- if(is.null(type)) {
    if(all(freq <= 30)) "bar" else "area"
  } else {
    match.arg(type, c("bar", "area"))
  }
  
  if(is.null(colnames(probs))) {
    colnames(probs) <- paste("Group", 1:ncol(probs), sep = ".")
  }
  
  df <- data.frame(orig = orig, probs, check.names = FALSE)
  i <- do.call(order, c(as.list(df), list(decreasing = TRUE)))
  df <- df[i, ] |> 
    dplyr::mutate(id = 1:dplyr::n()) |> 
    tidyr::pivot_longer(-c("id", "orig"), names_to = "pred", values_to = "prob")
  if(!is.null(freq.sep.line)) {
    levels(df$orig) <- paste0(
      names(freq), ifelse(freq.sep.line, "\n", " "), "(n = ", freq, ")"
    )
  }
  
  if(is.null(ylab)) ylab <- "Assignment Probability"
  
  p <- ggplot2::ggplot(df, ggplot2::aes_string("id", "prob")) +
    switch(
      type,
      area = ggplot2::geom_area(
        ggplot2::aes_string(fill = "pred"), 
        stat = "identity"
      ),
      bar = ggplot2::geom_bar(
        ggplot2::aes_string(fill = "pred"), 
        stat = "identity"
      )
    ) +
    ggplot2::scale_fill_discrete(
      guide = ggplot2::guide_legend(title = "Predicted")
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::facet_wrap(~ orig, scales = "free_x") +
    ggplot2::ylab(ylab) +
    ggplot2::theme(
      legend.position = "top",
      text = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
  if(plot) print(p)
  invisible(p)
}