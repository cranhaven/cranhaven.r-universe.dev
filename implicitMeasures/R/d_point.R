#' Plot either IAT or SC-IAT scores (points)
#'
#' Plot the individual \emph{D-score} or SC-IAT \emph{D}.
#'
#' @param data Dataframe with either class \code{dscore} or \code{dsciat}.
#' @param point_size Numeric. Indicates the size of the points in the graph.
#'                    Default is 1.
#' @param x_label Character. Label of the x-axis. Default is \code{Participant}.
#' @param x_values Logical. Shows the values for x-axis (default = \code{TRUE}).
#' @param order_sbj Character. Defines the order with which the participants are
#'                    displayed. Default is the default order of participants in
#'                    the dataframe.
#' @param col_point Character. Defines the color of the points. Default is
#'                    \code{"springgreen4"}.
#' @param include_stats Logical. Indicates whether to add descriptive statistics.
#'                    The \code{mean} is depicted with a solid line. The two
#'                    dashed lines represent +/2 \emph{s.d.} from the mean.
#'                    Default is \code{FALSE}.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \donttest{
#' # Plotting the IAT D-score
#'   data("raw_data") # import data
#'   iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
#'                           block_id = "blockcode",
#'                           mapA_practice = "practice.iat.Milkbad",
#'                           mapA_test = "test.iat.Milkbad",
#'                           mapB_practice = "practice.iat.Milkgood",
#'                           mapB_test = "test.iat.Milkgood",
#'                           latency_id = "latency",
#'                           accuracy_id = "correct",
#'                           trial_id = "trialcode",
#'                           trial_eliminate = c("reminder", "reminder1"),
#'                           demo_id = "blockcode",
#'                           trial_demo = "demo")
#'   iat_data <- iat_cleandata[[1]]
#' # calculate D-score
#'   iat_dscore <- compute_iat(iat_data,
#'                        Dscore =  "d2")
#'   d_point(iat_dscore) # default plot
#'   d_point(iat_dscore, order_sbj = "D-increasing") # D-score with increasing
#'                                                  # order
#'   d_point(iat_dscore, order_sbj = "D-decreasing",
#'          col_point = "salmon") # D-score with decreasing order changed color
#' # Plot the SC-IAT D for the first SC-IAT
#'   data("raw_data") # load data
#'   sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
#'                          block_id = "blockcode",
#'                          latency_id = "latency",
#'                          accuracy_id = "correct",
#'                          block_sciat_1 = c("test.sc_dark.Darkbad",
#'                                            "test.sc_dark.Darkgood"),
#'                          block_sciat_2 = c("test.sc_milk.Milkbad",
#'                                            "test.sc_milk.Milkgood"),
#'                          trial_id  = "trialcode",
#'                          trial_eliminate = c("reminder",
#'                                              "reminder1"))
#'
#'  sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
#'  d_sciat1 <- compute_sciat(sciat1,
#'                   mappingA = "test.sc_dark.Darkbad",
#'                   mappingB = "test.sc_dark.Darkgood",
#'                   non_response = "alert")
#'   d_point(d_sciat1, col_point = "salmon",
#'           include_stats = TRUE) # SC-IAT D with descriptive statistics
#'           }
d_point <- function(data, point_size = 1,
                   x_label = "Participant",
                   x_values = TRUE,
                   order_sbj = c("default", "D-increasing", "D-decreasing"),
                   col_point = "springgreen4",
                   include_stats = FALSE){
  # checki dataset class --------------------------
  if (is.na(class(data)[2])){
    stop("data must be an object of class dscore or dsciat")
  } else if (class(data)[2] == "dscore"){
    data$d <- data[, grep("dscore", colnames(data))]
    x_lab <- "D-score"
  } else if (class(data)[2] == "dsciat"){
    data$d <- data[, grep("d_sciat", colnames(data))]
    x_lab <- "D-sciat"
  } else {
    stop("data must be an object of class dscore or dsciat")
  }

  # initialize plots
  order_sbj <- match.arg(order_sbj)
  if (order_sbj == "default") {
    data <- data[order(data$participant), ]
    data$d_cres <- data$participant
    data$d_cres <- as.factor(data$d_cres)
    d_graph <- ggplot(data,
                      aes(y = .data$d, x = .data$d_cres)) +
      geom_point(col = col_point,
                 size = point_size)
    d_graph <- d_graph + scale_x_discrete(name = x_label,
                                          labels = data$participant) +
      ylab(x_lab)
    # include statistics
    if (include_stats == FALSE) {
      d_graph <- d_graph
    } else {
      d_graph <- d_graph + geom_hline(aes(yintercept = mean(.data$d)))
      d_graph <- d_graph + geom_hline(aes(yintercept = (mean(.data$d) +
                                                      2 * sd(.data$d))),
                                      linetype = "dotted")
      d_graph <- d_graph + geom_hline(aes(yintercept = (mean(data$d) -
                                                      2 * sd(.data$d))),
                                      linetype = "dotted")
    }
  } else if (order_sbj == "D-increasing") {
    data <- data[order(data$d), ]
    data$d_cres <- 1:nrow(data)
    data$d_cres <- factor(data$d_cres)
    coordinates_labels <- ifelse(length(unique(data$participant)) < 150,
                                 nrow(data) - 4, nrow(data) - 10 )
    d_graph <- ggplot(data,
                      aes(y = .data$d, x = .data$d_cres)) +
      geom_point(col = col_point,  size = point_size)
    d_graph <- d_graph + scale_x_discrete(name = x_label,
                                          labels = data$participant) +
      ylab(x_lab)
  } else if (order_sbj == "D-decreasing") {
    data <- data[order(data$d, decreasing = T), ]
    data$d_cres <- 1:nrow(data)
    data$d_cres <- factor(data$d_cres)
    coordinates_labels <- ifelse(length(unique(data$participant)) < 150,
                                 (nrow(data) - (nrow(data) - 4)),
                                 (nrow(data) - (nrow(data) - 15)) )
    d_graph <- ggplot(data,
                      aes(y = .data$d,
                          x = .data$d_cres)) +
      geom_point(col = col_point,  size = point_size)
    d_graph <- d_graph + scale_x_discrete(name = x_label,
                                          labels = data$participant) +
      ylab(x_lab)
  }

  d_graph <- d_graph  + theme_minimal()
  d_graph <- d_graph + theme(axis.text.x = element_text(size = 5))

  # include statistics
  if (include_stats == FALSE) {
    d_graph <- d_graph
  } else {
    d_graph <- d_graph + geom_hline(aes(yintercept = mean(.data$d)))
    d_graph <- d_graph + geom_hline(aes(yintercept = (mean(.data$d) +
                                                    2 * sd(.data$d))),
                                    linetype = "dotted")
    d_graph <- d_graph + geom_hline(aes(yintercept = (mean(.data$d) -
                                                    2 * sd(.data$d))),
                                    linetype = "dotted")
  }

  # remove participants labels --------------------------
  if (x_values == TRUE){
    d_graph <- d_graph
  } else {
    d_graph <- d_graph + theme(axis.text.x = element_blank())
  }
  # results --------------------------
  return(d_graph)
}
