#' Plot SC-IATs scores
#'
#' Plot the scores from two different SC-IATs.
#'
#' @param sciat1 Dataframe with class \code{dsciat}. Contains the \emph{D} for
#'                the first SC-IAT.
#' @param sciat2 Dataframe with class \code{dsciat}. Contains the \emph{D} for
#'                the second SC-IAT.
#' @param graph String. Type of graph to display, might be  \code{density} (default), \code{boxplot}, or \code{point}
#' @param x_values Logical. Shows the values for x-axis (default = \code{TRUE}).
#'                          Only for the point graph.
#' @param gcolors String. Colors palette for plotting the results, might be \code{dark} (default), \code{greens}, \code{blues}, or \code{pinks}.
#' @param label_sc1 String. Label to display in the graph for the first SC-IAT.
#'                   Default is \code{SC-IAT1}.
#' @param label_sc2 String. Label to display in the graph for the first SC-IAT.
#'                   Default is \code{SC-IAT2}.
#' @param label_y String. Label to plot on the y-axis.
#' @param dens_mean Logical. Whether to include the mean in the density plot.
#'                   Default is TRUE.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # calculate D for the SCIAT
#'   data("raw_data") # load data
#' sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
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
#'                     mappingA = "test.sc_dark.Darkbad",
#'                     mappingB = "test.sc_dark.Darkgood",
#'                     non_response = "alert") # dataframe with the first D
#'                                             # SC-IAT
#'
#'  sciat2 <- sciat_data[[2]] # Compute D for the second SC-IAT
#'  d_sciat2 <- compute_sciat(sciat2,
#'                     mappingA = "test.sc_milk.Milkbad",
#'                     mappingB = "test.sc_milk.Milkgood",
#'                     non_response = "alert") # dataframe with the first
#'                                             # D SC-IAT
#'  multi_dsciat(d_sciat1, d_sciat2) # plot the D of two SC-IATs with default
#'                                     # settings
multi_dsciat <- function(sciat1, sciat2,
                         graph = c("density", "boxplot", "point"),
                         x_values = TRUE,
                         gcolors = c("dark", "greens", "blues", "pinks"),
                         label_sc1 = "SC-IAT1",
                         label_sc2  = "SC-IAT2",
                         label_y = "SC-IAT scores",
                         dens_mean = TRUE){
  graph <- match.arg(graph)
  gcolors <- match.arg(gcolors)
  # check dataset class --------------------------
  # throws an error if objects without dsciat class are passed
  if(is.na(class(sciat1)[2]) | is.na(class(sciat2)[2])) {
    stop("Objects must be of class dsciat")
  }

  if (class(sciat1)[2] != "dsciat" | class(sciat2)[2] != "dsciat") {
    stop("Objects must be of class dsciat")
  }

  # prepare dataset --------------------------
  d_small1 <- sciat1[ , c("participant", "d_sciat")]
  d_small1$sciat <- label_sc1
  d_small2 <- sciat2[ , c("participant", "d_sciat")]
  d_small2$sciat <- label_sc2
  sciat_all <- rbind(d_small1, d_small2)
  sc_mean <- aggregate(sciat_all$d_sciat,
                       by = list(sciat_all$sciat),
                       FUN = mean)
  sc_mean <- aggregate(sciat_all$d_sciat,
                       by = list(sciat_all$sciat),
                       FUN = mean)
  colnames(sc_mean) <- c("sciat", "msc")
  sciat_all <- merge(sciat_all, sc_mean, by = "sciat")
  data <- sciat_all
   # plots --------------------------
  if (graph == "density") {
    d_graph <- ggplot(data,
                      aes(x = .data$d_sciat,
                          color = .data$sciat)) +
              geom_density(size = 1.1)  +
              theme_light() + theme(axis.title.y = element_blank()) +
              xlab(label_y)
    # add statistics
    if (dens_mean == TRUE) {
      d_graph <- d_graph + geom_vline(data=data,
                                      aes(xintercept = .data$msc,
                                          color = .data$sciat),
                                      linetype = "dashed",
                                      size = 0.9)
    } else {
      d_graph <- d_graph
    }
  } else if (graph == "boxplot") {
    d_graph <- ggplot(data,
                      aes(y = .data$d_sciat,
                          x = .data$sciat,
                          color = .data$sciat)) +
               geom_boxplot() +
               stat_summary(fun.data = mean_sdl,
                            geom = "pointrange",
                            color = "firebrick") +
               theme_light()  +
               ylab(label_y)
  } else if (graph == "point") {
    d_graph <- ggplot(data,
                      aes(x = .data$participant,
                          y = .data$d_sciat,
                          group = .data$sciat)) +
               geom_point(aes(shape = .data$sciat,
                                color = .data$sciat)) +
               theme_light() + theme(axis.text.x = element_text(size = 5)) +
               scale_shape_discrete(name  = "SC-IAT type") + ylab(label_y) +
      xlab("Participant")
    if (x_values == TRUE){
      d_graph <- d_graph
    } else {
      d_graph <- d_graph + theme(axis.text.x = element_blank())
    }
  }
  # palettes --------------------------
  if (gcolors == "greens"){
    d_graph <- d_graph + scale_color_manual(values = c("palegreen",
                                                       "palegreen4"),
                                            name = "SC-IAT type")
  } else if (gcolors == "blues"){
    d_graph <- d_graph + scale_color_manual(values = c("royalblue1",
                                                       "royalblue4"),
                                            name = "SC-IAT type")
  } else if (gcolors == "pinks") {
    d_graph <- d_graph + scale_color_manual(values = c("rosybrown1",
                                                       "plum1"),
                                            name = "SC-IAT type")
  } else {
    d_graph <- d_graph + scale_color_manual(values = c("gray74",
                                                       "gray8"),
                                            name = "SC-IAT type")
  }
  return(d_graph)
}
