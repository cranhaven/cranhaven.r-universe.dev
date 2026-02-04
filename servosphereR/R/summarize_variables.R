#' Calculate total path distance
#'
#' Calculate the total distance moved by an organism on the servosphere
#'
#' Determine the total distance of a path taken by an organism on a servosphere.
#'
#' @param list A list of data frames, each of which has a column recording the
#'   distance moved during each recording period.
#' @param summary.df The data frame object within which you are storing path
#'   summary variables. The default is NA if you do not currently have a summary
#'   data frame object started. When set to NA the function will create a new
#'   summary data frame. When an object is provided, the function will merge the
#'   summary data frame with the new data.
#' @return A named vector of numbers where each number corresponds to the total
#'   distance moved by an organism represented in the list of data frames. The
#'   numbers are ordered and named as they are in the data frames list.
#' @examples
#' # If a summary data frame has not been started
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#'
#' servosphere <- calcDistance(servosphere)
#'
#' summary_df <- summaryTotalDistance(servosphere, summary.df = NA)
#'
#' # If a summary data frame has been started
#'
#' summary_df <- data.frame(id = c(1, 2),
#'                          treatment = c("a", "b"),
#'                          date = c("2032018", "2042018"),
#'                          stimulus = c(0, 0))
#'
#' summary_df <- summaryTotalDistance(servosphere, summary.df = summary_df)
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%

summaryTotalDistance <- function(list, summary.df = NA) {
   # Done to appease the R CMD check gods
   stimulus <- NULL
   id_stim <- NULL
   distance <- NULL

  list <-  purrr::map_if(list, is.data.frame, function(.x) {
      total_distance <- .x %>%
         select(distance) %>%
         sum()
      if (any(names(.x) == "id_stim")) {
         out <- data.frame(id_stim = .x$id_stim[1],
                           total_distance = total_distance,
                           stringsAsFactors = FALSE)
      } else{
         out <- data.frame(id = .x$id[1],
                           total_distance = total_distance,
                           stringsAsFactors = FALSE)
      }

      if (any(!is.na(summary.df))){ # If summary.df is provided, join out with it
         # If data are split by stimulus, need to join by id_stim
         if (any(names(.x) == "id_stim")) {
            out <- inner_join(out, summary.df, by = "id_stim")
         } else {
            out <- inner_join(out, summary.df, by = "id")
         }

      } else {
         if (any(names(.x) == "id_stim")) {
            trial_cols <- .x %>%
               select(!!list$col.names, stimulus, -id_stim) %>%
               slice(1)
            out <- bind_cols(out, trial_cols)
         } else {
            trial_cols <- .x %>%
               select(!!list$col.names, stimulus, -id) %>%
               slice(1)
            out <- bind_cols(out, trial_cols)
         }

      }
      return(out)
   })
      magrittr::extract(list, 1:(length(list) - 1)) %>%
      bind_rows()
}

#' Calculate net displacement
#'
#' Calculate the net displacement for a path taken by an organism
#'
#' Net displacement is the distance between the start of a path and the end of a
#' path. This function calculates the net displacement for a path recorded from
#' the servosphere.
#'
#' @param list A list of data frames, each of which has a column representing dx
#'   and dy.
#' @param summary.df The data frame object within which you are storing path
#'   summary variables. The default is NA if you do not currently have a summary
#'   data frame object started. When set to NA the function will create a new
#'   summary data frame. When an object is provided, the function will merge the
#'   summary data frame with the new data.
#' @return A named vector of numbers where each number corresponds to the net
#'   displacement of a movement path. The numbers are ordered and named as they
#'   are in the data frames list.
#' @examples
#' # If a summary data frame has not been started
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#' servosphere <- calcXY(servosphere)
#'
#' summary_df <- summaryNetDisplacement(servosphere, summary.df = NA)
#'
#' # If a summary data frame has been started
#'
#' summary_df <- data.frame(id = c(1, 2),
#'                          treatment = c("a", "b"),
#'                          date = c("2032018", "2042018"),
#'                          stimulus = c(0, 0))
#'
#' summary_df <- summaryNetDisplacement(servosphere, summary.df = summary_df)}
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%

summaryNetDisplacement <- function(list, summary.df = NA) {
   # Done to appease the R CMD check gods
   stimulus <- NULL
   id_stim <- NULL

  list <-  purrr::map_if(list, is.data.frame, function(.x) {
     net_displacement <- sqrt((.x$x[nrow(.x)] ^ 2) + (.x$y[nrow(.x)] ^ 2))
     if (any(names(.x) == "id_stim")) {
        out <- data.frame(id_stim = .x$id_stim[1],
                          net_displacement = net_displacement,
                          stringsAsFactors = FALSE)
     } else{
        out <- data.frame(id = .x$id[1],
                          net_displacement = net_displacement,
                          stringsAsFactors = FALSE)
     }

     if (any(!is.na(summary.df))){ # If summary.df is provided, join out with it
        # If data are split by stimulus, need to join by id_stim
        if (any(names(.x) == "id_stim")) {
           out <- inner_join(out, summary.df, by = "id_stim")
        } else {
           out <- inner_join(out, summary.df, by = "id")
        }

     } else {
        if (any(names(.x) == "id_stim")) {
           trial_cols <- .x %>%
              select(!!list$col.names, stimulus, -id_stim) %>%
              slice(1)
           out <- bind_cols(out, trial_cols)
        } else {
           trial_cols <- .x %>%
              select(!!list$col.names, stimulus, -id) %>%
              slice(1)
           out <- bind_cols(out, trial_cols)
        }

     }
      return(out)
   })
     magrittr::extract(list, 1:(length(list) - 1)) %>%
     bind_rows()
}

#' Calculate tortuosity
#'
#' Calculate the tortuosity, or straightness, of a movement path
#'
#' To use this function, a summary data frame must already exist containing a
#' column for total distance and net displacement (in other words, your data
#' should have been processed by \code{summaryTotalDistance} and
#' \code{summaryNetDisplacement}.
#'
#' Tortuosity is a measure of how straight a path is. There are different
#' methods for calculating path straightness. This function calculates
#' tortuosity as the quotient of net displacement and total distance by default.
#' The quotient can be reversed by setting inverse to \code{TRUE}.
#'
#' @param summary.df The summary data frame containing total distance and net
#'   displacement for all movement paths
#' @param total.distance The unquoted variable name in a data frame containing
#'   the total distance for all movement paths
#' @param net.displacement The unquoted variable name in a data frame containing
#'   the net displacement for all movement paths.
#' @param inverse Defaults to \code{FALSE}. When set to \code{FALSE}, this
#'   function calculates tortuosity as net displacement divided by total
#'   distance. Setting inverse to \code{TRUE} causes the function to calculate
#'   tortuosity as total distance divided by net displacement.
#' @return The inputed data frame of numbers where each number corresponds to
#'   the tortuosity of a movement path. The numbers are ordered and named as
#'   they are in the data frames list.
#' @examples
#' # Calculate tortuosity as the ratio of net displacement to total distance
#'
#' summary_df <- data.frame(id = c(1, 2),
#'                          treatment = c("a", "b"),
#'                          date = c("2032018", "2042018"),
#'                          stimulus = c(0, 0),
#'                          total_distance = runif(2, 11, 20),
#'                          net_displacement = runif(2, 5, 10))
#'
#' summary_df <- summaryTortuosity(summary.df = summary_df,
#'   total.distance = total_distance,
#'   net.displacement = net_displacement)
#'
#' # Calculate tortuosity as the ratio of total distance to net displacement
#' # (the opposite of the previous example)
#'
#' summary_df <- summaryTortuosity(summary.df = summary_df,
#'   total.distance = total_distance,
#'   net.displacement = net_displacement,
#'   inverse = TRUE)
#' @export
#' @import dplyr
#' @importFrom magrittr %>%

summaryTortuosity <- function(summary.df,
                              total.distance,
                              net.displacement,
                              inverse = FALSE) {
   total.distance <- enquo(total.distance)
   net.displacement <- enquo(net.displacement)
   if (inverse == FALSE) {
      summary.df <- summary.df %>%
         mutate(tortuosity = !!net.displacement / !!total.distance)
   }
   else {
      summary.df <- summary.df %>%
         mutate(tortuosity = !!total.distance / !!net.displacement)
   }

   return(summary.df)
}


#' Calculate average bearing
#'
#' Calculate the average bearing, or direction of movement, for a movement path.
#'
#' The bearing is the direction of movement and and falls between 0 and 360.
#' Outdoors degree measures of 0/360 will typically correspond to due north. For
#' servosphere data, 0/360 will correspond to the  movement in the direction of
#' the positive y-axis.
#' @param list A list of data frames, each of which has a column for bearing.
#' @param summary.df The summary data frame containing a column recording
#'   the bearing of each recorded movement.
#' @return A list of two named vectors. The first named vector contains the
#'   average bearing calculated for each movement path. The second named vector
#'   contains the rho, a measure of concentration for the average bearing.
#' @examples
#' # If a summary data frame has not been started
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)),
#'                      c("id", "treatment", "date))
#' names(servosphere) <- c("1", "2", "col.names")
#' servosphere <- calcXY(servosphere)
#' servosphere <- calcBearing(servosphere)
#' summary_df <- summaryAvgBearing(servosphere, summary.df = NA)
#'
#' # If a summary data frame has been started
#'
#' summary_df <- data.frame(id = c(1, 2),
#'                          treatment = c("a", "b"),
#'                          date = c("2032018", "2042018"),
#'                          stimulus = c(0, 0))
#'
#' summary_df <- summaryAvgBearing(servosphere, summary.df = summary_df)
#' @export
#' @import dplyr
#' @importFrom magrittr %>%


summaryAvgBearing <- function(list, summary.df = NA) {
   # Done to appease the R CMD check gods
   stimulus <- NULL
   id_stim <- NULL

   out <- list %>% purrr::map_if(is.data.frame, function(.x) {
      b <- .x[!is.na(.x$bearing), "bearing"]
      # Get mean bearing by converting to radians then back to degrees
      r <- b * (pi / 180)
      mean.r <- atan3((sum(sin(r))) / length(r),
                      (sum(cos(r))) / length(r))
      mean.c <- mean.r * (180 / pi)
      if (mean.c < 0) {
         mean.c <- 360 + mean.c
      }
      # Get rho, or strength of association
      rho <- sqrt(((sum(sin(r))) / length(r)) ^ 2 +
                     ((sum(cos(r))) / length(r)) ^ 2)
      if (any(names(.x) == "id_stim")) {
         return(c(.x$id_stim[1], mean.c, rho))
      } else {
         return(c(.x$id[1], mean.c, rho))
      }
   })


   out <- magrittr::extract(out, 1:(length(out) - 1))
   out <- unlist(out)
   id <- unname(out[seq(1, length(out), by = 3)])
   circular.mean <-
      unname(out[seq(2, (length(out) - 1), by = 3)])
   circular.rho <- unname(out[seq(3, (length(out) - 0), by = 3)])
   if (any(names(list[[1]]) == "id_stim")) {
      out <- data.frame(
         id_stim = id,
         circular_mean = circular.mean,
         circular_rho = circular.rho,
         stringsAsFactors = FALSE
      )
   } else{
      out <- data.frame(
         id = id,
         circular_mean = circular.mean,
         circular_rho = circular.rho,
         stringsAsFactors = FALSE
      )
   }


   if (any(!is.na(summary.df))) {
      # If summary.df is provided, join out with it
      # If data are split by stimulus, need to join by id_stim
      if (any(names(list[[1]]) == "id_stim")) {
         out <- inner_join(out, summary.df, by = "id_stim")
      } else {
         out <- inner_join(out, summary.df, by = "id")
      }

   } else {
      if (any(names(list[[1]]) == "id_stim")) {
         trial_cols <- list %>%
            map_if(is.data.frame, function(.x) {
               .x %>%
                  select(!!list$col.names, stimulus, -id_stim) %>%
                  slice(1)
            })
         trial_cols <- trial_cols %>%
            magrittr::extract(1:((length(.data) - 1))) %>%
            bind_rows
         out <- bind_cols(out, trial_cols)
      } else {
         trial_cols <- list %>%
            map_if(is.data.frame, function(.x) {
               .x %>%
                  select(!!list$col.names, stimulus, -id) %>%
                  slice(1)
            })
         trial_cols <- trial_cols %>%
            magrittr::extract(1:((length(.data) - 1))) %>%
            bind_rows
         out <- bind_cols(out, trial_cols)
      }
      return(out)
   }

}

#' Calculate average velocity
#'
#' Calculate the average velocity for a movement path.
#'
#' Calculate the average velocity for a movement path. The units on velocity are
#' equal to the distance units used to record the data per second.
#' @param list A list of data frames, each of which has a column for velocity.
#' @param summary.df The data frame object within which you are storing path
#'   summary variables. The default is NA if you do not currently have a summary
#'   data frame object started. When set to \code{NA} the function will create a new
#' summary data frame. When an object is provided, the function will merge the
#' summary data frame with the new data.
#' @return The inputed summary data frame or a new data frame if summary.df is
#'   \code{NA}
#' @examples
#' # If a summary data frame has not been started
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#' servosphere <- calcVelocity(servosphere)
#'
#' summary_df <- summaryAvgVelocity(servosphere, summary.df = NA)
#' # If a summary data frame has been started
#'
#' summary_df <- data.frame(id = c(1, 2),
#'                          treatment = c("a", "b"),
#'                          date = c("2032018", "2042018"),
#'                          stimulus = c(0, 0))
#'
#' summary_df <- summaryAvgVelocity(servosphere, summary.df = summary_df)
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
summaryAvgVelocity <- function(list, summary.df = NA) {
   # Done to appease the R CMD check gods
   stimulus <- NULL
   id_stim <- NULL

   list <- list %>% purrr::map_if(is.data.frame, function(.x) {
      out <- data.frame(id = .x$id[1],
                 avg.velocity = mean(.x$velocity, na.rm = TRUE))
      if (any(names(.x) == "id_stim")) {
         out <- data.frame(id_stim = .x$id_stim[1],
                           avg_velocity = out$avg.velocity,
                           stringsAsFactors = FALSE)
      } else{
         out <- data.frame(id = .x$id[1],
                           avg_velocity = out$avg.velocity,
                           stringsAsFactors = FALSE)
      }

      if (any(!is.na(summary.df))){ # If summary.df is provided, join out with it
         # If data are split by stimulus, need to join by id_stim
         if (any(names(.x) == "id_stim")) {
            out <- inner_join(out, summary.df, by = "id_stim")
         } else {
            out <- inner_join(out, summary.df, by = "id")
         }

      } else {
         if (any(names(.x) == "id_stim")) {
            trial_cols <- .x %>%
               select(!!list$col.names, stimulus, -id_stim) %>%
               slice(1)
            out <- bind_cols(out, trial_cols)
         } else {
            trial_cols <- .x %>%
               select(!!list$col.names, stimulus, -id) %>%
               slice(1)
            out <- bind_cols(out, trial_cols)
         }
      }
   })
      magrittr::extract(list, 1:(length(list) - 1)) %>%
      bind_rows()

}


#' Summarize the number and length of stops
#'
#' Caulculate how many times an insect stopped walking during its recorded
#' movement and the average length of those stops.
#'
#' This function evaluates a path for the total number of stops made during the
#' recording, as well as the average length of those stops. The function
#' requires the user to provide a movement threshold, below which the insect is
#' considered to be stopped. The servosphere may record small movements of
#' insect appendages or side to side motion as movement but typically this will
#' be much slower than actual movement. Estimates of this threshold speed can be
#' obtained by comparing recordings of the data to videos of the actual movement
#' or in person observation.
#' @param list A list of data frames, each of which must have a column recording
#'   the velocity variable.
#' @param summary.df The data frame object within which you are storing path
#'   summary variables. The default is NA if you do not currently have a summary
#'   data frame object started. When set to \code{NA} the function will create a
#'   new summary data frame. When an object is provided, the function will merge
#'   the summary data frame with the new data.
#' @param stop.threshold The velocity below which the insect is considered to be
#'   stopped. The default is 0, but should be adjusted based on observations of
#'   the insect and the recording equipment.
#' @examples
#' # If a summary data frame has not been started with movement less than 0.1
#' # cm/s qualifying as a stop
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#'
#' servosphere <- calcVelocity(servosphere)
#'
#' summary_df <- summaryStops(servosphere,
#'                            summary.df = NA,
#'                            stop.threshold = 0.1)
#'
#' # If a summary data frame has been started with movement less than 0.05 cm/s
#' # qualifying as a stop
#'
#' summary_df <- data.frame(id = c(1, 2),
#'                          treatment = c("a", "b"),
#'                          date = c("2032018", "2042018"),
#'                          stimulus = c(0, 0))
#'
#'
#' summary_df <- summaryStops(servosphere,
#'                            summary.df = summary_df,
#'                            stop.threshold = 0.05)}
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
summaryStops <- function(list, summary.df = NA, stop.threshold = 0) {
   # Done to appease the R CMD check gods
   stimulus <- NULL
   id_stim <- NULL

   list <- list %>% purrr::map_if(is.data.frame, function(.x) {
      stops <- ifelse(.x$velocity <= stop.threshold, 0, .x$velocity)
      stops <- rle(stops)
      num_stops <- length(stops$lengths[stops$values == 0])
      len_stops <- mean(stops$lengths[stops$values == 0])
      out <- data.frame(id = .x$id[1],
                        number_stops = num_stops,
                        avg_length_stops = len_stops)
      if (any(names(.x) == "id_stim")) {
         out <- data.frame(id_stim = .x$id_stim[1],
                           number_stops = num_stops,
                           avg_length_stops = len_stops,
                           stringsAsFactors = FALSE)
      } else{
         out <- data.frame(id = .x$id[1],
                           number_stops = num_stops,
                           avg_length_stops = len_stops,
                           stringsAsFactors = FALSE)
      }

      if (any(!is.na(summary.df))){ # If summary.df is provided, join out with it
         # If data are split by stimulus, need to join by id_stim
         if (any(names(.x) == "id_stim")) {
            out <- inner_join(out, summary.df, by = "id_stim")
         } else {
            out <- inner_join(out, summary.df, by = "id")
         }

      } else {
         if (any(names(.x) == "id_stim")) {
            trial_cols <- .x %>%
               select(!!list$col.names, stimulus, -id_stim) %>%
               slice(1)
            out <- bind_cols(out, trial_cols)
         } else {
            trial_cols <- .x %>%
               select(!!list$col.names, stimulus, -id) %>%
               slice(1)
            out <- bind_cols(out, trial_cols)
         }
      }
   })
      magrittr::extract(list, 1:(length(list) - 1)) %>%
      bind_rows()
}


