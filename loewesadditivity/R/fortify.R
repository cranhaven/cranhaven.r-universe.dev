
#' Put GIA measurements into a dplyr format
#'
#' @param data data frame of GIA measurements
#' \describe{
#'  \item{well}{one of "IRBC", "uRBC", "RPMI", or "comb"}
#'  \item{dose_A}{dose of A in mg/mL}
#'  \item{dose_B}{dose of B in mg/mL}
#'  \item{exp(X)(Y)rep(Z)}{ where X = 1 or 2, Y = a or b, and Z = 1, 2, or 3}
#'  }
#' @return long data frame with columns well, dose_A, dose_B, plate, exp_num (experiment number), plate (a or b), rep_num (repetition number), gia_mean, and average iRBC and uRBC
#' @export
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @examples
#' df <- loewesadditivity::rh5_ama1ron2
#' df$dose_A <- df$RH5
#' df$dose_B <- df$AMA1RON2
#' fortified_df <- fortify_gia_data(df)
#' head(fortified_df)
fortify_gia_data <- function(data){

  gia_data <- data

  if(!("well" %in% colnames(gia_data))){
    gia_data$well <- "comb"
  }

  gia_long <- gia_data %>%
    dplyr::select(.data$well, .data$dose_A, .data$dose_B,
                  dplyr::matches("exp.+rep.+")) %>%
    tidyr::gather(key = "key", value = "gia_unscaled", -.data$well,
                  -.data$dose_A, -.data$dose_B) %>%
    dplyr::mutate(id = gsub("exp|rep", "", .data$key)) %>%
    dplyr::mutate(exp_num = substr(.data$id, 1, 1),
                  plate = substr(.data$id, 2, 2),
                  rep_num = substr(.data$id, 3, 3))

  well_df <- gia_long %>% dplyr::group_by(.data$well) %>%
    dplyr::summarize(mean = mean(.data$gia_unscaled, na.rm = TRUE))

  ## Scale by uRBC and iRBC
  if(any(well_df$well == "uRBC")){
    gia_long$uRBC <- well_df$mean[well_df$well == "uRBC"]
  } else{
    gia_long$uRBC <- 0
  }
  if(any(well_df$well == "iRBC")){
    gia_long$iRBC <- well_df$mean[well_df$well == "iRBC"]
  } else{
    gia_long$iRBC <- 100
  }
  if(any(well_df$well == "RPMI")){
    gia_long$RPMI <- well_df$mean[well_df$well == "RPMI"]
  } else{
    gia_long$RPMI <- 0
  }



  gia_mean <- gia_long %>%
    dplyr::filter(.data$well %in% c("comb", "RPMI")) %>%
    dplyr::group_by(.data$well, .data$dose_A, .data$dose_B,
                    .data$uRBC, .data$iRBC, .data$exp_num, .data$plate) %>%
    dplyr::summarize(B = mean(.data$gia_unscaled, na.rm = TRUE)) %>%
    dplyr::mutate(GIA = 100 * (1 - ((.data$B - .data$uRBC)/(.data$iRBC - .data$uRBC)))) %>%
    dplyr::mutate(GIA = ifelse(is.nan(.data$GIA), NA, .data$GIA)) %>%
    dplyr::select(-.data$B) %>% na.omit()
  return(gia_mean)
}


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
