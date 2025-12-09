#' Compute numerical daily dose from free-text prescribing instructions
#'
#' The function calls the \R package \strong{doseminer} to extract dose
#' information from free-text prescribing instructions, then computes the
#' average numerical daily dose according to a given decision rule.
#'
#' The general formula for computing numerical daily dose (ndd) is given by
#' \deqn{\mbox{ndd} = \mbox{DF} \times \mbox{DN} / \mbox{DI},}{ndd = DF * DN / DI,}
#' where
#' \describe{
#' \item{DF}{is dose frequency, the number of dose 'events' per day}
#' \item{DN}{is dose number, or number of units of drug taken during each dose 'event'}
#' \item{DI}{is dose interval, or the number of days between 'dose days', where an interval of 1 means every day}
#' }
#' Prescriptions can have a variable dose frequency or dose number, such as
#' '2-4 tablets up to 3 times per day'. In this case, the user can choose
#' to reduce these ranges to single values by taking the minimum, maximum or
#' average of these endpoints.
#'
#' @param data a data frame containing free-text prescribing instructions in a
#' column called \code{text}.
#' @param dose_fn function to summarise range of numbers by a single value
#' @param freq_fn function to summarise range of frequencies by a single value
#' @param interval_fn function to summarise range of intervals by a single value
#'
#' @examples
#' compute_ndd(dataset1, min, min, mean)
#'
#' @return
#' A data frame mapping the raw \code{text} to structured dosage information.
#'
#' @importFrom rlang .data
#' @importFrom doseminer extract_from_prescription
#' @importFrom purrr map_dbl
#' @import dplyr tidyr
#' @export
compute_ndd <- function(data, dose_fn = mean, freq_fn = mean, interval_fn = mean) {
  dose_fn <- match.fun(dose_fn)
  freq_fn <- match.fun(freq_fn)
  interval_fn <- match.fun(interval_fn)

  extracted <- unique(data[['text']]) %>%
    doseminer::extract_from_prescription() %>%
    dplyr::mutate(dplyr::across(c(.data$dose, .data$freq, .data$itvl),
                                strsplit, split = '-')) %>%
    dplyr::mutate(dplyr::across(c(.data$dose, .data$freq, .data$itvl),
                                lapply, FUN = as.numeric)) %>%
    dplyr::mutate(dose = purrr::map_dbl(.data$dose, dose_fn),
                  freq = purrr::map_dbl(.data$freq, freq_fn),
                  itvl = purrr::map_dbl(.data$itvl, interval_fn),
                  ndd = .data$dose * .data$freq / .data$itvl) %>%
    dplyr::select(text = .data$raw, .data$ndd, .data$optional)

  dplyr::left_join(data, extracted, by = 'text')
}
