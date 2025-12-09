#' Run drug preparation algorithm
#'
#' @param data data frame containing prescription data
#' @param plausible_values data frame containing variables \code{prodcode},
#' \code{min_qty}, \code{max_qty}, \code{min_ndd}, \code{max_ndd} describing
#' plausible ranges for values for each drug
#' @param decisions character vector of length 10
#'
#' @examples
#' plausible_values <- data.frame(
#'   prodcode = c('a', 'b', 'c'),
#'   min_qty = 0,
#'   max_qty = c(50, 100, 200),
#'   min_ndd = 0,
#'   max_ndd = c(10, 20, 30)
#' )
#' drug_prep(example_therapy,
#'           plausible_values,
#'           decisions = c('a', 'a', 'a', 'a', 'a',
#'                         'c', 'a', 'a', 'a', 'a'))
#'
#' @family decision functions
#'
#' @return A data frame including estimated \code{stop_date} for each prescription
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
drug_prep <- function(data,
                      plausible_values,
                      decisions = rep('a', 10)) {
  data <- dplyr::left_join(data, plausible_values, by = 'prodcode')
  # Implausible quantities
  data <- decision_1(data, decisions[1]) %>%
    dplyr::select(-.data$min_qty, -.data$max_qty)
  # Missing quantities
  data <- decision_2(data, decisions[2])
  # Implausible numerical daily doses
  data <- decision_3(data, decisions[3]) %>%
    dplyr::select(-.data$min_ndd, -.data$max_ndd)
  # Missing numerical daily doses
  data <- decision_4(data, decisions[4])
  # Choose method for calculating prescription duration (note switched order)
  data <- decision_6(data, decisions[6])
  # Truncate/delete overly long prescription durations
  data <- decision_5(data, decisions[5])
  # Impute missing prescription durations
  data <- decision_7(data, decisions[7])
  # Disambiguate prescriptions with the same start_date
  data <- decision_8(data, decisions[8])
  # Compute stop_date from duration
  data <- dplyr::mutate(data, stop_date = .data$start_date + .data$duration)
  # Disambiguate overlapping prescription intervals
  data <- decision_9(data, decisions[9])
  # Close short gaps between successive prescriptions
  data <- decision_10(data, decisions[10])
  return(data)
}

#' Human-friendly interface to the drug prep algorithm
#'
#' A helper function that allows specifying decision rules using English
#' words rather than alphanumeric codes. Translates the rules into the
#' corresponding codes and then passes them to \code{\link{drug_prep}} functions.
#'
#' @param implausible_qty implausible total drug quantities
#' @param implausible_ndd implausible daily dosage
#' @param implausible_duration overly-long prescription durations
#' @param missing_qty missing total drug quantities
#' @param missing_ndd missing daily dosage
#' @param missing_duration missing prescription duration
#' @param calculate_duration formula or variable to compute prescription duration
#' @param clash_start how to disambiguate prescriptions that start on the same date
#' @param overlapping how to handle prescription periods that overlap with one another
#' @param small_gaps how to handle short gaps between successive prescriptions
#'
#' The argument \code{decision_phrases} may contain the following terms (without brackets, separated with spaces).
#' Additional or incorrectly-named elements will be ignored.
#' \describe{
#' \item{implausible_qty}{(ignore|missing|mean|median|mode|next|previous) (individual|practice|population)}
#' \item{implausible_ndd}{(ignore|missing|mean|median|mode|next|previous) (individual|practice|population)}
#' \item{implausible_duration}{(ignore|missing|truncate) (6|12|24)}
#' \item{missing_qty}{(ignore|mean|median|mode|next|previous) (individual|practice|population)}
#' \item{missing_ndd}{(ignore|mean|median|mode|next|previous) (individual|practice|population)}
#' \item{missing_duration}{(ignore|mean) (individual|population|both)}
#' \item{calculate_duration}{(numdays|dose_duration|qty/ndd)}
#' \item{clash_start}{(ignore|mean|shortest|longest|sum)}
#' \item{overlapping}{(allow|shift)}
#' \item{small_gaps}{(ignore|close) (15|30|60)}
#' }
#'
#' @examples
#' make_decisions('ignore',
#'                'mean population',
#'                'missing',
#'                'mean practice',
#'                'truncate 6',
#'                'qty / ndd',
#'                'mean individual',
#'                'mean',
#'                'allow',
#'                'close 15')
#'
#' @return
#' A character vector suitable for passing to the \code{decisions} argument of
#' the \code{\link{drug_prep}} function.
#'
#' @import stringr
#' @export
make_decisions <- function(implausible_qty,
                           missing_qty,
                           implausible_ndd,
                           missing_ndd,
                           implausible_duration,
                           calculate_duration,
                           missing_duration,
                           clash_start,
                           overlapping,
                           small_gaps) {
  parse1or3 <- function(x) {
    if (stringr::str_detect(x, 'ignore'))
      return('a')
    if (stringr::str_detect(x, 'missing'))
      return('b')
    words <- stringr::str_split(x, ' ')[[1]]
    if (length(words) < 2)
      stop(paste('Too few terms in string:', x))
    paste0(c(mean = 'c',
             median = 'd',
             mode = 'e',
             'next' = 'f',
             previous = 'g')[words[1]],
           c(individual = '1',
             practice = '2',
             population = '3')[words[2]])
  }

  parse2or4 <- function(x) {
    if (stringr::str_detect(x, 'ignore'))
      return('a')
    words <- stringr::str_split(x, ' ')[[1]]
    if (length(words) < 2)
      stop(paste('Too few terms in string:', x))
    paste0(c(mean = 'b',
             median = 'c',
             mode = 'd',
             'next' = 'e',
             previous = 'f')[words[1]],
           c(individual = '1',
             practice = '2',
             population = '3')[words[2]])
  }

  parse5 <- function(x) {
    if (stringr::str_detect(x, 'ignore'))
      return('a')
    words <- stringr::str_split(x, ' ')[[1]]
    if (length(words) < 2)
      stop(paste('Too few terms in string:', x))
    paste(c(missing = 'b',
            truncate = 'c')[words[1]],
          words[2],
          sep = '_')
  }

  parse6 <- function(x) {
    switch(stringr::str_remove_all(x, '\\s'),
           numdays = 'a',
           dose_duration = 'b',
           'qty/ndd' = 'c')
  }

  parse7 <- function(x) {
    switch(x,
           ignore = 'a',
           'mean individual' = 'b',
           'mean population' = 'c',
           'mean both' = 'd')
  }

  parse8 <- function(x) {
    switch(x,
           ignore = 'a',
           mean = 'b',
           shortest = 'c',
           longest = 'd',
           sum = 'e')
  }

  parse9 <- function(x) {
    switch(x,
           allow = 'a',
           shift = 'b')
  }

  parse10 <- function(x) {
    if (stringr::str_detect(x, 'ignore'))
      return('a')
    words <- stringr::str_split(x, ' ')[[1]]
    paste('b', words[2], sep = '_')
  }

  c(implausible_qty = parse1or3(implausible_qty),
    missing_qty     = parse2or4(missing_qty),
    implausible_ndd = parse1or3(implausible_ndd),
    missing_ndd     = parse2or4(missing_ndd),
    implausible_duration = parse5(implausible_duration),
    calculate_duration   = parse6(calculate_duration),
    missing_duration     = parse7(missing_duration),
    clash_start = parse8(clash_start),
    overlapping = parse9(overlapping),
    small_gaps  = parse10(small_gaps)
  )
}

#' Decision 1: impute implausible total quantities
#'
#' A light wrapper around \code{\link{impute_qty}}.

#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#' \item{"a"}{do nothing; leave implausible values as-is}
#' \item{"b"}{set implausible values to missing}
#' \item{"c1"}{set to mean for individual's prescriptions for that drug}
#' \item{"c2"}{set to mean for practice's prescriptions for that drug}
#' \item{"c3"}{set to mean for populations's prescriptions for that drug}
#' \item{"d1"}{set to median for individual's prescriptions for that drug}
#' \item{"d2"}{set to median for practice's prescriptions for that drug}
#' \item{"d3"}{set to median for population's prescriptions for that drug}
#' \item{"e1"}{set to mode for individual's prescriptions for that drug}
#' \item{"e2"}{set to mode for practice's prescriptions for that drug}
#' \item{"e3"}{set to mode for population's prescriptions for that drug}
#' \item{"f1"}{use value of individual's next prescription}
#' \item{"f2"}{use value of practice's next prescription}
#' \item{"f3"}{use value of population's next prescription}
#' \item{"g1"}{use value of individual's previous prescription}
#' \item{"g2"}{use value of practice's previous prescription}
#' \item{"g3"}{use value of population's previous prescription}
#' }
#'
#' @note Decisions \code{f} and \code{g} are not yet implemented.
#'
#' @family decision functions
#'
decision_1 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', 'b', paste0(rep(letters[3:7], each = 3), 1:3)))
  impute_qty(data,
             method = get_implausible_method(decision),
             where = function(q) q < data[['min_qty']] | q > data[['max_qty']],
             group = get_decision_group(decision),
             replace_with = NA_real_)
}

#' Decision 2: impute missing total quantities
#'
#' A light wrapper around \code{\link{impute_qty}}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#' \item{"a"}{Leave as missing (implicitly drop this prescription)}
#' \item{"b1"}{set to mean for individual's prescriptions for that drug}
#' \item{"b2"}{set to mean for practice's prescriptions for that drug}
#' \item{"b3"}{set to mean for populations's prescriptions for that drug}
#' \item{"c1"}{set to median for individual's prescriptions for that drug}
#' \item{"c2"}{set to median for practice's prescriptions for that drug}
#' \item{"c3"}{set to median for population's prescriptions for that drug}
#' \item{"d1"}{set to mode for individual's prescriptions for that drug}
#' \item{"d2"}{set to mode for practice's prescriptions for that drug}
#' \item{"d3"}{set to mode for population's prescriptions for that drug}
#' \item{"e1"}{use value of individual's next prescription}
#' \item{"e2"}{use value of practice's next prescription}
#' \item{"e3"}{use value of population's next prescription}
#' \item{"f1"}{use value of individual's previous prescription}
#' \item{"f2"}{use value of practice's previous prescription}
#' \item{"f3"}{use value of population's previous prescription}
#' }
#'
#' @note Decisions \code{e} and \code{f} are not yet implemented.
#'
#' @family decision functions
#'
decision_2 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', paste0(rep(letters[2:6], each = 3), 1:3)))
  impute_qty(data,
             method = get_missing_method(decision),
             where = is.na,
             group = get_decision_group(decision))
}

#' Decision 3: impute implausible daily doses
#'
#' A light wrapper around \code{\link{impute_ndd}}.
#'
#' @inheritParams decision_1
#'
#' @note Decisions \code{f} and \code{g} are not yet implemented.
#'
#' @family decision functions
#'
decision_3 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', 'b', paste0(rep(letters[3:7], each = 3), 1:3)))
  impute_ndd(data,
             method = get_implausible_method(decision),
             where = function(n) n < data[['min_ndd']] | n > data[['max_ndd']],
             group = get_decision_group(decision),
             replace_with = NA_real_)
}

#' Decision 4: impute missing daily doses
#'
#' A light wrapper around \code{\link{impute_ndd}}.
#'
#' @inheritParams decision_2
#'
#' @note Decisions \code{e} and \code{f} are not yet implemented.
#'
#' @family decision functions
#'
decision_4 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', paste0(rep(letters[2:6], each = 3), 1:3)))
  impute_ndd(data,
             method = get_missing_method(decision),
             where = is.na,
             group = get_decision_group(decision))
}

#' Decision 5: impute implausible prescription durations
#'
#' A light wrapper around \code{\link{clean_duration}}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#'   \item{"a"}{leave duration as-is}
#' 		\item{"b_6"}{set to missing if > 6 months}
#' 		\item{"b_12"}{set to missing if > 12 months}
#' 		\item{"b_24"}{set to missing if > 24 months}
#' 		\item{"c_6"}{set to 6 months if > 6 months}
#' 		\item{"c_12"}{set to 12 months if > 12 months}
#' 		\item{"c_24"}{set to 24 months if > 24 months}
#' }
#'
#' @family decision functions
#'
decision_5 <- function(data, decision = 'a') {
  decision <- match.arg(decision, c('a',
                                    paste(rep(c('b', 'c'), each = 3),
                                          c(6, 12, 24), sep = '_')))
  if (decision == 'a') return(data)
  clean_duration(data,
                 max_months = as.integer(substring(decision, 3)),
                 method = switch(substring(decision, 2, 2),
                                 b = 'remove',
                                 c = 'truncate'))
}

#' Decision 6: choose method of calculating prescription duration
#'
#' This is just shorthand for defining a column equal to one of the specified
#' formulae. If the column(s) corresponding to \code{decision} are missing, an
#' error will be thrown.
#' If you have already calculated or obtained the column \code{duration} from
#' elsewhere, this step is not necessary.
#'
#' @note This step actually takes place \emph{before} \code{\link{decision_5}}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#' \item{"a"}{\code{numdays}}
#' 	 \item{"b"}{\code{dose_duration}}
#' 	 \item{"c"}{\code{qty / ndd}}
#' }
#'
#' @family decision functions
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
decision_6 <- function(data, decision = 'c') {
  decision <- match.arg(decision, letters[1:3])
  dplyr::mutate(data, duration = switch(decision,
                                        a = .data$numdays,
                                        b = .data$dose_duration,
                                        c = .data$qty / .data$ndd))
}

#' Decision 7: impute missing prescription durations
#'
#' A light wrapper around \code{\link{impute_duration}}.
#'
#' @param data a data frame
#' @param decision one  of the following strings:
#' \describe{
#' \item{"a"}{Leave missing durations as-is (implicitly drop the prescription)}
#' \item{"b"}{Use mean prescription duration for that drug, for that individual}
#' \item{"c"}{Use mean prescription duration for that drug, for the population}
#' \item{"d"}{Use individual mean duration; if not available use population mean}
#' }
#'
#' @family decision functions
#'
decision_7 <- function(data, decision = 'a') {
  decision <- match.arg(decision, letters[1:4])
  out <- impute_duration(data,
                         method = switch(decision,
                                         a = 'replace',
                                         b = 'mean',
                                         c = 'mean',
                                         d = 'mean'),
                         group = switch(decision,
                                        b = 'patid',
                                        c = 'population',
                                        d = 'patid'))
  if (decision != 'd') return(out)
  # If any individuals have no non-missing values, the previous
  # step will return duration of NaN. Run one more iteration to fill in those:
  impute_duration(out, method = 'mean', group = 'population')
}

#' Decision 8: disambiguate prescriptions with the same start date
#'
#' A light wrapper around \code{\link{impute_duration}}, followed by removing
#' duplicate rows with the same combination of \code{prodcode}, \code{patid}
#' and \code{start_date}.
#'
#' @param data a data frame
#' @param decision one of the following strings
#' \describe{
#' \item{"a"}{do nothing}
#'  \item{"b"}{replace with a prescription of duration equal to the mean}
#'  \item{"c"}{choose the shortest prescription}
#'  \item{"d"}{choose longest prescription}
#'  \item{"e"}{replace with a prescription of duration equal to the sum}
#' }
#'
#' @family decision functions
#'
#' @import dplyr
#' @importFrom rlang .data
decision_8 <- function(data, decision = 'a') {
  decision <- match.arg(decision, letters[1:5])
  if (decision == 'a') return(data)
  impute_duration(data,
                  method = switch(decision,
                                  b = 'mean',
                                  c = 'min',
                                  d = 'max',
                                  e = 'sum'),
                  where = function(x) length(x) > 1,
                  group = c('patid', 'start_date')) %>%
    dplyr::group_by(.data$prodcode, .data$patid, .data$start_date) %>%
    dplyr::slice(1L) %>% # remove duplicates
    dplyr::ungroup()
}

#' Decision 9: handle overlapping prescription periods
#'
#' In situations where one prescription starts before another (for the same
#' patient and drug) finishes, this function will either implicitly sum the
#' doses (i.e. do nothing) or it will divide the intervals into non-overlapping
#' subsets, shifting these sub-intervals forward in time until there is no
#' overlap.
#'
#' The underlying algorithm for shifting overlapping intervals is implemented
#' by the internal function \code{shift_interval}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
##' \describe{
##'  \item{"a"}{allow overlapping prescriptions (implicitly sum doses)}
##'  \item{"b"}{move later prescription to next available time that this product is not prescribed}
##' }
#'
#' @family decision functions
#'
#' @import dplyr
#' @importFrom rlang .data
decision_9 <- function(data, decision = 'a') {
  decision <- match.arg(decision, c('a', 'b'))
  if (decision == 'a') {
    return(data)
  } else isolate_overlaps(data) %>%
    dplyr::group_by(.data$patid, .data$prodcode) %>%
    dplyr::arrange(.data$start_date) %>%
    dplyr::group_modify(~ shift_interval(.x) %>%
                          dplyr::select(-.data$patid, -.data$prodcode),
                        .keep = TRUE) %>%
    dplyr::ungroup()
}

#' Decision 10: close small gaps between successive prescriptions
#'
#' Where one prescription (for the same drug and patient) starts only a short
#' time after the previous finishes, this function can close the gap, as if
#' the prescription was continuous over the entire period.
#'
#' The underlying function is called \code{close_small_gaps}
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#'  \item{"a"}{do nothing}
#'  \item{"b_15"}{change stop date of first prescription to start date of next if gap is \eqn{\leq 15} days}
#'  \item{"b_30"}{change stop date of first prescription to start date of next if gap is \eqn{\leq 30} days}
#'  \item{"b_60"}{change stop date of first prescription to start date of next if gap is \eqn{\leq 60} days}
#' }
#'
#' @family decision functions
#'
decision_10 <- function(data, decision = 'a') {
  decision <- match.arg(decision, c('a', 'b_15', 'b_30', 'b_60'))
  min_gap <- if (decision == 'a') 0 else as.integer(substring(decision, 3))
  close_small_gaps(data, min_gap)
}

get_decision_group <- function(decision_string) {
  switch(substring(decision_string, 2, 2),
         'population',
         '1' = 'patid',
         '2' = 'pracid',
         '3' = 'population')
}

get_implausible_method <- function(decision_string) {
  switch(substring(decision_string, 1, 1),
         a = 'ignore',
         b = 'replace',
         c = 'mean',
         d = 'median',
         e = 'mode',
         stop('Decision not yet implemented'))
}

get_missing_method <- function(decision_string) {
  switch(substring(decision_string, 1, 1),
         a = 'ignore',
         b = 'mean',
         c = 'median',
         d = 'mode',
         stop('Decision not yet implemented'))
}


