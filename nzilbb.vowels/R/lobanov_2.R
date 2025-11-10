#' Apply Lobanov 2.0 normalisation
#'
#' @description
#' `lobanov_2()` takes a data frame where the first four columns are:
#' 1. speaker identifiers,
#' 2. vowel identifiers,
#' 3. first formant values in Hertz,
#' 4. second formant values in Hertz.
#'
#' It returns a dataframe with two additional columns, `F1_lob2` and `F2_lob2`,
#' containing normalised formant values.
#'
#' @details
#' This functions applies Lobanov 2.0 normalisation presented in
#' Brand et al. (2021). This variant
#' of Lobanov normalisation is designed to work for datasets whether the vowel
#' types have different token counts from one
#' another. The Lobanov 2.0 value for a vowel is given by \deqn{F_{lobanov2.0_i}
#' = \frac{F_{raw_i} - \mu(\mu_{vowel_1}, \ldots,
#' \mu_{vowel_n})}{\sigma(\mu_{vowel_1}, \ldots, \mu_{vowel_n})}} where, for
#' ease of notation, we assume all values are from a single speaker. We signify
#' the n vowel types as vowel_1, \ldots, vowel_2, while i indicates the formant
#' number. We implement the function for F1 and F2.
#'
#' @param vowel_data a dataframe whose first four columns are speaker ids,
#' vowel ids, F1 values, and F2 values.
#' @returns a dataframe matching the input dataframe with additional columns
#'   `F1_lob2` and `F2_lob2`, containing the lobanov normalised F1 and F2 values
#'   respectively.
#' @importFrom dplyr group_by ungroup mutate summarise select left_join
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
#'
#' @examples
#' normed_vowels <- lobanov_2(onze_vowels)
#' head(normed_vowels)
#'
#' @references
#'   Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy (2021):
#'   Systematic co-variation of monophthongs across speakers of New Zealand
#'   English. Journal of Phonetics. Elsevier. 88. 101096.
#'   doi:10.1016/j.wocn.2021.101096
#' @export
lobanov_2 <- function(vowel_data) {

  base::stopifnot(
    "Column one must be a factor or character vector of speaker ids." =
      is.character(vowel_data[[1]]) | is.factor(vowel_data[[1]]),
    "Column two must be a factor or character vector of vowel ids." =
      is.character(vowel_data[[2]]) | is.factor(vowel_data[[2]]),
    "Column three must be a numeric vector of F1 values." =
      is.numeric(vowel_data[[3]]),
    "Column four must be a numeric vector of F2 values." =
      is.numeric(vowel_data[[4]])
  )

  # Assume speaker is first column, vowel is second, F1 is third, and F2 is
  # fourth.
  speaker_col_name <- names(vowel_data)[[1]]
  vowel_col_name <- names(vowel_data)[[2]]
  F1_col_name <- names(vowel_data)[[3]]
  F2_col_name <- names(vowel_data)[[4]]

  vowel_means <- vowel_data |>
    group_by(.data[[speaker_col_name]], .data[[vowel_col_name]]) |>
    summarise(
      vowel_mean_F1 = base::mean(.data[[F1_col_name]]),
      vowel_mean_F2 = base::mean(.data[[F2_col_name]]),
      vowel_sd_F1 = stats::sd(.data[[F1_col_name]]),
      vowel_sd_F2 = stats::sd(.data[[F2_col_name]]),
    ) |>
    group_by(.data[[speaker_col_name]]) |>
    mutate(
      mean_of_means_F1 = base::mean(.data$vowel_mean_F1),
      mean_of_means_F2 = base::mean(.data$vowel_mean_F2),
      sd_of_means_F1 = stats::sd(.data$vowel_mean_F1),
      sd_of_means_F2 = stats::sd(.data$vowel_mean_F2)
    )

  vowel_data |>
    left_join(
      vowel_means,
      by = c(speaker_col_name, vowel_col_name)
    ) |>
    ungroup() |>
    mutate(
      F1_lob2 = (.data[[F1_col_name]] - .data$mean_of_means_F1)/
        .data$sd_of_means_F1,
      F2_lob2 = (.data[[F2_col_name]] - .data$mean_of_means_F2)/
        .data$sd_of_means_F2
    ) |>
    # Remove working variables.
    select(
      -("vowel_mean_F1":"sd_of_means_F2")
    )

}

