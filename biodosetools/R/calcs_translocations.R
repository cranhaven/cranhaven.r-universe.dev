#' Calculate genomic conversion factor
#'
#' Method based on the paper by Lucas, J. N. et al. (1992). Rapid Translocation
#' Frequency Analysis in Humans Decades after Exposure to Ionizing Radiation.
#' International Journal of Radiation Biology, 62(1), 53-63.
#' <doi:10.1080/09553009214551821>.
#'
#' @param dna_table DNA content fractions table. Can be \code{dna_content_fractions_morton} or \code{dna_content_table_ihgsc}.
#' @param chromosomes Vector of stained chromosomes.
#' @param colors Vector of colors of the stains.
#' @param sex Sex of the individual.
#'
#' @return Numeric value of genomic conversion factor.
#' @export
#' @example man/examples/calculate_genome_factor_example.R
#' @importFrom rlang .data
calculate_genome_factor <- function(dna_table, chromosomes, colors, sex) {
  # Construct color/chromosome table
  color_table <- data.frame(
    color = colors,
    chromosome = as.character(chromosomes)
  )

  # Full table
  full_table <- dplyr::inner_join(color_table, dna_table, by = "chromosome") %>%
    dplyr::group_by(.data$color) %>%
    dplyr::summarise(
      genome_factor = sum(base::get(paste0("fraction_", sex)))
    )

  # Calculate first sum
  single_sum <- full_table %>%
    dplyr::select("genome_factor") %>%
    dplyr::summarise(
      single_sum = sum(.data$genome_factor * (1 - .data$genome_factor))
    ) %>%
    dplyr::pull(.data$single_sum)

  # Calculate second sum
  if (nrow(full_table) >= 2) {
    cross_sum <- full_table[["genome_factor"]] %>%
      utils::combn(2) %>%
      t() %>%
      as.data.frame() %>%
      dplyr::summarise(
        cross_sum = sum(.data$V1 * .data$V2)
      ) %>%
      dplyr::pull(.data$cross_sum)
  } else {
    cross_sum <- 0
  }

  return(2 / 0.974 * (single_sum - cross_sum))
}

#' Calculate Sigurdson's translocation rate
#'
#' Method based on the paper by Sigurdson, A. J. et al. (2008). International
#' study of factors affecting human chromosome translocations. Mutation
#' Research/Genetic Toxicology and Environmental Mutagenesis, 652(2), 112-121.
#' <doi:10.1016/j.mrgentox.2008.01.005>.
#'
#' @param cells Number of cells \code{N}.
#' @param genome_factor Genomic conversion factor.
#' @param age_value Age of the individual.
#' @param sex_bool If \code{TRUE}, \code{sex_value} will be used.
#' @param sex_value Sex of the individual, either "male" of "female".
#' @param smoker_bool Whether the individual smokes or not.
#' @param ethnicity_value Ethnicity of the individual.
#' @param region_value Region of the individual.
#'
#' @return Numeric value of translocation rate.
#' @example man/examples/calculate_trans_rate_sigurdson_example.R
#' @export
calculate_trans_rate_sigurdson <- function(cells, genome_factor, age_value,
                                           sex_bool = FALSE, sex_value = "none",
                                           smoker_bool = FALSE,
                                           ethnicity_value = "none", region_value = "none") {
  age_trans_frequency <- function(age) {
    trans_frequency <- exp(-7.925) + exp(-9.284) * (age * exp(0.01062 * age))
    return(trans_frequency)
  }

  sex_trans_list <- c("none" = 1, "male" = 1, "female" = 0.92)
  smoke_trans_list <- c("FALSE" = 1, "TRUE" = 1.19)
  ethnicity_trans_list <- c("none" = 1, "white" = 1, "asian" = 1.23, "black" = 0.84, "other" = 1.06)
  region_trans_list <- c("none" = 1, "n-america" = 1, "w-europe" = 0.99, "c-europe" = 1.75, "e-europe" = 1.75, "asia" = 0.86)

  sex_value <- ifelse(sex_bool, sex_value, "none")

  sex_trans_frequency <- sex_trans_list[[sex_value]]
  smoke_trans_frequency <- smoke_trans_list[[as.character(smoker_bool)]]
  ethnicity_trans_frequency <- ethnicity_trans_list[[ethnicity_value]]
  region_trans_frequency <- region_trans_list[[region_value]]

  # Expected aberrations
  expected_aberr <- cells * genome_factor *
    age_trans_frequency(age_value) *
    sex_trans_frequency *
    smoke_trans_frequency *
    ethnicity_trans_frequency *
    region_trans_frequency

  return(expected_aberr)
}

#' Calculate manual translocation rate
#'
#' @param cells Number of cells \code{N}.
#' @param genome_factor Genomic conversion factor.
#' @param expected_aberr_value Expected aberrations.
#'
#' @return Numeric value of translocation rate.
#' @example man/examples/calculate_trans_rate_manual_example.R
#' @export
calculate_trans_rate_manual <- function(cells, genome_factor, expected_aberr_value) {
  # Expected aberrations
  expected_aberr <- cells * genome_factor * expected_aberr_value

  return(expected_aberr)
}
