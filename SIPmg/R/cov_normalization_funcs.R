#' Normalize feature coverages to estimate absolute abundance or relative coverage using MAG/contig coverage values with or without multiplying total DNA concentration of the fraction
#'
#' @param f_tibble Can be either of
#' (1) a tibble with first column "Feature" that contains bin IDs, and the rest of the columns represent samples with bins' coverage values.
#' (2) a tibble as outputted by the program "checkm coverage" from the tool CheckM. Please check CheckM documentation - https://github.com/Ecogenomics/CheckM on the usage for "checkm coverage" program
#' @param contig_coverage tibble with contig ID names ("Feature" column), sample columns with **same sample names as in f_tibble** containing coverage values of each contig, contig length in bp ("contig_length" column), and the MAG the contig is associated ("MAG" column) **with same MAGs as in Feature column of f_tibble dataset**.
#' @param sequencing_yield tibble containing sample ID ("sample" column) with **same sample names as in f_tibble** and number of reads in bp recovered in that sample ("yield" column).
#' @param approach Please choose the method for coverage normalization as "relative_coverage", "greenlon", "starr" to estimate only relative coverage without multiplying DNA concentration of fraction, or as per methods in Greenlon et al. - https://journals.asm.org/doi/full/10.1128/msystems.00417-22 or Starr et al. - https://journals.asm.org/doi/10.1128/mSphere.00085-21
#' @param fractions_df fractions data frame
#'A fractions file with the following columns
#' - Replicate: Depends on how many replicates the study has
#' - Fractions: Typically in the range of 2-24
#' - Buoyant_density: As calculated from the refractometer for each fraction and replicate
#' - Isotope: "12C", "13C", "14N", "15N" etc.
#' - DNA_concentration
#' - Sample: In the format "'isotope'_rep_#_fraction_#".
#'   For instance, "12C_rep_1_fraction_1"
#'@import magrittr
#'@importFrom rlang .data
#'@importFrom data.table setnames
#'@return tibble containing normalized coverage in required format with MAG name as first column and the normalized coverage values in each sample as the rest of the columns.
#'@export
#'
#'@examples
#'
#'data(f_tibble)
#' \donttest{
#' rel.cov = coverage_normalization(f_tibble)
#' }
#'

coverage_normalization = function(f_tibble, contig_coverage, sequencing_yield, fractions_df, approach = "relative_coverage"){
  Feature <- relative_coverage <- `DNA_concentration(ng/uL)` <- yield <- relative_abundance <- dna_conc <- sequins <- MAG <- Sample <- full_join <-  NULL
  rel.cov.temp = f_tibble %>% tibble::column_to_rownames(var = "Feature")
  rel.cov = sweep(rel.cov.temp,2,colSums(rel.cov.temp),`/`)
  rel.cov = rel.cov %>% tibble::rownames_to_column(var = "Feature")
  rm(rel.cov.temp)

  if (approach == "relative_coverage") {
  return(rel.cov)
  }  else if (approach == "starr") {

    rel.cov_long = tidyr::pivot_longer(rel.cov, cols = -Feature, names_to = "Sample", values_to = "relative_coverage")

    rel.cov_long = dplyr::inner_join(rel.cov_long, fractions_df) %>%
      dplyr::mutate(absolute_coverage = relative_coverage*`DNA_concentration(ng/uL)`)

    abs.cov_long = rel.cov_long %>%
      dplyr::select(Feature, Sample, absolute_coverage)

    absolute_coverage = tidyr::pivot_wider(abs.cov_long, names_from = Sample, values_from = absolute_coverage, id_cols = Feature)
    return(absolute_coverage)

  } else if (approach == "greenlon") {
    sequencing_yield = sequencing_yield %>% dplyr::select(Sample, yield)
    absolute_abundance = fractions_df %>% dplyr::select(Sample)

    absolute_abundance = absolute_abundance %>%
      dplyr::inner_join(sequencing_yield) %>%
      dplyr::mutate(relative_abundance = purrr::map(Sample, ~ dplyr::select(contig_coverage, Feature, contig_length, MAG, dplyr::all_of(.)))) %>%
      dplyr::mutate(relative_abundance = purrr::map2(relative_abundance, Sample, ~ .x %>% dplyr::rename(coverage = .y))) %>%
      dplyr::mutate(relative_abundance = purrr::map2(relative_abundance, yield, ~ .x %>% dplyr::mutate(yield = .y))) %>%
      dplyr::select(-yield) %>%
      dplyr::mutate(relative_abundance = purrr::map(relative_abundance,  ~ .x %>%
                                        dplyr::mutate(relative_abundance = contig_length*coverage/yield) %>%
                                          dplyr::select(-c(coverage, contig_length, yield)))) %>%
      dplyr::mutate(relative_abundance = purrr::map(relative_abundance, ~ .x %>% dplyr::group_by(MAG) %>%
                                        summarise(relative_abundance = sum(relative_abundance)))) %>%
      dplyr::inner_join(fractions_df %>% dplyr::select(Sample, `DNA_concentration(ng/uL)`)) %>%
      dplyr::rename(dna_conc = `DNA_concentration(ng/uL)`) %>%
      dplyr::mutate(relative_abundance = purrr::map2(relative_abundance, dna_conc, ~ .x %>% dplyr::mutate(dna_conc = .y))) %>%
      dplyr::select(-dna_conc) %>%
      dplyr::mutate(absolute_abundance = purrr::map(relative_abundance, ~ .x %>% dplyr::mutate(absolute_abundance = relative_abundance*dna_conc) %>%
                                                      dplyr::select(-c(relative_abundance, dna_conc)))) %>%
      dplyr::mutate(absolute_abundance = purrr::map2(absolute_abundance, Sample, ~ .x %>% data.table::setnames(new = .y, old = "absolute_abundance")))

    absolute_abundance_all = absolute_abundance$absolute_abundance %>%
      purrr::reduce(full_join, by = "MAG") %>%
      dplyr::rename(Feature = MAG) %>%
      dplyr::anti_join(sequins %>% select(Feature))
    rm(absolute_abundance)
    return(absolute_abundance_all)
  } else {
    stop("Error: Approach not recognized. If you want to use sequin scaling, check out scale_features functions")
  }
}


