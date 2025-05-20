

#' a list of life-history / life-table data for a hypothetical species
#'
#' species_1 for examples.
#' @source Just values that might be typical of a fish.
#' @docType data
#' @name species_1_life_history
#' @usage species_1_life_history
NULL



#' a list of life-history / life-table data for another hypothetical species
#'
#' species_2 for examples.  Note that I just set the male and female
#' rates and parameters similar.
#' @source This is something used for simulation testing
#' @docType data
#' @name species_2_life_history
#' @usage species_2_life_history
NULL


#' a list of examples of ancestry-match matrices
#'
#' This is a list of matrices with names that describe what they
#' represent. The names are Relationship_Number-of-Generations, like:
#' "Unrelated_2gen", "Self_2gen", "Unrelated_3gen", "Unrelated_4gen",
#' "Unrelated_1gen", "Father_Offspring_2gen", "FullSibling_2gen", "PatHalfSibling_2gen",
#' "FullCousin_2gen", "HalfAuntNiece_2gen".
#' @source I just made these for some illustrative figures in the manuscript about this
#' R package.
#' @docType data
#' @name example_amms
#' @usage example_amms
NULL


#' relationship zone names
#'
#' A simple character vector of 15 relationship zones in the order they
#' are encountered when traversing an ancestry match matrix out to four
#' generations
#' @source I simply defined these
#' @docType data
#' @name relationship_zone_names
NULL


#' The result of running spip in the species_1_simulation vignette.
#'
#' This is stored as package data so that the vignette can be written
#' even if spip is not installed on the system.
#'
#' @source Simulation results
#' @docType data
#' @name species_1_slurped_results
#' @usage species_1_slurped_results
NULL


#' The result of running spip in the species_1_simulation vignette and slurping out with num_generations = 1.
#'
#' This is stored as package data so that the vignette can be written
#' even if spip is not installed on the system.
#'
#' @source Simulation results
#' @docType data
#' @name species_1_slurped_results_1gen
#' @usage species_1_slurped_results_1gen
NULL


#' The result of running spip in the species_1_simulation vignette with 100 loci.
#'
#' This is stored as package data so that the vignette can be written
#' even if spip is not installed on the system. This particular version
#' stores the results of running `run_spip()` calling for 100 loci segregating
#' in the population, then slurping the results up with `slurp_spip()`.
#' @source Simulation results
#' @docType data
#' @name species_1_slurped_results_100_loci
#' @usage species_1_slurped_results_100_loci
NULL


#' The result of running spip and slurping the output in the three population case with no migration
#'
#' This is stored as package data so that the vignette can be written
#' even if spip is not installed on the system.
#'
#' @source Simulation results
#' @docType data
#' @name three_pops_no_mig_slurped_results
#' @usage three_pops_no_mig_slurped_results
NULL


#' The result of running spip and slurping the output in the three population case with migration
#'
#' This is stored as package data so that the vignette can be written
#' even if spip is not installed on the system.
#'
#' @source Simulation results
#' @docType data
#' @name three_pops_with_mig_slurped_results
#' @usage three_pops_with_mig_slurped_results
NULL


#' A half-first cousin ancestry match matrix
#'
#' Just a simple AMM to use in some examples
#'
#' @source Simply wrote this down.
#' @docType data
#' @name half_first_cousin_amm
#' @usage half_first_cousin_amm
NULL


