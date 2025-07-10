#' Select: Determines Species Probabilities Based on Functional Traits
#'
#' This package determines species probabilities (i.e., relative abundances)
#' that satisfy a given functional trait profile. Restoring resilient ecosystems
#' requires a flexible framework for selecting assemblages that are based on the
#' functional traits of species. However, current trait-based models have been
#' limited to algorithms that can only select species by optimising specific trait
#' values, and could not elegantly accommodate the common desire among restoration
#' ecologists to produce functionally diverse assemblages. We have solved this
#' problem by applying a non-linear optimisation algorithm that optimises Rao Q,
#' a closed-form functional trait diversity index that incorporates species
#' abundances, subject to other linear constraints. This framework generalises
#' previous models that only optimised the entropy of the community, and can
#' optimise both functional diversity and entropy simultaneously.
#'
#' The Select package has two functions:
#'
#' @section Select functions:
#'
#' The selectSpecies() function is used to generate species assemblages
#' (relative abundances/probabilities) based on a functional trait profile.
#' The function can constrain the abundances to conform to certain average trait values,
#' and it can simultanesouly optimize functional diveristy.
#'
#' The plotProbs() function plots the resulting species abundances (probabilities)
#' using the resulting object from the selectSpecies() function.
#'
#' @name Select
#' @docType package
NULL
