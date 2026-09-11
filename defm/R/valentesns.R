#' Valente's SNS data
#'
#' This dataset contains the data used in Valente et al. (2013) to study the
#' influence of peers on adolescent smoking, drinking, and marijuana use. The
#' `valentesnsList` is a transformed version of the data ready to be used to
#' create defm objects.
#'
#' @format
#' The `valentesns` dataset has 1,722 records for 568 individuals, featuring the
#' following 18 columns:
#' 
#'  - `id`: Id of the individual.
#'  - `year`: Wave number.
#'  - `Hispanic`: Indicator variable equal to 1 if the individual is Hispanic.
#'  - `Female`: Indicator variable equal to 1 if the individual is female.
#'  - `Grades`: Academic grades ranging from 1 (mostly F) to 5 (mostly As).
#'  - `tobacco`: Indicator variable if the individual ever smoked tobacco.
#'  - `alcohol`: Indicator variable if the individual ever drink alcohol.
#'  - `mj`: : Indicator variable if the individual ever smoked marijuana.
#'  - `sibsmoke` : Indicator variable if the individual's sibling smokes.
#'  - `sibdrink`: Indicator variable if the individual's sibling drinks alcohol.
#'  - `adultdrink`: Indicator variable equal to one if there's an adult who
#'     drinks in the household.
#'  - `year_value`: Year of the survey.
#'  - `present`: Indicator variable equal to 1 if the individual was present.
#'  - `school`: School id.
#'  - `has_sib`: Indicator variable equal to 1 if the individual has siblings.
#'  - `exposure_smoke`: Proportion of friends who have smoked tobacco in the
#'    past.
#'  - `exposure_drink`: Proportion of friends who have drink alcohol in the
#'    past.
#'  - `exposure_mj`: Proportion of friends who have smoked marijuana in the
#'    past.
#'
#' Exposure variables are marked with -1 for each individuals' first wave.
#' @source
#' Valente, T. W., Fujimoto, K., Unger, J. B., Soto, D. W., & Meeker, D. (2013). Variations in network boundary and type: A study of adolescent peer influences. Social Networks, 35(3), 309–316. <doi:10.1016/j.socnet.2013.02.008>.
#' @aliases valentesnsList
"valentesns"