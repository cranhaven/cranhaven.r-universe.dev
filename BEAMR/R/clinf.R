#' Pediatric T-ALL Clinical Data from COG trial AALL0434
#'
#' A subset of clinical data from pediatric and young adult t-lineage acute
#'     lymphoblastic leukmia patients in the Children's Oncology Group trial
#'     AALL0434, published in Liu et al., 2017 Nature Genetics
#'
#' @format ## `clinf`
#' A data frame with 265 rows and 8 columns:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{MRD29}{Minimal residual disease measured at day 29}
#'   \item{RNA.clm}{Key to match to RNA matrix}
#'   \item{Lesion.clm}{Key to match Lesion matrix}
#'   \item{Lesion.id}{Key to match Lesion matrix}
#'   \item{RNA.id}{Key to match RNA matrix}
#'   \item{EFS}{Event-free survival Surv object}
#'   \item{OS}{Overall survival Surv object}
#' }
#' @source <https://www.nature.com/articles/ng.3909>
"clinf"
