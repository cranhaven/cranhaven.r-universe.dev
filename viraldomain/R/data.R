#' Predictive Modeling Data for Viral Load and CD4 Lymphocyte Counts
#'
#' This dataset serves as input for predictive modeling tasks related to HIV
#' research. It contains numeric measurements of CD4 lymphocyte counts (cd) and
#' viral load (vl) at three different time points: 2019, 2021, and 2022. These
#' measurements are crucial indicators of HIV disease progression.
#'
#' @docType data
#'
#' @usage data(viral)
#'
#' @format A tibble (data frame) with 35 rows and 6 columns.
#'
#' @keywords datasets
#'
#' @note
#' To explore more rows of this dataset, you can use the `print(n = ...)` function.
#'
#' @author
#' Juan Pablo Acuña González <acua6307@gmail.com>
#'
#' @examples
#' data(viral)
#' viral
"viral"


#' Seropositive Data for Applicability Domain Testing
#'
#' This dataset is designed for testing the applicability domain of methods
#' related to HIV research. It provides a tibble with 53 rows and 2 columns
#' containing numeric measurements of CD4 lymphocyte counts (cd_2022) and viral
#' load (vl_2022) for seropositive individuals in 2022. These measurements are
#' vital indicators of HIV disease status. This dataset is ideal for evaluating
#' the performance and suitability of various HIV-predictive models and as an
#' aid in developing diagnostic tools within a seropositive context.
#'
#' @docType data
#'
#' @usage data(sero)
#'
#' @format A tibble (data frame) with 53 rows and 2 columns.
#'
#' @keywords datasets
#'
#' @note
#' To explore more rows of this dataset, you can use the `print(n = ...)` function.
#'
#' @author
#'Juan Pablo Acuña González <acua6307@gmail.com>
#'
#' @examples
#' data(sero)
#' sero
"sero"