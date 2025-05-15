#' Explain Multivariate Adaptive Regression Splines Model
#'
#' Explains the predictions of a Multivariate Adaptive Regression Splines (MARS)
#' model for viral load or CD4 counts using the DALEX and DALEXtra tools.
#'
#' @import earth
#' @import DALEX
#' @import DALEXtra
#' @import Formula
#' @import parsnip
#' @import plotmo
#' @import plotrix
#' @import recipes
#' @import rsample
#' @import TeachingDemos
#' @import workflows
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom stats as.formula
#'
#' @param vip_featured A character value
#' @param hiv_data A data frame
#' @param nt  A numeric value
#' @param pd A numeric value
#' @param pru A character value
#' @param vip_train A data frame
#' @param vip_new A numeric vector
#'
#' @return A data frame
#' @export
#'
#' @examples
#' library(dplyr)
#' library(rsample)
#' library(Formula)
#' library(plotmo)
#' library(plotrix)
#' library(TeachingDemos)
#' cd_2019 <- c(824, 169, 342, 423, 441, 507, 559,
#'              173, 764, 780, 244, 527, 417, 800,
#'              602, 494, 345, 780, 780, 527, 556,
#'              559, 238, 288, 244, 353, 169, 556,
#'              824, 169, 342, 423, 441, 507, 559)
#' vl_2019 <- c(40, 11388, 38961, 40, 75, 4095, 103,
#'              11388, 46, 103, 11388, 40, 0, 11388,
#'              0,   4095,   40,  93,  49,  49,  49,
#'              4095,  6837, 38961, 38961, 0, 0, 93,
#'              40, 11388, 38961, 40, 75, 4095, 103)
#' cd_2021 <- c(992, 275, 331, 454, 479, 553,  496,
#'              230, 605, 432, 170, 670, 238,  238,
#'              634, 422, 429, 513, 327, 465,  479,
#'              661, 382, 364, 109, 398, 209, 1960,
#'              992, 275, 331, 454, 479, 553,  496)
#' vl_2021 <- c(80, 1690,  5113,  71,  289,  3063,  0,
#'              262,  0,  15089,  13016, 1513, 60, 60,
#'              49248, 159308, 56, 0, 516675, 49, 237,
#'              84,  292,  414, 26176,  62,  126,  93,
#'              80, 1690, 5113,    71, 289, 3063,   0)
#' cd_2022 <- c(700, 127, 127, 547, 547, 547, 777,
#'              149, 628, 614, 253, 918, 326, 326,
#'              574, 361, 253, 726, 659, 596, 427,
#'              447, 326, 253, 248, 326, 260, 918,
#'              700, 127, 127, 547, 547, 547, 777)
#' vl_2022 <- c(0,   0,   53250,   0,   40,   1901, 0,
#'              955,    0,    0,    0,   0,   40,   0,
#'              49248, 159308, 56, 0, 516675, 49, 237,
#'              0,    23601,   0,   40,   0,   0,   0,
#'              0,    0,     0,     0,    0,    0,  0)
#' x <- cbind(cd_2019, vl_2019, cd_2021, vl_2021, cd_2022, vl_2022) |>
#' as.data.frame()
#' set.seed(123)
#' hi_data <- rsample::initial_split(x)
#' set.seed(123)
#' hiv_data <- hi_data |>
#' rsample::training()
#' nt <- 3
#' pd <- 1
#' pru <- "none"
#' vip_featured <- c("cd_2022")
#' vip_features <- c("cd_2019", "vl_2019", "cd_2021", "vl_2021", "vl_2022")
#' set.seed(123)
#' vi_train <- rsample::initial_split(x)
#' set.seed(123)
#' vip_train <- vi_train |>
#' rsample::training() |>
#' dplyr::select(rsample::all_of(vip_features))
#' vip_new <- vip_train[1,]
#' viralx_mars(vip_featured, hiv_data, nt, pd, pru, vip_train, vip_new)
viralx_mars <- function(vip_featured, hiv_data, nt, pd, pru, vip_train, vip_new) {
  DALEXtra::explain_tidymodels(workflows::workflow() |>
                                 workflows::add_recipe(recipes::recipe(stats::as.formula(paste(vip_featured,"~.")), data = hiv_data)) |>
                                 workflows::add_model(parsnip::mars(num_terms = nt, prod_degree = pd, prune_method = pru) |>
                                                        parsnip::set_engine("earth") |>
                                                        parsnip::set_mode("regression")) |>
                                 parsnip::fit(data = hiv_data), data = vip_train,
                               y = vip_featured,
                               label = "MARS",
                               verbose = FALSE) |>
    DALEX::predict_parts(vip_new) |>
    as.data.frame() |>
    dplyr::select(1,2) |>
    dplyr::mutate_if(is.numeric, round, digits = 2)
}
