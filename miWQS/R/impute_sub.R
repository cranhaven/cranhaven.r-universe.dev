#' Imputing by Substitution
#'
#' @family imputation
#' @keywords imputation
#'
#' @description The values below the detection limit for each chemical are substituted by its detection limit/sqrt(2).
#'
#' @details A n x C matrix of components \emph{X} are interval-censored between zero and different detection limits \emph{DL}. Although \emph{X} may refer to a variable with no obvious \emph{DL}, we consider each chemical concentration being partially observed in mixture \emph{X}.
#'
#' @inheritParams impute.multivariate.bayesian
#' @return A n x C matrix where the BDL values of each chemical are substituted by its detection limit/sqrt(2).
#'
#' @importFrom tidyr replace_na
#' @export

#' @examples
#' data("simdata87")
#'
#' X.sub <- impute.sub(X = simdata87$X.bdl, DL = simdata87$DL, verbose = TRUE)
#'
#' # Compare substituted imputed data against the truth
#' probs <- c(0.01, 0.05, 0.09, 0.25, 0.5, 0.8, 1)
#' apply(X.sub, 2, quantile, probs)
#' round(apply(simdata87$X.true, 2, quantile, probs), 5)


impute.sub <- function(X, DL, verbose = FALSE) {
  X.sub <- X   # Copy BDL Matrix
  C <- ncol(X)

  # for (j in 1:C) {
  #   X.sub[, j] <- ifelse(is.na(X.sub)[, j], (DL / sqrt(2))[j], X.sub[, j])
  # }
  # Much faster -- added 1/29/2020
  X.sub <- tidyr::replace_na(as.data.frame(X), as.list(DL / sqrt(2)))

  if (verbose) {
    cat("Detection Limits/sqrt(2) \n")
    print(DL / sqrt(2))
    # Check: First three should be DL/sqrt(2); the last shouldbe differnt
    cat("Quantiles for X.sub \n")
    print(round(apply(X.sub, 2, quantile, c(0, 0.1, 0.25, 0.5, 0.8, 1)), 5))

    # Identical results?
    #  cat("Using tidyr::replace_na...\n")
    #  print(round(apply(X.sub2, 2, quantile, c(0, 0.1, 0.25, 0.5, 0.8, 1)), 5))

    # cat("Summaries between loop and tidyr::replace_na \n")
    #  print(summary(X.sub2 - X.sub))
  }

  return(X.sub)
}


# tidyr::replace_na() is equivalent to
# for (j in 1:C) {
#   X.sub[, j] <- ifelse(is.na(X.sub)[, j], (DL / sqrt(2))[j], X.sub[, j])
# }
# Summaries between loop and tidyr::replace_na
# print(summary(X.sub2 - X.sub))

# alpha-chlordane    dieldrin gamma-chlordane    lindane   methoxychlor      dde         ddt    pentachlorophenol
# Min.   :0       Min.   :0   Min.   :0       Min.   :0   Min.   :0     Min.   :0   Min.   :0   Min.   :0
# 1st Qu.:0       1st Qu.:0   1st Qu.:0       1st Qu.:0   1st Qu.:0     1st Qu.:0   1st Qu.:0   1st Qu.:0
# Median :0       Median :0   Median :0       Median :0   Median :0     Median :0   Median :0   Median :0
# Mean   :0       Mean   :0   Mean   :0       Mean   :0   Mean   :0     Mean   :0   Mean   :0   Mean   :0
# 3rd Qu.:0       3rd Qu.:0   3rd Qu.:0       3rd Qu.:0   3rd Qu.:0     3rd Qu.:0   3rd Qu.:0   3rd Qu.:0
# Max.   :0       Max.   :0   Max.   :0       Max.   :0   Max.   :0     Max.   :0   Max.   :0   Max.   :0
# pcb_105     pcb_118     pcb_138     pcb_153     pcb_170     pcb_180
# Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0
# 1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0
# Median :0   Median :0   Median :0   Median :0   Median :0   Median :0
# Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0
# 3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0
# Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0



# #Toy Example
# X.sub <- matrix(c(NA, 1, 2, 1, NA, 3), nrow = 3, ncol = 2)
# DL <-  l.data$DL[ ,1:2]/sqrt(2)  #Substitute

# library(dplyr)

# #perhaps: replace_na()
# X.sub %>%
# select_if(function(x) any(is.na(x)))
# %>%
# mutate_at( replace = DL)
# # X.sub[ is.na(X.sub[ ,j]) ] <-
