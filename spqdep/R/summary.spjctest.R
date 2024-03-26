#' @name summary.spjctest
#' @rdname summary.spjctest
#'
#' @title Summary of estimated objects of class \emph{spjctest}.
#'
#' @description  This function summarizes estimated \emph{spjctest} objects.
#'   The tables in the output include basic information for each test.
#'   blablabla...
#'
#' @param object An \emph{spjctest} object including a list of \emph{htest}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return An object of class \emph{summary.spjctest}
#'
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#'
#' @seealso
#'   \code{\link{print.summary.spqtest}},
#'   \code{\link[spdep]{joincount.test}},
#'   \code{\link[spdep]{joincount.multi}}
#'
#' @examples
#' ## Multinomial + Binomial using a sf multipolygon
#' data("provinces_spain")
#' sf::sf_use_s2(FALSE)
#' provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' provinces_spain$Older <- cut(provinces_spain$Older, breaks = c(-Inf,19,22.5,Inf))
#' levels(provinces_spain$Older) = c("low","middle","high")
#' f1 <- ~ Older + Mal2Fml
#' jc1 <- jc.test(formula = f1,
#'                data = provinces_spain,
#'                distr = "mc",
#'                alternative = "greater",
#'                zero.policy = TRUE)
#' summary(jc1)
#' @export
summary.spjctest <- function(object, ...) {
  z <- object
  stopifnot(inherits(z, "spjctest"))
  ## Build a tibble with the results...
  table_res <- NULL
  ## Defines variables to prevent next Note
  ## "no visible binding for global variable..."
  estimate1 <- estimate2 <- estimate3 <-  NULL
  statistic <- parameter <- NULL
  var.name <- type.test <- NULL
  method <- pairs <- NULL
  value <- `z-value` <- NULL
  p.value <- pvalue <- NULL
  Joincount <- Expected <- Variance <- NULL
  col_table_res <- c("var.name",
                     "pairs",
                     "Joincount",
                     "Expected",
                     "Variance",
                     "z-value",
                     "pvalue",
                     "rank_observed",
                     "alternative",
                     "distrib",
                     "method",
                     "type.test")
  for (i in 1:length(z)) {
    if (inherits(z[[i]], "jcmulti")) {
      alternative <- attributes(z[[i]])$alternative
      distribution <- attributes(z[[i]])$distribution
      table_resi <- as.data.frame(z[[i]])
      table_resi <- tidyr::as_tibble(table_resi)
      table_resi$`distrib` <- distribution
      if (distribution == "asymptotic") {
        table_resi$`rank_observed` <- NA
        table_resi$`method` <- "Join count test under nonfree sampling"
      }
      if (distribution == "mc") {
        table_resi$`z-value` <- NA
        table_resi$`method` <- "Monte-Carlo simulation of
        join-count statistic (nonfree sampling)"
      }
      table_resi$alternative <- paste("alternative: ",
                                      alternative,
                                      sep="")
      table_resi$`type.test` <- "multinomial"
      table_resi$`pairs` <- rownames(z[[i]])
      table_resi$`var.name` <- attr(z[[i]], "data.name")
      table_resi <- table_resi[,
                        col_table_res]
      table_res <- rbind(table_res,
                         table_resi)

    }
    if (inherits(z[[i]], "jclist")) {
      for (j in 1:length(z[[i]])) {
        alternative <- attributes(z[[i]][[j]])$alternative
        distribution <- attributes(z[[i]][[j]])$distribution
        table_resi <- broom::tidy(z[[i]][[j]])
        table_resi$`var.name` <- z[[i]][[j]]$data.name
        table_resi$`type.test` <- "binomial"
        table_resi$`pairs` <- z[[i]][[j]]$level
        table_resi$alternative <- paste("alternative: ",
                                        alternative,
                                        sep="")
        table_resi$`distrib` <- distribution
        if (distribution == "mc") {
          table_resi <- dplyr::rename(table_resi,
                                      `Joincount` = statistic)
          table_resi <- dplyr::rename(table_resi,
                                      `Expected` = estimate1)
          table_resi <- dplyr::rename(table_resi,
                                      `Variance` = estimate2)
          table_resi <- dplyr::rename(table_resi,
                                      `rank_observed` =
                                        parameter)
          table_resi$`z-value` <- NA
          } else {
          table_resi <- dplyr::rename(table_resi,
                                      `Joincount` = estimate1)
          table_resi <- dplyr::rename(table_resi,
                                      `Expected` = estimate2)
          table_resi <- dplyr::rename(table_resi,
                                      `Variance` = estimate3)
          table_resi <- dplyr::rename(table_resi,
                                      `z-value` = statistic)
          table_resi$`rank_observed` <- NA
        }
        table_resi <- dplyr::rename(table_resi,
                                    `pvalue` = p.value)
        table_resi <- table_resi[,
                                 col_table_res]
        table_res <- rbind(table_res,
                           table_resi)
      }
    }
  }
  if (distribution == "asymptotic") {
    tbl <- table_res %>%
      dplyr::select(var.name, type.test, alternative, method, pairs, `z-value`,
                    pvalue, Joincount, Expected, Variance) %>%
      dplyr::group_by(var.name, type.test, alternative, method)
    gt_tbl <- gt::gt(tbl) %>%
      gt::tab_header(
        title = "JoinCount Spatial Tests (asymptotic)") %>%
      gt::fmt_number(
        columns = c("z-value", "Expected", "Variance"),
        decimals = 2) %>%
      gt::fmt_number(
        columns = c("pvalue"),
        decimals = 5)
  } else {
    tbl <- table_res %>%
      dplyr::select(var.name, type.test, alternative, method, pairs,
                    pvalue, Joincount, Expected, Variance) %>%
      dplyr::group_by(var.name, type.test, alternative, method)
    gt_tbl <- gt::gt(tbl) %>%
      gt::tab_header(
        title = "JoinCount Spatial Tests (Monte Carlo)") %>%
      gt::fmt_number(
        columns = c("Expected", "Variance"),
        decimals = 2) %>%
      gt::fmt_number(
        columns = c("pvalue"),
        decimals = 5)

  }
  class(gt_tbl) <- c("summary.spjctest", "gt_tbl")
  return(gt_tbl)
}


