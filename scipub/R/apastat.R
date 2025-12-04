#' Format simple statistic test results for scientific publication
#'
#' The `apastat` function summarizes statistic test
#'  results scientific publication.
#' This currently will take `stats::t.test`,
#'  `stats::cor.test`, or `stats::lm` results as input.
#' The output is intended to be included as
#'  in-text parenthetical statistics in publication.
#' @param test The `stats::t.test`, `stats::cor.test`,
#'  or `stats::lm` object to be formatted.
#' @param roundN The number of decimal places to
#'  round all output to (default=2).
#' @param es Include effect side (Cohen's d for t-test or
#'  2-level factor lm variable), default to TRUE.
#' @param ci Include confidence interval of estimate, default to TRUE.
#' @param var Only for lm object, select name of variable to
#'  summarize (default=NULL), if NULL, will summarize overall model fit.
#' @return Output formatted statistics
#' @importFrom 	stats coef
#' @export
#' @examples
#' apastat(stats::cor.test(psydat$Age, psydat$Height))
#' apastat(stats::t.test(Height ~ Sex, data = psydat))
#' apastat(stats::lm(data = psydat, Height ~ Age + Sex))
#' apastat(stats::lm(data = psydat, Height ~ Age + Sex), var = "Age")
apastat <- function(test,
                    roundN = 2,
                    es = c(TRUE, FALSE),
                    ci = c(TRUE, FALSE),
                    var = NULL) {
  if (inherits(test, what = "htest")) {
    # t-test
    if (grepl("t-test", test$method)) {
      out <- paste0(
        "t(", format(round(test$parameter, roundN), roundN), ")=",
        format(round(test$statistic, roundN), roundN),
        ", p", ifelse(test$p.value < .001, "<.001",
          ifelse(test$p.value < .01,
            sub(format(round(test$p.value, 3), nsmall = 3),
              pattern = "0.", replacement = "=."
            ),
            sub(format(round(test$p.value, 2), nsmall = 2),
              pattern = "0.", replacement = "=."
            )
          )
        ),
        ifelse(es[1] == TRUE,
          paste0(", d=", format(
            round((2 * test$statistic) /
              sqrt(test$parameter), roundN),
            roundN
          )),
          ""
        )
      )
    }

    # cor.test() output
    if (grepl("Pearson", test$method)) {
      out <- paste0(
        "r=", sub(x = format(round(test$estimate, roundN), roundN), "0.", "."),
        ", t(", round(test$parameter, roundN), ")=",
        format(round(test$statistic, roundN), roundN),
        ifelse(ci[1] == TRUE,
          paste0(
            ", ",
            100 * attr(test$conf.int, "conf.level"),
            "% CI=[",
            sub(
              x = format(round(test$conf.int[1], roundN), roundN),
              "0.", "."
            ), ",",
            sub(
              x = format(round(test$conf.int[2], roundN), roundN),
              "0.", "."
            ), "]"
          ),
          ""
        ),
        ", p", ifelse(test$p.value < .001, "<.001",
          ifelse(test$p.value < .01,
            sub(format(round(test$p.value, 3), nsmall = 3),
              pattern = "0.", replacement = "=."
            ),
            sub(format(round(test$p.value, 2), nsmall = 2),
              pattern = "0.", replacement = "=."
            )
          )
        )
      )
    }
    if (grepl("Spearman", test$method)) {
      out <- paste0(
        "\u03C1", "=", sub(
          x = format(round(test$estimate, roundN), roundN),
          "0.", "."
        ),
        ", S=", format(round(test$statistic, roundN), roundN),
        ", p", ifelse(test$p.value < .001, "<.001",
          ifelse(test$p.value < .01,
            sub(format(round(test$p.value, 3), nsmall = 3),
              pattern = "0.", replacement = "=."
            ),
            sub(format(round(test$p.value, 2), nsmall = 2),
              pattern = "0.", replacement = "=."
            )
          )
        )
      )
    }
    if (grepl("Kendall", test$method)) {
      out <- paste0(
        "\u03C4", "=", sub(
          x = format(round(test$estimate, roundN), roundN),
          "0.", "."
        ),
        ", z=", format(round(test$statistic, roundN), roundN),
        ", p", ifelse(test$p.value < .001, "<.001",
          ifelse(test$p.value < .01,
            sub(format(round(test$p.value, 3), nsmall = 3),
              pattern = "0.", replacement = "=."
            ),
            sub(format(round(test$p.value, 2), nsmall = 2),
              pattern = "0.", replacement = "=."
            )
          )
        )
      )
    }
  }

  # lm
  if (inherits(test, what = "lm") | inherits(test, what = "summary.lm")) {
    if (inherits(test, what = "lm")) {
      tmp <- summary(test)
    } else {
      tmp <- test
    }
    # summarize model fit if no variable selected
    if (is.null(var)) {
      fp <- pf(tmp$fstatistic[1], tmp$fstatistic[2],
        tmp$fstatistic[3],
        lower.tail = FALSE
      )
      out <- paste0(
        "N=", NROW(tmp$residuals),
        ", F(", tmp$fstatistic[2], ",",
        tmp$fstatistic[3], ")=",
        format(round(tmp$fstatistic[1], roundN), roundN),
        ", R2=", sub(x = format(
          round(tmp$r.squared, roundN),
          roundN
        ), "0.", "."),
        ", adj. R2=", sub(
          x = format(round(tmp$adj.r.squared, roundN), roundN),
          "0.", "."
        ),
        ", p", ifelse(fp < .001, "<.001",
          ifelse(fp < .01, sub(format(round(fp, 3), nsmall = 3),
            pattern = "0.", replacement = "=."
          ),
          sub(format(round(fp, 2), nsmall = 2),
            pattern = "0.", replacement = "=."
          )
          )
        )
      )
    } else {
      varx <- rownames(coef(tmp))[grepl(var, rownames(coef(tmp)))]

      out <- paste0(
        "N=", NROW(tmp$residuals),
        ", b=", format(round(coef(tmp)[varx, "Estimate"], roundN), roundN),
        ", t(", round(tmp$fstatistic[3], roundN),
        ")=", format(round(coef(tmp)[varx, "t value"], roundN), roundN),
        ", p", ifelse(coef(tmp)[varx, "Pr(>|t|)"] < .001, "<.001",
          ifelse(coef(tmp)[varx, "Pr(>|t|)"] < .01,
            sub(format(round(coef(tmp)[varx, "Pr(>|t|)"], 3), nsmall = 3),
              pattern = "0.", replacement = "=."
            ),
            sub(format(round(coef(tmp)[varx, "Pr(>|t|)"], 2), nsmall = 2),
              pattern = "0.", replacement = "=."
            )
          )
        ),
        ifelse(es[1] == TRUE & (inherits(test, what = "lm")) &
          length(test$xlevels[[var]]) == 2,
        paste0(
          ", d=",
          format(
            round((coef(tmp)[varx, "t value"] *
              (table(test$model[var])[1] +
                table(test$model[var])[2]) /
              (sqrt((table(test$model[var])[1] *
                table(test$model[var])[2])) *
                sqrt(test$df.residual))), roundN),
            roundN
          )
        ),
        ""
        )
      )
    }
  }
  return(cat(out))
}
