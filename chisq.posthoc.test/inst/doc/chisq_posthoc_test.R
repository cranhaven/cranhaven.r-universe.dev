## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(chisq.posthoc.test)

## ----chisq_test----------------------------------------------------------
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
chisq.test(M)

## ----chisq_residuals-----------------------------------------------------
chisq.results <- chisq.test(M)
chisq.results$stdres

## ----chisq_post_hoc------------------------------------------------------
chisq.posthoc.test(M,
                   method = "bonferroni")

