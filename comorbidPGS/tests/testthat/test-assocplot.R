test_that("Missing score_table", {
  expect_error(assocplot())
})

test_that("Wrong class score_table", {
  expect_error(assocplot(score_table = "WRONG_DF"))
})

prs <- grep("PGS", names(comorbidData), value = T)
phenotype <- c("ethnicity","t2d","log_ldl","sbp_cat")
assoc <- cbind(prs, phenotype)
assoc <- na.omit(assoc)
score_table <- multiassoc(comorbidData, assoc)

test_that("No column Phenotype in score_table", {
  expect_error(assocplot(score_table[, -"Phenotype"]))
})

test_that("No column Phenotype_type in score_table", {
  expect_error(assocplot(score_table[, -"Phenotype_type"]))
})

test_that("No column Effect in score_table", {
  expect_error(assocplot(score_table[, -"Effect"]))
})

test_that("No column lower_CI in score_table", {
  expect_error(assocplot(score_table[, -"lower_CI"]))
})

test_that("No column upper_CI in score_table", {
  expect_error(assocplot(score_table[, -"upper_CI"]))
})

test_that("No column PGS in score_table", {
  expect_error(assocplot(score_table[, -"PGS"]))
})

test_that("No column P-value in score_table", {
  expect_error(assocplot(score_table[, -"P-value"]))
})

test_that("Missing axis", {
  expect_warning(assocplot(score_table, axis = NULL))
})

test_that("Wrong axis", {
  expect_warning(assocplot(score_table, axis = "WRONG_AXIS"))
})

test_that("Missing pval", {
  expect_error(assocplot(score_table, pval = NULL))
})

test_that("Wrong pval", {
  expect_error(assocplot(score_table, pval = "WRONG_PVAL"))
})

continuous_score_table <- score_table[which(score_table$Phenotype_type == "Continuous"),]
discrete_score_table <- score_table[which(score_table$Phenotype_type != "Continuous"),]
for (ax in c("horizontal", "vertical")) {
  for (pvalue in c(0.05, 0.5, T, F)) {
    test_that(paste("Test with only Continuous Phenotype ; axis =", ax, "; pval =", pvalue), {
      expect_s3_class(
        object = assocplot(continuous_score_table, ax, pvalue),
        class = "ggplot"
      )
    })

    test_that(paste("Test with only Discrete Phenotypes ; axis =", ax, "; pval =", pvalue), {
      expect_s3_class(
        object = assocplot(discrete_score_table, ax, pvalue),
        class = "ggplot"
      )
    })

    test_that(paste("Test with axis =", ax, "; pval =", pvalue), {
      expect_type(
        object = assocplot(score_table, ax, pvalue),
        type = "list"
      )
    })
  }
}
