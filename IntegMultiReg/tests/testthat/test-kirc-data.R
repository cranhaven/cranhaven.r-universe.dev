test_that("kircIMR matches the reduced TCGA-KIRC example structure", {
  data("kircIMR", package = "IntegMultiReg")

  expect_named(kircIMR$platforms, c("mrna", "mirna", "methylation"))
  expect_equal(sapply(kircIMR$platforms, ncol), c(mrna = 51L, mirna = 31L,
                                                  methylation = 51L))
  expect_equal(ncol(kircIMR$covariates), 5L)
  expect_equal(names(kircIMR$outcome.survival), c("id", "time", "status"))
  expect_equal(kircIMR$outcome, kircIMR$outcome.survival)

  all_ids <- unname(unlist(c(
    list(kircIMR$covariates$id, kircIMR$outcome.survival$id),
    lapply(kircIMR$platforms, `[[`, "id")
  )))
  expect_true(all(grepl("^KIRC[0-9]{3}$", all_ids)))
  expect_false(any(grepl("^TCGA-", all_ids)))
  expect_match(kircIMR$source$data_access, "public UCSC Xena")
  expect_match(kircIMR$source$data_access, "controlled-access")
  expect_match(kircIMR$source$id_policy, "package-internal")
  expect_match(kircIMR$source$id_policy, "not attempt participant re-identification")

  expect_equal(as.integer(kircIMR$model_subgroup_sizes),
               c(63L, 139L, 147L, 172L))
  expect_equal(names(kircIMR$model_subgroup_sizes),
               c("011", "101", "001", "111"))
  expect_equal(kircIMR$paper_alignment$reference_screened_features,
               c(mrna = 776L, mirna = 91L, methylation = 729L))
})
