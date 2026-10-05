test_that("Nothing gets lost", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  expect_identical(nrow(ISCA_step1), nrow(sim_data))
})

test_that("number of clusters are correct", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=7, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  expect_identical(length(unique(ISCA_step1$A1)), as.integer(7))
})

test_that("number of draws are correct", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=7, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  expect_identical(ncol(ISCA_step1), as.integer(ncol(sim_data)+5))
})

test_that("no missing values in draw variable", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=7, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  expect_identical(sum(is.na(ISCA_step1)), as.integer(0))
})


