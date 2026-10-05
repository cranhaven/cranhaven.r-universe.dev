test_that("Output contains all specified variables and the count and cluster variable", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  
  result_ISCA_clustertable <- ISCA_clustertable(data = ISCA_step1, 
                                                cluster_vars = c("native", "education", "age", "discrimination", "religiosity"), draws = 5)
  expect_identical(nrow(result_ISCA_clustertable), as.integer(17))
})

test_that("Output has correct number of columns, one per cluster plus of for variable", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=7, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  
  result_ISCA_clustertable <- ISCA_clustertable(data = ISCA_step1, 
                                                cluster_vars = c("native", "education", "religiosity"), draws = 5)
  expect_identical(ncol(result_ISCA_clustertable), as.integer(8))
})

test_that("Output is a data frame", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  
  result_ISCA_clustertable <- ISCA_clustertable(data = ISCA_step1, 
                                                cluster_vars = c("age", "native"), draws = 5)
  expect_identical(class(result_ISCA_clustertable), "data.frame", label = "Output is not a data frame")
})

test_that("Filtering the cluster error for dichotomous variables also works when variables have similar names", {
  data(sim_data)
  sim_data$native3 <- sim_data$native
  sim_data[c(2,4,6),8] <- 3
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), fuzzifier = 1.5, n_clusters=4, 
                                        draws=5, cluster_vars= c("female", "age", "education", "income"))
  
  result_ISCA_clustertable <- ISCA_clustertable(data = ISCA_step1, 
                                                 cluster_vars = c("native3", "native", "female", "age", "education"), draws = 5)
  expect_identical(nrow(result_ISCA_clustertable), as.integer(16))
})