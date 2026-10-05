test_that("Correct number of list enties", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
                                     model_spec="religiosity ~ native + female + age + education + discrimination", 
                                     draws = 5, n_clusters = 4)
  expect_identical(length(ISCA_modeling_res), as.integer(2))
})

test_that("Correct number of columns in first entry (i.e., entry containing model estimates)", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
                                     model_spec="religiosity ~ native + female + age + education + discrimination", 
                                     draws = 5, n_clusters = 4)
  estimates <- as.data.frame(ISCA_modeling_res[1])
  
  expect_identical(ncol(estimates), as.integer(5))
})

test_that("Correct number of rows in first entry (i.e., entry containing model estimates)", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
                                     model_spec="religiosity ~ native + female + age + education + discrimination", 
                                     draws = 5, n_clusters = 4)
  estimates <- as.data.frame(ISCA_modeling_res[1])
  
  expect_identical(nrow(estimates), as.integer(30))
})

test_that("Output is a data frame", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  
  ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
                                     model_spec="religiosity ~ native + female + age + education + discrimination", 
                                     draws = 5, n_clusters = 4)
  estimates <- as.data.frame(ISCA_modeling_res[1])
  expect_true(inherits(estimates, "data.frame"), label = "Output is not a data frame")
})


test_that("Correct number of columns in second entry (i.e., entry containing adjusted r-squared per cluster)", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
                                     model_spec="religiosity ~ native + female + age + education + discrimination", 
                                     draws = 5, n_clusters = 4)
  r_squared <- as.data.frame(ISCA_modeling_res[2])
  
  expect_identical(ncol(r_squared), as.integer(5))
})

test_that("Correct number of rows in second entry (i.e., entry containing adjusted r-squared per cluster)", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
                                     model_spec="religiosity ~ native + female + age + education + discrimination", 
                                     draws = 5, n_clusters = 4)
  r_squared <- as.data.frame(ISCA_modeling_res[2])
  
  expect_identical(nrow(r_squared), as.integer(1))
})

test_that("Output is a data frame", {
  data(sim_data)
  ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), 
                                        fuzzifier = 1.5, n_clusters=4, draws=5, 
                                        cluster_vars= c("female", "age", "education", "income"))
  
  ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
                                     model_spec="religiosity ~ native + female + age + education + discrimination", 
                                     draws = 5, n_clusters = 4)
  r_squared <- as.data.frame(ISCA_modeling_res[2])
  expect_true(inherits(r_squared, "data.frame"), label = "Output is not a data frame")
})
