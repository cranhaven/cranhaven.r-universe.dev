test_that(desc = "returns expexted value for normal parameters - small example",
					code = {
						conf_train <- 0.6
						conf_eval <- 0.5
						expect_type(object = consistency(conf_train = conf_train, conf_eval = conf_eval),
												type = "double")
						expect_vector(object= consistency(conf_train = conf_train, conf_eval = conf_eval),
													ptype = numeric(length = 0))
						expect_length(object = consistency(conf_train = conf_train, conf_eval = conf_eval),
													n = 1)
						expect_equal(object = consistency(conf_train = conf_train, conf_eval = conf_eval),
												 expected = -0.1)
						expect_equal(object = consistency(conf_train = 0.93, conf_eval = 0.21), expected = -0.72)
						expect_equal(object = consistency(conf_train = 0.43, conf_eval = 0.35), expected = -0.08)
						expect_equal(object = consistency(conf_train = 0.87, conf_eval = 0.71), expected = -0.16)
						expect_equal(object = consistency(conf_train = 0.67, conf_eval = 0.78), expected = 0.11)
						expect_equal(object = consistency(conf_train = 0.67, conf_eval = NA_real_), expected = NA_real_) %>% suppressWarnings()
						expect_equal(object = consistency(conf_train = NA_real_, conf_eval = 0.78), expected = NA_real_) %>% suppressWarnings()
						expect_equal(object = consistency(conf_train = NA_real_, conf_eval = NA_real_), expected = NA_real_) %>% suppressWarnings()
					}
)

test_that(desc = "returns expexted value for normal parameters - long example",
					code = {
						withr::local_seed(seed = 12345,
											 .rng_kind = "Mersenne-Twister",
											 .rng_normal_kind = "Inversion",
											 .rng_sample_kind = "Rejection")
						observations <- c(rep(x = FALSE, times = 500),
															rep(x = TRUE, times = 500))
						predictions <- c(runif(n = 500, min = 0, max = 0.7),
														 runif(n = 500, min = 0.3, max = 1))
						dataset <- data.frame(
							observations = observations,
							predictions = predictions,
							evaluation_mask = c(rep(x = FALSE, times = 250),
														 		  rep(x = TRUE, times = 250),
														 		  rep(x = FALSE, times = 250),
														 	 	  rep(x = TRUE, times = 250))
						)
						thresholds_whole <- thresholds(observations = dataset$observations,
																					 predictions = dataset$predictions)
						confidence_training <- confidence(observations = dataset$observations[!dataset$evaluation_mask],
																							predictions = dataset$predictions[!dataset$evaluation_mask],
																							thresholds = thresholds_whole) # 0.602
						confidence_evaluation <- confidence(observations = dataset$observations[dataset$evaluation_mask],
																								predictions = dataset$predictions[dataset$evaluation_mask],
																								thresholds = thresholds_whole) # 0.520
						expect_equal(object = round(x = consistency(conf_train = confidence_training, conf_eval = confidence_evaluation), digits = 8), expected = -0.08302792)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: conf_train",
					code = {
						expect_error(object = consistency(conf_eval = 0.6),
												 regexp = NULL)
						expect_warning(object = consistency(conf_train = 1.3, conf_eval = 0.6),
													 regexp = NULL)
						expect_warning(object = consistency(conf_train = -0.2, conf_eval = 0.6),
													 regexp = NULL)
						expect_warning(object = consistency(conf_train = NA_real_, conf_eval = 0.6),
													 regexp = NULL)
						expect_silent(object = consistency(conf_train = 0.8, conf_eval = 0.6))
						expect_error(object = consistency(conf_train = "0.8", conf_eval = 0.6),
												 regexp = NULL)
						expect_error(object = consistency(conf_train = numeric(length = 0), conf_eval = 0.6),
												 regexp = NULL)
						expect_warning(object = consistency(conf_train = c(0.7, 0.8), conf_eval = 0.6),
													 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: conf_eval",
					code = {
						expect_error(object = consistency(conf_train = 0.8),
												 regexp = NULL)
						expect_warning(object = consistency(conf_train = 0.8, conf_eval = 1.1),
													 regexp = NULL)
						expect_warning(object = consistency(conf_train = 0.8, conf_eval = -0.7),
													 regexp = NULL)
						expect_warning(object = consistency(conf_train = 0.8, conf_eval = NA_real_),
													 regexp = NULL)
						expect_silent(object = consistency(conf_train = 0.8, conf_eval = 0.6))
						expect_error(object = consistency(conf_train = 0.8, conf_eval = "0.6"),
												 regexp = NULL)
						expect_error(object = consistency(conf_train = 0.8, conf_eval = numeric(length = 0)),
												 regexp = NULL)
						expect_warning(object = consistency(conf_train = 0.8, conf_eval = c(0.7, 0.6)),
													 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - both parameters",
					code = {
						expect_error(object = consistency(),
												 regexp = NULL)
					}
)
