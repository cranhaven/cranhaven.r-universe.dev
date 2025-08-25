test_that(desc = "returns expexted value for normal parameters - small example",
					code = {
						observations <- c(0L, 0L, 0L, 1L, 1L, 1L)
						predictions <- c(0, 0.2, 0.5, 0.4, 0.6, 1)
						thresholds <- c(0.4, 0.6)
						expect_type(object = confidence(observations = observations,
																						predictions = predictions,
																						thresholds = thresholds,
																						type = "positive"),
												type = "double")
						expect_type(object = confidence(observations = observations,
																						predictions = predictions,
																						thresholds = thresholds,
																						type = "neutral"),
												type = "double")
						expect_vector(object= confidence(observations = observations,
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = "positive"),
													ptype = numeric(length = 0))
						expect_vector(object= confidence(observations = observations,
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = "neutral"),
													ptype = numeric(length = 0))
						expect_length(object = confidence(observations = observations,
																							predictions = predictions,
																							thresholds = thresholds,
																							type = "positive"),
													n = 1)
						expect_length(object = confidence(observations = observations,
																							predictions = predictions,
																							thresholds = thresholds,
																							type = "neutral"),
													n = 1)
						expect_equal(object = round(x = confidence(observations = observations,
																						           predictions = predictions,
																											 thresholds = thresholds,
																											 type = "positive"), digits = 7),
												 expected = 0.5)
						expect_equal(object = round(x = confidence(observations = observations,
																											 predictions = predictions,
																											 thresholds = thresholds,
																											 type = "neutral"), digits = 7),
												 expected = 0.6666667)
					}
)

test_that(desc = "returns expexted value for normal parameters - long example - logical",
					code = {
						withr::local_seed(seed = 12345,
											 .rng_kind = "Mersenne-Twister",
											 .rng_normal_kind = "Inversion",
											 .rng_sample_kind = "Rejection")
						observations_1000_logical <- c(rep(x = FALSE, times = 500),
																					 rep(x = TRUE, times = 500))
						predictions_1000 <- c(runif(n = 500, min = 0, max = 0.7),
																	runif(n = 500, min = 0.3, max = 1))
						expect_type(object = confidence(observations = observations_1000_logical, predictions = predictions_1000),
												type = "double")
						expect_vector(object= confidence(observations = observations_1000_logical, predictions = predictions_1000),
													ptype = numeric(length = 0))
						expect_length(object = confidence(observations = observations_1000_logical, predictions = predictions_1000),
													n = 1)
						expect_equal(object = round(x = confidence(observations = observations_1000_logical, predictions = predictions_1000), digits = 7),
												 expected = 0.5607064)
					}
)

test_that(desc = "returns expexted value for normal parameters - long example - integer",
					code = {
						withr::local_seed(seed = 12345,
											 .rng_kind = "Mersenne-Twister",
											 .rng_normal_kind = "Inversion",
											 .rng_sample_kind = "Rejection")
						observations_4000_integer <- c(rep(x = 0L, times = 3000),
																					 rep(x = 1L, times = 1000))
						predictions_4000 <- c(runif(n = 3000, min = 0, max = 0.8),
																	runif(n = 1000, min = 0.2, max = 0.9))
						expect_type(object = confidence(observations = observations_4000_integer,
																						predictions = predictions_4000,
																						type = "positive"),
												type = "double")
						expect_type(object = confidence(observations = observations_4000_integer,
																						predictions = predictions_4000,
																						type = "neutral"),
												type = "double")
						expect_vector(object= confidence(observations = observations_4000_integer,
																						 predictions = predictions_4000,
																						 type = "positive"),
													ptype = numeric(length = 0))
						expect_vector(object= confidence(observations = observations_4000_integer,
																						 predictions = predictions_4000,
																						 type = "neutral"),
													ptype = numeric(length = 0))
						expect_length(object = confidence(observations = observations_4000_integer,
																							predictions = predictions_4000,
																							type = "positive"),
													n = 1)
						expect_length(object = confidence(observations = observations_4000_integer,
																							predictions = predictions_4000,
																							type = "neutral"),
													n = 1)
						expect_equal(object = round(x = confidence(observations = observations_4000_integer,
																						           predictions = predictions_4000,
																											 type = "positive"), digits = 7),
												 expected = 0.7316017)
						expect_equal(object = round(x = confidence(observations = observations_4000_integer,
																											 predictions = predictions_4000,
																											 type = "neutral"), digits = 7),
												 expected = 0.814)
					}
)


test_that(desc = "returns expexted value for normal parameters - previously selected thresholds",
					code = {
						withr::local_seed(seed = 12345,
															.rng_kind = "Mersenne-Twister",
															.rng_normal_kind = "Inversion",
															.rng_sample_kind = "Rejection")
						observations_4000_integer <- c(rep(x = 0L, times = 3000),
																					 rep(x = 1L, times = 1000))
						predictions_4000 <- c(runif(n = 3000, min = 0, max = 0.8),
																	runif(n = 1000, min = 0.2, max = 0.9))
						strict_thresholds <- c(0.1, 0.9)
						permissive_thresholds <- c(0.4, 0.5)
						percentile_thresholds <- quantile(x = predictions_4000[observations_4000_integer == 1],
																							probs = c(0.1, 0.9))
						expect_equal(object = round(x = confidence(observations = observations_4000_integer,
																											 predictions = predictions_4000,
																											 thresholds = strict_thresholds,
																											 type = "neutral"), digits = 7),
												 expected = 0)
						expect_equal(object = round(x = confidence(observations = observations_4000_integer,
																											 predictions = predictions_4000,
																											 thresholds = permissive_thresholds,
																											 type = "neutral"), digits = 7),
												 expected = 0.862)
						expect_equal(object = round(x = confidence(observations = observations_4000_integer,
																											 predictions = predictions_4000,
																											 thresholds = percentile_thresholds,
																											 type = "neutral"), digits = 7),
												 expected = 0.2)
					}
)

test_that(desc = "returns expexted value for normal parameters - real-life case",
					code = {
						withr::local_seed(seed = 12345,
															.rng_kind = "Mersenne-Twister",
															.rng_normal_kind = "Inversion",
															.rng_sample_kind = "Rejection")
						observations_4000_integer <- c(rep(x = 0L, times = 3000),
																					 rep(x = 1L, times = 1000))
						predictions_4000 <- c(runif(n = 3000, min = 0, max = 0.8),
																	runif(n = 1000, min = 0.2, max = 0.9))
						dataset <- data.frame(
							observations = observations_4000_integer,
							predictions = predictions_4000,
							evaluation_mask = c(rep(x = FALSE, times = 250),
																  rep(x = TRUE, times = 250),
																  rep(x = FALSE, times = 250),
															 	 rep(x = TRUE, times = 250))
						)
						thresholds_whole <- thresholds(observations = dataset$observations,
																					 predictions = dataset$predictions)
						confidence(observations = dataset$observations[dataset$evaluation_mask],
											 predictions = dataset$predictions[dataset$evaluation_mask],
											 thresholds = thresholds_whole)

						expect_equal(object = round(x = confidence(observations = dataset$observations[dataset$evaluation_mask],
																											 predictions = dataset$predictions[dataset$evaluation_mask],
																											 thresholds = thresholds_whole), digits = 7),
												 expected = 0.7134831)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: observations",
					code = {
						predictions <- c(0, 0.2, 0.5, 0.4, 0.6, 1)
						thresholds <- c(0.4, 0.6)
						expect_error(object = confidence(predictions = predictions,
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
						expect_warning(object = confidence(observations = as.numeric(c(0L, 0L, 0L, 1L, 1L, 1L)),
																							 predictions = predictions,
																							 thresholds = thresholds,
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = as.character(c(0L, 0L, 0L, 1L, 1L, 1L)),
																							 predictions = predictions,
																							 thresholds = thresholds,
																							 type = "neutral"),
													 regexp = NULL)
						expect_silent(object = confidence(observations = c(0L, 0L, 0L, 1L, 1L, 1L),
																							predictions = predictions,
																							thresholds = thresholds,
																							type = "neutral"))
						expect_error(object = confidence(observations = c(0L, -2L, 0L, 1L, 1L, 1L),
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
						expect_error(object = confidence(observations = c(0L, 0L, 0L, 1L, 1L, 5L),
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
						expect_error(object = confidence(observations = integer(length = 0),
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: predictions",
					code = {
						observations <- c(0L, 0L, 0L, 1L, 1L, 1L)
						thresholds <- c(0.4, 0.6)
						expect_error(object = confidence(observations = observations,
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = as.character(c(0, 0.2, 0.5, 0.4, 0.6, 1)),
																							 thresholds = thresholds,
																							 type = "neutral"),
													 regexp = NULL)
						expect_silent(object = confidence(observations = observations,
																							predictions = c(0, 0.2, 0.5, 0.4, 0.6, 1),
																							thresholds = thresholds,
																							type = "neutral"))
						expect_warning(object = confidence(observations = observations,
																						 predictions = c(-0.6, 0.2, 0.5, 0.4, 0.6, 1),
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																						 predictions = c(0, 0.2, 0.5, 0.4, 0.6, 1.6),
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
						expect_error(object = confidence(observations = observations,
																						 predictions = numeric(length = 0),
																						 thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - both observations amd predictions",
					code = {
						expect_error(object = confidence(thresholds = thresholds,
																						 type = "neutral"),
												 regexp = NULL)
						expect_error(object = confidence(),
												 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: thresholds",
					code = {
						observations <- c(0L, 0L, 0L, 1L, 1L, 1L)
						predictions <- c(0, 0.2, 0.5, 0.4, 0.6, 1)
						expect_warning(object = confidence(observations = observations,
																						 predictions = predictions,
																						 thresholds = c("0.2", "0.5"),
																						 type = "neutral"),
												 regexp = NULL)
						expect_error(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = numeric(length = 0),
																							 type = "neutral"),
													 regexp = NULL)
						expect_error(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = 0.5,
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = c(0.2, 0.5, 0.7),
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = c(-0.2, 0.8),
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = c(NA_real_, 0.7),
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = c(0.2, 1.8),
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = c(0.2, NA_real_),
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = c(0.7, 0.5),
																							 type = "neutral"),
													 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = c(0.7, 0.7),
																							 type = "neutral"),
													 regexp = NULL)
						expect_silent(object = confidence(observations = observations,
																							predictions = predictions,
																							type = "neutral"))
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: type",
					code = {
						observations <- c(0L, 0L, 0L, 1L, 1L, 1L)
						predictions <- c(0, 0.2, 0.5, 0.4, 0.6, 1)
						thresholds <- c(0.4, 0.6)
						expect_error(object = confidence(observations = observations,
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = 5),
												 regexp = NULL)
						expect_error(object = confidence(observations = observations,
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = character(length = 0)),
												 regexp = NULL)
						expect_warning(object = confidence(observations = observations,
																						 predictions = predictions,
																						 thresholds = thresholds,
																						 type = c("positive", "neutral")),
												 regexp = NULL)
						expect_error(object = confidence(observations = observations,
																							 predictions = predictions,
																							 thresholds = thresholds,
																							 type = "pos"),
													 regexp = NULL)
					}
)
test_that(desc = "returns errors/warnings if needed - complex examples",
					code = {
						observations_4000_numeric <- c(rep(x = 0, times = 3000),
																					 rep(x = 1, times = 1000))
						predictions_4000_strange <- c(runif(n = 3000, min = -0.3, max = 0.4),
																					runif(n = 1000, min = 0.6, max = 1.5))
						expect_warning(object = expect_warning(object = confidence(observations = observations_4000_numeric,
											                                                 predictions = predictions_4000_strange,
																																			 thresholds = c(0.2, 0.7)),
																									 regexp = NULL),
													 regexp = NULL)
						mask_of_normal_predictions <- predictions_4000_strange >= 0 & predictions_4000_strange <= 1
						expect_silent(object = confidence(observations = as.integer(observations_4000_numeric)[mask_of_normal_predictions],
																							predictions = predictions_4000_strange[mask_of_normal_predictions],
																							thresholds = c(0.2, 0.7)))
					}
)
