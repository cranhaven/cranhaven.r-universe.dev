test_that(desc = "returns expexted value for normal parameters - default parameterization",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4, 0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_silent(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask))
						expect_type(object = measures(observations = observations,
																					predictions = predictions,
																					evaluation_mask = evaluation_mask),
												type = "double")
						expect_vector(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask),
													ptype = numeric(length = 0))
						expect_length(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask),
													n = 6)
						expect_equal(object = round(x = unname(measures(observations = observations,
																														predictions = predictions,
																														evaluation_mask = evaluation_mask)), digits = 8),
												 expected = c(0.80000000, 0.75000000, -0.05000000, 0.75000000, 0.66666667, -0.08333333))
						expect_equal(object = names(measures(observations = observations,
																								 predictions = predictions,
																								 evaluation_mask = evaluation_mask)),
												 expected = c("CP_train", "CP_eval", "DCP", "CPP_train", "CPP_eval", "DCPP"))
					}
)

test_that(desc = "returns expexted value for normal parameters - goodness = TRUE, ROCR available",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4,    0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_silent(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask,
																						goodness = TRUE))
						expect_equal(object = unname(is.na(measures(observations = observations,
																					              predictions = predictions,
																												evaluation_mask = evaluation_mask,
																												goodness = TRUE)[c("AUC", "maxTSS")])),
												 expected = c(FALSE, FALSE))
						expect_type(object = measures(observations = observations,
																					predictions = predictions,
																					evaluation_mask = evaluation_mask,
																					goodness = TRUE),
												type = "double")
						expect_vector(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask,
																						goodness = TRUE),
													ptype = numeric(length = 0))
						expect_length(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask,
																						goodness = TRUE),
													n = 8)
					}
)

test_that(desc = "returns expexted value for normal parameters - goodness = TRUE, ROCR unavailable",
					code = {
						mockery::stub(measures, "requireNamespace", FALSE)
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4,    0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_warning(object = measures(observations = observations,
																					   predictions = predictions,
																					   evaluation_mask = evaluation_mask,
																					   goodness = TRUE),
												 regexp = NULL)
						expect_equal(object =unname(measures(observations = observations,
																				         predictions = predictions,
																								 evaluation_mask = evaluation_mask,
																								 goodness = TRUE)[c("AUC", "maxTSS")]),
												 expected = c(NA_real_, NA_real_)) %>% suppressWarnings()
						expect_type(object = measures(observations = observations,
																					predictions = predictions,
																					evaluation_mask = evaluation_mask,
																					goodness = TRUE),
												type = "double") %>% suppressWarnings()
						expect_vector(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask,
																						goodness = TRUE),
													ptype = numeric(length = 0)) %>% suppressWarnings()
						expect_length(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask,
																						goodness = TRUE),
													n = 8) %>% suppressWarnings()
					}
)

test_that(desc = "returns expexted value for normal parameters - df = TRUE",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4, 0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_silent(object = measures(observations = observations,
																						predictions = predictions,
																						evaluation_mask = evaluation_mask,
																						df = TRUE))
						expect_type(object = measures(observations = observations,
																					predictions = predictions,
																					evaluation_mask = evaluation_mask,
																					df = TRUE),
												type = "list")
						expect_s3_class(object = measures(observations = observations,
																					  	predictions = predictions,
																					  	evaluation_mask = evaluation_mask,
																					  	df = TRUE),
														class = "data.frame")
						expect_equal(object = unname(unlist(round(x = measures(observations = observations,
																																	 predictions = predictions,
																																	 evaluation_mask = evaluation_mask,
																																	 df = TRUE), digits = 8))),
												 expected = c(0.80000000, 0.75000000, -0.05000000, 0.75000000, 0.66666667, -0.08333333))
						expect_equal(object = colnames(measures(observations = observations,
																								 predictions = predictions,
																								 evaluation_mask = evaluation_mask,
																								 df = TRUE)),
												 expected = c("CP_train", "CP_eval", "DCP", "CPP_train", "CPP_eval", "DCPP"))
						expect_equal(object = dim(measures(observations = observations,
																							 predictions = predictions,
																							 evaluation_mask = evaluation_mask,
																							 df = TRUE)),
												 expected = c(1L, 6L))
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: observations",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4, 0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_error(object = measures(predictions = predictions,
																					 evaluation_mask = evaluation_mask),
												 regexp = NULL)
						expect_error(object = measures(evaluation_mask = evaluation_mask),
												 regexp = NULL)
						expect_warning(object = measures(observations = as.character(observations),
																						 predictions = predictions,
																						 evaluation_mask = evaluation_mask),
												 regexp = NULL)
						expect_warning(object = measures(observations = as.numeric(observations),
																						 predictions = predictions,
																						 evaluation_mask = evaluation_mask),
													 regexp = NULL)
						expect_silent(object = measures(observations = as.logical(observations),
																						predictions = predictions,
																						evaluation_mask = evaluation_mask))
						expect_error(object = measures(observations <- c(0L, -3L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
							                             predictions = predictions,
																					 evaluation_mask = evaluation_mask),
												 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: predictions",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4, 0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_error(object = measures(observations = observations,
																					 evaluation_mask = evaluation_mask),
												 regexp = NULL)
						expect_error(object = measures(observations = observations),
												 regexp = NULL)
						expect_warning(object = measures(observations = observations,
																						 predictions = as.character(predictions),
																						 evaluation_mask = evaluation_mask),
													 regexp = NULL)
						expect_error(object = measures(observations <- observations,
																					 predictions = predictions[1:3],
																					 evaluation_mask = evaluation_mask),
												 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: evaluation_mask",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4, 0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_error(object = measures(observations = observations,
																					 predictions = predictions),
												 regexp = NULL)
						expect_error(object = measures(predictions = predictions),
												 regexp = NULL)
						expect_warning(object = measures(observations = observations,
																						 predictions = predictions,
																						 evaluation_mask = as.character(evaluation_mask)),
													 regexp = NULL)
						expect_warning(object = measures(observations = observations,
																						 predictions = predictions,
																						 evaluation_mask = as.numeric(evaluation_mask)),
													 regexp = NULL)
						expect_warning(object = measures(observations = observations,
																						 predictions = predictions,
																						 evaluation_mask = as.integer(evaluation_mask)),
													 regexp = NULL)
						expect_error(object = measures(observations <- observations,
																					 predictions = predictions,
																					 evaluation_mask = evaluation_mask[1:3]),
												 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: goodness",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4, 0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_warning(object = measures(observations <- observations,
																				  	 predictions = predictions,
																				  	 evaluation_mask = evaluation_mask,
																				  	 goodness = "FALSE"),
												 regexp = NULL)
						expect_warning(object = measures(observations <- observations,
																				  	 predictions = predictions,
																				  	 evaluation_mask = evaluation_mask,
																				  	 goodness = 0),
												 regexp = NULL)
						expect_error(object = measures(observations <- observations,
																					 predictions = predictions,
																					 evaluation_mask = evaluation_mask,
																					 goodness = logical(length = 0)),
												 regexp = NULL)
						expect_warning(object = measures(observations <- observations,
																					   predictions = predictions,
																					   evaluation_mask = evaluation_mask,
																					   goodness = c(FALSE, FALSE)),
												 regexp = NULL)
						expect_error(object = measures(observations <- observations,
																					 predictions = predictions,
																					 evaluation_mask = evaluation_mask,
																					 goodness = NA),
												 regexp = NULL)
					}
)

test_that(desc = "returns errors/warnings if needed - parameter: df",
					code = {
						observations <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
						predictions <- c(0.1, 0.2, 0.4, 0.5, 0.5, 0.2, 0.3, 0.3, 0.4, 0.3, 0.65, 0.9, 0.9, 1, 0.1, 0.5, 0.8, 0.8)
						evaluation_mask <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
						expect_warning(object = measures(observations <- observations,
																			  		 predictions = predictions,
																				  	 evaluation_mask = evaluation_mask,
																				  	 df = "FALSE"),
												 regexp = NULL)
						expect_warning(object = measures(observations <- observations,
																					   predictions = predictions,
																					   evaluation_mask = evaluation_mask,
																					   df = 0),
												 regexp = NULL)
						expect_error(object = measures(observations <- observations,
																					 predictions = predictions,
																					 evaluation_mask = evaluation_mask,
																					 df = logical(length = 0)),
												 regexp = NULL)
						expect_warning(object = measures(observations <- observations,
																						 predictions = predictions,
																						 evaluation_mask = evaluation_mask,
																						 df = c(FALSE, FALSE)),
													 regexp = NULL)
						expect_error(object = measures(observations <- observations,
																					 predictions = predictions,
																					 evaluation_mask = evaluation_mask,
																					 df = NA),
												 regexp = NULL)
					}
)
