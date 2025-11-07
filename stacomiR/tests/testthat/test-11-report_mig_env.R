context("report_mig_env")


test_that("test creating an instance of report_mig_env", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()
			r_mig_env <- new("report_mig_env")
			r_mig_env <- choice_c(
					r_mig_env,
					dc = c(5, 6, 12),
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ", "AGG", "CIV"),
					stationMesure = c("temp_gabion", "coef_maree", "phases_lune"),
					datedebut = "2010-01-01",
					datefin = "2010-12-31",
					silent = TRUE
			)
			r_mig_env <- charge(r_mig_env, silent = TRUE)
			r_mig_env <- connect(r_mig_env, silent = TRUE)
			expect_true(
					nrow(r_mig_env@report_env@data) > 0,
					"Data not loaded in the report_env part of the object"
			)
			expect_true(
					nrow(r_mig_env@report_mig_mult@data) > 0,
					"Data not loaded in the report_mig_mult part of the object"
			)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("test plot method", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_mig_env <- new("report_mig_env")
			r_mig_env <- choice_c(
					r_mig_env,
					dc = c(5, 6, 12),
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ", "AGG", "CIV"),
					stationMesure = c("temp_gabion", "coef_maree", "phases_lune"),
					datedebut = "2010-01-01",
					datefin = "2010-12-31",
					silent = TRUE
			)
			r_mig_env <- charge(r_mig_env, silent = TRUE)
			r_mig_env <- connect(r_mig_env, silent = TRUE)
			r_mig_env <- calcule(r_mig_env, silent = TRUE)
			expect_error({
						suppressWarnings(plot(r_mig_env, silent = TRUE))
						suppressWarnings(plot(
										r_mig_env,
										color_station = c(
												"temp_gabion" = "red",
												"coef_maree" = "blue",
												"phases_lune" = "pink"
										),
										color_dc = c("5" = "yellow", "6" = "orange", "12" = "purple"),
										silent = TRUE
								))}, NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})
