context("report_env")


test_that("test creating an instance of report_env", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_env <- new("report_env")
			r_env <- choice_c(
					r_env,
					stationMesure = c("temp_gabion", "coef_maree"),
					datedebut = "2008-01-01",
					datefin = "2008-12-31",
					silent = TRUE
			)
			r_env <- connect(r_env, silent = TRUE)
			expect_true(nrow(r_env@data) > 0,
					"There is a problem when loading data in the data slot")
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("test plot method", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_env <- new("report_env")
			r_env <- choice_c(
					r_env,
					stationMesure = c("temp_gabion", "coef_maree"),
					datedebut = "2008-01-01",
					datefin = "2008-12-31",
					silent = TRUE
			)
			r_env <- connect(r_env, silent = TRUE)
			expect_error(plot(r_env, silent=TRUE), NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})
