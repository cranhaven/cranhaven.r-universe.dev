context("report_df")

test_that("Test an instance of report_df", {
			skip_on_cran()
			stacomi(database_expected = FALSE, sch ="test")
			env_set_test_stacomi()
			r_df <- new("report_df")
			r_df <- choice_c(
					r_df,
					2,
					horodatedebut = "2013-01-01",
					horodatefin = "2013-12-31",
					silent = TRUE
			)
			expect_gt(nrow(r_df@df@data),
					0,
					label = "There should be data loaded by the choice_c method in the data slot of
							the ref_df slot,nrow(r_df@df@data)")
			expect_s4_class(r_df,
					"report_df")
			expect_error(
					BfDF <- choice_c(
							r_df,
							2,
							horodatedebut = "2013 01 011",
							horodatefin = "2013-12-31",
							silent = TRUE
					)
			)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("report_df charge method works", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()
			r_df <- new("report_df")
			r_df <- choice_c(
					r_df,
					2,
					horodatedebut = "2013-01-01",
					horodatefin = "2013-12-31",
					silent = TRUE
			)
			r_df <- charge(r_df, silent = TRUE)
			r_df <- connect(r_df, silent = TRUE)
			expect_equal(nrow(r_df@data), 5)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("report_df plot method works", {
			stacomi(database_expected = FALSE, sch ="test")
			data(r_df)
			r_df <- r_df
			# expect_error(expr,NA) tests for an absence of error
			expect_error({
						invisible(capture.output(plot(r_df, plot.type = "1", silent = TRUE)))
						invisible(capture.output(plot(r_df,
								plot.type = "2",
								silent = TRUE,
								main = "An example title")))
						plot(r_df,
								plot.type = "3",
								silent = TRUE,
								main = "An example title")
						plot(r_df,
								plot.type = "4",
								silent = TRUE,
								main = "An example title")}, NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("report_df summary method works", {
			skip_on_cran()
			stacomi(database_expected = FALSE, sch ="test")
			data(r_df)
			r_df <- r_df
			#expected <- ifelse(Sys.getlocale(category = "LC_TIME")=="French_France.utf8", "statistiques", "summary")
			expect_output(summary(r_df, silent = TRUE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("report_df print method works", {
			skip_on_cran()
			stacomi(database_expected = FALSE, sch ="test")
			data(r_df)
			r_df <- r_df
			expect_output(print(r_df))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})