context("report_dc")

test_that("Test an instance of report_dc", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_dc <- new("report_dc")
			r_dc <- choice_c(
					r_dc,
					5,
					horodatedebut = "2013-01-01",
					horodatefin = "2013-12-31",
					silent = TRUE
			)
			expect_gt(nrow(r_dc@dc@data),
					0,
					label = "There should be data loaded by the choice_c method in the data slot of
							the ref_dc slot,nrow(r_dc@dc@data)")
			expect_s4_class(r_dc,
					"report_dc")
			expect_error(
					BfDC <- choice_c(
							r_dc,
							5,
							horodatedebut = "2013 01 011",
							horodatefin = "2013-12-31",
							silent = TRUE
					)
			)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
			
		})

test_that("report_dc charge method works", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()			
			r_dc <- new("report_dc")
			r_dc <- choice_c(
					r_dc,
					5,
					horodatedebut = "2013-01-01",
					horodatefin = "2013-12-31",
					silent = TRUE
			)
			r_dc <- charge(r_dc, silent = TRUE)
			r_dc <- connect(r_dc, silent = TRUE)
			expect_equal(nrow(r_dc@data), 7)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("report_dc plot method works", {
			stacomi(database_expected = FALSE, sch ="test")
			data(r_dc)
			r_dc <- r_dc
			expect_error({
						invisible(capture.output(plot(r_dc, plot.type = "1", silent = TRUE)))
						invisible(capture.output(plot(r_dc,
								plot.type = "2",
								silent = TRUE,
								main = "An example title")))
						plot(r_dc,
								plot.type = "3",
								silent = TRUE,
								main = "An example title")
						plot(r_dc,
								plot.type = "4",
								silent = TRUE,
								main = "An example title")
					}, NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("report_dc summary method works", {
			skip_on_os("linux")
			skip_on_os("solaris")
			stacomi(database_expected = FALSE, sch ="test")
			data(r_dc)
			r_dc <- r_dc
			expect_output(summary(r_dc, silent = TRUE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("report_dc print method works", {
			stacomi(database_expected = FALSE, sch ="test")
			data(r_dc)
			r_dc <- r_dc
			expect_output(print(r_dc))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})