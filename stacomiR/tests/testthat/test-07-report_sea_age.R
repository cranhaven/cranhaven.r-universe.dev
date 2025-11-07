context("report_sea_age")


test_that("test creating an instance of report_sea_age with data loaded (logrami required)",
		{
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			r_seaa <- new("report_sea_age")
			env_set_test_stacomi()    
			r_seaa <- suppressWarnings(
					choice_c(
							r_seaa,
							dc = c(19,20),
							horodatedebut = "2012-01-01",
							horodatefin = "2012-12-31",
							limit1hm = 675,
							limit2hm = 875,
							silent = TRUE
					)
			)
			# warnings No data for par 1786No data for par 1785
			expect_error( r_seaa <- connect(r_seaa, silent = TRUE), NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("test charge + connect method", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch= "test")
			env_set_test_stacomi()
			r_seaa <- new("report_sea_age")
			suppressWarnings(r_seaa <- choice_c(
							r_seaa,
							dc = c(19,20),
							horodatedebut = "2012-01-01",
							horodatefin = "2012-12-31",
							limit1hm = 675,
							limit2hm = 875,
							silent = TRUE
					))
			r_seaa <- charge(r_seaa)
			r_seaa <- connect(r_seaa,silent = TRUE)
			expect_gt(nrow(r_seaa@data),0)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("test that loading bad limits fails", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch= "test")
			env_set_test_stacomi()
			r_seaa <- new("report_sea_age")
			
			expect_error(
					r_seaa <- choice_c(
							r_seaa,
							dc = c(19,20),
							horodatedebut = "2012-01-01",
							horodatefin = "2012-12-31",
							limit1hm = 675,
							limit2hm = "strawberry",
							silent = FALSE
					)
			)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("test calcule method r_seaa", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch= "test")
			env_set_test_stacomi()
			r_seaa <- new("report_sea_age")
			suppressWarnings(r_seaa <- choice_c(
							r_seaa,
							dc = c(19,20),
							horodatedebut = "2012-01-01",
							horodatefin = "2012-12-31",
							limit1hm = 675,
							limit2hm = 875,
							silent = TRUE
					))
			r_seaa <- connect(r_seaa,silent = TRUE)
			r_seaa <- calcule(r_seaa,silent = TRUE)
			expect_gt(nrow(r_seaa@calcdata[[1]]),0)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("test plot summary print r_seaa", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch= "test")
			env_set_test_stacomi()
			data(r_seaa)
			expect_error(plot(r_seaa, plot.type=1,  silent=TRUE), NA)
			expect_error(plot(r_seaa, plot.type=2, silent=TRUE), NA)
			expect_error(plot(r_seaa, plot.type=2, silent=TRUE), NA)
			expect_output(summary(r_seaa))
			expect_output(print(r_seaa))				
				rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			})
	
	
	test_that("test write supprime print r_seaa", {
				skip_on_cran()
				stacomi(database_expected = TRUE, sch= "test")
				env_set_test_stacomi()
				r_seaa <- new("report_sea_age")
				suppressWarnings(r_seaa <- choice_c(
								r_seaa,
								dc = c(19,20),
								horodatedebut = "2012-01-01",
								horodatefin = "2012-12-31",
								limit1hm = 675,
								limit2hm = 875,
								silent = TRUE
						))
				r_seaa <- connect(r_seaa,silent = TRUE)
				r_seaa <- calcule(r_seaa,silent = TRUE)
				expect_error(supprime(r_seaa), NA)
				expect_error(write_database(r_seaa), NA)			
				rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			})