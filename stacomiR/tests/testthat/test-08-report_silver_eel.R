context("report_silver_eel")


test_that(
		"test creating an instance of report_silver_eel with data loaded",
		{
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ='test')
			env_set_test_stacomi()		
			r_silver <- new("report_silver_eel")
      suppressWarnings(
			r_silver <- choice_c(
					r_silver,
					dc = c(21, 31),
					horodatedebut = "2012-09-01",
					horodatefin = "2014-10-04",
					silent = TRUE
			))
			r_silver <- connect(r_silver, silent = TRUE)
			# warnings No data for par 1786No data for par 1785
			r_silver <- suppressWarnings(calcule(r_silver, silent = TRUE))
			expect_error({
						plot(r_silver, plot.type = 1)
						plot(r_silver, plot.type = 2)
						suppressWarnings(plot(r_silver, plot.type = 3))
						plot(r_silver, plot.type = 4)
						# print a summary statistic, and save the output in a list for later use
						stats <- summary(r_silver, silent = TRUE)
					},NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		}
)

test_that(
    "report silver eel doesn't  crashes at choice_c when no silver",
    {
      skip_on_cran()
      stacomi(database_expected = TRUE, sch ='test')
      env_set_test_stacomi()		
      r_silver <- new("report_silver_eel")
      expect_error(r_silver <- choice_c(
          r_silver,
          dc = 10,
          horodatedebut = "2012-09-01",
          horodatefin = "2014-10-04",
          silent = TRUE
      ))
      rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
    }
)
