context("report_ge_weight")



test_that("test creating instance report_ge_weight", {

			stacomi(database_expected = FALSE, sch ="test")	
			r_gew <- new("report_ge_weight")
			expect_s4_class(r_gew, "report_ge_weight")
		})

test_that("test choice_c method for report_ge_weight", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_gew <- new("report_ge_weight")
			expect_error(r_gew<-choice_c(r_gew,
					dc=c(6),			
					start_year="2009",
					end_year="2013",
					selectedvalue=">1",
					silent=TRUE), NA)
		})

test_that("test connect method for report_ge_weight", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_gew <- new("report_ge_weight")
			r_gew<-choice_c(r_gew,
							dc=c(6),			
							start_year="2009",
							end_year="2013",
							selectedvalue=">1",
							silent=TRUE)
		expect_message({r_gew <- connect(r_gew)}, NA)	
		})


			

test_that("test calcule method report_ge_weight", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_gew <- new("report_ge_weight")
			r_gew <- choice_c(r_gew,
					dc=c(6),			
					start_year="2009",
					end_year="2013",
					selectedvalue=">1",
					silent=TRUE)
			expect_silent(r_gew <- connect(r_gew, silent=TRUE))	
			expect_silent(r_gew <- calcule(r_gew, silent=TRUE))
			expect_gt(nrow(r_gew@calcdata[[1]]),
					0,
					"No data in calcdata after running calculations")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)			
		})

test_that("test that plot method works", {
			stacomi(database_expected = FALSE, sch ="test")	
			data("r_gew")
			expect_error({
# A ggplot showing the trend in weight
			plot(r_gew, plot.type=1, silent=TRUE)
# A plot showing both the data and the trend as recorded in the database
			plot(r_gew, plot.type=2, silent=TRUE)
# Same as plot.type=1 but with size according to size of the sample,
# usefull for wet weights where weight are recorded on a number of glass eel
			plot(r_gew, plot.type=3, silent=TRUE)
		}, NA)			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("test that model method works", {
			stacomi(database_expected = FALSE, sch ="test")	
			data("r_gew")		
			assign("datawd","", envir = envir_stacomi)
			# First model with nls, see Guerault and Desaunay (1993) 
			expect_output(model(r_gew,model.type="seasonal"))
			expect_output(model(r_gew,model.type="seasonal1"))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("test supprime method ref_coe et write method report_ge_weight", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()	
			assign("datawd","", envir = envir_stacomi)
			r_gew <- new("report_ge_weight")
			r_gew <- choice_c(r_gew,
					dc=c(6),			
					start_year="2009",
					end_year="2013",
					selectedvalue="tous",
					silent=TRUE)
			r_gew <- connect(r_gew, silent=TRUE)
		  r_gew <- calcule(r_gew, silent=TRUE)
			r_gew <- model(r_gew, silent=TRUE)
			expect_error(write_database(r_gew, silent=TRUE), NA)		
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("test bug in report ge weight", {
      skip_on_cran()
      stacomi(database_expected = TRUE, sch ="test")
      r_gew<-new("report_ge_weight")
      r_gew@liste<-charge(object=r_gew@liste,listechoice=c("=1",">1","tous"),label="")
# here I'm using weights when number are larger than 1i.e.wet weight
# always choose a date from one year to the next eg 2010 to 2011
# as the dates are from august to august
      r_gew<-choice_c(r_gew,
          dc=c(6),			
          start_year="2010",
          end_year="2013",
          selectedvalue="tous",
          silent=TRUE)
      r_gew<-connect(r_gew)	
      r_gew<-calcule(r_gew)
      expect_gt(mean(r_gew@calcdata$data$w),0.26)
      expect_lt(mean(r_gew@calcdata$data$w),0.27)
    })


