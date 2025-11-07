context("report_mig_interannual")


test_that("Test an instance of report_mig_interannual loaded with choice_c",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch ="test")
			r_mig_interannual <- new("report_mig_interannual")
			# the following will load data for size,
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			suppressWarnings(r_mig_interannual <- choice_c(
					r_mig_interannual,
					dc = 6,
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					start_year = 2010,
					end_year = 2013,
					silent = TRUE
			))
			r_mig_interannual <- connect(r_mig_interannual, silent = TRUE)
			# three warning produced, none shown due to silent=TRUE
			options(warn = 0)
			expect_s4_class(r_mig_interannual, "report_mig_interannual")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

#test_that("Test bug when writing data with Alice and Sebastien",
#    {
#      skip_on_cran()
#     # env_set_test_stacomi()
#       options(					
#           		stacomiR.dbname = "bd_contmig_nat",
#           		stacomiR.host ="********************",
#           		stacomiR.port = "5432",
#           		stacomiR.user = "********************",
#           		stacomiR.password = "****************"				
#               )
#      stacomi(database_expected = TRUE, sch ="seinormigr")
#      r_mig_interannual <- new("report_mig_interannual")
#      # the following will load data for size,
#      # parameters 1786 (total size) C001 (size at video control)
#      # dc 5 and 6 are fishways located on the Arzal dam
#      # two stages are selected
#      suppressWarnings(r_mig_interannual <- choice_c(
#              r_mig_interannual,
#              dc = 102,
#              taxa = c("Anguilla anguilla"),
#              stage = c("4"),
#              start_year = 2014,
#              end_year = 2023,
#              silent = TRUE
#          ))
#      r_mig_interannual <- connect(r_mig_interannual, silent = TRUE)
#      # three warning produced, none shown due to silent=TRUE
#      options(warn = 0)
#      expect_s4_class(r_mig_interannual, "report_mig_interannual")
#      rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
#      
#    })


test_that("Test method summary in report_mig_interannual", {
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch ="test")
			# overriding user schema to point to test
			r_mig_interannual <- new("report_mig_interannual")
			# the following will load data for size,
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			r_mig_interannual <- choice_c(
					r_mig_interannual,
					dc = 6,
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					start_year = 2010,
					end_year = 2012,
					silent = TRUE
			)
			r_mig_interannual <- connect(r_mig_interannual, silent = TRUE)
			invisible(capture.output(ss <- summary(object = r_mig_interannual, year_choice=2012, silent = TRUE)))
			# this one is failing in covr but not in test. I don't understand why
			if ("year" %in% colnames(ss$`6`)) expect_equal(ss$`6`[1, "year"],"2012") else
			  expect_equal(ss$`6`[1, "année"],"2012")
			# two warning produced
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})
#
#test_that("Test example report_mig_interannual-example",
#		{
#			# check if built with examples (Rtools install --example)
#			# the file is generate it examples but later loaded to examples from the class using @example
#			# be sure you have built Roxygen documentation before running
#			example_path <-
#					file.path(.libPaths(),
#							"stacomiR",
#							"R-ex",
#							"report_mig_interannual-class.R")
#			test <- file.access(example_path, 0)
#			if (test[1] != 0)
#				warnings("Package example dir not created ?")
#			else
#				suppressWarnings(source(example_path))
#			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
#		})




test_that("Test that loading two taxa will fail",
		{
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			r_mig_interannual <- new("report_mig_interannual")
			# the following will load data for size,
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			invisible(capture.output(expect_error(									
									choice_c(
											r_mig_interannual,
											dc = 5,
											taxa = c("Anguilla anguilla", "Petromyzon marinus"),
											stage = c("AGJ"),
											start_year = "2010",
											end_year = 2015,
											silent = TRUE
									)
							)))
			
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("Test that report_mig_interannual displays message when silent = FALSE",
		{
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()			
			r_mig_interannual <- new("report_mig_interannual")
			expect_output(
					r_mig_interannual <- choice_c(
							r_mig_interannual,
							dc = 6,
							taxa = c("Anguilla anguilla"),
							stage = c("AGJ"),
							start_year = 2011,
							end_year = 2012,
							silent = FALSE
					))
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("Test supprime method",
		{
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()			
			bmi_cha <- new("report_mig_interannual") #châtelrault
			bmi_cha <- 
					choice_c(
							bmi_cha,
							dc = c(19),
							taxa = c("Salmo salar"),
							stage = c("5"),
							start_year = "2010",
							end_year = "2012",
							silent = TRUE
					)
			
			
			bmi_cha <- charge(bmi_cha, silent = TRUE)
			# deleting all data to ensure everything is loaded
			supprime(bmi_cha)
			expect_output(bmi_cha <- connect(bmi_cha, silent = TRUE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("Test that different sums are the same, for  report_mig_interannual, report_mig_mult",
		{
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			# overriding user schema
			env_set_test_stacomi()
			# this chunk is not launched from examples but loads the r_mig dataset if connection works
			r_mig_interannual <- new("report_mig_interannual")
			r_mig_interannual <- choice_c(
					r_mig_interannual,
					dc = 6,
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					start_year = 2010,
					end_year = 2010,
					silent = TRUE
			)
			r_mig_interannual <- connect(r_mig_interannual, silent=TRUE)
			
			nb_r_mig_interannual <- sum(r_mig_interannual@data[r_mig_interannual@data$"bjo_labelquantite"=="Effectif_total","bjo_valeur" ])
			r_mig_mult <- new("report_mig_mult")
			r_mig_mult <- choice_c(
					r_mig_mult,
					dc = 6,
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					datedebut = "2010-01-01",
					datefin = "2010-12-31",
					silent = TRUE
			)
			r_mig_mult <- charge(r_mig_mult, silent=TRUE)
			r_mig_mult <- connect(r_mig_mult, silent=TRUE)
			r_mig_mult <- calcule(r_mig_mult, silent=TRUE)
			nb_r_mig_mult = sum(r_mig_mult@calcdata$dc_6$data$Effectif_total)
			expect_equal(round(nb_r_mig_interannual),round(nb_r_mig_mult))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("Test bmi plots", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()
			report_mig_interannual <- new("report_mig_interannual")
			# the following will load data for size,
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			report_mig_interannual <- choice_c(
					report_mig_interannual,
					dc = 6,
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					start_year = 2010,
					end_year = 2013,
					silent = TRUE
			)
      # first test if there is nothing in data 
			expect_output(plot(report_mig_interannual, plot.type = "step", silent = FALSE))
			report_mig_interannual <- connect(report_mig_interannual, silent = TRUE)
			report_mig_interannual <- calcule(report_mig_interannual, silent = TRUE)
      # test plots
			expect_error(suppressWarnings(plot(report_mig_interannual, plot.type = "step", silent = TRUE)),NA)
			expect_error(suppressWarnings(plot(report_mig_interannual, plot.type = "line", silent = TRUE)),NA)
			expect_error(suppressWarnings(plot(report_mig_interannual, plot.type = "standard", silent = TRUE)),NA)
			expect_error(plot(report_mig_interannual, plot.type = "barchart", silent = TRUE),NA)
      expect_error(plot(report_mig_interannual, plot.type = "barchart", timesplit="2 weeks", silent = TRUE),NA)
      
      expect_error(plot(report_mig_interannual, plot.type = "barchart", timesplit="week", silent = TRUE),NA)
      
      expect_error(plot(report_mig_interannual, plot.type = "barchart", timesplit="day", silent = TRUE),NA)
     
      expect_error(suppressMessages(plot(report_mig_interannual, plot.type = "pointrange", silent = TRUE)),NA)
			expect_error(plot(report_mig_interannual, plot.type = "density", silent = TRUE),NA)		
			expect_error(plot(report_mig_interannual, plot.type = "seasonal", silent = TRUE),NA)
			expect_error(suppressWarnings(plot(report_mig_interannual, plot.type = "seasonal", timesplit="semaine", silent = TRUE)),NA)			
			expect_error(suppressWarnings(plot(report_mig_interannual, plot.type = "seasonal", timesplit="day", silent = TRUE)),NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("Test bmi plots bug", {
      skip_on_cran()
      stacomi(database_expected = TRUE, sch ="test")
      env_set_test_stacomi()
      report_mig_interannual <- new("report_mig_interannual")
      # the following will load data for size,
      # parameters 1786 (total size) C001 (size at video control)
      # dc 5 and 6 are fishways located on the Arzal dam
      # two stages are selected
      report_mig_interannual <- choice_c(
          report_mig_interannual,
          dc = 5,
          taxa = c("Anguilla anguilla"),
          stage = c("AGJ"),
          start_year = 2010,
          end_year = 2013,
          silent = TRUE
      )

      report_mig_interannual <- connect(report_mig_interannual, silent = TRUE)
      report_mig_interannual <- calcule(report_mig_interannual, silent = TRUE)
      # test plots
      expect_error(plot(report_mig_interannual, plot.type = "density", silent = TRUE),NA)		
           rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
      
    })


test_that("Test bmi for several dc", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()
			r_mig_interannual <- new("report_mig_interannual")
			# the following will load data for size,
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			r_mig_interannual <- choice_c(
					r_mig_interannual,
					dc = c(5,6),
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					start_year = 2010,
					end_year = 2013,
					silent = TRUE
			)
			r_mig_interannual <- connect(r_mig_interannual, silent = TRUE)
			# calcule is only needed for seasonal
			r_mig_interannual <- calcule(r_mig_interannual, silent = TRUE)
			expect_error(suppressWarnings(plot(r_mig_interannual, plot.type = "step", silent = TRUE)),NA)
			expect_error(suppressWarnings(plot(r_mig_interannual, plot.type = "line", silent = TRUE)),NA)
			expect_error(suppressWarnings(plot(r_mig_interannual, plot.type = "standard", silent = TRUE)),NA)
			expect_error(plot(r_mig_interannual, plot.type = "barchart", timesplit="day",silent = TRUE),NA)
			expect_error(suppressMessages(plot(r_mig_interannual, plot.type = "pointrange", silent = TRUE)),NA)
			expect_error(plot(r_mig_interannual, plot.type = "density", silent = TRUE),NA)	
			expect_error(plot(r_mig_interannual, plot.type = "seasonal", silent = TRUE),NA)
			expect_error(suppressWarnings(plot(r_mig_interannual, plot.type = "seasonal", timesplit="semaine", silent = TRUE)),NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})





test_that("Test method summary when data are not every month", {
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch ="test")
			# overriding user schema to point to test
			# the following will load data for size,
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			rmi <- new("report_mig_interannual")
			rmi <- choice_c(rmi,
					dc=c(5),
					taxa=c("Salmo trutta trutta"),
					stage=c("IND"),
					start_year="2010",
					end_year="2013",
					silent=TRUE)
			invisible(capture.output(rmi <- connect(rmi, silent=TRUE)))
			rmi <- charge(rmi, silent=TRUE)
			rmi <- calcule(rmi)
			invisible(capture.output(ss <- summary(object = rmi, year_choice=2013, silent = TRUE)))
			expect_equal(nrow(ss$`5`),4) # only 4 month of data in 2013
			
# two warning produced
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})
