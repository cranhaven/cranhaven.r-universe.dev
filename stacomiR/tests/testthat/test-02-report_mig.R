context("report_mig")

test_that("Test an instance of report_mig", {
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch ='test')
			report_mig <- new("report_mig")
			options(warn = -1)
			report_mig <- choice_c(
					report_mig,
					dc = c(6),
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					datedebut = "2013-01-01",
					datefin = "2013-12-31"
			)
			options(warn = 0)
			expect_s4_class(report_mig,
					"report_mig")
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that(
		"Test an instance of report_mig, check that operations accross two years are split correcly",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch = 'test')   
			report_mig <- new("report_mig")
			options(warn = -1)
			report_mig <- choice_c(
					report_mig,
					dc = c(6),
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ"),
					datedebut = "2010-01-01",
					datefin = "2010-12-31"
			)
			options(warn = 0)
			report_mig <- charge(report_mig, silent = TRUE)
			report_mig <- connect(report_mig, silent = TRUE)
			report_mig <- calcule(report_mig, silent = TRUE)
			# before doing the split per year the sum was 27458
			# now it is less
			expect_equal(round(sum(report_mig@calcdata[["dc_6"]][["data"]]$Effectif_total)),
          27430)
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		}
)



test_that("Test connect method", {
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch ="test")
			# overriding user schema
			r_mig = new("report_mig")
			r_mig = choice_c(
					r_mig,
					dc = 19,
					taxa = c("Salmo salar"),
					stage = c("5"),
					datedebut = "2012-01-01",
					datefin = "2012-12-31"
			)
			r_mig <- charge(r_mig, silent = TRUE)
			r_mig <- connect(r_mig, silent = TRUE)
			
			expect_length(r_mig@data, 11)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

#test_that("Test example 02_report_mig",
#		{
#			# check if built with examples (Rtools install --example)
#			# the file is generate it examples but later loaded to examples from the class using @example
#			# be sure you have built Roxygen documentation before running
#			skip_on_cran()
#			test <- file.access(example_path, 0)
#			if (test[1] != 0)
#				warnings("Package example dir not created ?")
#			else
#				source(example_path)
#		})


test_that("Summary method works",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = FALSE, sch ="test")			
			# overriding user schema
      r_mig = new("report_mig")
      r_mig = choice_c(
          r_mig,
          dc = 5,
          taxa = c("Chelon ramada"),
          stage = c("IND"),
          datedebut = "2015-01-01",
          datefin = "2015-12-31"
      )
      r_mig <- charge(r_mig, silent=TRUE)
      r_mig <- connect(r_mig, silent=TRUE)
      expect_output(r_mig <- calcule(r_mig, silent = TRUE))
			expect_silent(summary(r_mig, silent = TRUE))		

			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("Test writing an example to the database",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch ="test")
			data("r_mig")
			r_mig <- calcule(r_mig, silent = TRUE)
			
			expect_silent(write_database(object = r_mig, silent = TRUE))
			# by default in r_mig we don't want to check for multiannual bilan
			# it is written again in the database
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that(
		"Test that different sums are the same, for report_mig, report_mig_interannual, report_annual",
		{
			skip_on_cran()
			env_set_test_stacomi()	
			stacomi(database_expected = TRUE, sch ="test")
			data("r_mig")
			r_mig <- calcule(r_mig, silent = TRUE)
			expect_equal(sum(r_mig@calcdata$dc_5$data$Effectif_total),
					sum(r_mig@data[r_mig@data$ope_dic_identifiant == 5, "value"]))
			write_database(object = r_mig, silent = TRUE)
			# using setAs to transform the report_mig into report_mig_interannual
			bili = as(r_mig, "report_mig_interannual")
			bila = as(bili, "report_annual")
			bila <- connect(bila, silent = TRUE)
			# we test that the report_annual has the same number as
			# report_mig
			expect_equal(
					sum(r_mig@calcdata$dc_5$data$Effectif_total),
					bila@data$effectif,
					label = "The sum of number in the report_mig are different to the
							number in the report_annual class"
			)			
			bili <- connect(bili, check = TRUE, silent = TRUE)
			expect_equal(
					sum(r_mig@calcdata$dc_5$data$Effectif_total),
					sum(bili@data$bjo_valeur[bili@data$bjo_labelquantite == "Effectif_total"]),
					label = "The sum of number in the report_mig are different to the
							number in the report_mig_interannual"
			)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		}
)


test_that("print method works",
		{
			stacomi(database_expected = FALSE, sch ="test")
			# overriding user schema
			data("r_mig")
			expect_output(print(r_mig), "report_mig=choice_c", info = NULL)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})



test_that("test example for fd80",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected = TRUE, sch ='test')
			bM_EclusierVaux = new("report_mig")
			bM_EclusierVaux = choice_c(
					bM_EclusierVaux,
					dc = 31,
					taxa = c("Anguilla anguilla"),
					stage = c("AGG"),
					datedebut = "2013-01-01",
					datefin = "2013-12-31"
			)
			bM_EclusierVaux <- charge(bM_EclusierVaux, silent = TRUE)
			bM_EclusierVaux <- connect(bM_EclusierVaux, silent = TRUE)
			bM_EclusierVaux <- calcule(bM_EclusierVaux, silent = TRUE)
			expect_output(plot(bM_EclusierVaux, silent = FALSE))
			expect_output(summary(bM_EclusierVaux, silent = FALSE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("test example with glass eel",
		{
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			bM_Arzal_civ = new("report_mig")
			bM_Arzal_civ = choice_c(
					bM_Arzal_civ,
					dc = 6,
					taxa = c("Anguilla anguilla"),
					stage = c("CIV"),
					datedebut = "2013-01-01",
					datefin = "2013-12-31"
			)
			bM_Arzal_civ <- charge(bM_Arzal_civ, silent = TRUE)
			bM_Arzal_civ <- connect(bM_Arzal_civ, silent = TRUE)
			bM_Arzal_civ <- calcule(bM_Arzal_civ, silent = TRUE)
			expect_silent(plot(bM_Arzal_civ, silent = TRUE))
			expect_error(plot(bM_Arzal_civ, plot.type="step", silent = TRUE), NA)
			# some additional arguments passed to plot via ...
			expect_silent(plot(bM_Arzal_civ, silent = TRUE, bty = "n"))
			expect_output(summary(bM_Arzal_civ, silent = FALSE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


