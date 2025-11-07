context("report_species")


test_that("test creating an instance of report_species", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			bilesp <- new("report_species")
			# split is one of "none", "year", "week", "month
			
			bilesp <- choice_c(
					bilesp,
					dc = c(5, 6),
					split = "year",
					start_year = "2009",
					end_year = "2012",
					silent = TRUE
			)
		
			
			bilesp <- charge(bilesp, silent = TRUE)
			expect_error(bilesp <- connect(bilesp, silent = TRUE),NA)
			expect_s4_class(bilesp,
					"report_species")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("test that having ref_taxa in envir_stacomi does not mess with things", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			bilesp <- new("report_species")
			# split is one of "none", "year", "week", "month
			assign("ref_taxa",new("ref_taxa"),envir=envir_stacomi)
			bilesp <- choice_c(
					bilesp,
					dc = c(5, 6),
					split = "year",
					start_year = "2009",
					end_year = "2012",
					silent = TRUE
			)	
			expect_error(bilesp <- connect(bilesp, silent = TRUE),NA)
			expect_s4_class(bilesp,
					"report_species")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("test calcule method report_species", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			bilesp <- new("report_species")
			# split is one of "none", "year", "week", "month
			
			bilesp <- choice_c(
					bilesp,
					dc = c(5, 6),
					split = "year",
					start_year = "2009",
					end_year = "2012",
					silent = TRUE
			)
			bilesp <- charge(bilesp, silent = TRUE)
			bilesp <- connect(bilesp, silent = TRUE)
			bilesp <- calcule(bilesp, silent = TRUE)
			expect_gt(nrow(bilesp@calcdata),
					0,
					"No data in calcdata after running calculations")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("test method report_species with different options for taxa", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()		
			bilesp <- new("report_species")
			# split is one of "none", "year", "week", "month
			
			bilesp <- choice_c(
					bilesp,
					dc = c(5, 6),
					taxa="all",
					split = "year",
					start_year = "2009",
					end_year = "2012",
					silent = TRUE
			)
			bilesp <- charge(bilesp, silent = TRUE)
			bilesp <- connect(bilesp, silent = TRUE)
			bilesp <- calcule(bilesp, silent = TRUE)
			expect_gt(nrow(bilesp@calcdata),
					0,
					"No data in calcdata after running calculations")
			bilesp <- choice_c(
					bilesp,
					dc = c(5, 6),
					taxa=c("2038","2086", "2055" , "2108" ,  "2183"),
					split = "year",
					start_year = "2009",
					end_year = "2012",
					silent = TRUE
			)
			bilesp <- charge(bilesp, silent = TRUE)
			bilesp <- connect(bilesp, silent = TRUE)
			bilesp <- calcule(bilesp, silent = TRUE)
			expect_gt(nrow(bilesp@calcdata),
					0,
					"No data in calcdata after running calculations")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("test that plot method works", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")	
			bilesp <- new("report_species")
			# split is one of "none", "year", "week", "month			
			bilesp <- choice_c(
					bilesp,
					dc = c(5, 6),
					split = "year",
					start_year = "2009",
					end_year = "2012",
					silent = TRUE
			)
			bilesp <- charge(bilesp, silent = TRUE)
			bilesp <- connect(bilesp, silent = TRUE)
			bilesp <- calcule(bilesp, silent = TRUE)
			expect_error({
			plot(bilesp, plot.type = "pie", silent = TRUE)
			plot(bilesp, plot.type = "barplot", silent = TRUE)
			mycolorrampblue <-
					grDevices::colorRampPalette(c("#395B74", "#010F19"))
			mycolorrampyellow <-
					grDevices::colorRampPalette(c("#B59C53", "#271D00"))
			mycolorrampred <-
					grDevices::colorRampPalette(c("#B56F53", "#270B00"))
			#length(unique(bilesp@calcdata$taxa_stage)) # 15
			# here creating a vector of length 15 with nice blending colours
			color <- c(mycolorrampblue(3),
					mycolorrampyellow(3),
					mycolorrampred(4))
			plot(bilesp,
					plot.type = "barplot",
					color = color,
					silent = TRUE)},NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("test that summary method works", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			bilesp <- new("report_species")
			# split is one of "none", "year", "week", "month
			
			bilesp <- choice_c(
					bilesp,
					dc = c(5, 6),
					split = "year",
					start_year = "2009",
					end_year = "2012",
					silent = TRUE
			)
			bilesp <- charge(bilesp, silent = TRUE)
			bilesp <- connect(bilesp, silent = TRUE)
			bilesp <- calcule(bilesp, silent = TRUE)
			expect_output(summary(bilesp, silent = FALSE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})
