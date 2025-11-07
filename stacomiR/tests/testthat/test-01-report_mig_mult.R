context("report_mig_mult")



test_that("Test an instance of report_mig_mult", {
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")		
			report_mig_mult <- new("report_mig_mult")
			#options(warn = -1)
			report_mig_mult <- suppressWarnings(
					choice_c(
							report_mig_mult,
							dc = c(19,20),
							taxa = c( "Salmo salar"),
							stage = c("5"),
							datedebut = "2011-01-01",
							datefin = "2011-12-31",
							silent = TRUE
					)
			)
			#options(warn = 0)
			expect_s4_class(report_mig_mult,
					"report_mig_mult")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

## This test check that the code above works with numeric and a different formating for date
test_that("Test another instance of report_mig_mult", {
			
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			#options(warn = -1)
			report_mig_mult <- suppressWarnings(
					choice_c(
							report_mig_mult,
							dc = c(6, 21, 31),
							taxa = c(2038, 2220),
							stage = c("AGG", "AGJ", "CIV"),
							datedebut = "2012-01-01",
							datefin = "31/12/2012",
							silent = TRUE
					)
			)
			#options(warn = 0)
			expect_s4_class(report_mig_mult,
					"report_mig_mult")
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("Tests one instance with error (dc does not exist)",
		{			
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")	
			env_set_test_stacomi()
			
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			expect_error(
					choice_c(
							report_mig_mult,
							dc = c(6, 7000),
							taxa = c("Anguilla anguilla", "Salmo salar"),
							stage = c("AGG", "AGJ", "CIV"),
							datedebut = "2012-01-01",
							datefin = "31/12/2012",
							silent = TRUE
					)
			)
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("Test charge method for report_mig_mult",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			report_mig_mult <- choice_c(
					report_mig_mult,
					dc = c(6, 20),
					taxa = c(2038),
					stage = c("AGG", "AGJ", "CIV"),
					datedebut = "2012-01-01",
					datefin = "31/12/2012",
					silent = TRUE
			)
			options(warn = 0)
			report_mig_mult <- charge(report_mig_mult, silent = TRUE)
			expect_is(get("report_df", envir = envir_stacomi), "report_df")
			expect_is(get("report_dc", envir = envir_stacomi), "report_dc")
			expect_is(get("report_ope", envir = envir_stacomi), "report_ope")
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("Test connect method for report_mig_mult",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			report_mig_mult <- choice_c(
					report_mig_mult,
					dc = c(6, 20),
					taxa = c(2038),
					stage = c("AGG", "AGJ", "CIV"),
					datedebut = "2012-01-01",
					datefin = "31/12/2012",
					silent = TRUE
			)
			options(warn = 0)
			report_mig_mult <- charge(report_mig_mult, silent = TRUE)
			report_mig_mult <- connect(report_mig_mult, silent = TRUE)
			expect_gt(nrow(report_mig_mult@data), 0)
			report_ope <- get("report_ope", envir = envir_stacomi)
			expect_gt(nrow(report_ope@data), 0)
			report_df <- get("report_df", envir = envir_stacomi)
			expect_gt(nrow(report_df@data), 0)
			report_dc <- get("report_dc", envir = envir_stacomi)
			expect_gt(nrow(report_dc@data), 0)
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})



test_that("Test funtable and funstat",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			report_mig_mult <- choice_c(
					report_mig_mult,
					dc = c(6, 20),
					taxa = c(2038),
					stage = c("AGJ"),
					datedebut = "2012-01-01",
					datefin = "31/12/2012",
					silent = TRUE
			)
			options(warn = 0)
			report_mig_mult <- charge(report_mig_mult, silent = TRUE)
			report_mig_mult <- connect(report_mig_mult, silent = TRUE)
			report_mig_mult <- calcule(report_mig_mult, silent = TRUE)			
			the_taxa = report_mig_mult@taxa@data[report_mig_mult@taxa@data$tax_code %in% report_mig_mult@taxa@taxa_selected, ]
			the_stages = report_mig_mult@stage@data[report_mig_mult@stage@data$std_code %in% report_mig_mult@stage@stage_selected, ]
			lesdc = as.numeric(report_mig_mult@dc@dc_selected)
			data <- report_mig_mult@calcdata[[stringr::str_c("dc_", lesdc[1])]][["data"]]
			data <- data[data$lot_tax_code == the_taxa[1, "tax_code"] &
							data$lot_std_code == the_stages[1, "std_code"], ]
			if (any(duplicated(data$No.pas))) stop("duplicated values in No.pas")
			data_without_hole <- merge(data.frame(No.pas = as.numeric(strftime(report_mig_mult@time.sequence,
											format = "%j")) - 1, debut_pas = report_mig_mult@time.sequence),
					data, by = c("No.pas", "debut_pas"), all.x = TRUE)
			data_without_hole$CALCULE[is.na(data_without_hole$CALCULE)] <- 0
			data_without_hole$MESURE[is.na(data_without_hole$MESURE)] <- 0
			data_without_hole$EXPERT[is.na(data_without_hole$EXPERT)] <- 0
			data_without_hole$PONCTUEL[is.na(data_without_hole$PONCTUEL)] <- 0
			
			resum = funstat(tableau = data_without_hole, time.sequence = report_mig_mult@time.sequence,
					the_taxa$tax_nom_latin, the_stages$std_libelle, lesdc[1], TRUE)
			expect_gt(nrow(resum), 1)
			data_without_hole$debut_pas <- as.Date(data_without_hole$debut_pas)
			data_without_hole <- data_without_hole[, -match("fin_pas", colnames(data_without_hole))]
			expect_output(funtable(tableau = data_without_hole, time.sequence = report_mig_mult@time.sequence,
							the_taxa$tax_nom_latin, the_stages$std_libelle, lesdc[1], resum, silent=FALSE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("Test funtable for glass eel weights",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			report_mig_mult <- choice_c(
					report_mig_mult,
					dc = c(6),
					taxa = c(2038),
					stage = c("CIV"),
					datedebut = "2012-01-01",
					datefin = "31/12/2012",
					silent = TRUE
			)
			options(warn = 0)
			report_mig_mult <- charge(report_mig_mult, silent = TRUE)
			report_mig_mult <- connect(report_mig_mult, silent = TRUE)
			report_mig_mult <- calcule(report_mig_mult, silent = TRUE)			
			the_taxa = report_mig_mult@taxa@data[report_mig_mult@taxa@data$tax_code %in% report_mig_mult@taxa@taxa_selected, ]
			the_stages = report_mig_mult@stage@data[report_mig_mult@stage@data$std_code %in% report_mig_mult@stage@stage_selected, ]
			lesdc = as.numeric(report_mig_mult@dc@dc_selected)
			data <- report_mig_mult@calcdata[[stringr::str_c("dc_", lesdc)]][["data"]]
			data <- data[data$lot_tax_code == the_taxa[1, "tax_code"] &
							data$lot_std_code == the_stages[1, "std_code"], ]
			if (any(duplicated(data$No.pas))) stop("duplicated values in No.pas")
			data_without_hole <- merge(data.frame(No.pas = as.numeric(strftime(report_mig_mult@time.sequence,
											format = "%j")) - 1, debut_pas = report_mig_mult@time.sequence),
					data, by = c("No.pas", "debut_pas"), all.x = TRUE)
			data_without_hole$CALCULE[is.na(data_without_hole$CALCULE)] <- 0
			data_without_hole$MESURE[is.na(data_without_hole$MESURE)] <- 0
			data_without_hole$EXPERT[is.na(data_without_hole$EXPERT)] <- 0
			data_without_hole$PONCTUEL[is.na(data_without_hole$PONCTUEL)] <- 0
			resum = funstat(tableau = data_without_hole, time.sequence = report_mig_mult@time.sequence,
					the_taxa$tax_nom_latin, the_stages$std_libelle, lesdc[1], TRUE)
			data_without_hole$debut_pas <- as.Date(data_without_hole$debut_pas)
			data_without_hole <- data_without_hole[, -match("fin_pas", colnames(data_without_hole))]
			expect_output(funtable(tableau = data_without_hole, time.sequence = report_mig_mult@time.sequence,
							the_taxa$tax_nom_latin, the_stages$std_libelle, lesdc, resum, silent=FALSE))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})


test_that("Test summary",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			report_mig_mult <- choice_c(
					report_mig_mult,
					dc = c(6, 20),
					taxa = c(2038),
					stage = c("AGJ"),
					datedebut = "2012-01-01",
					datefin = "31/12/2012",
					silent = TRUE
			)
			options(warn = 0)
			report_mig_mult <- charge(report_mig_mult, silent = TRUE)
			report_mig_mult <- connect(report_mig_mult, silent = TRUE)
			report_mig_mult <- calcule(report_mig_mult, silent = TRUE)			
			expect_output(summary(report_mig_mult))
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("Test plot",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			report_mig_mult <- choice_c(
					report_mig_mult,
					dc = c(6, 20),
					taxa = c(2038),
					stage = c("AGJ","CIV"),
					datedebut = "2012-01-01",
					datefin = "31/12/2012",
					silent = TRUE
			)
			options(warn = 0)
			report_mig_mult <- charge(report_mig_mult, silent = TRUE)
			report_mig_mult <- connect(report_mig_mult, silent = TRUE)
			report_mig_mult <- calcule(report_mig_mult, silent = TRUE)			
			expect_error(plot(report_mig_mult,plot.type="standard",silent=TRUE), NA)
			expect_error(plot(report_mig_mult,plot.type="step",silent=TRUE), NA)
			expect_error(plot(report_mig_mult,plot.type="multiple",silent=TRUE), NA)

			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})



test_that("Test that step plot returns only one curve",
		{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")				
			report_mig_mult <- new("report_mig_mult")
			options(warn = -1)
			report_mig_mult <- choice_c(
					report_mig_mult,
					dc = c(5),
					taxa = c(2038),
					stage = c("AGJ"),
					datedebut = "2012-01-01",
					datefin = "2012-12-31",
					silent = TRUE
			)
			options(warn = 0)
			report_mig_mult <- charge(report_mig_mult, silent = TRUE)
			report_mig_mult <- connect(report_mig_mult, silent = TRUE)
			report_mig_mult <- calcule(report_mig_mult, silent = TRUE)			
			expect_error(plot(report_mig_mult,plot.type="step",silent=TRUE), NA)
			expect_error(plot(report_mig_mult,plot.type="multiple",silent=TRUE), NA)
			
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

