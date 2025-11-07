context("report_annual")


test_that("Test an instance of report_annual loaded with choice_c", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()	
			r_ann <- new("report_annual")
			r_ann <- choice_c(
					r_ann,
					dc = c(5, 6),
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ", "AGG"),
					start_year = "2010",
					end_year = "2013",
					silent = TRUE
			)
			r_ann <- connect(r_ann, silent = TRUE)
			expect_s4_class(r_ann, "report_annual")
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})


test_that("Test methods in report_annual", {
			skip_on_cran()
			stacomi(database_expected = TRUE, sch ="test")
			env_set_test_stacomi()			# should not produce any error
			expect_error({
						r_ann <- new("report_annual")
						r_ann <- choice_c(
								r_ann,
								dc = c(5, 6),
								taxa = c("Anguilla anguilla"),
								stage = c("AGJ", "AGG"),
								start_year = "2010",
								end_year = "2013",
								silent = TRUE
						)
						r_ann <- connect(r_ann, silent = TRUE)
						dev.new()
						plot(r_ann, silent = TRUE)
						dev.new()
						barplot(r_ann)},		NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

#test_that("Test example report_mig_annual-example",
#		{
#			# check if built with examples (Rtools install --example)
#			# the file is generate it examples but later loaded to examples from the class using @example
#			# be sure you have built Roxygen documentation before running
#			example_path <- file.path(getwd(),"inst", "examples", "report_annual-example.R")
#			source(example_path)
#			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
#			
#		})

test_that("Complement to example",
		{
			
			stacomi(database_expected = FALSE, sch ="test")
			data(r_ann)
			xtr_ann <- stacomiR::xtable(
					r_ann,
					dc_name = c("Passe bassins", "Piege anguille RG", "Piege anguille RD"),
					tax_name = "Anguille",
					std_name = c("Arg.", "Jaun.")
			)
			path = file.path(
					path.expand(get("datawd", envir = envir_stacomi)),
					paste(
							paste(r_ann@dc@dc_selected, collapse = "+"),
							"_",
							paste(r_ann@taxa@taxa_selected, collapse = "+"),
							"_",
							paste(r_ann@stage@stage_selected, collapse = "+"),
							"_",
							r_ann@start_year@year_selected,
							":",
							r_ann@end_year@year_selected,
							".html",
							sep = ""
					),
					fsep = "/"
			)
			
			# here you can add an argument file=path
			expect_output(print(xtr_ann, type = "html"))
			
			# the following uses the "addtorow" argument which creates nice column headings,
			# format.args creates a thousand separator
			# again this will need to be saved in a file using the file argument
			expect_output(
					print(
							xtr_ann,
							add.to.row = get("addtorow", envir_stacomi),
							include.rownames = TRUE,
							include.colnames = FALSE,
							format.args = list(big.mark = " ", decimal.mark = ",")
					)
			)
			# barplot transforms the data, further arguments can be passed as to barplot
			dev.new()
			barplot(r_ann)
			dev.new()
			barplot(
					r_ann,
					args.legend = list(x = "topleft", bty = "n"),
					col = c("#CA003E", "#1A9266", "#E10168", "#005327", "#FF9194")
			)
			
			# An example with custom arguments for legend.text (overriding plot defauts)
			data(r_ann_adour)
			if (requireNamespace("RColorBrewer", quietly = TRUE)) {
				lesdc <-
						r_ann_adour@dc@data$dc_code[r_ann_adour@dc@data$dc %in% r_ann_adour@dc@dc_selected]
				barplot(
						r_ann_adour,
						legend.text = lesdc,
						args.legend = list(x = "topleft", bty = "n"),
						col = RColorBrewer::brewer.pal(9, "Spectral"),
						beside = TRUE
				)
			}
			dev.new()
			plot(r_ann_adour, silent = TRUE)
			graphics.off()
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("test xtable method for report_annual", {
			
			skip_on_cran()
			stacomi(database_expected = FALSE, sch ="test")
			env_set_test_stacomi()			
			r_ann <- new("report_annual")
			r_ann <- choice_c(
					r_ann,
					dc = c(5, 6),
					taxa = c("Anguilla anguilla"),
					stage = c("AGJ", "AGG"),
					start_year = "2010",
					end_year = "2013",
					silent = TRUE
			)
			r_ann <- connect(r_ann)
			xtr_mig_annual <- stacomiR::xtable(
					r_ann,
					dc_name = c("Passe bassins", "Piege anguille RG"),
					tax_name = "Anguille",
					std_name = c("Arg.", "Jaun.")
			)
			# now without name
			xtr_mig_annual <- stacomiR::xtable(
					r_ann,
					dc_name =NULL,
					tax_name = NULL,
					std_name = NULL
			)
			expect_equal(class(xtr_mig_annual)[1],
					"xtable",
					"report_annual should have an xtable method")
			xtr_mig_annual <- stacomiR::xtable(
					r_ann,
					dc_name = c("Passe bassins", "Piege anguille RG"),
					tax_name = "Anguille",
					std_name = c("Arg.", "Jaun.")
			)
			expect_equal(class(xtr_mig_annual)[1],
					"xtable",
					"report_annual should have an xtable method")
			path = file.path(
					path.expand(get("datawd", envir = envir_stacomi)),
					paste(
							paste(r_ann@dc@dc_selected, collapse = "+"),
							"_",
							paste(r_ann@taxa@taxa_selected, collapse = "+"),
							"_",
							paste(r_ann@stage@stage_selected, collapse = "+"),
							"_",
							r_ann@start_year@year_selected,
							":",
							r_ann@end_year@year_selected,
							".html",
							sep = ""
					),
					fsep = "/"
			)
			
			expect_output(print(xtr_mig_annual, type = "html"))
			expect_output(
					print(
							xtr_mig_annual,
							add.to.row = get("addtorow", envir_stacomi),
							include.rownames = TRUE,
							include.colnames = FALSE,
							format.args = list(big.mark = " ", decimal.mark = ",")
					)
			)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
			
		})

test_that("test plot methods for report_annual", {
			data(r_ann_adour)
			dev.new()
			if (requireNamespace("RColorBrewer", quietly = TRUE)) {
				lesdc <-
						r_ann_adour@dc@data$dc_code[r_ann_adour@dc@data$dc %in% r_ann_adour@dc@dc_selected]
				expect_error({
							barplot(
									r_ann_adour,
									legend.text = lesdc,
									args.legend = list(x = "topleft", bty = "n"),
									col = RColorBrewer::brewer.pal(9, "Spectral"),
									beside = TRUE
							)
						}, NA)
				
			}
			expect_error({
						dev.new()
						plot(r_ann_adour, silent = TRUE)
						graphics.off()
					}, NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})

test_that("test that plot method does not return wrong axis", {
			skip_on_cran()
			env_set_test_stacomi()	
			stacomi(database_expected = TRUE, sch="test")
			r_ann <- new("report_annual")
			r_ann <- choice_c(
					r_ann,
					dc = c(20),
					taxa = c("Salmo salar"),
					stage = c(5),
					start_year = "2010",
					end_year = "2012",
					silent = TRUE
			)
			r_ann <- connect(r_ann, silent = TRUE)

			expect_error({
						dev.new()
						plot(r_ann, silent = TRUE)
						graphics.off()
					}, NA)
			rm(list = ls(envir = envir_stacomi), envir = envir_stacomi)
		})
