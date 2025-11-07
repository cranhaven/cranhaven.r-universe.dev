context("ref_horodate")
test_that("Test that the parsing of many kind of dates works",
		{
			ref_horodate <- new("ref_horodate")
			# regular expression to test string "1] nous avons le choix dans la date\n"
			# default string returned by the method
			expect_that(
					ref_horodate <- choice_c(ref_horodate,
							horodate = "01/01/2013 00:00:00"),
					prints_text("^\\[1\\].+date.+")
			)
			expect_that(
					ref_horodate <- choice_c(ref_horodate,
							horodate = "01/01/2013 00:00"),
					prints_text("^\\[1\\].+date.+")
			)
			expect_that(
					ref_horodate <- choice_c(ref_horodate,
							horodate = "01-01-2013 00:00"),
					prints_text("^\\[1\\].+date.+")
			)
			expect_that(
					ref_horodate <- choice_c(ref_horodate,
							horodate = "2013-01-01 00:00"),
					prints_text("^\\[1\\].+date.+")
			)
			expect_that(
					ref_horodate <- choice_c(ref_horodate,
							horodate = "01-01-2013"),
					prints_text("^\\[1\\].+date.+")
			)
			expect_error(ref_horodate <- choice_c(ref_horodate,
							horodate = "2013/01/01 00:00:00"))
		})



test_that("Test that the parsing of wrong character formats gets an error",
		{
			ref_horodate <- new("ref_horodate")
			options(warn = -1)
			expect_error(ref_horodate <- choice_c(ref_horodate,
							horodate = "2013 01 01"))
			options(warn = 1)
			
		})

context("ref_df")

test_that("Test that ref_df choice_c method loads character, numeric, but not rubbish",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_df <- new("ref_df")
			ref_df <- charge(ref_df)
			expect_silent(ref_df <- choice_c(ref_df,	2))
			expect_silent(ref_df <- choice_c(ref_df,	"2"))
			expect_error(ref_df <-
							suppressWarnings(choice_c(ref_df,	"semoule")))
		})

context("ref_dc")

test_that("Test ref_dc ",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_dc <- new("ref_dc")
			ref_dc <- charge(ref_dc)
			expect_silent(ref_dc <- choice_c(ref_dc,	6))

		})

context("ref_taxa")

test_that("Test ref_taxa charge",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_taxa <- new("ref_taxa")
			expect_silent(ref_taxa <- charge(ref_taxa))
		})



test_that("Test choice method for reftaxa ",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_taxa <- new("ref_taxa")
			ref_taxa <- charge(ref_taxa)
			ref_taxa <- choice_c(ref_taxa,"2038")
			expect_equal(ref_taxa@taxa_selected,"2038")
			ref_taxa <- choice_c(ref_taxa,"2038")
			expect_equal(ref_taxa@taxa_selected,"2038")
			ref_taxa <- choice_c(ref_taxa,"Anguilla anguilla")
			expect_equal(ref_taxa@taxa_selected,"2038")
		})


context("ref_stage")

test_that("Test ref_stage charge",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_stage <- new("ref_stage")
			expect_silent(ref_stage <- charge(ref_stage))
		})

test_that("Test choice_c method for ref_stage ",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_stage <- new("ref_stage")
			ref_stage <- charge(ref_stage)
			ref_stage <- choice_c(ref_stage,"CIV")
			expect_equal(ref_stage@stage_selected,"CIV")
		})




context("ref_coe")


test_that("Test that ref_coe charge method works",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_coe <- new("ref_coe")
			ref_coe@datedebut<-strptime('01/01/2010',format='%d/%m/%Y')
			ref_coe@datefin<-strptime('01/01/2011',format='%d/%m/%Y')
			expect_error(ref_coe <- charge(ref_coe), NA)
		})

context("ref_par")
test_that("Test that ref_par works",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()
			ref_par <- new("ref_par")
			ref_par <- charge(ref_par)
			nr1 <- nrow(ref_par@data)
			expect_gt(nr1,0)
			ref_par <- charge_with_filter(ref_par,
					dc_selected=6,
					taxa_selected=2038,
					stage_selected=c('AGJ','CIV'))
			nr2 <- nrow(ref_par@data)
			expect_gt(nr1, nr2) 	
			ref_par <- choice_c(ref_par,"B002")
			expect_identical(ref_par@par_selected,"B002")
		})
context("ref_parqual")
test_that("Test that ref_parqual works",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()			
			ref_parqual <- new("ref_parqual")
			ref_parqual <- charge(ref_parqual)
			nr3 <- nrow(ref_parqual@data)
			expect_gt(nr3,0, label = "nrow ref_parqual@data", expected.label = "0")
			ref_parqual <- charge_with_filter(ref_parqual,
					dc_selected=6,
					taxa_selected=2038,
					stage_selected=c('AGJ','CIV'))
			nr4 <- nrow(ref_parqual@data)
			expect_gt(nr3, nr4) 
			expect_error(ref_parqual <- charge_complement(ref_parqual))
			ref_parqual@par_selected <-'B002' 
			expect_error(ref_parqual <- charge_complement(ref_parqual), NA)
			nr5 <- nrow(ref_parqual@valqual)
			expect_gt(nr5, 0, label= "ref_parqual@valqual", expected.label = "0")
			ref_parqual <- choice_c(ref_parqual,"B002")
			expect_identical(ref_parqual@par_selected,"B002")
		})

context("ref_parquan")
test_that("Test that ref_parquan works",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()			
			ref_parquan <- new("ref_parquan")
			ref_parquan <- charge(ref_parquan)
			nr1 <- nrow(ref_parquan@data)
			expect_gt(nr1,0, label = "nrow ref_parquan@data", expected.label = "0")
			ref_parquan <- charge_with_filter(ref_parquan,
					dc_selected=6,
					taxa_selected=2038,
					stage_selected=c('AGJ','CIV'))
			nr2 <- nrow(ref_parquan@data)
			expect_lt(nr2, nr1) 
			ref_parquan <- choice_c(ref_parquan,"1786")
			expect_identical(ref_parquan@par_selected,"1786")

		})

context("ref_env")
test_that("Test that ref_env works",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()			
			ref_env <- new("ref_env")
			ref_env <- charge(ref_env)
			nr1 <- nrow(ref_env@data)
			expect_gt(nr1,0, label = "nrow ref_env@data", expected.label = "0")
			ref_env <- choice_c(ref_env, stationMesure="temp_gabion")
			expect_identical(ref_env@env_selected,"temp_gabion")
			
		})


context("ref_textbox")
test_that("Test that ref_textbox works",
		{
			skip_on_cran()
			stacomi(database_expected=TRUE, sch ="test")
			env_set_test_stacomi()			
			ref_textbox <- new("ref_textbox")
			expect_error(ref_textbox <- charge(ref_textbox,title='un titre',label='20'),NA)
	
			
		})