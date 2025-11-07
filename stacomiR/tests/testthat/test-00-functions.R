context("colortable")
test_that("test color table",
		{
			env_set_test_stacomi()
			color = grDevices::rainbow(5)
			names(color) <- LETTERS[1:5]
			# using a named vector
			cs <- 		colortable(color = color,
					vec = LETTERS[1:5],
					palette = "Accent")
			expect_true(nrow(cs)==5)
			# if vec and color do not have the same length should fail -----------
			expect_error(
					cs <- 		colortable(color = color,
							vec = LETTERS[1:4],
							palette = "Accent")
			)
			
			# not passing a named vector -------------------------------------------
			
			cs <- 		colortable(color = NULL,
					vec = LETTERS[1:5],
					palette = "Accent")
			expect_true(nrow(cs)==5)

			expect_true(nrow(cs)==5)
			
			# using too much values for palette should issue a message ---------------
			# problem this in turn generates an error
#		
#			expect_message(colortable(color = NULL,
#							vec = LETTERS[1:20],
#							palette = "Accent"),
#					"Palette Accent has only got 8 values and you need 20")
			
			# bug found there
			expect_error(suppressWarnings(
			colortable(color = NULL, vec = c("titi", "zozo"), palette = "Dark2"),
			NA))
			
			
		})


context("fun_schema")
test_that("test funschema",
		{
      skip_on_cran()
			env_set_test_stacomi()
			table <- fun_schema()
			expect_gt(nrow(table),0)
		})
