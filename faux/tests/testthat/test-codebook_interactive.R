f <- file()
faux_options(connection = f)
faux_options(verbose = FALSE)

on.exit({
  faux_options(connection = stdin()) # reset connection
  close(f) # close the file
  faux_options(verbose = TRUE)
})

set.seed(1)
data <- sim_design(within = list(A = c("A1","A2")), 
                   between = list(B = c("B1","B2")), 
                   plot = FALSE)

test_that("interactive", {
  desc <- c("Subject ID", "Factor B", "Level 1 of Factor A", "Level 2 of Factor A")
  
  # set up interactive answers
  c("s", desc[1], "n",
    "s", desc[2], "y", "2", "Level 1", "Level 2",
    "f", desc[3],
    "f", desc[4]) %>%
    paste(collapse = "\n") %>%
    write(f)
  
  ol_expected <- c(
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [s]",
    "  Column description [id]",
    "  Is this column a factor? [y/n]",
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [s]",
    "  Column description [B]",
    "  Is this column a factor? [y/n]",
    "  How many levels does it have? [2]",
    "B1 description [B1]",
    "B2 description [B2]",
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [f]",
    "  Column description [A1]",
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [f]",
    "  Column description [A2]"
  )
  
  op_expected <- c(
    "\033[32mid has 200 unique values (e.g., S001,S002,S003,S004,S005)\033[39m",
    "\033[32mB has 2 unique values (B1, B2)\033[39m",
    "\033[32mA1 has 200 unique values (-2.30797839905936 to 2.59232766994599)\033[39m",
    "\033[32mA2 has 200 unique values (-2.88892067167955 to 2.64916688109488)\033[39m"
  )
  
  expect_message(
      ol <- capture_output_lines({cb <- codebook_interactive(data) })
    , op_expected[1], fixed = TRUE)
  
  expect_equal(ol, ol_expected)
  
  expect_equal(class(cb), c("psychds_codebook", "list"))
  n <- sapply(cb$variableMeasured, `[[`, "name")
  d <- sapply(cb$variableMeasured, `[[`, "description")
  dt <- sapply(cb$variableMeasured, `[[`, "dataType") %>% unname()
  expect_equal(n, names(data))
  expect_equal(d, desc)
  expect_equal(dt, c("string", "string", "float", "float"))
  expect_equal(cb$variableMeasured[[2]]$levels, 
               list(B1 = "Level 1", B2 = "Level 2"))
})

test_that("warnings and defaults", {
  # set up interactive answers
  c("v", "s", "", "", "n",
    "S", "", "y", "2", "", "",
    "float", "f", "",
    "f", "") %>%
    paste(collapse = "\n") %>%
    write(f)
  
  ol_expected <- c(
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [s]",
    "\033[31mError:\033[39m Enter only i, s, f or b",
    "  Column description [id]",
    "  Is this column a factor? [y/n]",
    "\033[31mError:\033[39m Enter only y or n",
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [s]",
    "  Column description [B]",
    "  Is this column a factor? [y/n]",
    "  How many levels does it have? [2]",
    "B1 description [B1]",
    "B2 description [B2]",
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [f]",
    "\033[31mError:\033[39m Enter only i, s, f or b",
    "  Column description [A1]",
    "  Is it an integer (i), string (s), float (f), or boolean (b)? [f]",
    "  Column description [A2]"
  )
  
  ol <- capture_output_lines({ cb <- codebook_interactive(data) })
  
  expect_equal(ol, ol_expected)
  
  expect_equal(class(cb), c("psychds_codebook", "list"))
  n <- sapply(cb$variableMeasured, `[[`, "name")
  d <- sapply(cb$variableMeasured, `[[`, "description")
  dt <- sapply(cb$variableMeasured, `[[`, "dataType") %>% unname()
  expect_equal(n, names(data))
  expect_equal(d, names(data))
  expect_equal(dt, c("string", "string", "float", "float"))
  expect_equal(cb$variableMeasured[[2]]$levels, 
               list(B1 = "B1", B2 = "B2"))
})



