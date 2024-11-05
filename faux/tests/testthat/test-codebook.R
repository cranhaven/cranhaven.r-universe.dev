user_opts <- faux_options("sep", "verbose", "plot", "connection")
on.exit(faux_options(user_opts))

faux_options(plot = FALSE)

set.seed(8675309)
data <- sim_design()

# defaults ----
test_that("defaults", {
  cb <- codebook(data)
  
  compare <- '{
    "@context": "https://schema.org/",
    "@type": "Dataset",
    "name": "data",
    "schemaVersion": "Psych-DS 0.1.0",
    "variableMeasured": [
        {
            "@type": "PropertyValue",
            "name": "id",
            "description": "id",
            "dataType": "string"
        },
        {
            "@type": "PropertyValue",
            "name": "y",
            "description": "value",
            "dataType": "float"
        }
    ]
}
'
  class(compare) <- "json"
  
  expect_equal(cb, compare)
})

# warnings ----
test_that("warnings", {
  # all valid properties
  vardesc <- list("description" = c(id = "Subject ID"), 
                  "privacy" = c(T, F), 
                  "dataType" = c("string", "float"),
                  "minValue" = c(y = -100), 
                  "maxValue" = c(y = 100),
                  "levels" = list(id = LETTERS), 
                  "levelsOrdered" = c(id = TRUE), 
                  "na" = c(id = "NA"), 
                  "naValue" = c(id = "NOPE"),
                  "alternateName" = c(id = "part_id"), 
                  "unitCode" = c(id = "huh?"))
  expect_silent(cb <- codebook(data, "data", vardesc, return = "list"))
  
  expect_equal(cb$variableMeasured[[1]], 
               list(`@type` = "PropertyValue",
                     name = "id",
                     description = "Subject ID",
                     privacy = TRUE,
                     dataType = "string",
                     levels = as.list(LETTERS),
                     levelsOrdered = TRUE,
                     na = "NA",
                     naValue = "NOPE",
                     alternateName = "part_id",
                     unitCode = "huh?"))
  
  expect_equal(cb$variableMeasured[[2]], 
               list(`@type` = "PropertyValue",
                    name = "y",
                    description= "value",
                    privacy = FALSE,
                    dataType = "float",
                    minValue = -100,
                    maxValue = 100))
  
  # add an invalid property
  vardesc$invalid <- c(id = "STOP")
  expect_warning(cb <- codebook(data, "data", vardesc, return = "list"),
                 "The following variable properties are not standard: invalid", fixed = TRUE)
  
  # valid dataset properties
  faux_options(verbose = FALSE)
  expect_silent(codebook(cars, license = "MIT", author = "Lisa",
                         citation = "no", funder = "ERC", 
                         url = "http", identifier = "doi",
                         privacyPolicy = "x", keywords = c("a", "b")))
  
  # non-standard dataset properties
  expect_warning(cb <- codebook(cars, stuff = 1, more = 2),
                 "The following dataset properties are not standard: stuff, more", fixed = TRUE)
  faux_options(verbose = TRUE)
  
  # invalid data type
  dt <- list(dataType = list(speed = "nope", dist = "yup"))
  expect_warning(codebook(cars, vardesc = dt),
                 "speed does not have a valid dataType (nope)", 
                 fixed = TRUE)
  expect_warning(codebook(cars, vardesc = dt),
                 "dist does not have a valid dataType (yup)", 
                 fixed = TRUE)
  
  dt <- list(description = c("a", "b", "c"))
  expect_warning(codebook(cars, vardesc = dt),
                 "Couldn't set description for speed", fixed = TRUE)
  expect_warning(codebook(cars, vardesc = dt),
                 "Couldn't set description for dist", fixed = TRUE)
})

# no name ----
test_that("no name", {
  # should give dataset name if no name is specified
  cb <- codebook(iris, return = "list")
  expect_equal(cb$name, "iris")
  
  cb <- codebook(data.frame(a = 1:5), return = "list")
  expect_equal(cb$name, "data.frame(a = 1:5)")
  
  # piped data
  cb <- data.frame(
    a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    c = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    d = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    e = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    f = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    g = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    h = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  ) %>% codebook(return = "list")
  expect_equal(cb$name, "[unnamed data]")
  
  # multiline input
  cb <- codebook(data.frame(
    a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    c = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    d = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    e = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    f = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    g = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    h = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  ), return = "list")
  expect_equal(cb$name, "data.frame(a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ")
  
  # 64 character limit
  cb <- codebook(data.frame(a = c("a very long string will it be parsed into separate line or will it parse as a single very long line and be cut off by the limit of 64 characters for the dataset name?")), return = "list")
  expect_equal(cb$name, "data.frame(a = c(\"a very long string will it be parsed into sepa")
})

# no vardesc ----
test_that("no vardesc", {
  set.seed(8675309)
  data <- sim_design(2, 2, plot = FALSE)
  cb <- codebook(data, return = "list")
  
  vm <- cb$variableMeasured
  
  expect_equal(cb[["@type"]], "Dataset")
  expect_equal(cb[["schemaVersion"]], "Psych-DS 0.1.0")
  expect_equal(length(vm), 4)
  
  expect_equal(vm[[1]], list(`@type` = "PropertyValue",
                             name = "id",
                             description = "id", 
                             dataType = "string"))
  
  expect_equal(vm[[2]], list(`@type` = "PropertyValue",
                             name = "B1",
                             description = "B1",
                             levels = list(B1a = "B1a", B1b = "B1b"),
                             dataType = "string",
                             levelsOrdered = FALSE))
  
  expect_equal(vm[[3]], list(`@type` = "PropertyValue",
                             name = "W1a",
                             description = "W1a", 
                             dataType = "float"))
  
  expect_equal(vm[[4]], list(`@type` = "PropertyValue",
                             name = "W1b",
                             description = "W1b", 
                             dataType = "float"))
})

# named factor levels ----
test_that("named factor levels", {
  between <- list(
    pet = c(cat = "Has a cat", dog = "Has a dog")
  )
  data <- sim_design(between = between,
                     dv = list(y = "Happiness Score"),
                     id = list(id = "Subject ID"),
                     plot = FALSE)
  expect_message(cb <- codebook(data), "id set to dataType string")
  expect_message(cb <- codebook(data), "pet set to dataType string")
  expect_message(cb <- codebook(data, return = "list"), "y set to dataType float")
  
  output <- capture_output(print(cb))
  expect_equal(output, "Codebook for data (Psych-DS 0.1.0)\n\nDataset Parameters\n\n* name: data\n* schemaVersion: Psych-DS 0.1.0\n\nColumn Parameters\n\n* id (string): Subject ID\n* pet (string)\n  * Levels\n    * cat: Has a cat\n    * dog: Has a dog\n  * Ordered: FALSE\n* y (float): Happiness Score")
})

# with vardesc ----
test_that("with vardesc", {
  set.seed(8675309)
  data <- sim_design(2, 2, plot = FALSE)
  vardesc <- list(description = c(id = "Subject ID",
                                  B1 = "Between-subject factor",
                                  W1a = "Condition 1",
                                  W1b = "Condition 2"),
                  levels = list(B1 = c(B1a = "First level",
                                       B1b = "Second level")))
  cb <- codebook(data, "My Data", vardesc, return = "list")
  
  output <- capture_output(print(cb))
  expect_equal(output, "Codebook for My Data (Psych-DS 0.1.0)\n\nDataset Parameters\n\n* name: My Data\n* schemaVersion: Psych-DS 0.1.0\n\nColumn Parameters\n\n* id (string): Subject ID\n* B1 (string): Between-subject factor\n  * Levels\n    * B1a: First level\n    * B1b: Second level\n  * Ordered: FALSE\n* W1a (float): Condition 1\n* W1b (float): Condition 2")
  
  # unseen levels
  vardesc <- list(description = c(id = "Subject ID",
                                  B1 = "Between-subject factor",
                                  W1a = "Condition 1",
                                  W1b = "Condition 2"),
                  levels = list(B1 = c(B1a = "First level",
                                       B1b = "Second level",
                                       B1c = "Third level")))
  cb <- codebook(data, "My Data", vardesc, return = "list")
  # should convert levels to a list
  expect_equal(cb$variableMeasured[[2]]$levels,
               list(B1a = "First level",
                    B1b = "Second level",
                    B1c = "Third level"))
})

test_that("ignores extra vardesc", {
  set.seed(8675309)
  data <- sim_design(2, 2, plot = FALSE)
  vardesc <- list(description = c(id = "Subject ID",
                                  B1 = "Between-subject factor",
                                  C = "Extra Factor",
                                  W1a = "Condition 1",
                                  W1b = "Condition 2"),
                  levels = list(W1 = c(W1a = "First level",
                                       W1b = "Second level"),
                                C = c("C1", "C2"))
  )
  
  # turn off messages to check there are no warnings
  faux_options(verbose = FALSE)
  expect_silent(
    cb <- codebook(data, "My Data", vardesc, return = "list")
  )
  faux_options(verbose = TRUE)
})

# conversion ----
test_that("conversion", {
  data <- data.frame(
    i = as.integer(1:10),
    d = as.double(1:10),
    s = LETTERS[1:10],
    f = rnorm(10),
    b = rep(c(T, F), 5),
    l = rep(0:1, 5)
  )
  
  # no explicit conversion
  ndata <- codebook(data, return = "data")
  expect_equal(typeof(ndata$i), "integer")
  expect_equal(typeof(ndata$d), "integer")
  expect_equal(typeof(ndata$s), "character")
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "logical")
  expect_equal(typeof(ndata$l), "integer")
  
  # convert all to string
  vd <- list(dataType = rep("s", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "character")
  expect_equal(typeof(ndata$d), "character")
  #expect_equal(typeof(ndata$s), "character") # error on x86_64-w64-mingw32 (64-bit)
  expect_equal(typeof(ndata$f), "character")
  expect_equal(typeof(ndata$b), "character")
  expect_equal(typeof(ndata$l), "character")
  
  # convert all to int
  vd <- list(dataType = rep("i", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "integer")
  expect_equal(typeof(ndata$d), "integer")
  #expect_equal(typeof(ndata$s), "character") # error on x86_64-w64-mingw32 (64-bit)
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "integer")
  expect_equal(typeof(ndata$l), "integer")
  
  # convert all to float
  vd <- list(dataType = rep("f", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "double")
  expect_equal(typeof(ndata$d), "double")
  #expect_equal(typeof(ndata$s), "character") # error on x86_64-w64-mingw32 (64-bit)
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "double")
  expect_equal(typeof(ndata$l), "double")
  
  # convert all to bool
  vd <- list(dataType = rep("b", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "integer")
  expect_equal(typeof(ndata$d), "double")
  #expect_equal(typeof(ndata$s), "character") # error on x86_64-w64-mingw32 (64-bit)
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "logical")
  expect_equal(typeof(ndata$l), "logical")
})

# doi conversion ----
test_that("doi conversion", {
  cb <- codebook(cars, doi = "test", return = "list")
  expect_equal(cb$identifier, "https://doi.org/test")
  
  cb <- codebook(cars, doi = "doi: test", return = "list")
  expect_equal(cb$identifier, "https://doi.org/test")
  
  cb <- codebook(cars, doi = "https://doi.org/test", return = "list")
  expect_equal(cb$identifier, "https://doi.org/test")
  expect_true(is.null(cb$doi))
})

# from design ----
test_that("from design", {
  within <- list(time = c(day = "Daytime", night = "Nighttime"),
              wave = c('1' = "First", '2' = "Second"))
  between <- list(pet = c(cat = "Cats", dog = "Dogs"))
  vardesc <- list(description = list(time = "Time of Day",
                                     wave = "Wave of Study",
                                     pet = "Type of Pet"))
  data <- sim_design(within, between, 10, id = c(id = "ID"), plot = FALSE)
  cb <- codebook(data, vardesc = vardesc, return = "list")
  
  names <- c("id", "pet", "day_1", "day_2", "night_1", "night_2")
  descs <- c("ID", "Type of Pet", "Daytime First", "Daytime Second", 
             "Nighttime First", "Nighttime Second")
             
  cb_names <- sapply(cb$variableMeasured, `[[`, "name")
  cb_descs <- sapply(cb$variableMeasured, `[[`, "description")
  
  expect_equal(names, cb_names)
  expect_equal(descs, cb_descs)
  
  # long
  data <- sim_design(within, between, 10, , id = c(id = "ID"), 
                     dv = c(y = "Score"), long = TRUE)
  cb <- codebook(data, vardesc = vardesc, return = "list")
  
  names <- c("id", "pet", "time", "wave", "y")
  descs <- c("ID", "Type of Pet", "Time of Day", "Wave of Study", "Score")
  
  cb_names <- sapply(cb$variableMeasured, `[[`, "name")
  cb_descs <- sapply(cb$variableMeasured, `[[`, "description")
  
  expect_equal(names, cb_names)
  expect_equal(descs, cb_descs)
})

# vardesc ----
test_that("vardesc", {
  # check named and unnamed values, single values, partial named
  vd <- list(description = list(speed = "Speed (mph)", 
                                dist = "Stopping Distance (ft)"),
             dataType = "integer",
             minValue = c(0, 1),
             maxValue = c(speed = 25))
  
  cb <- codebook(cars, vardesc = vd, return = "list")
  
  s <- cb$variableMeasured[[1]]
  expect_equal(s$description, "Speed (mph)")
  expect_equal(s$dataType, "int")
  expect_equal(s$minValue, 0)
  expect_equal(s$maxValue, 25)
  
  d <- cb$variableMeasured[[2]]
  expect_equal(d$description, "Stopping Distance (ft)")
  expect_equal(d$dataType, "int")
  expect_equal(d$minValue, 1)
  expect_equal(d$maxValue, NULL)
})

# interactive ----
test_that("interactive", {
  data <- data.frame(x = rnorm(10))
  f <- file()
  write("\nxx", f)
  faux_options(connection = f)
  on.exit({
    faux_options(connection = stdin()) # reset connection
    close(f) # close the file
  })
  
  ol <- capture_output_lines(
    cb <- codebook(data, interactive = TRUE, return = "list")
  )
  
  x <- cb$variableMeasured[[1]]
  expect_equal(x$description, "xx")
  expect_equal(x$dataType[[1]], "float")
})
