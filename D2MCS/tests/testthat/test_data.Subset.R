testthat::test_that("Subset: initialize function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  testthat::expect_is(Subset$new(dataset = corpus,
                                 class.index = 50,
                                 class.values = factor(corpus[[50]]),
                                 positive.class = 1),
                      "Subset")

  testthat::expect_is(Subset$new(dataset = corpus),
                      "Subset")
})

testthat::test_that("Subset: initialize function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  testthat::expect_error(Subset$new(dataset = NULL,
                                    class.index = 50,
                                    class.values = factor(corpus[[50]]),
                                    positive.class = 1),
                         "[Subset][FATAL] Dataset empty or incorrect (must be a data.frame). Aborting...",
                         fixed = TRUE)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  testthat::expect_error(Subset$new(dataset = corpus,
                                    class.index = "a",
                                    class.values = factor(corpus[[50]]),
                                    positive.class = 1),
                         "[Subset][FATAL] Class index parameter is incorrect. Must be between 1 and 50. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Subset$new(dataset = corpus,
                                    class.index = 50,
                                    class.values = 3,
                                    positive.class = 1),
                         "[Subset][FATAL] Class values parameter must be defined as 'factor' type. Aborting...",
                         fixed = TRUE)

  set.seed(123)
  testthat::expect_error(Subset$new(dataset = corpus,
                                    class.index = 50,
                                    class.values = factor(sample(c(0, 1), nrow(corpus), replace = TRUE)),
                                    positive.class = 1),
                         "[Subset][FATAL] Class values parameter is incorrect. Must match with the values in column 50 in the dataset. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Subset$new(dataset = corpus,
                                    class.index = 50,
                                    class.values = factor(corpus[[50]]),
                                    positive.class = 2),
                         "[Subset][FATAL] Positive Class parameter is incorrect. Must be '0' '1'. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("Subset: getColumnNames function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1)

  testthat::expect_equal(subset$getColumnNames(), names(corpus[, -50]))
})

testthat::test_that("Subset: getFeatures function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                      what = "character", quiet = TRUE),
                                 split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1)

  testthat::expect_true(all(subset$getFeatures(feature.names = NULL) == corpus[, -50]))

  testthat::expect_equal(subset$getFeatures(feature.names = "Gender"), corpus["Gender"])

  subset <- Subset$new(dataset = corpus,
                       class.index = NULL,
                       class.values = NULL,
                       positive.class = NULL)

  testthat::expect_true(all(subset$getFeatures(feature.names = NULL) == subset$.__enclos_env__$private$data))

  testthat::expect_equal(subset$getFeatures(feature.names = "Gender"), corpus[["Gender"]])
})

testthat::test_that("Subset: getID function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$getID(), NULL)

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = 2)

  testthat::expect_equal(subset$getID(), names(corpus)[2])
})

testthat::test_that("Subset: getIterator function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_message(subset$getIterator(chunk.size = "wrong",
                                              verbose = FALSE),
                           "[Subset][WARNING] Chunk size is not valid. Assuming default value",
                           fixed = TRUE)

  testthat::expect_message(subset$getIterator(chunk.size = 10000,
                                              verbose = "a"),
                           "[Subset][WARNING] Verbose type is not valid. Assuming 'FALSE' as default value",
                           fixed = TRUE)

  testthat::expect_is(subset$getIterator(chunk.size = 10000,
                                              verbose = TRUE),
                      "DIterator")
})

testthat::test_that("Subset: getClassValues function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_true(all(subset$getClassValues() == factor(corpus[[50]])))
})

testthat::test_that("Subset: getClassBalance function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$getClassBalance(target.value = NULL), 1)
  testthat::expect_message(subset$getClassBalance(target.value = 2),
                           "[Subset][WARNING] Target class not found. Assuming default '1' value",
                           fixed = TRUE)

  subset <- Subset$new(dataset = corpus,
                       class.index = NULL,
                       class.values = NULL,
                       positive.class = NULL,
                       feature.id = NULL)

  testthat::expect_message(subset$getClassBalance(target.value = 2),
                           "[Subset][WARNING] Subset has no associated class. Task not performed",
                           fixed = TRUE)
})

testthat::test_that("Subset: getClassIndex function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$getClassIndex(), 50)
})

testthat::test_that("Subset: getClassName function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$getClassName(), "Class")
})

testthat::test_that("Subset: getNcol function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$getNcol(), 50)
})

testthat::test_that("Subset: getNrow function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$getNrow(), 202)
})

testthat::test_that("Subset: getPositiveClass function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$getPositiveClass(), 1)
})

testthat::test_that("Subset: isBlinded function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = factor(corpus[[50]]),
                       positive.class = 1,
                       feature.id = NULL)

  testthat::expect_equal(subset$isBlinded(), FALSE)
})
