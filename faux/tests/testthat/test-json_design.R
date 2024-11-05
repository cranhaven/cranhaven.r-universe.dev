context("test-json_design")

# defaults ----
test_that("defaults", {
  des <- check_design(mu = 1.123456789, plot = FALSE)
  json <- json_design(des)
  txt <- '{"within":[],"between":[],"dv":{"y":"value"},"id":{"id":"id"},"vardesc":[],"n":{"y":100},"mu":{"y":{"y":1.12345679}},"sd":{"y":{"y":1}},"r":[],"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
  
  des <- check_design(2,2, plot = FALSE)
  json <- json_design(des)
  txt <- '{"within":{"W1":{"W1a":"W1a","W1b":"W1b"}},"between":{"B1":{"B1a":"B1a","B1b":"B1b"}},"dv":{"y":"value"},"id":{"id":"id"},"vardesc":{"W1":"W1","B1":"B1"},"n":{"B1a":100,"B1b":100},"mu":{"B1a":{"W1a":0,"W1b":0},"B1b":{"W1a":0,"W1b":0}},"sd":{"B1a":{"W1a":1,"W1b":1},"B1b":{"W1a":1,"W1b":1}},"r":{"B1a":[[1,0],[0,1]],"B1b":[[1,0],[0,1]]},"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
})

# filename ----
test_that("filename", {
  testfile <- tempfile(fileext = ".json")
  des <- check_design(plot = FALSE)
  json <- json_design(des, testfile)
  des2 <- jsonlite::read_json(testfile)
  json2 <- jsonlite::toJSON(des2, auto_unbox = TRUE)
  des$params <- NULL
  expect_equivalent(des, des2)
  expect_equal(json, json2)
  
  file.remove(testfile)
  
  # no .json suffix
  testfile <- tempfile()
  testfile_json <- paste0(testfile, ".json")
  json <- json_design(des, testfile)
  des2 <- jsonlite::read_json(testfile_json)
  json2 <- jsonlite::toJSON(des2, auto_unbox = TRUE)
  des$params <- NULL
  expect_equivalent(des, des2)
  expect_equal(json, json2)
  
  file.remove(testfile_json)
})

# digits ----
test_that("digits", {
  des <- check_design(mu = 1.123456789, plot = FALSE)
  json <- json_design(des, digits = 3)
  txt <- '{"within":[],"between":[],"dv":{"y":"value"},"id":{"id":"id"},"vardesc":[],"n":{"y":100},"mu":{"y":{"y":1.123}},"sd":{"y":{"y":1}},"r":[],"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
  
  json <- json_design(des, digits = 4)
  txt <- '{"within":[],"between":[],"dv":{"y":"value"},"id":{"id":"id"},"vardesc":[],"n":{"y":100},"mu":{"y":{"y":1.1235}},"sd":{"y":{"y":1}},"r":[],"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
})

# pretty ----
test_that("pretty", {
  des <- check_design(plot = FALSE)
  json <- json_design(des, pretty = TRUE)
  txt <- '{
  "within": [],
  "between": [],
  "dv": {
    "y": "value"
  },
  "id": {
    "id": "id"
  },
  "vardesc": [],
  "n": {
    "y": 100
  },
  "mu": {
    "y": {
      "y": 0
    }
  },
  "sd": {
    "y": {
      "y": 1
    }
  },
  "r": [],
  "sep": "_"
}'
  
  class(txt) <- "json"
  expect_equal(json, txt)
})

