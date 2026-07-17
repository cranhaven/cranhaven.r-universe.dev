# Generate test file
data <- "Line with additional information

   1  2  3  4  5  6  7  8  9 10 11 12
A  1  2  3  4  5  6  7  8  9 10 11 12
B 13 14 15 16 17 18 19 20 21 22 23 24
C 25 26 27 28 29 30 31 32 33 34 35 36
D 37 38 39 40 41 42 43 44 45 46 47 48
E 49 50 51 52 53 54 55 56 57 58 59 60
F 61 62 63 64 65 66 67 68 69 70 71 72
G 73 74 75 76 77 78 79 80 81 82 83 84
H 85 86 87 88 89 90 91 92 93 94 95 96"
file_path <- tempfile()
writeLines(data, file_path, sep = "\n")

# Generate test data
m <- matrix(1:96, nrow = 8, byrow = TRUE)
row.names(m) <- LETTERS[1:8]
colnames(m) <- as.character(1:12)
expected_result <- data.frame(m, check.names = F)
attr(expected_result, "info") <- "Line with additional information"

test_that("ask_validity_method() correctly distinguishes valid from invalid options", {
  
  # Call function
  result <- read_plate(file_path)
  
  # Test
  expect_equal(result, expected_result)
})





