## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(SafeMapper)

## -----------------------------------------------------------------------------
# Basic usage: returns a list
squares <- s_map(1:5, function(x) x^2)
print(squares)

# Using formula syntax (recommended)
squares <- s_map(1:5, ~ .x^2)
print(unlist(squares))

## -----------------------------------------------------------------------------
# Convert numbers to formatted strings
labels <- s_map_chr(1:5, ~ paste0("Item_", .x))
print(labels)

# Extract character elements from lists
persons <- list(
  list(name = "Alice", age = 25),
  list(name = "Bob", age = 30),
  list(name = "Carol", age = 28)
)
names <- s_map_chr(persons, ~ .x$name)
print(names)

## -----------------------------------------------------------------------------
# Calculate square roots
roots <- s_map_dbl(c(1, 4, 9, 16, 25), sqrt)
print(roots)

# Statistical calculations
datasets <- list(
  a = rnorm(100, mean = 0),
  b = rnorm(100, mean = 5),
  c = rnorm(100, mean = 10)
)
means <- s_map_dbl(datasets, mean)
print(means)

## -----------------------------------------------------------------------------
# Calculate string lengths
words <- c("apple", "banana", "cherry")
lengths <- s_map_int(words, nchar)
print(lengths)

# Count list elements
nested <- list(1:3, 1:5, 1:10)
counts <- s_map_int(nested, length)
print(counts)

## -----------------------------------------------------------------------------
# Check conditions
numbers <- c(2, 5, 8, 12, 15)
is_even <- s_map_lgl(numbers, ~ .x %% 2 == 0)
print(is_even)

# Check for null values
data <- list(a = 1, b = NULL, c = 3, d = NULL)
has_value <- s_map_lgl(data, ~ !is.null(.x))
print(has_value)

## -----------------------------------------------------------------------------
# Create multiple data frames and row-bind
create_record <- function(id) {
  data.frame(
    id = id,
    value = rnorm(1),
    timestamp = Sys.time()
  )
}

records <- s_map_dfr(1:5, create_record)
print(records)

## -----------------------------------------------------------------------------
# Create multiple columns and column-bind
create_column <- function(name) {
  df <- data.frame(x = rnorm(3))
  names(df) <- name
  df
}

columns <- s_map_dfc(c("A", "B", "C"), create_column)
print(columns)

## -----------------------------------------------------------------------------
# Process two vectors simultaneously
x <- 1:5
y <- 10:14

# Calculate sums
sums <- s_map2(x, y, ~ .x + .y)
print(unlist(sums))

# Calculate products
products <- s_map2_dbl(x, y, ~ .x * .y)
print(products)

## -----------------------------------------------------------------------------
# Batch create file paths
dirs <- c("data", "output", "logs")
files <- c("results.csv", "summary.txt", "debug.log")

paths <- s_map2_chr(dirs, files, ~ file.path(.x, .y))
print(paths)

# Compare two vectors
a <- c(1, 5, 3, 8, 2)
b <- c(2, 3, 5, 6, 4)

comparison <- s_map2_chr(a, b, function(x, y) {
  if (x > y) "greater"
  else if (x < y) "less"
  else "equal"
})
print(comparison)

## -----------------------------------------------------------------------------
# Use named parameter list
params <- list(
  x = 1:3,
  y = 4:6,
  z = 7:9
)

# Calculate x + y * z
results <- s_pmap(params, function(x, y, z) x + y * z)
print(unlist(results))

## -----------------------------------------------------------------------------
# Iterate over data frame rows
df <- data.frame(
  name = c("Alice", "Bob", "Carol"),
  age = c(25, 30, 28),
  city = c("New York", "London", "Tokyo")
)

# Create description for each row
descriptions <- s_pmap(df, function(name, age, city) {
  sprintf("%s is %d years old and lives in %s", name, age, city)
})
print(unlist(descriptions))

## -----------------------------------------------------------------------------
# Use named vector
scores <- c(math = 85, english = 92, science = 78)

report <- s_imap(scores, ~ sprintf("%s: %d points", .y, .x))
print(unlist(report))

# Use unnamed vector (get position index)
items <- c("apple", "banana", "cherry")

indexed <- s_imap(items, ~ sprintf("[%s] %s", .y, .x))
print(unlist(indexed))

## -----------------------------------------------------------------------------
# Print output
s_walk(1:3, ~ cat("Processing item", .x, "\n"))

# Can be used in pipelines
result <- 1:5 |>
  s_walk(~ cat("Checking", .x, "...\n")) |>
  # s_walk returns original input, can continue processing

sum()

print(paste("Sum:", result))

## -----------------------------------------------------------------------------
# Batch write (example: print instead of actual write)
files <- c("file1.txt", "file2.txt", "file3.txt")
contents <- c("Content A", "Content B", "Content C")

s_walk2(files, contents, function(f, c) {
  cat(sprintf("Would write to %s: '%s'\n", f, c))
})

## -----------------------------------------------------------------------------
# Pass additional arguments to function
data <- list(
  c(1, 2, NA, 4),
  c(5, NA, 7, 8),
  c(9, 10, 11, NA)
)

# Pass na.rm = TRUE to mean function
means <- s_map_dbl(data, mean, na.rm = TRUE)
print(means)

# Multiple additional arguments
rounded <- s_map_dbl(c(1.234, 2.567, 3.891), round, digits = 1)
print(rounded)

## -----------------------------------------------------------------------------
# Auto-generate session ID (recommended for most scenarios)
result1 <- s_map(1:10, ~ .x^2)

# Manually specify session ID (for precise control scenarios)
result2 <- s_map(1:10, ~ .x^2, .session_id = "my_calculation_v1")

# Different session IDs don't affect each other
result3 <- s_map(1:10, ~ .x^2, .session_id = "my_calculation_v2")

## -----------------------------------------------------------------------------
# Use pipeline for data transformation
data <- 1:10

result <- data |>
  s_map_dbl(~ .x^2) |>     # Square
  s_map_dbl(~ .x + 10) |>  # Add 10
  s_map_chr(~ sprintf("%.1f", .x))  # Format

print(result)

## -----------------------------------------------------------------------------
# Execute different operations based on conditions
values <- c(-2, 0, 3, -5, 7)

processed <- s_map_dbl(values, function(x) {
  if (x < 0) abs(x) * 2
  else x^2
})
print(processed)

## -----------------------------------------------------------------------------
# Handle data that might contain NULLs
data <- list(a = 1, b = NULL, c = 3, d = NULL, e = 5)

# Use s_map with conditional check
safe_values <- s_map_dbl(data, ~ if (is.null(.x)) NA_real_ else .x)
print(safe_values)

