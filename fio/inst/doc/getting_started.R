## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  out.width = "70%"
)

set.seed(123)

## ----addin, fig.cap="Import input-output data addin", echo=FALSE--------------
knitr::include_graphics("img/addin_1.png")

## ----new----------------------------------------------------------------------
# Create a new iom object
my_iom <- fio::iom$new(
  id = "my_iom",
  intermediate_transactions = matrix(as.numeric(sample(100:1000, 25)), nrow = 5),
  total_production = matrix(as.numeric(sample(5000:10000, 5)), nrow = 1)
)

print(my_iom)

## ----tech_coeff---------------------------------------------------------------
# Compute technical coefficients matrix
my_iom$compute_tech_coeff()

print(my_iom)

## ----access-------------------------------------------------------------------
# Access slots
print(my_iom$technical_coefficients_matrix)

## ----chain--------------------------------------------------------------------
# Chain methods
my_iom$compute_tech_coeff()$compute_leontief_inverse()$compute_key_sectors()

print(my_iom$key_sectors)

