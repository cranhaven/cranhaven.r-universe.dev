N <- 500    # Number of observations
n <- 10      # Dimension of multivariate normal random variables
pi <- 0.5   # Probability threshold
rho <- 0.1  # Dispersion parameter

# Generate overdispersed binomial variables
New_overdispersed_data <- GenerateBOD(N, n, pi, rho)

context_start_file("Checking the GenerateBOD function")
test_that("simulated BOD data length",{
  expect_equal(length(New_overdispersed_data),N)
})
test_that("generated binomial random variable",{
  expect_equal(sort(unique(New_overdispersed_data)),0:n)
})
test_that("class of the output",{
  expect_equal(class(New_overdispersed_data),"numeric")
})
test_that("minimum value in binomial random variable",{
  expect_lte(min(New_overdispersed_data),0)
})
test_that("maximum value in binomial random variable",{
  expect_gte(max(New_overdispersed_data),n)
})

test_that("Check for Nan values",{
  expect_error(GenerateBOD(N, n, pi, NA),"NA or Infinite or NAN values in the N,n,pi or rho")
})
test_that("inputs greater than length of one",{
  expect_error(GenerateBOD(N, n, c(pi,0.1), rho),"N, n, pi or rho has a value greater than length one")
})
test_that("input values for N or n",{
  expect_error(GenerateBOD(N,-10,pi,rho),"N or n cannot be negative values")
})
test_that("input value for pi",{
  expect_error(GenerateBOD(N,n,pi-1,rho),"probability of success cannot be greater than or equal to one \nor less than or equal to zero")
})
test_that("input value for overdispersion parameter",{
  expect_error(GenerateBOD(N,n,pi,rho-1),"dispersion parameter rho cannot be less than zero")
})
