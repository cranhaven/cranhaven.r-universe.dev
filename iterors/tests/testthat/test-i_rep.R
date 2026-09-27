test_that("i_rep matches first example from base::rep", {
  it <- i_rep(1:4, 2)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_rep matches second example from base::rep", {
  it <- i_rep(1:4, each=2)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_rep matches fifth example from base::rep", {
  it <- i_rep(1:4, each=2, length.out=4)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_rep replicates a list and matches tenth example from base::rep", {
  # 8 integers plus two recycled 1's.
  fred <- list(happy=1:10, name="squash")
  it <- i_rep(fred, times=5)
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), 1:10)
  expect_equal(nextOr(it, NA), "squash")
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_rep replicates a factor and matches last example from base::rep", {
  # 8 integers plus two recycled 1's.
  x <- factor(LETTERS[1:4])
  it <- i_rep(x, 2)
  expect_equal(nextOr(it, NA), x[1])
  expect_equal(nextOr(it, NA), x[2])
  expect_equal(nextOr(it, NA), x[3])
  expect_equal(nextOr(it, NA), x[4])
  expect_equal(nextOr(it, NA), x[1])
  expect_equal(nextOr(it, NA), x[2])
  expect_equal(nextOr(it, NA), x[3])
  expect_equal(nextOr(it, NA), x[4])
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_rep_len works on numeric vectors", {
  it <- i_rep(1:4, length.out=3)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), NA)
})

# Related to Issue #33
test_that("i_rep matches base::rep() when both times and each args are given", {
  it <- i_rep(1:4, times=2, each=3)
  expected_vector <- rep(1:4, times=2, each=3)
  expect_equal(unlist(as.list(it)), expected_vector)
})

test_that("test01", {
    x <- as.list(rnorm(10))
    each <- 2
    actual <- as.list(i_rep(x, each = each))
    expected <- rep(x, each = each)
    expect_equal(expected, actual)
})

test_that("test02", {
    x <- as.list(rnorm(10))
    each <- 2
    actual <- as.list(i_rep(x, each = each))
    expected <- rep(x, each = each)
    expect_equal(expected, actual)
    each <- rep(2, length(x))
    actual <- as.list(i_rep(x, each = each))
    expected <- rep(x, times = each) #rep is different here
    expect_equal(expected, actual)
})

test_that("test03", {
    x <- as.list(rnorm(10))
    times <- 2
    actual <- as.list(i_rep(x, times))
    expected <- rep(x, times)
    expect_equal(expected, actual)
    times <- rep(2, length(x))
    actual <- as.list(i_rep(x, each=times)) #different from rep!
    expected <- rep(x, times)
    expect_equal(expected, actual)
})

test_that("test04", {
    x <- as.list(rnorm(10))
    length.out <- 33
    actual <- as.list(i_rep(x, length.out = length.out))
    expected <- rep(x, length.out = length.out)
    expect_equal(expected, actual)
})

test_that("test05", {
    x <- as.list(rnorm(10))
    length.out <- 33
    actual <- as.list(i_rep(x, each = 2, length.out = length.out))
    expected <- rep(x, each = 2, length.out = length.out)
    expect_equal(expected, actual)
})

test_that("test06", {
    x <- as.list(rnorm(10))
    times <- 2
    each <- 3
    actual <- as.list(i_rep(x, each = each, times = times))
    expected <- rep(x, each = each, times = times)
    expect_equal(expected, actual)
})

test_that("test07", {
    x <- as.list(rnorm(10))
    times <- 2
    each <- seq(length = length(x) * times)
    actual <- as.list(i_rep(x, each = each, times = times))
    #! i_rep different from rep here...
    expected <- rep(rep(x, times=times), times=each)
    expect_equal(expected, actual)
})

test_that("test08", {
    expect_error(i_rep(1:3, times = integer()))
})

test_that("test09", {
    expect_error(i_rep(1:3, times = -1))
})

test_that("test10", {
    expect_error(i_rep(1:3, times = c(1, 1, -1)))
})

test_that("test11", {
    x <- as.list(rnorm(10))
    actual <- as.list(i_rep(x))
    expected <- rep(x)
    expect_equal(expected, actual)
})

test_that("test12", {
    x <- list()
    times <- 100
    actual <- as.list(i_rep(x, times = times))
    expected <- rep(x, times = times)
    expect_equal(expected, actual)
    each <- 10
    actual <- as.list(i_rep(x, each = each))
    expected <- rep(x, each = each)
    expect_equal(expected, actual)
    actual <- as.list(i_rep(x, times = times, each = each))
    expected <- rep(x, times = times, each = each)
    expect_equal(expected, actual)
})

test_that("test13", {
    x <- list()
    times <- 100
    actual <- as.list(i_rep(iteror(x), times = times))
    expected <- rep(x, times = times)
    expect_equal(expected, actual)
    each <- 10
    actual <- as.list(i_rep(iteror(x), each = each))
    expected <- rep(x, each = each)
    expect_equal(expected, actual)
    actual <- as.list(i_rep(iteror(x), times = times, each = each))
    expected <- rep(x, times = times, each = each)
    expect_equal(expected, actual)
})
