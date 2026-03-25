# Tests for twfeiv_decomp()
# We test whether twfeiv_decomp() deals with the errors in the data properly

# Unbalanced Panel
test_that("twfeiv_decomp() errors on unbalanced panels", {
  data_unbalanced <- data.frame(
  id        = c(rep(1:3, each = 6), rep(4, 5)),   
  time      = c(rep(2000:2005, times = 3), 2000:2004),
  instrument= c(
    0,0,1,1,1,1,   
    0,1,1,1,1,1,   
    0,0,0,0,0,1,   
    0,1,1,1,1      
  ),
  education = c(
    10,11,12,13,14,15,
    9,10,11,12,13,14,
    8,9,10,11,12,13,
    7,8,9,10,11
  ),
  wage      = c(
    20,21,22,23,24,25,
    18,19,20,21,22,23,
    16,17,18,19,20,21,
    14,15,16,17,18
  )
)

expect_error(twfeiv_decomp(wage ~ education|instrument,
                     data = data_unbalanced,
                     id_var = "id",
                     time_var = "time"), "The dataset is an unbalanced panel")
})

# NA observations without controls
test_that("twfeiv_decomp() errors when NA in data (without controls)", {
  data_NA_without_controls <- data.frame(
  id        = rep(1:4, each = 6),
  time      = rep(2000:2005, times = 4),
  instrument= c(0,0,1,1,1,1,  0,1,1,1,1,1,  0,0,0,0,0,1,  0,0,0,1,1,1),
  education = c(10,11,12,13,14,15,
                9,10,11,12,13,14,
                8,9,10,11,12,13,
                7,8,9,10,11,NA),
  wage      = c(20,21,22,23,24,25,
                18,19,20,21,22,23,
                16,17,18,19,20,21,
                14,15,16,17,18,19)
)

expect_error(twfeiv_decomp(wage ~ education|instrument,
                     data = data_NA_without_controls,
                     id_var = "id",
                     time_var = "time"), "NA observations are detected in the data")
})

# NA observations with controls
test_that("twfeiv_decomp() errors when NA in data (with controls)", {
  data_NA_with_controls <- data.frame(
  id        = rep(1:4, each = 6),
  time      = rep(2000:2005, times = 4),
  instrument= c(0,0,1,1,1,1,  0,1,1,1,1,1,  0,0,0,0,0,1,  0,0,0,1,1,1),
  education = c(10,11,12,13,14,15,
                9,10,11,12,13,14,
                8,9,10,11,12,13,
                7,8,9,10,11,12),
  wage      = c(20,21,22,23,24,25,
                18,19,20,21,22,23,
                16,17,18,19,20,21,
                14,15,16,17,18,19),
  age       = c(
      30:35,   
      28:33,   
      25:30,   
      40,41,42,NA,44,45
  )
)

expect_error(twfeiv_decomp(wage ~ education + age|age + instrument,
                     data = data_NA_with_controls,
                     id_var = "id",
                     time_var = "time"), "NA observations are detected in the data")
})

# Non-staggered instrument
test_that("twfeiv_decomp() errors when the instrument assignment is non-staggered", {
  data_non_staggered_instrument <- data.frame(
  id        = rep(1:4, each = 6),
  time      = rep(2000:2005, times = 4),
  instrument= c(0,0,1,1,0,1,  0,1,1,1,1,1,  0,0,0,0,0,1,  0,0,0,1,1,1),
  education = c(10,11,12,13,14,15,
                9,10,11,12,13,14,
                8,9,10,11,12,13,
                7,8,9,10,11,12),
  wage      = c(20,21,22,23,24,25,
                18,19,20,21,22,23,
                16,17,18,19,20,21,
                14,15,16,17,18,19)
)

expect_error(twfeiv_decomp(wage ~ education|instrument,
                     data = data_non_staggered_instrument,
                     id_var = "id",
                     time_var = "time"), "The instrument assignment is non-staggered")
})  