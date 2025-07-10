context("SCRIPsimu")

# sub_expre_data <- data(acinar.data)
# params <- data(params_acinar)
data(acinar.data)
data(params_acinar)

test_that("SCRIPSimulate output is valid", {
  SCRIPsimu(data=acinar.data, params=params_acinar)
 })

