test_that("An example for a simulation study", {
  result=GDILM_SEIRS_Sim_Par_Est(5,5,8,30,0.7, 0.5, 1, 2.5, 0,30, 50,0.5,0.5, 5, 5, 10, 3)
  expect_type(result, "list")
})
