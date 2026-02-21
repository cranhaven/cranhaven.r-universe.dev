test_that("Check notification works", {
  expect_snapshot(vascr_notify("success", "yay"))
  expect_snapshot(vascr_notify("info", "Test1"))  
  expect_snapshot(vascr_notify("warning", "TEST"))  
  expect_error(vascr_notify("error", "ERROR"))  

})
