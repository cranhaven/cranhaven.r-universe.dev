context("getTotalAltersByStatus")

test_that("test getTotalAltersByStatus produces an error if individualID or status has multiple columns.", {
  
  individualID = data.frame(c(1, 2, 3), c(1, 2, 3))
  status = data.frame(c(1, 2, 3), c(1, 2, 3))
  alters = data.frame(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3))
  
  expect_error(getTotalAltersByStatus(individualID, status, alters), "Error: individualID and status must only have one column")
  expect_error(getTotalAltersByStatus(individualID, status, alters), "Error: individualID and status must only have one column")
  
})

test_that("test getTotalAltersByStatus produces an error if the parameter inputs don't have the same number of rows.", {
  
  individualID = data.frame(c(1, 2, 3, 4))
  status = data.frame(c(1, 2, 3))
  alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(2, 1, 4, 3))
  expect_error(getTotalAltersByStatus(individualID, status, alters), "Error: The rows of the parameter inputs differ! They should all have the same number of rows!")
  
  individualID = data.frame(c(1, 2, 3, 4))
  status = data.frame(c(1, 2, 3, 4))
  alters = data.frame(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3))
  expect_error(getTotalAltersByStatus(individualID, status, alters), "Error: The rows of the parameter inputs differ! They should all have the same number of rows!")

  individualID = data.frame(c(1, 2, 3))
  status = data.frame(c(1, 2, 3, 4))
  alters = data.frame(c(1, 2, 3, 4), c(1, 2, 3, 4), c(1, 2, 3, 4))
  expect_error(getTotalAltersByStatus(individualID, status, alters), "Error: The rows of the parameter inputs differ! They should all have the same number of rows!")

})

test_that("test getTotalAltersByStatus can properly create the data.frame to store information.", {
  
  individualID = data.frame(c(1, 2, 3, 4))
  status = data.frame(c(10, 20, 30, 20))
  alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(2, 1, 4, 3))
  
  totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)
  
  expect_equal(nrow(totalAltersByStatus), 4)
  expect_equal(colnames(totalAltersByStatus), c("numberOfAltersWithStatus10", "numberOfAltersWithStatus20", "numberOfAltersWithStatus30", "numberOfAltersWithStatusNA"))
})

test_that("test getTotalAltersByStatus can return the correct data.frame when there are no nonnominators.", {
  
  individualID = data.frame(c(1, 2, 3, 4))
  status = data.frame(c(10, 20, 30, 20))
  alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(2, 1, 4, 3))
  
  totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)
  actualTotalAltersByStatus = data.frame(matrix(rbind(c(0, 2, 1, 0), c(1, 1, 1, 0), c(1, 2, 0, 0), c(1, 1, 1, 0)), 4, 4, dimnames = list(c(), c("numberOfAltersWithStatus10", "numberOfAltersWithStatus20", "numberOfAltersWithStatus30", "numberOfAltersWithStatusNA"))))
  
  expect_equal(totalAltersByStatus, actualTotalAltersByStatus)
})

test_that("test getTotalAltersByStatus can return the correct data.frame when there are nonnominators. We will need to utilize the NA column, as the status of nonnominators are unknown i.e. NA.", {
  
  individualID = data.frame(c(1, 2, 3, 4))
  status = data.frame(c("RegularSmoke", "Nonsmoker", "CasualSmoker", "Nonsmoker"))
  alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(5, 1, 5, 3))
  
  totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)
  actualTotalAltersByStatus = data.frame(matrix(rbind(c(0, 1, 1, 1), c(1, 1, 1, 0), c(1, 1, 0, 1), c(1, 1, 1, 0)), 4, 4, dimnames = list(c(), c("numberOfAltersWithStatusRegularSmoke", "numberOfAltersWithStatusNonsmoker", "numberOfAltersWithStatusCasualSmoker", "numberOfAltersWithStatusNA"))))
  
  expect_equal(totalAltersByStatus, actualTotalAltersByStatus)
})

test_that("test getTotalAltersByStatus can return the correct data.frame when some of the values of status are NA.", {
  
  individualID = data.frame(c(1, 2, 3, 4))
  status = data.frame(c(NA, "Nonsmoker", "CasualSmoker", "Nonsmoker"))
  alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(5, 1, 5, 3))
  
  totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)
  actualTotalAltersByStatus = data.frame(matrix(rbind(c(1, 1, 1), c(1, 1, 1), c(1, 0, 2), c(1, 1, 1)), 4, 3, dimnames = list(c(), c("numberOfAltersWithStatusNonsmoker", "numberOfAltersWithStatusCasualSmoker", "numberOfAltersWithStatusNA"))))
  
  expect_equal(totalAltersByStatus, actualTotalAltersByStatus)
})

test_that("test getTotalAltersByStatus can return the correct data.frame when the alters are NA if the individual chooses to nominate less than the amount of nominees.", {
  
  individualID = data.frame(c(10, 20))
  status = data.frame(c(NA, "Nonsmoker"))
  alters = data.frame(c(NA, 10), c(20, NA))
  
  totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)
  actualTotalAltersByStatus = data.frame(matrix(rbind(c(1, 0), c(0, 1)), 2, 2, dimnames = list(c(), c("numberOfAltersWithStatusNonsmoker", "numberOfAltersWithStatusNA"))))
  
  expect_equal(totalAltersByStatus, actualTotalAltersByStatus)
})

test_that("test getTotalAltersByStatus can return the correct data.frame when the alters are NA if the individual chooses to nominate less than the amount of nominees and one of the individualIDs takes the value of NA.", {
  
  individualID = data.frame(c(NA, 20))
  status = data.frame(c("Smoker", "Nonsmoker"))
  alters = data.frame(c(NA, 10), c(20, NA))
  
  totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)
  actualTotalAltersByStatus = data.frame(matrix(rbind(c(0, 1, 0), c(0, 0, 1)), 2, 3, dimnames = list(c(), c("numberOfAltersWithStatusSmoker", "numberOfAltersWithStatusNonsmoker", "numberOfAltersWithStatusNA"))))
  
  expect_equal(totalAltersByStatus, actualTotalAltersByStatus)
})
