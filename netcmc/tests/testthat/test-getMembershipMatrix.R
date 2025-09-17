context("getMembershipMatrix")

test_that("test getMembershipMatrix produces an error if the parameter inputs don't have the same number of rows.", {
  
  individualID = data.frame(c(1, 2, 3, 4, 5))
  alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(2, 1, 4, 3))
  expect_error(getMembershipMatrix(individualID, alters), "Error: The rows of the parameter inputs differ! They should all have the same number of rows!")
  
})

test_that("test getMembershipMatrix replaces an alter with NA if the alter is not in the set indivdualID.", {
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(5, 3, 2), c(5, 6, 1))
  
  alters = getMembershipMatrix(individualID, alters)$alters
  actualAlters = data.frame(matrix(rbind(c(NA, NA), c(3, NA), c(2, 1)), 3, 2, dimnames = list(c(), c("c.5..3..2.", "c.5..6..1."))))
  
  expect_equal(alters, actualAlters)
  
})

test_that("test getMembershipMatrix can return the expected membership matrix.", {
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, 2), c(NA, NA, 1))
  
  membershipMatrix = getMembershipMatrix(individualID, alters)$membershipMatrix
  actualMembershipMatrix = data.frame(matrix(rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 1)), 3, 3, dimnames = list(c(), c("Alter3", "Alter2", "Alter1"))))
  
  expect_equal(membershipMatrix, actualMembershipMatrix)
  
})


test_that("test getMembershipMatrix can return the expected membership matrix. Second version.", {
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, NA), c(NA, NA, 1))
  
  membershipMatrix = getMembershipMatrix(individualID, alters)$membershipMatrix
  actualMembershipMatrix = data.frame(matrix(rbind(c(0, 0), c(1, 0), c(0, 1)), 3, 2, dimnames = list(c(), c("Alter3", "Alter1"))))
  
  expect_equal(membershipMatrix, actualMembershipMatrix)
  
})


test_that("test getMembershipMatrix can return the expected membership matrix. Third version.", {
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, NA), c(6, NA, 1))
  
  membershipMatrix = getMembershipMatrix(individualID, alters)$membershipMatrix
  actualMembershipMatrix = data.frame(matrix(rbind(c(0, 0), c(1, 0), c(0, 1)), 3, 2, dimnames = list(c(), c("Alter3", "Alter1"))))
  
  expect_equal(membershipMatrix, actualMembershipMatrix)
  
})


test_that("test getMembershipMatrix can return the expected row normalized membership matrix.", {
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, 2), c(NA, NA, 1))
  
  rowNormalizedMembershipMatrix = getMembershipMatrix(individualID, alters)$rowNormalizedMembershipMatrix
  actualRowNormalizedMembershipMatrix = data.frame(matrix(rbind(c(0, 0, 0), c(1, 0, 0), c(0, 0.5, 0.5)), 3, 3, dimnames = list(c(), c("Alter3", "Alter2", "Alter1"))))
  
  expect_equal(rowNormalizedMembershipMatrix, actualRowNormalizedMembershipMatrix)
  
})


test_that("test getMembershipMatrix can return the expected row normalized membership matrix. Second version.", {
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, NA), c(NA, NA, 1))
  
  rowNormalizedMembershipMatrix = getMembershipMatrix(individualID, alters)$rowNormalizedMembershipMatrix
  actualRowNormalizedMembershipMatrix = data.frame(matrix(rbind(c(0, 0), c(1, 0), c(0, 1)), 3, 2, dimnames = list(c(), c("Alter3", "Alter1"))))
  
  expect_equal(rowNormalizedMembershipMatrix, actualRowNormalizedMembershipMatrix)
  
})
