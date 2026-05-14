library(iCARH)
context("Pathways")

keggid = list(c("C08363", "C00712"), "Unk1", "C01241","C04308","Unk2","Unk3", "C00350", "C00350")

test_that("iCARH.getPathwaysMat gives correct pathways", {
  expect_match(names(iCARH.getPathwaysMat(keggid, "rno")), "path:rno00564")
  expect_match(names(iCARH.getPathwaysMat(keggid[-6], "rno")), "path:rno00564")
  expect_match(names(iCARH.getPathwaysMat(keggid[-1], "rno")), "path:rno00564")
  expect_equal(names(iCARH.getPathwaysMat(keggid[-c(7,8)], "rno")), "path:rno00564")
})

test_that("iCARH.getPathwaysMat gives correct matrices", {
  expect_equal(iCARH.getPathwaysMat(keggid, "rno")[[2]][3,4], 1)
  expect_equal(iCARH.getPathwaysMat(keggid, "rno")[[2]][7,8], 0)
  expect_equal(sum(is.infinite(iCARH.getPathwaysMat(keggid, "rno")[[2]][2,-2])), 7)
})

test_that("iCARH.getPathwaysMat deals with special cases", {
  expect_warning(iCARH.getPathwaysMat(c("C08363", "C00712"), "rno"))
  expect_equal(nrow(iCARH.getPathwaysMat(c("C01241","C04308"), "rno")[[1]]),2)
  expect_equal(nrow(iCARH.getPathwaysMat(list(c("C01241","C04308")), "rno")[[1]]),1)
})
