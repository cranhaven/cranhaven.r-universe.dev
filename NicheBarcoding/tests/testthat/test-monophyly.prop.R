# Test Start ###
test_that("class testing on output",{
  library(ape)
  tree<-ape::rtree(20)
  tree$tip.label<-sample(tree$tip.label[1:10],size=20,replace = TRUE)
  sppVector<-tree$tip.label

  expect_equal(length(monophyly.prop(tree,sppVector,singletonsMono=F)),4)
})
