#Test that rcatfish_collections returns proper collection information.


test_that("Test that rcatfish_collections returns proper collection information",{
  GetCollection <-try(rcatfish_collections(query = NULL, abbreviation = "UMMZ", country = NULL))
  if ("try-error"%in%class(GetCollection)) {
    skip("could not connect to remote database")
  }else{
    expect_equal(unlist(GetCollection$Name), "University of Michigan Museum of Zoology, Ann Arbor, Michigan, U.S.A.")
    expect_equal(unlist(GetCollection$Country), "U.S.A.")
  }
})
