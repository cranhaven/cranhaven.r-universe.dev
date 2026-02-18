#Test that rcatfish_updates returns proper information


test_that("Test that rcatfish_updates returns proper definitions",{
  updates.hit <-try(rcatfish_updates())
  if ("try-error"%in%class(updates.hit)) {
    skip("could not connect to remote database")
    }else{
      expect_contains(names(updates.hit), c("UpdateDate", "Changes", "AuthorshipChanges","AddedGenera", "AddedSpecies"))#Check names are correct
      expect_equal(grep("Updated on \\d{1,2} [a-zA-Z]+[[:space:]]\\d{4}", updates.hit$UpdateDate), 1)#Check it say update and has month and year
    }
  }
)
