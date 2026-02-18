#Test that rcatfish_journals returns proper journal information


test_that("Test that rcatfish_journals returns proper journal information",{
  journal.hit <-try(rcatfish_journals(query = "Buntbarsche"))
  if ("try-error"%in%class(journal.hit)) {
    skip("could not connect to remote database")
    }else{
      expect_identical(as.numeric(grep("American Cichlid Association; 1966 up to the present / Journal of the American cichlid Association.",as.vector(journal.hit$Journals))), 1)
    }
  }
)
