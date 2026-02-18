#Test that rcatfish_classification returns proper information


test_that("Test that rcatfish_classification returns proper groupings",{
  classification.hit <-try(rcatfish_classification())
  if ("try-error"%in%class(classification.hit)) {
    skip("could not connect to remote database")
    }else{
      expect_equal(classification.hit$CommonName[which(classification.hit$Family == "Lamnidae")], "mackerel sharks")
      expect_equal(classification.hit$Author[which(classification.hit$Family == "Lamnidae")], "Bonaparte 1835")
      expect_equal(classification.hit$Class[which(classification.hit$Family == "Lamnidae")], "Elasmobranchii")
    }
  }
)
