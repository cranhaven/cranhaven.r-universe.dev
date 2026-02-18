#Test that rcatfish_glossary returns proper definitions


test_that("Test that rcatfish_glossary returns proper definitions",{
  glossary.hit <-try(rcatfish_glossary())
  if ("try-error"%in%class(glossary.hit)) {
    skip("could not connect to remote database")
    }else{
      expect_equal(glossary.hit$Definition[3], "An Article of the Code of Zoological Nomenclature.")
    }
  }
)
