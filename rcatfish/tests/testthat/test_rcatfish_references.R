#Test that rcatfish_references returns proper reference information


test_that("rcatfish_references returns proper information",{
  reference.hit <-try(rcatfish_references(query = "Heroina", type = "keyword"))
  if ("try-error"%in%class(reference.hit)) {
    skip("could not connect to remote database")
    }else{
      expect_identical(as.numeric(reference.hit$RefNo), 22574)
    }
  reference.hit2 <-try(rcatfish_references(query = "2541", type = "RefNo"))
  if ("try-error"%in%class(reference.hit2)) {
    skip("could not connect to remote database")
    }else{
      expect_identical(as.numeric(grep(pattern = "A review of the Sparidae and related families of perch-like fishes found in the waters of Japan", reference.hit2$Reference)), 1)
    }
  }
)
