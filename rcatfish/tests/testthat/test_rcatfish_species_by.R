#Test that rcatfish_species_by returns proper information


test_that("Test that rcatfish_species_by returns proper information",{
  species.hit <-try(rcatfish_species_by(c("Ceratodontiformes", "Coelacanthiformes")))
  if ("try-error"%in%class(species.hit)) {
    skip("could not connect to remote database")
    }else{
      expect_identical(species.hit$Ceratodontiformes$Valid.Genera[1], 3)#Check names are correct
      expect_equal(species.hit$Coelacanthiformes$Valid.Species[1], 2)#Check it say update and has month and year
    }
  }
)
