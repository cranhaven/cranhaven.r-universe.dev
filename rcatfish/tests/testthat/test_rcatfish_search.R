#Test that rcatfish_search returns proper information


test_that("rcatfish_search returns proper information",{
  if((.Platform$OS.type == "windows") & (grepl(pattern = "\\(OpenSSL", curl::curl_version()$ssl_version) == TRUE)){
    expect_error(rcatfish_search(query = "Cichla", type = "Species"), regexp = cat("openSSL backend for curl is required for the Windows version of this package, but it not detected as being active. Please see the vignette on how to configure curl with openSSL for this function to work. You can access the vignette with the following: vignette('rcatfish')."))
  }else{
    query.hit <-try(rcatfish_search("valid as Abactochromis", "Species", taxon.history = TRUE, phrase = TRUE))
    if ("try-error"%in%class(query.hit)) {
      skip("could not connect to remote database")
    }else{
      expect_identical(as.numeric(query.hit$TaxonSummary$DescriptionYear), 1935)
      expect_identical(query.hit$TaxonHistory$Authority[1], "Trewavas [E.] 1935")
      expect_identical(query.hit$TaxonHistory$RefNo[1], "4451")
    }
  }
})
