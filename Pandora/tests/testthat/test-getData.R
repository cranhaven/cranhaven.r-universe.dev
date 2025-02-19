test_that("Test getData() from xlsx", {
  testLoaded <-
    getData(name = "14CARHU - Radiocarbon Dates of Helsinki University") %>%
    passOnErrorMsg()
  
  if (foundResource(testLoaded)) {
    expect_true(nrow(testLoaded) > 2000)
    expect_true(all(
      c(
        "14CARHU.dates.under.the.OASIS.database.v1.0.(21/10/2015)",
        "X2",
        "X3",
        "X4",
        "X5"
      )
      %in% colnames(testLoaded)
    ))
  }
})

test_that("Test getData() from csv if isOldROnWindows() machine", {
  # following tests show encryption issues with Windows
  if (isOldROnWindows()) {
    # error on old windows
    expect_error(getData(name = "Isotopic measurements in CSV format"))
    
    # error without specific encoding
    expect_error(getData(name = "MAIA Humans CSV",
                         options = dataOptions(sep = ";")))
    
    # no error with windows encoding
    testLoaded <- getData(name = "MAIA Humans CSV",
                          options = dataOptions(sep = ";",
                                                fileEncoding = "windows-1252")) %>%
      passOnErrorMsg()
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # less rows on old windows
    testLoaded <- getData(name = "IsoMedIta Humans 21-12-22 - CSV",
                          options = dataOptions(sep = ";"))
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) < 2000)
  }
})

test_that("Test getData() from csv if newer Windows machine", { 
  if (!isOldROnWindows() && Sys.info()["sysname"] == "Windows") {
    # no error without specific encoding
    testLoaded <- getData(name = "Isotopic measurements in CSV format")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    testLoaded <- getData(name = "MAIA Humans CSV",
                          options = dataOptions(sep = ";")) %>%
      passOnErrorMsg()
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # no error with windows encoding
    testLoaded <- getData(name = "MAIA Humans CSV",
                          options = dataOptions(sep = ";",
                                                fileEncoding = "windows-1252")) %>%
      passOnErrorMsg()
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # more rows if newer windows
    testLoaded <- getData(name = "IsoMedIta Humans 21-12-22 - CSV",
                          options = dataOptions(sep = ";"))
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
  }
  })

test_that("Test getData() from csv if linux or mac", {
  if (!Sys.info()["sysname"] == "Windows") { # linux or mac
    # no error without specific encoding
    testLoaded <- getData(name = "MAIA Humans CSV",
                          options = dataOptions(sep = ";")) %>%
      passOnErrorMsg()
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # less data with windows encoding
    testLoaded <-
      getData(name = "MAIA Humans CSV",
              options = dataOptions(sep = ";",
                                    fileEncoding = "windows-1252")) %>%
      passOnErrorMsg()
    
    if (foundResource(testLoaded)) {
      expect_true(nrow(testLoaded) > 700)
      expect_true(nrow(testLoaded) < 800)
    }
  }
})

test_that("Test getData() from csv", {
  testLoaded <- getData(name = "CIMA Animals 29.05.2021 CSV",
                        options = dataOptions(sep = ";")) %>%
    passOnErrorMsg()
  if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 4000)
  
  testLoaded <- getData(name = "CIMA Plants 29.05.2021 CSV",
                        options = dataOptions(sep = ";")) %>%
    passOnErrorMsg()
  if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 100)
  
  
  testLoaded <- getData(name = "Zanadamu CSV format",
                        options = dataOptions(fileEncoding = "ISO-8859-1"))  %>%
    passOnErrorMsg()
  if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 200)
  
  # run only for TDD:
  # test random files to check if errors are caught
  # allResources <- getResources()
  # for (i in 1:10) {
  #   testResource <- allResources[sample(nrow(allResources), 1), ]
  #   getData(name = testResource[["name"]])
  # }
  
  expect_error(getData(name = "Amalthea Bibliography 05.03.2021"))
  expect_error(getData(name = "Isotòpia Humans csv (19.09.2023)"))
})

test_that("Test loadData()", {
  testResource <-
    getResources(fileType = "xlsx",
                 network = "IsoMemo",
                 pattern = "14carhu")
  testLoaded <-
    loadData(path = testResource[1, "url"], type = testResource[1, "format"])
  expect_true(nrow(testLoaded) > 2000)
  expect_true(all(
    c(
      "14CARHU.dates.under.the.OASIS.database.v1.0.(21/10/2015)",
      "X2",
      "X3",
      "X4",
      "X5"
    )
    %in% colnames(testLoaded)
  ))
})
