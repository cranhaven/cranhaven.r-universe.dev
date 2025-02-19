test_that("Test getResources()", {
  expect_true(nrow(getResources(repository = "aghfjdhfjgkhj")) == 0)
  
  expect_equal(
    getResources(fileType = c("csv"), pattern = "victoria"),
    structure(list(repository = c(
      "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia", 
      "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia"),
      name = c("Austarch 1-3 and IDASQ 28Nov13-1", "Austarch 1-3 and IDASQ 28Nov13-1 Citation\t"), 
      format = c("csv", "csv"), 
      url = c(
        "https://archaeologydataservice.ac.uk/catalogue/adsdata/arch-1661-1/dissemination/csv/Austarch_1-3_and_IDASQ_28Nov13-1.csv", 
        "https://archaeologydataservice.ac.uk/catalogue/adsdata/arch-1661-1/dissemination/csv/Austarch_1-3_and_IDASQ_28Nov13-1_Citation.csv")
    ), class = "data.frame", row.names = c(NA, -2L))
  )
})

test_that("Test getFileTypes()", {
  expect_true(nrow(getFileTypes(repository = "aghfjdhfjgkhj")) == 0)
  
  expect_equal(
    getFileTypes(pattern = "victoria"),
    structure(list(
      name = "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia", 
      format = "csv"), 
      row.names = 1L, class = "data.frame")
  )
})

test_that("Test getRepositories()", {
  expect_true(nrow(getRepositories(network = "aghfjdhfjgkhj")) == 0)
  
  testRepos <- getRepositories(order = FALSE, renameColumns = FALSE)
  expect_equal(colnames(testRepos),
               c("title", "name", "notes", "ext_doi", "doi", "version", "author", 
                 "author_email", "maintainer", "maintainer_email", "temporal_start", 
                 "temporal_end", "spatial")
               )
  expect_true(
    all(c("isomedita-a-stable-isotope-database-for-medieval-italy", 
          "northern-hemisphere-modern-leaf-wax-ddn-alkane-dataset", 
          "base-de-datos-iber-crono") %in% testRepos$name)
  )
  expect_true(
    all(c("Equine Biometry from Medieval and Modern sites in the Czech Republic", 
          "Tooth Formation Age Dataset for Early Childhood Bioarchaeological and Medical Studies", 
          "Database of equine osteological remains from Greece and Cyprus"
    ) %in% testRepos$title)
  )
  
  testRepos <- getRepositories(pattern = "victor", network = "isomemo", order = FALSE)
  expect_equal(
    "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia",
    testRepos$Name
  )
  
  expect_equal(
    "AustArch: A Database of 14C and Luminescence Ages from Archaeological Sites in Australia",
    testRepos$Repository
  )
  
  testRepos <- getRepositories(order = FALSE, renameColumns = TRUE)
  expect_equal(colnames(testRepos), 
               c("Repository", "Name", "Description", "Existing DOI", "Assigned DOI", 
                 "Version", "Author", "Author Email", "Maintainer", "Maintainer Email", 
                 "Chronological range (min)", "Chronological range (max)", "Spatial Box"
               ))
})

test_that("Test getNetworks()", {
  expect_true(nrow(getNetworks(pattern = "aghfjdhfjgkhj")) == 0)
  
  expect_equal(
    getNetworks(),
    structure(list(name = "isomemo-group", 
                   display_name = "IsoMemo Network", 
                   description = "IsoMemo is a network of autonomous isotopic databases."), 
              class = "data.frame", row.names = 1L)
  )
})

test_that("Test filterPattern()", {
  testRes <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  expect_true(nrow(filterPattern(testRes, pattern = "Roman")) < nrow(testRes))
  expect_equal(filterPattern(testRes, pattern = "Roman"),
               filterPattern(testRes, pattern = "rOmAn"))
  expect_true(nrow(filterPattern(testRes, pattern = "cjyvfljdosijvckjnlsfnsdkfnak")) == 0)
})
