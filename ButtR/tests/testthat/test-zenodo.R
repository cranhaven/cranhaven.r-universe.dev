library(testthat)

# To manually perform some very slow tests:
# Zenodotest-latest and Zenodotest-4 should be identical and contain DNG raw files (and not ARW)
# Zenodotest-3 should contain ARW raw files (and not DNG)
# get_Oz_butterflies(species = "Taractrocera dolon", db_folder = "Zenodotest-latest")
# get_Oz_butterflies(species = "Taractrocera dolon", db_folder = "Zenodotest-4", db_version = 4)
# get_Oz_butterflies(species = "Taractrocera dolon", db_folder = "Zenodotest-3", db_version = 3)


test_that("zenodo interface works", {
  # Get list of files from the Zenodo repo (OzButterflies)
  # (https://doi.org/10.5281/zenodo.15881960)
  f <- listFilesInZenodo(BUTTR_DEPOSITION)

  # This is what we expect to get. Don't assume order
  # NOTE that testing uses locale C, which is different from interactive use
  #Sys.setlocale("LC_COLLATE", "C")
  f <- f[order(f$file), ]
  expected <- c("Hesperiidae_Arrhenes.zip", "Hesperiidae_Cephrenes.zip",
                                      "Hesperiidae_Hesperilla.zip", "Hesperiidae_Mesodina.zip", "Hesperiidae_Netrocoryne.zip",
                                      "Hesperiidae_Notocrypta.zip", "Hesperiidae_Ocybadistes.zip",
                                      "Hesperiidae_Parnara.zip", "Hesperiidae_Pelopidas.zip", "Hesperiidae_Sabera.zip",
                                      "Hesperiidae_Suniana.zip", "Hesperiidae_Tagiades.zip", "Hesperiidae_Taractrocera.zip",
                                      "Hesperiidae_Telicota.zip", "Hesperiidae_Toxidia.zip", "Hesperiidae_Trapezites.zip",
                                      "Lycaenidae_Arhopala.zip", "Lycaenidae_Candalides.zip", "Lycaenidae_Catochrysops.zip",
                                      "Lycaenidae_Catopyrops.zip", "Lycaenidae_Deudorix.zip", "Lycaenidae_Erysichton.zip",
                                      "Lycaenidae_Euchrysops.zip", "Lycaenidae_Famegana.zip", "Lycaenidae_Hypochrysops.zip",
                                      "Lycaenidae_Hypolycaena.zip", "Lycaenidae_Jamides.zip", "Lycaenidae_Lampides.zip",
                                      "Lycaenidae_Leptotes.zip", "Lycaenidae_Megisba.zip", "Lycaenidae_Nacaduba.zip",
                                      "Lycaenidae_Neolucia.zip", "Lycaenidae_Paralucia.zip", "Lycaenidae_Prosotas.zip",
                                      "Lycaenidae_Psychonotis.zip", "Lycaenidae_Theclinesthes.zip",
                                      "Lycaenidae_Zizina.zip", "Lycaenidae_Zizula.zip", "Nymphalidae_Acraea.zip",
                                      "Nymphalidae_Cethosia.zip", "Nymphalidae_Cupha.zip", "Nymphalidae_Danaus.zip",
                                      "Nymphalidae_Doleschallia.zip", "Nymphalidae_Euploea.zip", "Nymphalidae_Heteronympha.zip",
                                      "Nymphalidae_Hypocysta.zip", "Nymphalidae_Hypolimnas.zip", "Nymphalidae_Junonia.zip",
                                      "Nymphalidae_Melanitis.zip", "Nymphalidae_Mycalesis.zip", "Nymphalidae_Mynes.zip",
                                      "Nymphalidae_Neptis.zip", "Nymphalidae_Pantoporia.zip", "Nymphalidae_Phaedyma.zip",
                                      "Nymphalidae_Tirumala.zip", "Nymphalidae_Tisiphone.zip", "Nymphalidae_Vagrans.zip",
                                      "Nymphalidae_Vanessa.zip", "Nymphalidae_Yoma.zip", "Nymphalidae_Ypthima.zip",
                                      "Oz_butterflies.csv", "Oz_butterflies.json", "Oz_butterflies.xlsx",
                                      "Oz_butterflies_summary.csv", "Oz_butterflies_summary.json",
                                      "Oz_butterflies_summary.xlsx", "Papilionidae_Cressida.zip", "Papilionidae_Graphium.zip",
                                      "Papilionidae_Pachliopta.zip", "Papilionidae_Papilio.zip", "Pieridae_Belenois.zip",
                                      "Pieridae_Catopsilia.zip", "Pieridae_Cepora.zip", "Pieridae_Delias.zip",
                                      "Pieridae_Elodina.zip", "Pieridae_Eurema.zip", "Pieridae_Pieris.zip",
                                      "README.txt", "filter_holders.zip", "standard-blue.ProcSpec", "standard-green.ProcSpec",
                                      "standard-red.ProcSpec")
  # Ignore the size and URL columns since they can change from version to version
  expect_equal(f$file, expected)

  # Download README.txt
  ri <- which(f$file == "README.txt")
  tmpnm <- tempfile(f$file[ri])
  expect_error(utils::download.file(f$url[ri], destfile = tmpnm, quiet = TRUE), NA)
  # Check it has the expected content
  line1 <- readLines(tmpnm, n = 1)
  # This is the first line in the README.txt file
  expect_equal(line1, "OzButterflies database")
})

test_that("local file interface works", {
  # This is to test that the test mock Dryad interface works. It is tested
  # because it is the basis of further tests, so if it doesn't work, later tests
  # will fail spuriously

  # This should list the files in the testdata folder
  f <- listLocalFiles(testthat::test_path("testdata/db"))
  # Check that various files exist
  expect_true("README.txt" %in% f$file)

  # Download the README.txt file
  readmeIdx <- which(f$file == "README.txt")
  origContent <- readLines(file.path(testthat::test_path("testdata/db"), f$file[readmeIdx]))
  # Check that it can be downloaded and has the same contents as the original
  withr::with_dir(tempdir(), {
    utils::download.file(url = f$url[readmeIdx], destfile = f$file[readmeIdx], quiet = TRUE)
    expect_true(file.exists(f$file[readmeIdx]))
    newContent <- readLines(f$file[readmeIdx])
    expect_equal(newContent, origContent)
  })
})
