test_that("Available sections can be called and returns in the proper format", {
  adresser_sections <- c(
    "adgangsadresser",
    "adresser",
    "autocomplete",
    "navngivneveje",
    "vejstykker",
    "vejnavne",
    "vejnavnpostnummerrelationer"
  )

  dagi_sections <- c(
    "afstemningsomraader",
    "kommuner",
    "landsdele",
    "menighedsraadsafstemningsomraader",
    "opstillingskredse",
    "politikredse",
    "postnumre",
    "regioner",
    "retskredse",
    "sogne",
    "storkredse",
    "supplerendebynavne2",
    "valglandsdele"
  )

  matrikelkortet <- c("ejerlav", "jordstykker")

  stednavne <- c("steder", "stednavne2", "bebyggelser")

  bygningspolygoner <- c("bygninger")

  expect_list <- list(
    adresser = adresser_sections,
    dagi = dagi_sections,
    matrikelkortet = matrikelkortet,
    stednavne = stednavne,
    bygningspolygoner = bygningspolygoner
  )

  expect <- sort(c(
    adresser_sections,
    dagi_sections,
    matrikelkortet,
    stednavne,
    bygningspolygoner
  ))

  expect_equal(available_sections(), expect)
  expect_equal(available_sections(as_list = FALSE), expect)
  expect_equal(available_sections(as_list = TRUE), expect_list)
})
