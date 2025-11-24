test_that("update_klass gir riktig resultat ved enkle endringer", {
  data(klass_131_2020_graph)
  graph <- klass_131_2020_graph

  changes_url <- paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    131, "/changes?from=0001-01-01"
  )

  api_endringer <- jsonlite::fromJSON(klassR:::GetUrl2(changes_url),
    flatten = TRUE
  )[["codeChanges"]]

  endringer_kommunestruktur_enkle <-
    api_endringer %>%
    # 2020 hadde mange enkle endringer
    dplyr::filter(changeOccurred == "2020-01-01") %>%
    dplyr::group_by(oldCode) %>%
    dplyr::filter(dplyr::n() == 1) %>% # vi tester ikke delinger av koder
    dplyr::group_by(newCode) %>%
    dplyr::filter(dplyr::n() == 1) %>% # vi tester ikke sammenslåtte koder
    dplyr::ungroup() %>%
    dplyr::select(oldCode, newCode, changeOccurred)

  omkodet <-
    endringer_kommunestruktur_enkle %>%
    dplyr::mutate(oppdatert = update_klass(
      codes = oldCode,
      classification = 131,
      output = "code",
      report = FALSE,
      graph = graph
    ))

  feil <-
    omkodet %>%
    dplyr::rowwise() %>%
    dplyr::filter(!newCode %in% oppdatert)

  expect_equal(nrow(feil), 0)
})

test_that("update_klass gir riktig resultat ved sammenslåtte koder", {
  data(klass_131_1964_graph)
  graph <- klass_131_1964_graph

  changes_url <- paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    131, "/changes?from=0001-01-01"
  )

  api_endringer <- jsonlite::fromJSON(klassR:::GetUrl2(changes_url),
    flatten = TRUE
  )[["codeChanges"]]

  endringer_kommunestruktur_sammenslåinger <-
    api_endringer %>%
    dplyr::filter(changeOccurred == "1964-01-01") %>%
    dplyr::group_by(oldCode, changeOccurred) %>%
    dplyr::filter(dplyr::n() == 1) %>% # vi tester ikke delinger av koder
    dplyr::group_by(newCode) %>%
    dplyr::filter(dplyr::n() > 1)

  omkodet <-
    endringer_kommunestruktur_sammenslåinger %>%
    dplyr::mutate(
      oppdatert = update_klass(
        codes = oldCode,
        dates = as.Date(changeOccurred) - 1,
        classification = 131,
        graph = graph
      ),
      oppdatert_ikkecomb = update_klass(
        codes = oldCode,
        dates = as.Date(changeOccurred) - 1,
        classification = 131,
        graph = graph,
        combine = FALSE
      )
    )

  omkodet %>%
    dplyr::rowwise() %>%
    dplyr::filter(!newCode %in% oppdatert) %>%
    # `1441 Selje` is split in 1964 to `1441 Selje` and `1439 Vågsøy`,
    # but only `1439 Vågsøy` is recorded in the changes API
    dplyr::filter(!oldCode == "1441" & changeOccurred == "1964-01-01") %>%
    nrow() %>%
    expect_equal(expected = 0)

  omkodet %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.na(oppdatert_ikkecomb)) %>%
    nrow() %>%
    expect_equal(expected = 0)
})

test_that("update_klass gir riktig resultat ved ugyldige koder", {
  data(klass_131_graph)
  graph <- klass_131_graph

  expect_true(
    all(
      is.na(
        update_klass(c("foo", "bar", "egg", "ham"),
          graph = graph
        )
      )
    )
  )
})

test_that("update_klass gir riktig resultat ved delte koder", {
  data(klass_131_1964_graph)
  graph <- klass_131_1964_graph

  changes_url <- paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    131, "/changes?from=0001-01-01"
  )

  api_endringer <- jsonlite::fromJSON(klassR:::GetUrl2(changes_url),
    flatten = TRUE
  )[["codeChanges"]]

  endringer_kommunestruktur_delinger <-
    api_endringer %>%
    # 1964 hadde klart flest delinger av koder
    dplyr::filter(changeOccurred == "1964-01-01") %>%
    dplyr::group_by(oldCode) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(oldCode, newCode, changeOccurred)

  omkodet <-
    endringer_kommunestruktur_delinger %>%
    dplyr::mutate(oppdatert = update_klass(
      codes = oldCode,
      dates = as.Date(changeOccurred) - 1,
      classification = 131,
      graph = graph
    ))

  expect_true(all(is.na(omkodet$oppdatert)))
})


test_that("update_klass gir forventet format på output", {
  data(klass_131_graph)
  graph <- klass_131_graph

  update_helper <- function(output, report) {
    update_klass(
      codes = "0301",
      dates = "1838-01-01",
      output = output,
      report = report,
      graph = graph
    )
  }

  expect_type(update_helper(output = "code", report = FALSE), "character")
  expect_type(update_helper(output = "code", report = TRUE), "list")
  expect_type(update_helper(output = TRUE, report = FALSE), "list")
  expect_type(update_helper(output = "code", report = TRUE)[[1]], "character")
  expect_s3_class(
    update_helper(output = c("code", "name"), report = TRUE)[[1]],
    "data.frame"
  )
  expect_type(update_helper(output = c("code", "name"), report = TRUE), "list")
  expect_type(update_helper(output = TRUE, report = FALSE), "list")
  expect_s3_class(update_helper(output = TRUE, report = FALSE)[[1]], "data.frame")
  expect_equal(nrow(update_helper(output = TRUE, report = FALSE)[[1]]), 1)
  expect_equal(
    nrow(update_helper(output = c("code", "name"), report = FALSE)[[1]]),
    1
  )
  expect_equal(length(update_helper(output = "code", report = FALSE)), 1)
})

test_that("update_klass oppdaterer koder som har hatt navneendringer", {
  ## ... og der navneendringene ikke er logget som endringer, jf. [#56]

  data(klass_131_graph)
  graph <- klass_131_graph

  expect_equal(
    update_klass(
      codes = "1420",
      dates = "1838-01-01",
      graph = graph
    ),
    "4640",
    ignore_attr = TRUE
  )
})
