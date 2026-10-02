test_that("Postal code bounding box is the same as regions", {
  # This should ensure that issue #100 does not re-occur.
  # https://github.com/aleksanderbl29/dawaR/issues/100
  # https://github.com/aleksanderbl29/geodk/issues/1

  skip_on_cran()

  regioner <- get_map_data("regioner") |>
    sf::st_bbox(dk)
  postnumre <- get_map_data("postnumre") |>
    sf::st_bbox(postnumre)

  expect_equal(regioner, postnumre)
})
