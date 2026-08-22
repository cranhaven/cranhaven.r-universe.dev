test_that("lapopWelcomeMessage includes package metadata", {
  msg <- lapop:::lapopWelcomeMessage("lapop")

  expect_type(msg, "character")
  expect_length(msg, 1)
  expect_match(msg, "Version:")
  expect_match(msg, "browseVignettes\\('lapop'\\)")
})

test_that("lapop_fonts registers bundled fonts", {
  expect_invisible(lapop_fonts())

  families <- sysfonts::font_families()
  expect_true("inter" %in% families)
  expect_true("inter-light" %in% families)
})

test_that("lapop_save writes svg and png outputs", {
  fig <- ggplot2::ggplot(
    data.frame(x = c("A", "B"), y = c(1, 2)),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_col()

  svg_file <- tempfile(fileext = ".svg")
  png_file <- tempfile(fileext = ".png")

  expect_identical(lapop_save(fig, svg_file, format = "svg"), svg_file)
  expect_true(file.exists(svg_file))
  expect_gt(file.info(svg_file)$size, 0)

  expect_identical(lapop_save(fig, png_file, format = "png"), png_file)
  expect_true(file.exists(png_file))
  expect_gt(file.info(png_file)$size, 0)
})

test_that("lapop_map returns a ggplot object for simple input", {
  skip_if_not_installed("sf")

  map_data <- data.frame(
    code = c("US", "BR", "MX"),
    score = c(37, 52, 61),
    stringsAsFactors = FALSE
  )

  p <- lapop_map(
    data = map_data,
    pais_lab = "code",
    outcome = "score",
    survey = "AmericasBarometer"
  )

  expect_s3_class(p, "ggplot")
})

test_that("lpr_data maps country codes and returns a survey object", {
  toy <- data.frame(
    pais = factor(c("Brazil", "Mexico")),
    upm = c(1, 2),
    strata = c(10, 20),
    weight1500 = c(1.5, 0.5),
    stringsAsFactors = TRUE
  )

  survey_tbl <- lpr_data(toy)

  expect_s3_class(survey_tbl, "tbl_svy")
  expect_identical(survey_tbl$variables$pais_lab, c("BR", "MX"))
})

test_that("lpr_resc rescales and flips labelled vectors", {
  x <- haven::labelled(
    c(1, 2, 3, NA),
    labels = c(Low = 1, Medium = 2, High = 3)
  )

  resc <- lpr_resc(x, min = 0, max = 100)
  flipped <- lpr_resc(x, only_flip = TRUE)

  expect_equal(unname(resc[1:3]), c(0, 50, 100))
  expect_equal(as.numeric(flipped[1:3]), c(3, 2, 1))
  expect_named(attr(flipped, "labels"), c("Low", "Medium", "High"))
})

test_that("lpr_set_attr propagates notes and respects overwrite", {
  dat <- data.frame(
    q1 = 1:2,
    q1_1 = 3:4,
    q2 = 5:6
  )

  notes <- data.frame(
    variable_name = c("q1", "q2"),
    note_id = c("qtext_en", "qtext_en"),
    note_value = c("Question one", "Question two"),
    stringsAsFactors = FALSE
  )

  out <- lpr_set_attr(
    data = dat,
    notes = notes,
    noteid = "qtext_en",
    attribute_name = "qwording_en"
  )

  expect_identical(attr(out$q1, "qwording_en"), "Question one")
  expect_identical(attr(out$q1_1, "qwording_en"), "Question one")
  expect_identical(attr(out$q2, "qwording_en"), "Question two")

  attr(dat$q2, "qwording_en") <- "Keep me"
  out_no_overwrite <- lpr_set_attr(
    data = dat,
    notes = notes,
    noteid = "qtext_en",
    attribute_name = "qwording_en",
    overwrite = FALSE
  )

  expect_identical(attr(out_no_overwrite$q2, "qwording_en"), "Keep me")
})

test_that("lpr_set_ros stores formatted response options", {
  dat <- data.frame(ing4 = c(1, 2, 1))
  attr(dat, "label.table") <- list(
    ing4_en = c("Strongly support" = 1, "Support" = 2, "Missing" = 1000)
  )

  out <- lpr_set_ros(dat, lang_id = "en", attribute_name = "roslabel")

  expect_identical(
    attr(out$ing4, "roslabel"),
    "Response Options: (1) Strongly support (2) Support"
  )
})
