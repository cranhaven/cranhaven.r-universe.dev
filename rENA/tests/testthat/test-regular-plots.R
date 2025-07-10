suppressMessages(library(rENA, quietly = F, verbose = F))
context("Test plotting sets")

data(RS.data)
codenames <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");

title = "ENA Plot"
dimension.labels = c("","")
font.size = 10
font.color = "#000000"
font.family = "Arial"
scale.to = "network"

accum <- suppressWarnings(rENA:::ena.accumulate.data.file(
  RS.data, units.by = c("UserName", "Condition"),
  conversations.by = c("ActivityNumber", "GroupName"),
  codes = codenames, as.list = FALSE
));
enaset <- suppressWarnings(ena.make.set(accum, as.list = FALSE))
test_that("Test for ENAplot", {


  testthat::expect_warning(ENAplot$new(enaset,
                     title,
                     dimension.labels,
                     font.size,
                     font.color,
                     font.family,
                     scale.to = scale.to
                   ))
})

test_that("Test extra args", {

  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum)
  newplot <- ENAplot$new(set,
                     title,
                     dimension.labels,
                     font.size,
                     font.color,
                     font.family,
                     multiplier = 2.5,
                     point.size = 20,
                     scale.to = "points"
                     ,ticks = list(color = "red")
                   )
  testthat::expect_equal(newplot$get("multiplier"), 2.5)
  testthat::expect_equal(newplot$point$size, 20)
  testthat::expect_equal(newplot$plot$x$layoutAttrs[[1]]$xaxis$tickcolor, "red")

  newplot <- ENAplot$new(set,
                     title,
                     dimension.labels,
                     font.size,
                     font.color,
                     font.family,
                     scale.to = c(1, 25)
                   )
  testthat::expect_equal(newplot$plot$x$layoutAttrs[[1]]$xaxis$range[2], 25)

  newplot <- ENAplot$new(set,
                     title,
                     dimension.labels,
                     font.size,
                     font.color,
                     font.family,
                     scale.to = list(
                       x = c(-100, 100),
                       y = c(-200, 200)
                     )
                   )
  testthat::expect_equal(newplot$plot$x$layoutAttrs[[1]]$xaxis$range[2], 100)
  testthat::expect_equal(newplot$plot$x$layoutAttrs[[1]]$yaxis$range[2], 200)

  newplot <- ENAplot$new(set,
                     title,
                     dimension.labels,
                     font.size,
                     font.color,
                     font.family,
                     scale.to = list(
                       x = c(-100, 100)
                     )
                   )
  testthat::expect_equal(newplot$plot$x$layoutAttrs[[1]]$xaxis$range[2], 100)
  testthat::expect_lte(newplot$plot$x$layoutAttrs[[1]]$yaxis$range[2], 1)

  newplot <- ENAplot$new(set,
                     title,
                     dimension.labels,
                     font.size,
                     font.color,
                     font.family,
                     scale.to = list(
                       y = c(-200, 200)
                     )
                   )
  testthat::expect_lte(newplot$plot$x$layoutAttrs[[1]]$xaxis$range[2], 1)
  testthat::expect_equal(newplot$plot$x$layoutAttrs[[1]]$yaxis$range[2], 200)
})

test_that("Test plot points", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum);
  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  newplot <- ena.plot.points(newplot, label.font.family = 1)
  testthat::expect_equal(newplot$plot$x$attrs[[length(newplot$plot$x$attrs)]]$textfont$family, newplot$get("font.family"))

  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  newplot <- ena.plot.points(newplot, label.font.family = "Helvetica")
  testthat::expect_equal(newplot$plot$x$attrs[[length(newplot$plot$x$attrs)]]$textfont$family, "Helvetica")
})
test_that("Test plot bad args", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum);

  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  testthat::expect_error(ena.plot.points(newplot, shape = "nothing"), regexp = "Unrecognized shapes")
  testthat::expect_error(ena.plot.points(newplot, label.offset = "top top"), regexp = "Unrecognized label.offsets")

  testthat::expect_message(ena.plot.points(newplot, confidence.interval = "crosshair", outlier.interval = "crosshair"), "cannot both be crosshair")
})

test_that("Test plot ci/oi intervals w/ crosshairs", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum);

  int.values = matrix(c(-1,1,-0.5,0.5), ncol=2)
  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  plot <- ena.plot.points(newplot, points = set$points$UserName$`steven z`, confidence.interval = "crosshair", confidence.interval.values = int.values)
  testthat::expect_equal(plot$plot$x$attrs[[2]]$error_x$array, c(-1, 1))
  testthat::expect_equal(plot$plot$x$attrs[[2]]$error_y$array, c(-0.5, 0.5))

  int.values = matrix(c(-0.7, 0.7, -0.25, 0.25), ncol=2)
  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  plot <- ena.plot.points(newplot, points = set$points$UserName$`steven z`, outlier.interval = "crosshair", outlier.interval.values = int.values)
  testthat::expect_equal(plot$plot$x$attrs[[2]]$error_x$array, c(-0.7,  0.7))
  testthat::expect_equal(plot$plot$x$attrs[[2]]$error_y$array, c(-0.25, 0.25))
})

test_that("Test plot ci/oi intervals w/ boxes", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum);

  int.values = matrix(c(-1,1,-0.5,0.5), ncol=2)
  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  plot <- ena.plot.points(newplot, points = set$points$UserName$`steven z`, confidence.interval = "box", confidence.interval.values = int.values)
  testthat::expect_true(all(
    as.vector(matrix(int.values[ c(c(1,3), c(2,3), c(2,4), c(1,4), c(1,3))], ncol = 2, byrow = T)) ==
    as.vector(as.matrix(tail(plot$plot$x$visdat, 1)[[1]]()))
  ))

  int.values = matrix(c(-0.7, 0.7, -0.25, 0.25), ncol=2)
  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  plot <- ena.plot.points(newplot, points = set$points$UserName$`steven z`, outlier.interval = "box", outlier.interval.values = int.values)
  testthat::expect_true(all(
    as.vector(matrix(int.values[ c(c(1,3), c(2,3), c(2,4), c(1,4), c(1,3))], ncol = 2, byrow = T)) ==
    as.vector(as.matrix(tail(plot$plot$x$visdat, 1)[[1]]()))
  ))
})

test_that("Test plot resize axes", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum);

  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  plot <- ena.plot.points(newplot$clone(deep = TRUE), points = as.matrix(set$points$UserName$`steven z`) * 100)
  testthat::expect_lt(plot$axes$x$range[1], newplot$axes$x$range[1])
  testthat::expect_gt(plot$axes$x$range[2], newplot$axes$x$range[2])
})

test_that("Test plot groups", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum);

  newplot <- ENAplot$new(set, title, dimension.labels, font.size, font.color, font.family);
  testthat::expect_error(ena.plot.group(newplot), regexp = "Points must be provided")
  testthat::expect_message(ena.plot.group(
    newplot, points = set$points$Condition$FirstGame,
    outlier.interval = "crosshair", confidence.interval = "crosshair"
  ), regexp = "cannot both be crosshair")

  newplot <- ena.plot.group(newplot, points = set$points$Condition$FirstGame, colors = c("red", "blue"))
})
