test_that("deductive codes are read and applied properly", {

  # devtools::load_all("C:/pC/git/R/rock");

  extdataPath <- system.file("extdata",
                             package="rock");

  sourceName <- "kristina-example-1.rock";

  parsedSource <-
    rock::parse_source(
      file = file.path(extdataPath, sourceName)
    );

  testthat::expect_equal(
    parsedSource$qdt$loss[10],
    1
  );

});
