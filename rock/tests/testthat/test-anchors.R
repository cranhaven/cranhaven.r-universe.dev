testthat::context("anchor tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading anchors works", {

  ### devtools::load_all();

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                    "anchor-example-1.rock"));

  testthat::expect_s3_class(testres, "rock_parsedSource");

});

###-----------------------------------------------------------------------------

testthat::test_that("reading anchors works", {

  ### devtools::load_all();

  examplePath <- file.path(system.file(package="rock"), 'extdata', 'streams');

  testres <- rock::parse_sources(
    examplePath
  );

  syncedres <- rock::sync_streams(
    testres,
    primaryStream = "streamA",
    columns = c("Code1", "Code2", "Code3"),
    prependStreamIdToColName = TRUE,
    silent = TRUE
  );

  # devtools::load_all(); syncedres <- rock::sync_streams(
  #   testres, primaryStream = "streamA",
  #   columns = c("Code1", "Code2", "Code3"),
  #   prependStreamIdToColName = TRUE, compressFunPart = betterSum);

  syncedres$syncResults$mergedSourceDf[, c("streamB_Code3", "streamC_Code1")];

  testthat::expect_equal(
    syncedres$syncResults$mergedSourceDf[
      7,
      "streamB_Code3"
    ],
    "1 1");

});


###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
