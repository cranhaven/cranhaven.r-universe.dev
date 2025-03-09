testthat::context("general ROCK tests")
require(rock);

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading a source with no ROCK stuff works properly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                    "lorum-ipsum.rock"));

  testthat::expect_s3_class(testres, "rock_parsedSource");

});

###-----------------------------------------------------------------------------

testthat::test_that("an inductive code tree is read correctly", {

  ### devtools::load_all();

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(
    file.path(
      examplePath, "longer-test.rock"
      #examplePath, "streams", "Source2_StreamA.rock"
    ),
    silent=TRUE
  );

  testthat::expect_equal(testres$inductiveCodeTrees$codes$inductFather$inducChild3$label,
                         "inducChild3");

});

###-----------------------------------------------------------------------------

testthat::test_that("example 2 is read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- rock::parse_source(file.path(examplePath,
                                          "example-2.rock"),
                                silent=TRUE);

  testres_fragment <-
    testres$qdt[
      ,
      c('uids', 'utterances_clean',
        'sectionBreak_paragraph_break_counter', 'caseId',
        'childCode', 'grandChildCode', 'grandChildCode2', 'justAcode',
        'region', 'age')
    ];

  testthat::expect_equal(nrow(testres_fragment),
                         16);

});

###-----------------------------------------------------------------------------

testthat::test_that("a code tree is printed correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                    "longer-test.rock"),
                          silent=TRUE);

  testthat::expect_output(print(testres),
                          "This source contained inductive coding trees.");

  testthat::expect_output(print(testres),
                          "This source contained deductive coding trees.");

});

###-----------------------------------------------------------------------------

testthat::test_that("an inductive code tree is read correctly using parse_sources()", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(path = examplePath,
                           regex = "longer-test.rock",
                           silent=TRUE);

  testthat::expect_equal(testres$inductiveCodeTrees$codes$inductFather$inducChild3$label,
                         "inducChild3");

});

###-----------------------------------------------------------------------------

testthat::test_that("a single deductive code tree is read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                "second-test-file.rock"));

  testthat::expect_equal(testres$deductiveCodeTrees$parentCode1$someParent$childCode1$label,
                         "childCode1");

});

###-----------------------------------------------------------------------------

testthat::test_that("Multiple sources are read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  rock::opts$set(warnForMultipleAesthetics = FALSE);

  testres <- parse_sources(examplePath,
                           extension="rock",
                           silent=TRUE);

  rock::opts$set(warnForMultipleAesthetics = TRUE);

  testthat::expect_equal(testres$deductiveCodeTrees$children$parentCode1$someParent$childCode2$label,
                         "childCode2");

});

###-----------------------------------------------------------------------------

testthat::test_that("multiple sources without deductive code trees are read correctly (i.e. as NA)", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(examplePath,
                           regex = "ipsum");

  testthat::expect_true(identical(testres$deductiveCodeTrees, NA));

});

###-----------------------------------------------------------------------------

testthat::test_that("A deductive code tree is read correctly from multiple DCT files", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(examplePath,
                           extension="dct",
                           silent=TRUE);

  testthat::expect_equal(testres$deductiveCodeTrees$behavior_xl67k7w8j$intention_71vr5q3q$attitude_71vqm37n$label,
                         "Attitude");

});

###-----------------------------------------------------------------------------

testthat::test_that("Sources are exported to html properly", {

  # devtools::load_all();
  # rock::opts$set(debug = TRUE);

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  rock::opts$set(warnForMultipleAesthetics = FALSE);

  testres <- parse_sources(examplePath,
                           extension="rock",
                           silent=TRUE);

  testres <- export_to_html(testres);

  testthat::expect_true(grepl('<span class="code codes">[[grandchildCode2]]</span>',
                              testres[["example-1.rock"]],
                              fixed=TRUE));

});

###-----------------------------------------------------------------------------

testthat::test_that("Coded fragments are collected properly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  rock::opts$set(warnForMultipleAesthetics = FALSE);

  testres_parsed <- parse_sources(examplePath,
                                  extension="rock",
                                  silent=TRUE);

  testres <- collect_coded_fragments(testres_parsed);

  testthat::expect_true(grepl('\n#### Topic2 *(path: codes>Topic2)*\n\n-----\n\n\n\n**Source: `longer-test.rock`**\n\n<div class=\"utterance\">It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. <span class=\"code codes\">[[Topic2]]</span>',
                              testres,
                              fixed=TRUE));

});

###-----------------------------------------------------------------------------

testthat::test_that("the example in export_codes_to_txt.Rd runs properly", {

  testthat::expect_true({
    ### Get path to example source
    examplePath <-
      system.file("extdata", package="rock");

    rock::opts$set(warnForMultipleAesthetics = FALSE);

    ### Parse all example sources in that directory
    parsedExamples <- rock::parse_sources(examplePath);

    ### Show results of exporting the codes
    export_codes_to_txt(parsedExamples);

    ### Only show select a narrow set of codes
    export_codes_to_txt(parsedExamples,
                        leavesOnly=TRUE,
                        includePath=FALSE,
                        onlyChildrenOf = "parentCode2",
                        regex="5|6");

    TRUE;

  });

});

###-----------------------------------------------------------------------------

testthat::test_that("recoding works", {

  testthat::expect_true({
    ### Get path to example source
    examplePath <-
      system.file("extdata", package="rock");

    ### Get filename
    exampleFile <-
      file.path(examplePath, "example-1.rock");

    ### Load a source
    loadedExample <- rock::load_source(exampleFile);

    ### Take a subset to keep the overview
    subExample <- loadedExample[21:27];

    rock::recode_rename(
      subExample,
      c(childCode4 = "bla")
    );

    TRUE;
  });
});

###-----------------------------------------------------------------------------


###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("merging two sources works properly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  tmpDir <- tempdir(check=TRUE);

  # devtools::load_all();
  # rock::opts$set(debug = TRUE);

  testres <- merge_sources(
    input = examplePath,
    output = tmpDir,
    filenameRegex = "merging-test-1",
    primarySourcesRegex = "merging-test-1-primary",
    preventOverwriting = FALSE,
    silent = TRUE
  );

  testResult <-
    readLines(file.path(tmpDir, "merging-test-1-primary_merged.rock"));

  testthat::expect_equal(
    testResult[9],
    "---<<some_section_break>>--- ---<<\\s*([a-zA-Z][a-zA-Z0-9_]*)\\s*>>---"
  );

  ### readLines(file.path(examplePath, "merging-test-1-primary.rock"))[15]
  ### readLines(file.path(examplePath, "merging-test-1-secondary.rock"))[14]

  testthat::expect_equal(
    testResult[16],
    "[[uid=7d8m7295]] Quisque non pretium mi. [[code2]] [[code3]] [[code2>code4]] [[code5]]"
  );

});


###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
