test_that("an RPE coding file can be prepared", {

  # devtools::load_all("C:/pC/git/R/rock");
  # devtools::load_all("C:/pC/git/R/limonaid");

  lsFilesPath <- system.file("limesurvey",
                             package="rock");
  extdataPath <- system.file("extdata",
                             package="rock");
  tmpDir <- tempdir(check=TRUE);

  lsDat <-
    limonaid::ls_import_data(sid = 795779,
                             path = lsFilesPath,
                             ### To no longer need {sticky}
                             setLabels = FALSE);

  ### Add empty labels for variables without labels in case we have
  ### an old {limonaid} version
  attributes(lsDat)$variable.labels <-
    c(attributes(lsDat)$variable.labels,
      names(lsDat)[(length(attributes(lsDat)$variable.labels)+1):ncol(lsDat)]);

  labelDf <-
    limonaid::ls_process_labels(
      lsDat,
      lengthToWrap = Inf
    );

  item_varNames <-
    c(item1 = "item1",
      item2 = "item2");

  metaquestionIdentifiers <-
    list(
      item1 = c("item1mq1",
                "item1mq2"),
      item2 = c("item2mq1",
                "item2mq2")
    );

  mq_varNames <-
    c(item1mq1 = "item1mq1",
      item1mq2 = "item1mq2",
      item2mq1 = "item2mq1",
      item2mq2 = "item2mq2");

  item_questionTextMatches <-
    match(labelDf$varNames.raw, item_varNames);
  item_questionTextIndices <-
    which(item_questionTextMatches %in% na.omit(item_questionTextMatches));
  item_contents <-
    stats::setNames(
      labelDf[item_questionTextIndices, "questionText"],
      nm = item_varNames
    );

  mq_questionTextMatches <-
    match(labelDf$varNames.raw, mq_varNames);
  mq_questionTextIndices <-
    which(mq_questionTextMatches %in% na.omit(mq_questionTextMatches));
  mq_itemContents <-
    stats::setNames(
      labelDf[mq_questionTextIndices, "questionText"],
      nm = names(mq_varNames)
    );

  itemSource <-
    rpe_create_source_with_items(
      data = lsDat,
      iterationId = "iterationId",
      batchId = "batchId",
      populationId = "populationId",
      itemVarNames = item_varNames,
      metaquestionIdentifiers = metaquestionIdentifiers,
      metaquestionVarNames = mq_varNames,
      itemContents = item_contents,
      metaquestionContents = mq_itemContents,
      coderId = "coder1",
      caseIds = lsDat$id,
      #outputFile = file.path(extdataPath, "simple-rpe-example.rock"),
      outputFile = file.path(tmpDir, "simple-rpe-example.rock"),
      preventOverwriting = FALSE
    );

  parsedItemSource <-
    rock::parse_source(
      #file = file.path(extdataPath, "simple-rpe-example.rock")
      file = file.path(tmpDir, "simple-rpe-example.rock")
    );

  testthat::expect_equal(
    parsedItemSource$qdt$comment[26],
    "none"
  );

});
