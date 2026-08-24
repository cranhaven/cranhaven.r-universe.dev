## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## -----------------------------------------------------------------------------
dat <- get(data("BBC_pp15.1", package="behaviorchange"));

## -----------------------------------------------------------------------------
subdets <-
  grep(
    "highDose_AttBeliefs_",
    names(dat),
    value=TRUE
  );

## ----eval=TRUE, echo=FALSE----------------------------------------------------

oldKableViewOption <- getOption("kableExtra_view_html", NULL);
options(kableExtra_view_html = FALSE);

if (!exists('headingLevel') || !is.numeric(headingLevel) || (length(headingLevel) != 1)) {
  headingLevel <- 0;
}


## ----results='asis', eval=TRUE------------------------------------------------
ufs::heading("Determinant Selection Table",
             headingLevel=headingLevel);

## ----echo=echoPartial, results='asis', eval=TRUE------------------------------

if (requireNamespace("kableExtra", quietly = TRUE)) {
  print(
    prettyDeterminantSelectionTable(
      x,
      digits=digits
    )
  );
} else {
  knitr::kable(x,
               digits=digits,
               row.names=FALSE);
}


## ----echo=FALSE---------------------------------------------------------------
if (!is.null(oldKableViewOption)) {
  options(kableExtra_view_html = oldKableViewOption);
}

## -----------------------------------------------------------------------------
behaviorchange::determinant_selection_table(
  data=dat,
  determinants = subdets,
  target = 'highDose_intention'
);

## ----eval=TRUE, echo=FALSE----------------------------------------------------

oldKableViewOption <- getOption("kableExtra_view_html", NULL);
options(kableExtra_view_html = FALSE);

if (!exists('headingLevel') || !is.numeric(headingLevel) || (length(headingLevel) != 1)) {
  headingLevel <- 0;
}


## ----results='asis', eval=TRUE------------------------------------------------
ufs::heading("Determinant Selection Table",
             headingLevel=headingLevel);

## ----echo=echoPartial, results='asis', eval=TRUE------------------------------

if (requireNamespace("kableExtra", quietly = TRUE)) {
  print(
    prettyDeterminantSelectionTable(
      x,
      digits=digits
    )
  );
} else {
  knitr::kable(x,
               digits=digits,
               row.names=FALSE);
}


## ----echo=FALSE---------------------------------------------------------------
if (!is.null(oldKableViewOption)) {
  options(kableExtra_view_html = oldKableViewOption);
}

## -----------------------------------------------------------------------------
behaviorchange::determinant_selection_table(
  data=dat,
  determinants = subdets,
  target = 'highDose_intention',
  sortBy=6
);

## ----labels-------------------------------------------------------------------

translations <- matrix(
  c("If I use a high dose of ecstasy,\nmy trip is...", "shorter", "longer",
    "If I use a high dose of ecstasy,\nmy trip is...", "more mild", "more intense",
    "If I use a high dose of ecstasy,\nI get...", "much less\nintoxicated", "much more\nintoxicated",
    "A high dose of ecstasy gives me...", "much less\nenergy", "much more\nenergy",
    "With a high dose of ecstasy,\nthe happy, euphoric feeling is...", "much weaker", "much stronger",
    "If I use a high dose of ecstasy,\nI learn...", "much less\nabout myself", "much more\nabout myself",
    "If I use a high dose of ecstasy,\nI feel...", "much less\nconnected\n to others", "much more\nconnected\nto others",
    "If I use a high dose of ecstasy,\nI can make contact with others...", "much harder", "much easier",
    "If I use a high dose of ecstasy,\nI feel like having sex...", "much less", "much more",
    "If I use a high dose of ecstasy,\nI forget my problems...", "much slower", "much faster",
    "If I use a high dose of ecstasy,\nI feel...", "much less\nself-absorbed", "much more\nself-absorbed",
    "With a high dose of ecstasy,\nI can test my boundaries...", "much less", "much better",
    "If I use a high dose of ecstasy,\nthe music sounds...", "much worse", "much better",
    "If I use a high dose of ecstasy,\nI hallucinate...", "much less", "much more",
    "If I use a high dose of ecstasy,\nI feel time passes...", "much slower", "much faster",
    "If I use a high dose of ecstasy,\nafterwards I remember...", "much less", "much more",
    "For my health, using a high\ndose of ecstasy is...", "much worse", "much better",
    "My experience with using a high\ndose of ecstasy is...", "worse", "better",
    "Do you worry as much, more, or less\nabout the physical side effects of\nusing a high dose of ecstasy?", "worry\nmuch less", "worry\nmuch more",
    "Do you worry as much, more, or less\nabout the emotional and psychic side\neffects of using a high dose of ecstasy?", "worry\nmuch less", "worry\nmuch more",
    "Do you have as much, more, or less\nregret after using a high dose of ecstasy?", "much less\nregret", "much more\nregret"), ncol=3, byrow=TRUE);

subQuestions <- translations[, 1];
leftAnchors <- translations[, 2];
rightAnchors <- translations[, 3];

subDeterminantLabels <-
  paste0(subQuestions, " [ ",
         leftAnchors, " - ",
         rightAnchors, " ]");


## ----eval=TRUE, echo=FALSE----------------------------------------------------

oldKableViewOption <- getOption("kableExtra_view_html", NULL);
options(kableExtra_view_html = FALSE);

if (!exists('headingLevel') || !is.numeric(headingLevel) || (length(headingLevel) != 1)) {
  headingLevel <- 0;
}


## ----results='asis', eval=TRUE------------------------------------------------
ufs::heading("Determinant Selection Table",
             headingLevel=headingLevel);

## ----echo=echoPartial, results='asis', eval=TRUE------------------------------

if (requireNamespace("kableExtra", quietly = TRUE)) {
  print(
    prettyDeterminantSelectionTable(
      x,
      digits=digits
    )
  );
} else {
  knitr::kable(x,
               digits=digits,
               row.names=FALSE);
}


## ----echo=FALSE---------------------------------------------------------------
if (!is.null(oldKableViewOption)) {
  options(kableExtra_view_html = oldKableViewOption);
}

## -----------------------------------------------------------------------------
behaviorchange::determinant_selection_table(
  data=dat,
  determinants = subdets,
  target = 'highDose_intention',
  sortBy=6,
  determinantLabels = subDeterminantLabels
);

