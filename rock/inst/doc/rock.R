## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, results='asis'-----------------------------------------------
exampleROCK <-
  rock::as.rock_source(c(
    "Some example text,",
    "and some more.       [[look_a_code]]",
    "And the end."));
rock::prettify_source(
  exampleROCK
);

## ----echo=FALSE, results='asis'-----------------------------------------------
rock::prettify_source(
  system.file(
    "extdata", "example-1.rock",
    package="rock"
  )
);

