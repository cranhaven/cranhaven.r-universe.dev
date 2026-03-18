## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  message = FALSE
)

## ----echo = FALSE-------------------------------------------------------------
tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,  
  "1",     1,   1,   5,
  "1",     2, 1.5,  7.5,
  "1",     3,   2,  10,
  "2",     1,   5,  14,
  "2",     2,   4,  10.5,
  "2",     3,   3,   7,
  "3",     1,  15,   8,
  "3",     2,  12,  12,
  "3",     3,   9,  16
)

## ----echo = FALSE, message = FALSE--------------------------------------------
library(panelr)
wide <- widen_panel(panel_data(tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,
  "1",     1,   1,   5,
  "1",     2, 1.5,  7.5,
  "1",     3,   2,  10,
  "2",     1,   5,  14,
  "2",     2,   4,  10.5,
  "2",     3,   3,   7,
  "3",     1,  15,   8,
  "3",     2,  12,  12,
  "3",     3,   9,  16
)), separator = "_W")

(wide <- wide[c("id", "Q1_W1", "Q1_W2", "Q1_W3", "Q2_W1", "Q2_W2", "Q2_W3")])

## ----echo = FALSE-------------------------------------------------------------
wide

## -----------------------------------------------------------------------------
long_panel(wide, prefix = "_W", begin = 1, end = 3, label_location = "end")

## -----------------------------------------------------------------------------
reshape(as.data.frame(wide), sep = "_W", times = c(1, 2, 3), direction = "long",
        varying = c("Q1_W1", "Q1_W2", "Q1_W3", "Q2_W1", "Q2_W2", "Q2_W3"))

## ----echo = FALSE-------------------------------------------------------------
(wide <- tibble::tribble(
  ~WA_Q1, ~WB_Q1, ~WC_Q1, ~WA_Q2, ~WC_Q2,
  1,      1.5,     2,      5,      10,
  5,      4,       3,      14,     7,
  15,     12,      9,      8,      16
))

## -----------------------------------------------------------------------------
long_panel(wide, prefix = "W", suffix = "_", label_location = "beginning",
           begin = "A", end = "C")

## ----echo = FALSE-------------------------------------------------------------
(wide <- tibble::tribble(
  ~id, ~Q1_W1,   ~Q1_W2, ~Q1_W3, ~race_W1,
    1,      1,      1.5,      2,  "white",      
    2,      4,        3,      2,  "black",
    3,     15,       12,      9,  "white",      
))

## ----echo = FALSE-------------------------------------------------------------
tibble::tribble(
  ~id, ~wave,    ~race, ~Q1,
    1,     1,  "white",   1,
    1,     2,       NA, 1.5,
    1,     3,       NA,   2,
    2,     1,  "black",   4,
    2,     2,       NA,   3,
    2,     3,       NA,   2,
    3,     1,  "white",  15,
    3,     2,       NA,  12,
    3,     3,       NA,   9
)

## -----------------------------------------------------------------------------
long_panel(wide, prefix = "_W", label_location = "end", begin = 1, end = 3)

## ----echo = FALSE-------------------------------------------------------------
(wide <- tibble::tribble(
  ~CaseID, ~Consent, ~A1, ~B1, ~C1, 
        1,     TRUE,   5,   4,   3,
        2,     TRUE,   6,   7,   8,
        3,     TRUE,  10,   8,   6
))

## -----------------------------------------------------------------------------
long_panel(wide, begin = "A", end = "C", label_location = "beginning", id = "CaseID")

## -----------------------------------------------------------------------------
long_panel(wide, begin = "A", end = "C", label_location = "beginning", 
           id = "CaseID", match = "\\d+.*")

## ----echo = FALSE-------------------------------------------------------------
(long_data <- panel_data(tibble::tribble(
  ~person, ~time, ~Q1, ~Q2,   ~race,
  "1",     1,       1,   5, "white",
  "1",     2,     1.5, 7.5, "white",
  "1",     3,       2,  10, "white",
  "2",     1,       5,  14, "black",
  "2",     2,       4,10.5, "black",
  "2",     3,       3,   7, "black",
  "3",     1,      15,   8, "white",
  "3",     2,      12,  12, "white",
  "3",     3,       9,  16, "white"
), id = person, wave = time))

## -----------------------------------------------------------------------------
widen_panel(long_data, separator = "_")

