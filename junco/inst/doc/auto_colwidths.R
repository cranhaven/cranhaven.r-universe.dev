## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

# TODO change to pharmaverse data to parmaversejnj versions

## -----------------------------------------------------------------------------
library(junco)

adsl2 <- ex_adsl
adsl2$ARM2 <- adsl2$ARM
levels(adsl2$ARM2) <- c("A", "B", "C")
adsl2$ARM3 <- adsl2$ARM
levels(adsl2$ARM3) <- c("Full Drug Name Of Drug X", "Current Best-Practice Standard Of Care", "The Weird Other Arm")

## col-labels unmodified (middling width)
lyt1 <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("RACE") |>
  summarize_row_groups(format = "xx (xx.xx%)") |>
  analyze("DCSREAS")

tbl1 <- build_table(lyt1, adsl2)

head(tbl1)

## super narrow column labels
lyt2 <- basic_table() |>
  split_cols_by("ARM", labels_var = "ARM2") |>
  split_rows_by("RACE") |>
  summarize_row_groups(format = "xx (xx.xx%)") |>
  analyze("DCSREAS")

tbl2 <- build_table(lyt2, adsl2)

head(tbl2)

## super wide column labels
lyt3 <- basic_table() |>
  split_cols_by("ARM", labels_var = "ARM3") |>
  split_rows_by("RACE") |>
  summarize_row_groups(format = "xx (xx.xx%)") |>
  analyze("DCSREAS")

tbl3 <- build_table(lyt3, adsl2)
head(tbl3)

## -----------------------------------------------------------------------------
propose_column_widths(tbl1)

## -----------------------------------------------------------------------------
propose_column_widths(tbl2)

## -----------------------------------------------------------------------------
propose_column_widths(tbl3)

## -----------------------------------------------------------------------------
def_colwidths(tbl1, fontspec = font_spec(), label_width_ins = 2, col_gap = 0)
def_colwidths(tbl2, fontspec = font_spec(), label_width_ins = 2, col_gap = 0)
def_colwidths(tbl3, fontspec = font_spec(), label_width_ins = 2, col_gap = 0)

## -----------------------------------------------------------------------------
## bigger than 2, but not what we got from propose_column_labels
def_colwidths(tbl1, fontspec = font_spec(), label_width_ins = 2.2, col_gap = 0)
## bigger than required so we get same row label width as propose_column_widths
def_colwidths(tbl1, fontspec = font_spec(), label_width_ins = 6, col_gap = 0)

## -----------------------------------------------------------------------------
fspec_times <- font_spec("Times", 9)
propose_column_widths(tbl3, fontspec = fspec_times)

## -----------------------------------------------------------------------------
def_colwidths(tbl3, fontspec = fspec_times, label_width_ins = 2, col_gap = 0)

## -----------------------------------------------------------------------------
sum(propose_column_widths(tbl3, fontspec = fspec_times))

## -----------------------------------------------------------------------------
formatters::page_lcpp(fontspec = fspec_times)$cpp

## -----------------------------------------------------------------------------
library(rlistings)

adae <- pharmaverseadam::adae
adae$AEOUT <- gsub("/", " / ", adae$AEOUT)
adsl <- pharmaverseadam::adsl

adsl <- adsl[, c("USUBJID", setdiff(names(adsl), names(adae)))]

lstdat <- merge(adae, adsl, by = "USUBJID")
var_labels(lstdat) <- c(var_labels(adae), var_labels(adsl)[-1])
lstdat$demog <- with_label(paste(lstdat$RACE, lstdat$SEX, lstdat$AGE, sep = " / "), "Demographic Information")

lsting <- as_listing(lstdat,
  key_cols = c("USUBJID"),
  disp_cols = c("ACTARM", "COUNTRY", "demog", "AESEV", "AEBODSYS", "AEDECOD", "ASTDTM", "AENDTM", "AEOUT", "EOSSTT")
)

## -----------------------------------------------------------------------------
demcell <- lstdat$demog[nrow(lstdat)]
demcell

## -----------------------------------------------------------------------------
wrds <- strsplit(demcell, "[ -]")[[1]]
wrds

## -----------------------------------------------------------------------------
max(nchar(wrds))

## -----------------------------------------------------------------------------
packed_widths <- function(...) {
  lst <- list(...)
  nchar(vapply(lst, paste, collapse = " ", ""))
}
packed_widths(
  wrds[1:2],
  wrds[3],
  wrds[4],
  wrds[5:8]
)

## -----------------------------------------------------------------------------
packed_widths(wrds[1:3], wrds[4], wrds[5:8])

## -----------------------------------------------------------------------------
packed_widths(
  wrds[1:3],
  wrds[4:8]
)

## -----------------------------------------------------------------------------
fspec_times8 <- font_spec("Times", 8, 1)
cw <- listing_column_widths(lsting, col_gap = 0, fontspec = fspec_times8, verbose = TRUE)

txt <- export_as_txt(lsting,
  pg_width = inches_to_spaces(8.88, fontspec = fspec_times8),
  lpp = NULL, colwidths = cw,
  fontspec = fspec_times8, col_gap = 0
)

txt2 <- strsplit(txt, "\n", fixed = FALSE)[[1]]
head(txt2)
length(txt2)

## -----------------------------------------------------------------------------
txtbad <- export_as_txt(lsting,
  pg_width = inches_to_spaces(8.88, fontspec = fspec_times8),
  lpp = NULL, colwidths = rep(floor(320 / 11), 11),
  fontspec = fspec_times8, col_gap = 0
)
txt2bad <- strsplit(txtbad, "\n", fixed = TRUE)[[1]]
head(txt2bad)
length(txt2bad)

## ----calc, include = FALSE----------------------------------------------------
saved <- (length(txt2bad) - length(txt2)) / length(txt2bad) * 100
dec_saved <- round(saved, 2)

