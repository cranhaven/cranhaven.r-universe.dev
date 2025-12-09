## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ibuprofen----------------------------------------------------------------
p1 <- 'Take 1 or 2 tablets up to 3 times a day, as required'

## -----------------------------------------------------------------------------
library(doseminer)
extract_from_prescription(p1)

## -----------------------------------------------------------------------------
p2 <- c('Take 1 or 2 tablets up to 3 times a day, as required',
        'Swallow 1 or 2 capsules with water, up to three times a day as required.',
        'Two to four 5ml spoonfuls up to 4 times a day')
extract_from_prescription(p2)

## -----------------------------------------------------------------------------
library(tidyr)
extract_from_prescription(p1) %>%
  separate_rows(freq:dose, convert = TRUE)

## -----------------------------------------------------------------------------
extract_from_prescription(example_prescriptions)

## -----------------------------------------------------------------------------
words2number(c('one', 'two', 'three', 'forty two', 'one million'))

## -----------------------------------------------------------------------------
replace_numbers(c('I have three apples',
                  'The answer is forty two',
                  'Take one and a half tablets'))

## -----------------------------------------------------------------------------
words2number(c('three point one four', 'four fifths'))

