## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(knitr.kable.NA = '')

## ----import-------------------------------------------------------------------
data(cprd, package = 'doseminer')
str(cprd)

## ----message = FALSE----------------------------------------------------------
library(doseminer)

free_text <- with(cprd, text[!duplicated(text) & nchar(text) > 0])
extracted <- extract_from_prescription(free_text)
head(extracted)

## -----------------------------------------------------------------------------
dosages <- merge(extracted, cprd, by.x = 'raw', by.y = 'text', all.x = TRUE)
head(dosages)

## ---- warning = FALSE, message = FALSE----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
dosages %>%
  separate(dose, c('min_dose', 'max_dose'), sep = '-',
           convert = TRUE, fill = 'right') %>%
  mutate(dose = coalesce((min_dose + max_dose) / 2, min_dose),
         itvl = replace_na(as.numeric(itvl), 1),
         freq = as.numeric(freq),
         daily_dose = freq * dose / itvl,
         end_date = date + qty / daily_dose) %>%
  ggplot() +
  aes(y = as.factor(patid), xmin = date, xmax = end_date) +
  geom_errorbarh(height = .5) +
  ylab('patient ID')

