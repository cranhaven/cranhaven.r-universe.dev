## ----echo=F, message=F, warning=F---------------------------------------------
knitr::opts_chunk$set(echo = F)
library(rfars)
library(stringr)
library(dplyr)
library(knitr)
library(tidyr)

## ----eval=F, echo=T-----------------------------------------------------------
#  View(rfars::fars_codebook)
#  View(rfars::gescrss_codebook)

## ----results='asis', warning=F, message=F-------------------------------------
temp <-
  fars_codebook %>%
  mutate(value_label = gsub("`", "", value_label)) %>%
  separate_rows(years) %>%
  rename(year=years) %>%
  filter(!(name_ncsa %in% c("VPICMODEL", "MAK_MOD", "STATE", "VPICMAKE", "ALC_RES", "DR_WGT", "AGE", "PBAGE", "DRUGRES1", "DRUGRES2", "DRUGRES3", "TRAV_SP"))) %>%
  filter(!(str_detect(name_ncsa, "HOUR"))) %>%
  filter(!(str_detect(name_ncsa, "MINUTE"))) %>%
  filter(!(str_detect(name_ncsa, "_HR"))) %>%
  filter(!(str_detect(name_ncsa, "_MIN"))) %>%
  filter(value != value_label) %>%
  filter(year %in% c(2015, 2020)) %>%
  select(label, name_rfars, file, value, year, value_label) %>%
  
  # Filter out numeric 1:1 translations (e.g., Age = 1 = 1, Speed = 50 = 50)
  mutate(
    v  = ifelse(is.na(as.numeric(value)),       "-x-x-x-x-x-x", as.numeric(value)),
    vl = ifelse(is.na(as.numeric(value_label)), "-x-x-x-x-x-x", as.numeric(value_label))
    ) %>% filter(v != vl) %>%
  select(-v, -vl) %>%
  
  pivot_wider(names_from = "year", values_from = "value_label") %>%
  filter(!(is.na(`2015`) & is.na(`2020`))) %>%
  arrange(label)

knitr::kable(temp, format = "pipe")

