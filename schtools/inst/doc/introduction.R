## ----setup, include = FALSE---------------------------------------------------
schtools::set_knitr_opts()
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>"
)

## ----deps---------------------------------------------------------------------
library(schtools)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

## ----calc_relabun-------------------------------------------------------------
shared_dat <- read_tsv(system.file("extdata", "test.shared",
  package = "schtools"
))
relabun_dat <- shared_dat %>% calc_relabun()
head(relabun_dat)

## ----pivot_wider--------------------------------------------------------------
wide_dat <- relabun_dat %>%
  pivot_wider(names_from = "otu", values_from = "rel_abun")
head(wide_dat)

## ----sum1---------------------------------------------------------------------
wide_dat %>%
  select(starts_with("Otu")) %>%
  rowSums()

## ----read_tax-----------------------------------------------------------------
tax_dat <- read_tax(system.file("extdata", "test.taxonomy",
  package = "schtools"
))
head(tax_dat)

## ----italic-genus-------------------------------------------------------------
library(ggtext)
set.seed(20220427)

relabun_dat %>%
  mutate(
    sample_num = stringr::str_remove(sample, "p") %>% as.integer(),
    treatment = case_when(
      sample_num %% 2 == 1 ~ "A",
      TRUE ~ "B"
    )
  ) %>%
  inner_join(tax_dat, by = "otu") %>%
  ggplot(aes(x = rel_abun, y = label_html, color = treatment)) +
  geom_jitter(alpha = 0.7, height = 0.2) +
  labs(x = "Relative abundance", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_markdown())

## ----pool_genus---------------------------------------------------------------
tax_dat <- read_tax(system.file("extdata", "test.taxonomy",
  package = "schtools"
))
shared_dat <- readr::read_tsv(system.file("extdata", "test.shared",
  package = "schtools"
))
pool_taxon_counts(shared_dat, tax_dat, "genus")

## ----pool_phylum--------------------------------------------------------------
pool_taxon_counts(shared_dat, tax_dat, "phylum")

## ----read_dist----------------------------------------------------------------
dist_filepath <- system.file("extdata",
  "sample.final.thetayc.0.03.lt.ave.dist",
  package = "schtools"
)
dist_tbl <- read_dist(dist_filepath)
head(dist_tbl)

## ----oxford-------------------------------------------------------------------
animals <- c("cats", "dogs", "fish")

