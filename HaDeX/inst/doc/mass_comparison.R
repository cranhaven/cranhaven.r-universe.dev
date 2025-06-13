## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))

options(digits=3)

library(tidyr)
library(dplyr)

calculate_mass <- function(dat){
  
  proton_mass <- 1.00727647
  
  dat %>%
    mutate(exp_mass = Center*z - z*proton_mass) %>%
    select(-Center, -z, -Modification, -Fragment) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein, File) %>%
    summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
    ungroup(.) %>% 
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein) %>%
    summarize(mass = mean(avg_exp_mass, na.rm = TRUE),
              err_mass = coalesce(sd(avg_exp_mass, na.rm = TRUE)/sqrt(sum(!is.na(avg_exp_mass))), 0),
              num = (sum(!is.na(avg_exp_mass)))) %>%
    ungroup(.) %>%
    arrange(Start, End, Start, Exposure) %>%
    as.data.frame()
  
}

calculate_mass_no_inten <- function(dat){
  
  proton_mass <- 1.00727647
  
  dat %>%
    mutate(exp_mass = Center*z - z*proton_mass) %>%
    select(-Center, -z, -Modification, -Fragment) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein, File) %>%
    summarize(avg_exp_mass = mean(exp_mass, na.rm = TRUE)) %>%
    ungroup(.) %>% 
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein) %>%
    summarize(mass = mean(avg_exp_mass, na.rm = TRUE),
              err_mass = coalesce(sd(avg_exp_mass, na.rm = TRUE)/sqrt(sum(!is.na(avg_exp_mass))), 0)) %>%
    ungroup(.) %>%
    arrange(Start, End, Start, Exposure) %>%
    as.data.frame()
  
}


## -----------------------------------------------------------------------------
library(HaDeX)
dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

## ----eval=FALSE---------------------------------------------------------------
#  avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)

## ----include=FALSE------------------------------------------------------------
dat_no_weight <- calculate_mass_no_inten(dat) %>%
  arrange(Start, End, State, Exposure) %>%
  mutate(source = "no_weight") %>%
  select(Sequence, Start, End, State, Exposure, mass, err_mass, source)

dat_weight <- calculate_mass(dat) %>%
  arrange(Start, End, State, Exposure) %>%
  mutate(source = "weight") %>%
  select(Sequence, Start, End, State, Exposure, mass, err_mass, source)

tmp <- bind_rows(dat_no_weight, dat_weight) %>%
  gather(type, value, -Sequence, -Start, -End, -source, -State, -Exposure) %>%
  spread(source, value) %>%
  mutate(diff = (weight-no_weight))

## ----echo=FALSE---------------------------------------------------------------
tmp %>%
  filter(type == "mass") %>%
  select(-type) %>%
  filter(Sequence == "ARSQKSGIRLQGHF")

## ----echo=FALSE---------------------------------------------------------------
tmp %>%
  filter(type == "err_mass") %>%
  select(-type) %>%
  filter(Sequence == "ARSQKSGIRLQGHF")

## ----echo=FALSE---------------------------------------------------------------
mean(tmp[ tmp[["type"]] == "mass" , "diff"])

## ----echo=FALSE---------------------------------------------------------------
mean(tmp[ tmp[["type"]] == "err_mass" , "diff"])

## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
library(gridExtra)
library(ggplot2)

p1 <- ggplot(filter(tmp, type == "mass"), aes(diff)) +
  geom_histogram() +
  labs(title = "Differences between mass",
       x = "Difference")

p2 <- ggplot(filter(tmp, type == "err_mass"), aes(diff)) +
  geom_histogram() +
  labs(title = "Differences between uncertainties of mass",
       x = "Difference")

grid.arrange(p1, p2, ncol=2)

