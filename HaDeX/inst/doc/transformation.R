## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))

## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
library(HaDeX)
library(dplyr) 
library(ggplot2)

tmp_seq <- "LCKDRSGDCSPETSLKQLRLKRDPGIDGVGEISSQL"
tmp_state <- "CD160"
tmp_time <- "1 min"
proton_mass <- 1.00727647

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv")) %>%
  filter(Exposure == 1, State == "CD160", Sequence ==  "LCKDRSGDCSPETSLKQLRLKRDPGIDGVGEISSQL") 

## -----------------------------------------------------------------------------
dat

## ----echo=FALSE---------------------------------------------------------------
dat %>%
  filter(File == "KD_160530_CD160_1min_01") %>%
ggplot() +
  geom_segment(aes(x = Start, xend = End, y = Center, yend = Center, colour = z)) +
  coord_cartesian( xlim = c(30, 75)) +
  labs(title = paste0("Measurements for sequence in ", tmp_state, " state"),
       x = "Position in the sequence")

## ----echo=FALSE---------------------------------------------------------------
dat %>%
  filter(File == "KD_160530_CD160_1min_01") %>%
mutate(exp_mass = z*(Center - proton_mass)) %>%
  ggplot() +
  geom_segment(aes(x = Start, xend = End, y = exp_mass, yend = exp_mass, colour = z)) +
  coord_cartesian( xlim = c(30, 75)) +
  labs(title = "", # paste0("Measurements for sequence in ", tmp_state, " state"),
       x = "Position in the sequence",
       y = "Measured mass [Da]")

## ----echo=FALSE---------------------------------------------------------------
dat %>%
mutate(exp_mass = z*(Center - proton_mass)) %>%
  ggplot() +
  geom_segment(aes(x = Start, xend = End, y = exp_mass, yend = exp_mass, colour = z, linetype = File)) +
  coord_cartesian( xlim = c(30, 75)) +
  labs(title = "", # paste0("Measurements for sequence in ", tmp_state, " state"),
       x = "Position in the sequence",
       y = "Measured mass [Da]")

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
dat %>%
  mutate(exp_mass = z*(Center - proton_mass)) %>%
  group_by(Sequence, Start, End, File) %>%
  summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
  ungroup(.) %>%
  ggplot() +
  geom_segment(aes(x = Start, xend = End, y = avg_exp_mass, yend = avg_exp_mass, linetype = File)) +
  coord_cartesian( xlim = c(30, 75), ylim = c(3920.3, 3920.6)) +
  labs(title = "", # paste0("Measurements for sequence in ", tmp_state, " state"),
       x = "Position in the sequence",
       y = "Measured mass [Da]")

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
dat %>%
  mutate(exp_mass = z*(Center - proton_mass)) %>%
  group_by(Sequence, Start, End, File) %>%
  summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
  ungroup(.) %>%
  group_by(Sequence, Start, End) %>%
  summarize(aggMass = mean(avg_exp_mass),
            err_aggMass = sd(avg_exp_mass)) %>%
  ggplot() +
  geom_segment(aes(x = Start, xend = End, y = aggMass, yend = aggMass)) +
  geom_errorbar(aes(x = 51.5, ymin = aggMass - err_aggMass, ymax = aggMass + err_aggMass)) + 
  coord_cartesian( xlim = c(30, 75), ylim = c(3920.3, 3920.6)) +
  labs(title = paste0("Measurements for sequence in ", tmp_state, " state"),
       x = "Position in the sequence",
       y = "Measured mass [Da]")

