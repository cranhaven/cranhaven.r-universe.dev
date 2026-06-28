## ---- eval = F, include=FALSE-------------------------------------------------
#  knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
#  library(devtools); load_all()

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DemoKin)
library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
# First, get vectors for a given year
swe_surv_2015 <- swe_px[,"2015"]
swe_asfr_2015 <- swe_asfr[,"2015"]
# Run kinship models
swe_2015 <- kin(p = swe_surv_2015, f = swe_asfr_2015, time_invariant = TRUE)

## -----------------------------------------------------------------------------
head(swe_2015$kin_full)

## -----------------------------------------------------------------------------
head(swe_2015$kin_summary)

## ---- message=FALSE, warning=FALSE--------------------------------------------
kin_summary_example <- 
  swe_2015$kin_full %>% 
  select(year, cohort, kin, age_focal, age_kin, living, dead) %>% 
  group_by(year, cohort, kin, age_focal) %>% 
  summarise(count_living = sum(living)) 

head(kin_summary_example)

## ---- fig.height=6, fig.width=8-----------------------------------------------
swe_2015[["kin_summary"]] %>%
  ggplot() +
  geom_line(aes(age_focal, count_living)) +
  theme_bw() +
  labs(y = "Expected number of living relatives") +
  facet_wrap(~kin)

## ---- fig.height=6, fig.width=8, echo=FALSE-----------------------------------
demokin_codes %>% 
  kable

## ---- fig.height=6, fig.width=8-----------------------------------------------
swe_2015[["kin_full"]] %>%
  filter(age_focal == 35) %>% 
  ggplot() +
  geom_line(aes(age_kin, living))  +
  geom_vline(xintercept = 35, color=2) +
  labs(y = "Expected number of living relatives") +
  theme_bw() +
  facet_wrap(~kin)

## ---- fig.height=6, fig.width=8-----------------------------------------------
swe_2015[["kin_summary"]] %>% 
  filter(age_focal == 35) %>% 
  select(kin, count_living, mean_age, sd_age) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable()

## ---- fig.height=6, fig.width=8, dpi=900, message=FALSE, warning=FALSE--------
swe_2015[["kin_summary"]] %>% 
  filter(age_focal == 35) %>% 
  select(kin, count = count_living) %>% 
  plot_diagram(rounding = 2)

## ---- fig.height=6, fig.width=8-----------------------------------------------
swe_time_varying <- 
  kin(
    p = swe_px,
    f = swe_asfr,
    n = swe_pop,
    time_invariant =FALSE,
    output_cohort = 1960,
    output_kin = c("d","gd","ggd","m","gm","ggm")
    )

swe_time_varying$kin_summary %>%
  ggplot(aes(age_focal,count_living,color=factor(cohort))) +
  scale_y_continuous(name = "",labels = seq(0,3,.2),breaks = seq(0,3,.2))+
  geom_line(color = 1)+
  geom_vline(xintercept = 35, color=2)+
  labs(y = "Expected number of living relatives") +
  facet_wrap(~kin,scales = "free")+
  theme_bw()


## ---- fig.height=6, fig.width=8, message=FALSE, warning=FALSE-----------------
swe_time_varying$kin_summary %>%
  ggplot() +
  geom_line(aes(age_focal, count_cum_dead)) +
  labs(y = "Expected number of deceased relatives") +
  theme_bw() +
  facet_wrap(~kin,scales="free")

## -----------------------------------------------------------------------------
swe_time_varying$kin_summary %>% 
  filter(age_focal == 50) %>% 
  select(kin,count_cum_dead,mean_age_lost) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable()

## ---- message=FALSE, warning=FALSE, fig.height=6, fig.width=10----------------
# let´s create some prevalence by age
swe_2015_prevalence <- 
  tibble(
    age_kin = unique(swe_2015$kin_full$age_kin),
    prev = .005 * exp(.05 * age_kin)
    )
# join to kin count estimates and plot
swe_2015$kin_full %>% 
  left_join(swe_2015_prevalence) %>% 
  group_by(kin, age_focal) %>% 
  summarise(
    prevalent = sum(living * prev),
    no_prevalent = sum(living * (1-prev))
    ) %>% 
  pivot_longer(cols = prevalent:no_prevalent, names_to = "prevalence_state", values_to = "count") %>% 
  ggplot(aes(x=age_focal, y = count)) + 
  geom_area(aes(fill=prevalence_state)) +
  facet_wrap(~kin) +
  theme_bw()


## -----------------------------------------------------------------------------
# use birth_female=1 because fertility is for female only
demokin_svk1980_caswell2020 <- 
  kin_multi_stage(
    U = svk_Uxs,
    f = svk_fxs,
    D = svk_pxs,
    H = svk_Hxs, 
    birth_female=1,
    parity = TRUE)

## ---- message=FALSE, warning=FALSE, fig.height=6, fig.width=10----------------
demokin_svk1980_caswell2020 %>% 
  filter(kin %in% c("oa","ya"), age_focal %in% c(20,60)) %>% 
  mutate(parity = as.integer(stage_kin)-1,
         parity = case_when(parity == 5 ~ "5+", T ~ as.character(parity))
         ) %>% 
  group_by(age_focal, age_kin, parity) %>% 
  summarise(count= sum(living)) %>% 
  ggplot() +
  geom_bar(aes(x=age_kin, y = count, fill=parity), stat = "identity") +
  geom_vline(aes(xintercept = age_focal), col=2) +
  labs(y = "Number of aunts") +
  theme_bw() +
  facet_wrap(~age_focal, nrow = 2)

## ---- message=FALSE, warning=FALSE, fig.height=6, fig.width=10----------------
demokin_svk1980_caswell2020 %>% 
  filter(kin %in% c("d","m")) %>% 
  mutate(parity = as.integer(stage_kin)-1,
         parity = case_when(parity == 5 ~ "5+", T ~ as.character(parity))) %>% 
  group_by(age_focal, kin, parity) %>% 
  summarise(count= sum(living)) %>% 
  DemoKin::rename_kin() %>% 
  ggplot() +
  geom_bar(aes(x=age_focal, y = count, fill=parity), stat = "identity") +
  labs(y = "Kin count") +
  theme_bw() +
  facet_wrap(~kin, nrow = 2)

