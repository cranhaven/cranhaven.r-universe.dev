## ---- eval = F, include=FALSE-------------------------------------------------
#  knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
#  library(devtools); load_all()

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DemoKin)
library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)

## -----------------------------------------------------------------------------
fra_fert_f <- fra_asfr_sex[,"ff"]
fra_fert_m <- fra_asfr_sex[,"fm"]
fra_surv_f <- fra_surv_sex[,"pf"]
fra_surv_m <- fra_surv_sex[,"pm"]

sum(fra_fert_m)-sum(fra_fert_f)

data.frame(value = c(fra_fert_f, fra_fert_m, fra_surv_f, fra_surv_m),
           age = rep(0:100, 4),
           sex = rep(c(rep("f", 101), rep("m", 101)), 2),
           risk = c(rep("fertility rate", 101 * 2), rep("survival probability", 101 * 2))) %>%
  ggplot(aes(age, value, col=sex)) + 
  geom_line() + 
  facet_wrap(~ risk, scales = "free_y") + 
  theme_bw()

## -----------------------------------------------------------------------------
kin_result <- kin2sex(
  pf = fra_surv_f,
  pm = fra_surv_m, 
  ff = fra_fert_f, 
  fm = fra_fert_m, 
  time_invariant = TRUE,
  sex_focal = "f", 
  birth_female = .5
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
kin_out <- kin_result$kin_summary %>% 
  mutate(kin = case_when(kin %in% c("s", "s") ~ "s",
                         kin %in% c("ya", "oa") ~ "a",
                         T ~ kin)) %>%
  filter(kin %in% c("d", "m", "gm", "ggm", "s", "a"))

kin_out %>% 
  group_by(kin, age_focal, sex_kin) %>%
  summarise(count=sum(count_living)) %>%
  ggplot(aes(age_focal, count, fill=sex_kin))+
  geom_area()+
  theme_bw() +
  facet_wrap(~kin)

## -----------------------------------------------------------------------------
kin_result$kin_summary %>% 
  filter(kin == "d", sex_kin == "m") %>% 
  head()

## ---- message=FALSE, warning=FALSE--------------------------------------------
kin_out %>% 
  group_by(kin, age_focal) %>%
  summarise(sex_ratio=sum(count_living[sex_kin=="m"], na.rm=T)/sum(count_living[sex_kin=="f"], na.rm=T)) %>%
  ggplot(aes(age_focal, sex_ratio))+
  geom_line()+
  theme_bw() +
  facet_wrap(~kin, scales = "free")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# sex ratio
kin_out %>%
  group_by(kin, sex_kin, age_focal) %>%
  summarise(count=sum(count_dead)) %>%
  ggplot(aes(age_focal, count, col=sex_kin))+
  geom_line()+
  theme_bw() +
  facet_wrap(~kin)

## -----------------------------------------------------------------------------
years <- ncol(swe_px)
ages <- nrow(swe_px)
swe_surv_f_matrix <- swe_px
swe_surv_m_matrix <- swe_px ^ 1.5 # artificial perturbation for this example
swe_fert_f_matrix <- swe_asfr
swe_fert_m_matrix <- rbind(matrix(0, 5, years),  
            swe_asfr[-((ages-4):ages),]) * 1.05 # artificial perturbation for this example

## -----------------------------------------------------------------------------
bind_rows(
  data.frame(age = 0:100, sex = "Female", component = "Fertility rate", value = swe_fert_f_matrix[,"1900"]),
  data.frame(age = 0:100, sex = "Male", component = "Fertility rate", value = swe_fert_m_matrix[,"1900"]),
  data.frame(age = 0:100, sex = "Female", component = "Survival probability", value = swe_surv_f_matrix[,"1900"]),
  data.frame(age = 0:100, sex = "Male", component = "Survival probability", value = swe_surv_m_matrix[,"1900"])) %>% 
  ggplot(aes(age, value, col = sex)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~component, scales = "free")

## -----------------------------------------------------------------------------
kin_out_time_variant <- kin2sex(
                      pf = swe_surv_f_matrix, 
                      pm = swe_surv_m_matrix,
                      ff = swe_fert_f_matrix, 
                      fm = swe_fert_m_matrix,
                      sex_focal = "f",
                      time_invariant = FALSE,
                      birth_female = .5,
                      output_cohort = 1900
                      )

## ---- message=FALSE, warning=FALSE--------------------------------------------
kin_out_time_invariant <- kin2sex(
                      swe_surv_f_matrix[,"1900"], swe_surv_m_matrix[,"1900"], 
                      swe_fert_f_matrix[,"1900"], swe_fert_m_matrix[,"1900"], 
                      sex_focal = "f", birth_female = .5)


kin_out_time_variant$kin_summary %>%
  filter(cohort == 1900) %>% mutate(type = "variant") %>%
  bind_rows(kin_out_time_invariant$kin_summary %>% mutate(type = "invariant")) %>% 
  mutate(kin = case_when(kin %in% c("ys", "os") ~ "s",
                         kin %in% c("ya", "oa") ~ "a",
                         T ~ kin)) %>%
  filter(kin %in% c("d", "m", "gm", "ggm", "s", "a")) %>%
  group_by(type, kin, age_focal, sex_kin) %>%
  summarise(count=sum(count_living)) %>%
  ggplot(aes(age_focal, count, linetype=type))+
  geom_line()+ theme_bw() +
  facet_grid(cols = vars(kin), rows=vars(sex_kin), scales = "free")

## ---- message=FALSE, warning=FALSE--------------------------------------------
kin_out <- kin2sex(fra_surv_f, fra_surv_m, fra_fert_f, fra_fert_m, sex_focal = "f", birth_female = .5)

kin_out_androgynous <- kin2sex(fra_surv_f, fra_surv_f, fra_fert_f, fra_fert_f, sex_focal = "f", birth_female = .5)

bind_rows(
  kin_out$kin_summary %>% mutate(type = "full"),
  kin_out_androgynous$kin_summary %>% mutate(type = "androgynous")) %>%
  group_by(kin, age_focal, sex_kin, type) %>%
  summarise(count = sum(count_living)) %>%
  ggplot(aes(age_focal, count, linetype = type)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_blank()) +
  facet_grid(row = vars(sex_kin), col = vars(kin), scales = "free")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# with gkp
kin_out_1sex <- kin(fra_surv_f, fra_fert_f, birth_female = .5)

kin_out_GKP <- kin_out_1sex$kin_summary%>%
  mutate(count_living = case_when(kin == "m" ~ count_living * 2,
                            kin == "gm" ~ count_living * 4,
                            kin == "ggm" ~ count_living * 8,
                            kin == "d" ~ count_living * 2,
                            kin == "gd" ~ count_living * 4,
                            kin == "ggd" ~ count_living * 4,
                            kin == "oa" ~ count_living * 4,
                            kin == "ya" ~ count_living * 4,
                            kin == "os" ~ count_living * 2,
                            kin == "ys" ~ count_living * 2,
                            kin == "coa" ~ count_living * 8,
                            kin == "cya" ~ count_living * 8,
                            kin == "nos" ~ count_living * 4,
                            kin == "nys" ~ count_living * 4))

bind_rows(
  kin_out$kin_summary %>% mutate(type = "full"),
  kin_out_androgynous$kin_summary %>% mutate(type = "androgynous"),
  kin_out_GKP %>% mutate(type = "gkp")) %>%
  mutate(kin = case_when(kin %in% c("ys", "os") ~ "s",
                          kin %in% c("ya", "oa") ~ "a",
                          kin %in% c("coa", "cya") ~ "c",
                          kin %in% c("nys", "nos") ~ "n",
                          T ~ kin)) %>%
  filter(age_focal %in% c(5, 15, 30, 60, 80)) %>%
  group_by(kin, age_focal, type) %>%
  summarise(count = sum(count_living)) %>%
  ggplot(aes(type, count)) +
  geom_bar(aes(fill=type), stat = "identity") +
  theme_bw()+theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")+
  facet_grid(col = vars(kin), row = vars(age_focal), scales = "free")

