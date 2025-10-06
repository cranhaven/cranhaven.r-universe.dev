## ----message=F----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(rfars)

## ----results='asis'-----------------------------------------------------------
myFARS <- get_fars(years = 2023, proceed = TRUE)

counts(myFARS, involved = 'alcohol') %>% knitr::kable(format = "html")

## ----results='asis'-----------------------------------------------------------
counts(
  df = myFARS, 
  what = "fatalities",
  involved = 'alcohol'
) %>%
  knitr::kable(format = "html")

## -----------------------------------------------------------------------------
temp <- myFARS$flat %>% 
  select(year:per_no, age, sex, per_typ, inj_sev, alc_res, dr_drink, a1:a10) %>%
  filter(inj_sev == "Fatal Injury (K)")

for(i in 1:10) {
  imputation_col <- paste0("a", i)
  temp[[paste0("FPC", i)]] <- ifelse(temp[[imputation_col]] == 0, 1, 0)  # BAC = 0.00
  temp[[paste0("SPC", i)]] <- ifelse(temp[[imputation_col]] >= 1 & temp[[imputation_col]] <= 7, 1, 0)  # BAC = 0.01-0.07
  temp[[paste0("TPC", i)]] <- ifelse(temp[[imputation_col]] >= 8, 1, 0)  # BAC = 0.08+
}

## ----results='asis'-----------------------------------------------------------
temp %>% 
  select(st_case, a1:a10, starts_with("FPC"), starts_with("SPC"), starts_with("TPC")) %>% 
  slice(1:10) %>% 
  t() %>%
  knitr::kable(format = "html")

## ----results='asis'-----------------------------------------------------------
temp %>% 
  slice(1) %>% 
  select(st_case, a1:a10, starts_with("FPC"), starts_with("SPC"), starts_with("TPC")) %>%
  pivot_longer(-1) %>%
  mutate(
    iter = gsub("\\D", "", name),
    name = gsub("[^A-Za-z]", "", name)
  ) %>%
  pivot_wider() %>%
  knitr::kable(format = "html")

## -----------------------------------------------------------------------------
case_results <- list()

for(i in 1:10) {
  fpc_col <- paste0("FPC", i)
  spc_col <- paste0("SPC", i)
  tpc_col <- paste0("TPC", i)
  
  case_results[[i]] <-
    temp %>%
    summarise(
      TOTAL = n(),
      !!paste0("FSBAC", i) := sum(!!sym(fpc_col), na.rm = TRUE),
      !!paste0("SSBAC", i) := sum(!!sym(spc_col), na.rm = TRUE),
      !!paste0("TSBAC", i) := sum(!!sym(tpc_col), na.rm = TRUE),
      .groups = 'drop'
    )
}

## ----results='asis'-----------------------------------------------------------
bind_rows(
  data.frame(case_results[[1]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=1),
  data.frame(case_results[[2]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=2),
  data.frame(case_results[[3]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=3),
  data.frame(case_results[[4]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=4),
  data.frame(case_results[[5]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=5),
  data.frame(case_results[[6]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=6),
  data.frame(case_results[[7]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=7),
  data.frame(case_results[[8]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=8),
  data.frame(case_results[[9]])  %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=9),
  data.frame(case_results[[10]]) %>% select(TOTAL, FSBAC=2, SSBAC=3, TSBAC=4) %>% mutate(iter=10)
  ) %>%
  knitr::kable(format = "html")

## -----------------------------------------------------------------------------
calc <- case_results[[1]]
  for(i in 2:10) {
    calc <- calc %>% bind_cols(case_results[[i]] %>% select(-TOTAL))
  }

calc <-
  calc %>%
  rowwise() %>%
  mutate(
    SBAC0 = round(mean(c_across(starts_with("FSBAC")), na.rm = TRUE)), # BAC 0.00
    SBAC1 = round(mean(c_across(starts_with("SSBAC")), na.rm = TRUE)), # BAC 0.01-0.07
    SBAC2 = round(mean(c_across(starts_with("TSBAC")), na.rm = TRUE))  # BAC 0.08+
  ) %>%
  ungroup()

## ----results='asis'-----------------------------------------------------------
select(calc, SBAC0:SBAC2) %>% knitr::kable(format = "html")

## -----------------------------------------------------------------------------
x <-
  myFARS$flat %>% 
  select(year:per_no, age, sex, per_typ, inj_sev, alc_res, dr_drink, a1:a10) %>%
  filter(inj_sev == "Fatal Injury (K)") %>%
  mutate_at(paste0("a", 1:10), function(x) 1*(x>=8)) %>%
  group_by(year) %>%
  summarize_at(paste0("a", 1:10), sum, na.rm=T) %>%
  rowwise() %>%
  mutate(a = round(mean(c_across(a1:a10)))) 

x$a

