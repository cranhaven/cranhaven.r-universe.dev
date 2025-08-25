## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, message = FALSE, warning = FALSE, include = FALSE-----------------
library(tidycomm)

## -----------------------------------------------------------------------------
WoJ

## -----------------------------------------------------------------------------
WoJ %>% 
  crosstab(reach, employment)

## -----------------------------------------------------------------------------
WoJ %>% 
  crosstab(reach, employment, add_total = TRUE, percentages = TRUE)

## -----------------------------------------------------------------------------
WoJ %>% 
  crosstab(reach, employment, chi_square = TRUE)

## -----------------------------------------------------------------------------
WoJ %>% 
  crosstab(reach, employment, country, percentages = TRUE)

## -----------------------------------------------------------------------------
WoJ %>% 
  crosstab(reach, employment, percentages = TRUE) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  crosstab(reach, employment) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  t_test(temp_contract, autonomy_selection, autonomy_emphasis)

## -----------------------------------------------------------------------------
WoJ %>% 
  t_test(temp_contract)

## -----------------------------------------------------------------------------
WoJ %>% 
  t_test(employment, autonomy_selection, autonomy_emphasis)

WoJ %>% 
  t_test(employment, autonomy_selection, autonomy_emphasis, levels = c("Full-time", "Freelancer"))

## -----------------------------------------------------------------------------
WoJ %>% 
  t_test(autonomy_emphasis, mu = 3.9)

## -----------------------------------------------------------------------------
WoJ %>% 
  t_test(temp_contract, autonomy_selection, autonomy_emphasis) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  unianova(employment, autonomy_selection, autonomy_emphasis)

## -----------------------------------------------------------------------------
WoJ %>% 
  unianova(employment, descriptives = TRUE)

## -----------------------------------------------------------------------------
WoJ %>% 
  unianova(employment, autonomy_selection, autonomy_emphasis, post_hoc = TRUE)

## -----------------------------------------------------------------------------
WoJ %>% 
  unianova(employment, autonomy_selection, autonomy_emphasis, post_hoc = TRUE) %>% 
  dplyr::select(Variable, post_hoc) %>% 
  tidyr::unnest(post_hoc)

## -----------------------------------------------------------------------------
WoJ %>% 
  unianova(employment, autonomy_selection, autonomy_emphasis) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  correlate(work_experience, autonomy_selection, autonomy_emphasis)

## -----------------------------------------------------------------------------
WoJ %>% 
  correlate()

## -----------------------------------------------------------------------------
WoJ %>% 
  correlate(autonomy_selection, autonomy_emphasis, with = work_experience)

## -----------------------------------------------------------------------------
WoJ %>% 
  correlate(autonomy_selection, autonomy_emphasis, partial = work_experience)

## -----------------------------------------------------------------------------
WoJ %>% 
  correlate(work_experience, autonomy_selection) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  correlate(work_experience, autonomy_selection, autonomy_emphasis) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  correlate(work_experience, autonomy_selection, autonomy_emphasis) %>% 
  to_correlation_matrix()

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government)

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government,
          check_independenterrors = TRUE,
          check_multicollinearity = TRUE,
          check_homoscedasticity = TRUE)

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government) %>% 
  visualize(which = "correlogram")

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government) %>% 
  visualize(which = "resfit")

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government) %>% 
  visualize(which = "pp")

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government) %>% 
  visualize(which = "qq")

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government) %>% 
  visualize(which = "scaloc")

## -----------------------------------------------------------------------------
WoJ %>% 
  regress(autonomy_selection, work_experience, trust_government) %>% 
  visualize(which = "reslev")

