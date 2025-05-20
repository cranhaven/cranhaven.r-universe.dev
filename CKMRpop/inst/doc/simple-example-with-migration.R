## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(tidyverse)
library(CKMRpop)

## -----------------------------------------------------------------------------
pars <- list()
pars$`max-age` <- 5
pars$`fem-surv-probs` <- c(1, 0.7, 0.8, 0.8, 0.8)
pars$`male-surv-probs` <- c(1, 0.7, 0.8, 0.8, 0.8)
pars$`fem-prob-repro` <- c(0, 0, 1, 1, 1)
pars$`male-prob-repro` <- c(0, 0, 1, 1, 1)
pars$`fem-asrf` <- c(0, 0, .5, .7, 1)
pars$`male-asrp` <- c(0, 0, .5, .7, 1)
pars$`fem-rep-disp-par` <- 0.25
pars$`male-rep-disp-par` <- 0.25
pars$`sex-ratio` <- 0.5

## -----------------------------------------------------------------------------
pars$`number-of-years` <- 25

# given cohort sizes of 250 the stable age distribution
# can be found and used for the initial number of indivs
L <- leslie_from_spip(pars, 250)


# then we add those to the spip parameters
pars$`initial-males` <- floor(L$stable_age_distro_fem)
pars$`initial-females` <- floor(L$stable_age_distro_male)

pars$`cohort-size` <- "const 250"

## -----------------------------------------------------------------------------
pars$`discard-all` <- 0
pars$`gtyp-ppn-fem-pre` <- "10-25 0.05 0.00 0.00 0.00 0.00"
pars$`gtyp-ppn-male-pre` <- "10-25 0.05 0.00 0.00 0.00 0.00"
pars$`gtyp-ppn-fem-post` <- "10-25 0.00 0.01 0.01 0.01 0.01"
pars$`gtyp-ppn-male-post` <- "10-25 0.00 0.01 0.01 0.01 0.01"

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(15)
#  one_pop_dir <- run_spip(pars = pars, num_pops = 1)

## -----------------------------------------------------------------------------
pars_list <- list(
  pars,
  pars,
  pars
)

## ---- echo=FALSE, results='hide', message=FALSE-------------------------------
# NOTE the following if()...else() blocks are here
# to test whether spip has been installed yet.
# If spip is not available (for example, on CRAN's build machines) this
# is noted and stored package data are used for the variable
# "slurped" to build the remainder of the vignette.
if(spip_exists()) {
  message("spip is installed and will be used")
  set.seed(15)
  three_pop_dir <- run_spip(
    pars = pars_list,
    num_pops = 3
  )
  slurp3 <- slurp_spip(three_pop_dir, num_generations = 1)
} else {
  message("Using stored package data because spip is not installed")  
  slurp3 <- three_pops_no_mig_slurped_results
}

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(15)
#  three_pop_dir <- run_spip(
#    pars = pars_list,
#    num_pops = 3
#  )
#  slurp3 <- slurp_spip(three_pop_dir, num_generations = 2)

## ---- fig.width=5, out.width = '100%'-----------------------------------------
ggplot_census_by_year_age_sex(slurp3$census_postkill)

## -----------------------------------------------------------------------------
# out-migration rates pops 0, 1, 2
pars_list[[1]]$`fem-prob-mig-out` <- "5-25 1 .20"
pars_list[[1]]$`male-prob-mig-out` <- "5-25 1 .20"

pars_list[[2]]$`fem-prob-mig-out` <- "5-25 1 .15"
pars_list[[2]]$`male-prob-mig-out` <- "5-25 1 .15"

pars_list[[3]]$`fem-prob-mig-out` <- "5-25 1 .05"
pars_list[[3]]$`male-prob-mig-out` <- "5-25 1 .05"

# in-migration rates
pars_list[[1]]$`fem-prob-mig-in` <-  "5-25 1 0.00 0.80 0.20"
pars_list[[1]]$`male-prob-mig-in` <- "5-25 1 0.00 0.80 0.20"

pars_list[[2]]$`fem-prob-mig-in` <-  "5-25 1 0.05 0.00 0.95"
pars_list[[2]]$`male-prob-mig-in` <- "5-25 1 0.05 0.00 0.95"

pars_list[[3]]$`fem-prob-mig-in` <-  "5-25 1 0.10 0.90 0.00"
pars_list[[3]]$`male-prob-mig-in` <- "5-25 1 0.10 0.90 0.00"


## ---- echo=FALSE, results='hide', message=FALSE-------------------------------
# NOTE the following if()...else() blocks are here
# to test whether spip has been installed yet.
# If spip is not available (for example, on CRAN's build machines) this
# is noted and stored package data are used for the variable
# "slurped" to build the remainder of the vignette.
if(spip_exists()) {
  message("spip is installed and will be used")
  set.seed(15)
  mig_dir <- run_spip(
    pars = pars_list,
    num_pops = 3
  )
  slurp_mig <- slurp_spip(mig_dir, num_generations = 1)
} else {
  message("Using stored package data because spip is not installed")  
  slurp_mig <- three_pops_with_mig_slurped_results
}

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(15)
#  mig_dir <- run_spip(
#    pars = pars_list,
#    num_pops = 3
#  )
#  slurp_mig <- slurp_spip(mig_dir, num_generations = 1)

## -----------------------------------------------------------------------------
slurp_mig$migrants %>%
  count(age, from_pop, to_pop)

## -----------------------------------------------------------------------------
# compile relationships
crel <- compile_related_pairs(slurp_mig$samples)

# count number of PO pairs by which populations
# the members were born in
crel %>%
  filter(dom_relat == "PO") %>%
  mutate(
    parent_born_pop = case_when(
      upper_member == 1 ~ born_pop_1,
      upper_member == 2 ~ born_pop_2,
      TRUE ~ NA_integer_
    ),
    child_born_pop = case_when(
      upper_member == 1 ~ born_pop_2,
      upper_member == 2 ~ born_pop_1,
      TRUE ~ NA_integer_
    )
  ) %>% 
  count(parent_born_pop, child_born_pop)
  

