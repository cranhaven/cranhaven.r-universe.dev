## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(tidyverse)
library(CKMRpop)

## -----------------------------------------------------------------------------
species_1_life_history

## -----------------------------------------------------------------------------
SPD <- species_1_life_history

## -----------------------------------------------------------------------------
# before we tell spip what the cohort sizes are, we need to 
# tell it how long we will be running the simulation
SPD$`number-of-years` <- 100  # run the sim forward for 100 years

# this is our cohort size
cohort_size <- 300

# Do some matrix algebra to compute starting values from the
# stable age distribution:
L <- leslie_from_spip(SPD, cohort_size)

# then we add those to the spip parameters
SPD$`initial-males` <- floor(L$stable_age_distro_fem)
SPD$`initial-females` <- floor(L$stable_age_distro_male)

# tell spip to use the cohort size
SPD$`cohort-size` <- paste("const", cohort_size, collapse = " ")



## -----------------------------------------------------------------------------
samp_frac <- 0.03
samp_start_year <- 50
samp_stop_year <- 75
SPD$`discard-all` <- 0
SPD$`gtyp-ppn-fem-post` <- paste(
  samp_start_year, "-", samp_stop_year, " ", 
  samp_frac, " ", samp_frac, " ", samp_frac, " ",
  paste(rep(0, SPD$`max-age` - 3), collapse = " "),
  sep = ""
)
SPD$`gtyp-ppn-male-post` <- SPD$`gtyp-ppn-fem-post`

## ---- echo=FALSE, results='hide', message=FALSE-------------------------------
# NOTE the following if()...else() blocks are here
# to test whether spip has been installed yet.
# If spip is not available (for example, on CRAN's build machines) this
# is noted and stored package data are used for the variable
# "slurped" to build the remainder of the vignette.
if(spip_exists()) {
  message("spip is installed and will be used")
  set.seed(5)
  spip_dir <- run_spip(pars = SPD)
  
  # now read that in and find relatives within the grandparental range
  slurped <- slurp_spip(spip_dir, 2)
} else {
  message("Using stored package data because spip is not installed")  
  slurped <- species_1_slurped_results
}

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(5)  # set a seed for reproducibility of results
#  spip_dir <- run_spip(pars = SPD)  # run spip
#  slurped <- slurp_spip(spip_dir, 2) # read the spip output into R

## ---- fig.width = 7, fig.height = 5.5-----------------------------------------
ggplot_census_by_year_age_sex(slurped$census_postkill)

## -----------------------------------------------------------------------------
surv_rates <- summarize_survival_from_census(slurped$census_postkill)

## -----------------------------------------------------------------------------
surv_rates$survival_tibble %>%
  slice(1:40)

## ---- fig.width = 9, fig.height=7.5, out.height=600, out.width=700------------
surv_rates$plot_histos_by_age_and_sex

## ---- fig.width = 9, fig.height=7.5, out.height=600, out.width=700------------
surv_rates2 <- summarize_survival_from_census(
  census = slurped$census_prekill,
  fem_surv_probs = SPD$`fem-surv-probs`, 
  male_surv_probs = SPD$`male-surv-probs`
)

# print the plot
surv_rates2$plot_histos_by_age_and_sex

## -----------------------------------------------------------------------------
offs_and_mates <- summarize_offspring_and_mate_numbers(
  census_postkill = slurped$census_postkill,
  pedigree = slurped$pedigree,
  deaths = slurped$deaths, lifetime_hexbin_width = c(1, 2)
)

## ---- fig.width = 7, fig.height = 5.5-----------------------------------------
offs_and_mates$plot_age_specific_number_of_offspring

## ---- fig.height = 6, fig.width = 7-------------------------------------------
offs_and_mates$plot_lifetime_output_vs_age_at_death

## ---- fig.width=7, fig.height=7-----------------------------------------------
offs_and_mates$plot_fraction_of_offspring_from_each_age_class

## -----------------------------------------------------------------------------
mates <- count_and_plot_mate_distribution(slurped$pedigree)

## -----------------------------------------------------------------------------
head(mates$mate_counts)

## ---- fig.width=7, fig.height=5-----------------------------------------------
mates$plot_mate_counts

## -----------------------------------------------------------------------------
crel <- compile_related_pairs(slurped$samples)

## -----------------------------------------------------------------------------
crel %>%
  slice(1:10)

## -----------------------------------------------------------------------------
relat_counts <- count_and_plot_ancestry_matrices(crel)

## -----------------------------------------------------------------------------
relat_counts$highly_summarised

## -----------------------------------------------------------------------------
relat_counts$dr_counts

## ---- fig.width=7, fig.height=7-----------------------------------------------
relat_counts$dr_plots$FC

## ---- fig.width=7, fig.height=7-----------------------------------------------
relat_counts$dr_plots$Si

## -----------------------------------------------------------------------------
relat_counts$anc_mat_counts

## ---- fig.width=7, fig.height=7-----------------------------------------------
relat_counts$anc_mat_plots[[1]]

## ---- fig.width=7, fig.height=7-----------------------------------------------
relat_counts$anc_mat_plots[[2]]

## -----------------------------------------------------------------------------
nrow(slurped$samples)

## -----------------------------------------------------------------------------
slurped$samples %>% 
  mutate(ns = map_int(samp_years_list, length)) %>% 
  summarise(tot_times = sum(ns))

## -----------------------------------------------------------------------------
SS2 <- slurped$samples %>%
  filter(map_int(samp_years_list, length) > 1) %>%
  select(ID, samp_years_list)

SS2

## -----------------------------------------------------------------------------
# first indiv:
SS2$samp_years_list[[1]]

# second indiv:
SS2$samp_years_list[[2]]

## -----------------------------------------------------------------------------
subsampled_pairs <- downsample_pairs(
  S = slurped$samples,
  P = crel,
  n = 100
)

## -----------------------------------------------------------------------------
# num samples before downsampling
ns_bd <- nrow(slurped$samples)

# num samples after downsampling
ns_ad <- nrow(subsampled_pairs$ds_samples)

# ratio of sample sizes
ssz_rat <- ns_ad / ns_bd

# square of the ratio
sq_rat <- ssz_rat ^ 2

# ratio of number of pairs found amongst samples
num_pairs_before <- nrow(crel)
num_pairs_after_downsampling = nrow(subsampled_pairs$ds_pairs)

ratio <- num_pairs_after_downsampling / num_pairs_before

# compare these two things
c(sq_rat, ratio)

## -----------------------------------------------------------------------------
# because we jitter some points, we can set a seed to get the same
# result each time
set.seed(22)
spag <- uncooked_spaghetti(
  Pairs = crel, 
  Samples = slurped$samples
)

## ---- fig.width=7.5, fig.height=9.5-------------------------------------------
spag$plot

## -----------------------------------------------------------------------------
crel %>%
  slice(1:10)

## ---- echo=FALSE, results='hide', message=FALSE-------------------------------
# NOTE the following if()...else() blocks are here
# to test whether spip has been installed yet.
# If spip is not available (for example, on CRAN's build machines) this
# is noted and stored package data are used for the variable
# "slurped" to build the remainder of the vignette.
if(spip_exists()) {
  # read the spip output in and find relatives within the parental range
  slurped_1gen <- slurp_spip(spip_dir, num_generations = 1)
} else {
  message("Using stored package data for 1gen results because spip is not installed")  
  slurped_1gen <- species_1_slurped_results_1gen
}

## ---- eval=FALSE--------------------------------------------------------------
#  slurped_1gen <- slurp_spip(spip_dir, num_generations = 1)

## -----------------------------------------------------------------------------
crel_1gen <- compile_related_pairs(slurped_1gen$samples)

## -----------------------------------------------------------------------------
nrow(crel_1gen)

## -----------------------------------------------------------------------------
set.seed(10)
ssp_1gen <- downsample_pairs(
  S = slurped_1gen$samples,
  P = crel_1gen,
  n = 150
)

## -----------------------------------------------------------------------------
ssp_1gen$ds_pairs %>%
  count(conn_comp) %>%
  arrange(desc(n))

## ---- fig.width=6, fig.height=6-----------------------------------------------
# for some reason, the aes() function gets confused unless
# ggraph library is loaded...
library(ggraph)

one_gen_graph <- plot_conn_comps(ssp_1gen$ds_pairs)
one_gen_graph$plot



## ---- fig.width=6, fig.height=6-----------------------------------------------
one_gen_graph + 
  ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 1.2) +
  scale_edge_color_manual(values = c(`PO-1` = "tan2", `Si-1` = "gold", `Si-2` = "blue"))

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot_conn_comps(crel)$plot


## -----------------------------------------------------------------------------
set.seed(10)
freqs <- lapply(1:100, function(x) {
  nA = 1 + rpois(1, 3)
  f = runif(nA)
  f/sum(f)
})

## ---- echo=FALSE, results='hide', message=FALSE-------------------------------
# now we can run spip with those as input
if(spip_exists()) {
  message("spip is installed and will be used")
  set.seed(5)
  spip_dir <- run_spip(
    pars = SPD, 
    allele_freqs = freqs
  )
  
  # now read that in and find relatives within the grandparental range
  slurped <- slurp_spip(spip_dir, 2)
} else {
  message("Using stored package data because spip is not installed")  
  slurped <- species_1_slurped_results_100_loci
}

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(5)
#  spip_dir <- run_spip(
#    pars = SPD,
#    allele_freqs = freqs
#  )
#  # now read that in and find relatives within the grandparental range
#  slurped <- slurp_spip(spip_dir, 2)

## -----------------------------------------------------------------------------
slurped$genotypes[1:10, 1:5]

