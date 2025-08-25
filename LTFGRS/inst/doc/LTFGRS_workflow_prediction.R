## ----include=FALSE, results = 'hide'------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results = 'hide',warning = FALSE, message = FALSE-----------------
library(LTFGRS)
library(dplyr)
library(lubridate)
library(rmarkdown)

## -----------------------------------------------------------------------------
# population parameters and seed
set.seed(555)
h2 = .5 # heritability
K = .3 # population prevalence

## -----------------------------------------------------------------------------
# assuming we have been provided a CIP object of the following style:
CIP = expand.grid(list(age = 1:100,
                       birth_year = 1900:2024,
                       sex = 0:1)) %>%
  group_by(sex, birth_year) %>%
  mutate(cip = (1:n() - 1)/n() * K) %>%
  ungroup() %>% 
  print(n = 10)

## -----------------------------------------------------------------------------
# hand curated trio information, taken from LTFHPlus vignette:
# https://emilmip.github.io/LTFHPlus/articles/FromTrioToFamilies.html
family = tribble(
  ~id, ~momcol, ~dadcol,
  "pid", "mom", "dad",
  "sib", "mom", "dad",
  "mhs", "mom", "dad2",
  "phs", "mom2", "dad",
  "mom", "mgm", "mgf",
  "dad", "pgm", "pgf",
  "dad2", "pgm2", "pgf2",
  "paunt", "pgm", "pgf",
  "pacousin", "paunt", "pauntH",
  "hspaunt", "pgm", "newpgf",
  "hspacousin", "hspaunt", "hspauntH",
  "puncle", "pgm", "pgf",
  "pucousin", "puncleW", "puncle",
  "maunt", "mgm", "mgf",
  "macousin", "maunt", "mauntH",
  "hsmuncle", "newmgm", "mgf",
  "hsmucousin", "hsmuncleW", "hsmuncle"
)

## -----------------------------------------------------------------------------
# creating a graph for the family
graph = prepare_graph(.tbl = family, icol = "id", mcol = "momcol", fcol = "dadcol")
# calculating the kinship matrix based on the graph
cov_mat = get_covmat(fam_graph = graph, h2 = h2, index_id = "pid")
# creating a phenotype for the family
liabs = MASS::mvrnorm(n = 1, mu = rep(0, nrow(cov_mat)), Sigma = cov_mat)

## -----------------------------------------------------------------------------
# these values are simulated only for illustrative purposes and not to make sense(!)
pheno = tibble(
  id = names(liabs),
  status = liabs > qnorm(K, lower.tail = F),
  # no consideration for generation etc in sex, fdato or birth_year:
  sex = sample(0:1, size = length(liabs), replace = TRUE),
  fdato = dmy(paste0(sample(1:28, length(liabs), replace = TRUE), "/", sample(1:12, length(liabs), replace = T), "/", sample(1940:2000, length(liabs), replace = TRUE))),
  birth_year = year(fdato),
  # age of onset only after fdato
  adhd = purrr::map2_chr(.x = status, .y = birth_year,
      ~ if(.x) paste0(sample(1:28, 1), "/", sample(1:12, 1), "/", sample((.y + 1):2010, 1)) else NA),
  # end of follow up assigned here
  indiv_eof = dmy("31/12/2010")) %>% # blanket time stop, meant to simulate end of registers
  mutate(
    # converting to date format
    adhd = dmy(adhd),
    # eof either blanket time stop or event date
    indiv_eof = pmin(indiv_eof, adhd, na.rm = TRUE)) %>% 
  filter(id != "pid_g") %>% # remove the genetic liability of the proband
  # status is not required here. Only used it to generate the age of diagnosis.
  select(-status) 
paged_table(pheno)

## -----------------------------------------------------------------------------
graph = prepare_graph(.tbl = family, 
                      icol = "id",
                      mcol = "momcol", 
                      fcol = "dadcol",
                      node_attributes = select(pheno, id, sex))

## -----------------------------------------------------------------------------
# Identify family members of degree n
family_graphs = get_family_graphs(pop_graph = graph,
                                  ndegree = 1,
                                  proband_vec = pheno$id,
                                  fid = "fid",
                                  fam_graph_col = "fam_graph")
family_graphs %>% print(n = 4)

## -----------------------------------------------------------------------------
family_graphs$fam_graph[[1]]

## -----------------------------------------------------------------------------
# calculate family specific thresholds
info = familywise_censoring(
  family_graphs = family_graphs,
  tbl = pheno,
  start = "fdato",
  end = "indiv_eof",
  event = "adhd",
  merge_by = setNames(c("id"), c("pid")))

paged_table(info)

## ----results = 'hide'---------------------------------------------------------
fam_thrs = prepare_thresholds(
  .tbl = info,
  CIP = CIP,
  age_col = "age" ,
  lower_equal_upper = FALSE,
  personal_thr = TRUE, 
  fid_col = "fid",
  personal_id_col = "pid",
  interpolation = "xgboost"
)

## -----------------------------------------------------------------------------
paged_table(fam_thrs)

## -----------------------------------------------------------------------------
# attach family specific thresholds
ltfgrs_input = familywise_attach_attributes(family_graphs = family_graphs,
                                            fam_attr = fam_thrs,
                                            fam_graph_col = "fam_graph",
                                            attached_fam_graph_col = "masked_fam_graph",
                                            cols_to_attach = c("lower", "upper"),
                                            censor_proband_thrs = TRUE)
ltfgrs_input %>% print(n = 4)

## -----------------------------------------------------------------------------
ltfgrs_input$masked_fam_graph[[1]]

## -----------------------------------------------------------------------------
ltfgrs_pa = estimate_liability(family_graphs = ltfgrs_input %>% rename(pid = fid),
                               h2 = h2, 
                               fid = "fid",
                               pid = "pid",
                               family_graphs_col = "masked_fam_graph",
                               method = "PA", # <- METHOD
                               useMixture = F)
paged_table(ltfgrs_pa)

## -----------------------------------------------------------------------------
ltfgrs_gibbs = estimate_liability(family_graphs = ltfgrs_input %>% rename(pid = fid), 
                                  h2 = h2,
                                  fid = "fid",
                                  pid = "pid",
                                  family_graphs_col = "masked_fam_graph",
                                  method = "Gibbs", # <- METHOD
                                  useMixture = F)
paged_table(ltfgrs_gibbs)

