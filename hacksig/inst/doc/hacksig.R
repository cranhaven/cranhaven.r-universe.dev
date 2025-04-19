## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 5L, tibble.print_max = 5L)

## ---- message=FALSE-----------------------------------------------------------
library(hacksig)

# to plot and transform data
library(dplyr)
library(ggplot2)
library(purrr)
library(tibble)
library(tidyr)

# to get the MSigDB gene signatures
library(msigdbr)

# to parallelize computations
library(future)

theme_set(theme_bw())

## -----------------------------------------------------------------------------
get_sig_info()

## ---- eval=FALSE--------------------------------------------------------------
#  get_sig_info() %>% print(n = Inf)
#  # or
#  View(get_sig_info())

## -----------------------------------------------------------------------------
get_sig_genes("ifng")

## -----------------------------------------------------------------------------
check_sig(test_expr)

## -----------------------------------------------------------------------------
check_sig(test_expr, signatures = c("metab", "cinsarc"))

## -----------------------------------------------------------------------------
hallmark_list <- msigdbr(species = "Homo sapiens", category = "H") %>%
    distinct(gs_name, gene_symbol) %>%
    nest(genes = c(gene_symbol)) %>%
    mutate(genes = map(genes, compose(as_vector, unname))) %>%
    deframe()

check_sig(test_expr, hallmark_list)

## -----------------------------------------------------------------------------
check_sig(test_expr, hallmark_list) %>% 
    filter(signature_id == "HALLMARK_NOTCH_SIGNALING") %>% 
    pull(missing_genes)

## -----------------------------------------------------------------------------
hack_sig(test_expr)

## -----------------------------------------------------------------------------
hack_sig(test_expr, signatures = "estimate", method = "zscore")

## -----------------------------------------------------------------------------
hack_sig(test_expr, hallmark_list, 
         method = "ssgsea", sample_norm = "separate", alpha = 0.5)

## -----------------------------------------------------------------------------
set.seed(123)
rand_dm <- sample(c(0, 1), size = ncol(test_expr), replace = TRUE)
hack_cinsarc(test_expr, rand_dm)

## -----------------------------------------------------------------------------
hack_estimate(test_expr)

## -----------------------------------------------------------------------------
hack_immunophenoscore(test_expr)

## -----------------------------------------------------------------------------
hack_immunophenoscore(test_expr, extract = "all")

## -----------------------------------------------------------------------------
test_expr %>% 
    hack_sig("estimate", method = "singscore", direction = "up") %>% 
    hack_class()

## -----------------------------------------------------------------------------
plan(multisession)
hack_sig(test_expr, hallmark_list, method = "ssgsea")

## -----------------------------------------------------------------------------
kegg_list <- msigdbr(species = "Homo sapiens", subcategory = "KEGG") %>%
    distinct(gs_name, gene_symbol) %>%
    nest(genes = c(gene_symbol)) %>%
    mutate(genes = map(genes, compose(as_vector, unname))) %>%
    deframe()

kegg_ok <- check_sig(test_expr, kegg_list) %>% 
    filter(frac_present > 0.66) %>% 
    pull(signature_id)

## -----------------------------------------------------------------------------
kegg_scores <- map_dfr(list(zscore = "zscore", ssgsea = "ssgsea"), 
                       ~ hack_sig(test_expr,
                                  kegg_list[kegg_ok],
                                  method = .x,
                                  sample_norm = "separate"),
                       .id = "method")

## -----------------------------------------------------------------------------
kegg_scores <- kegg_scores %>% 
    pivot_longer(starts_with("KEGG"), 
                 names_to = "kegg_id", values_to = "kegg_score")

## ---- fig.dim=c(9,5)----------------------------------------------------------
purity_scores <- hack_estimate(test_expr) %>% select(sample_id, purity_score)

kegg_scores %>% 
    left_join(purity_scores, by = "sample_id") %>% 
    ggplot(aes(x = kegg_id, y = kegg_score)) +
    geom_boxplot(outlier.alpha = 0) +
    geom_jitter(aes(color = purity_score), alpha = 0.8, width = 0.1) +
    facet_wrap(facets = vars(method), scales = "free_x") +
    coord_flip() +
    scale_color_viridis_c() +
    labs(x = NULL, y = "enrichment score", color = "Tumor purity") +
    theme(legend.position = "top")

