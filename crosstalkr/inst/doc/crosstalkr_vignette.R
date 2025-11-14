## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(crosstalkr)
library(dplyr)
#library(dnet)

## ----load_data, warning = FALSE, message = FALSE, include = FALSE-------------

df <- try(readr::read_csv(file = "https://github.com/DavisWeaver/disruptr/raw/main/inst/test_data/rld_Counts.csv"))
if(inherits(df, "try-error")) {
  knitr::knit_exit() #just put
}

## ----load_more_data, warning = FALSE, message = FALSE-------------------------
#Load PPI we will be using
load(url("https://github.com/DavisWeaver/disruptr/blob/main/inst/test_data/stringdb.Rda?raw=true"))
g_ppi <- g

#To download directly from stringdb - see load_ppi()
df <- df %>% select(1, '5_A673_DMSO') %>% 
  rename(expression = '5_A673_DMSO')
colnames(df)[1] <- 'gene_name'
df <- df %>% 
  filter(expression !=0) %>%
  mutate(expression = log2(expression)) %>% 
  filter(!is.na(expression))

exp <- df$expression
names(exp) <- df$gene_name
print(head(df))

## ----filter-------------------------------------------------------------------
#start by getting the PPI since we don't want to download it twice
g <- gfilter(method = "value", g=g_ppi, cache = NULL, val=exp, val_name = "expression", 
             use_ppi = FALSE, desc = TRUE,n=100)
length(igraph::V(g))
head(igraph::get.vertex.attribute(g, name = "expression"))

## ----filter again-------------------------------------------------------------
g <- gfilter(g=g, igraph_method = "betweenness", n = 5, desc= TRUE, use_ppi=FALSE, val_name = "betweenness")
igraph::V(g)

## ----compute_crosstalk, fig.width = 8, fig.height=5, caption = "PPI Subnetwork defined by GAPDH, HSP90AA1, EEF1A1, HNRNPC, and TPT1. Seed proteins are highlighted blue while proteins identified to have high affinity for seeds are highlighted in orange. Example of using the plot_ct function with default settings"----
out <- gfilter.ct(seeds = names(igraph::V(g)), g = g_ppi, use_ppi = FALSE, 
                  return_df=TRUE, cache = NULL, seed_name = "vignette", n = 100, 
                  agg_int= 10, ncores = 1)

plot_ct(out[[2]], out[[1]], prop_keep=0.2)


## ----crosstalk_describe-------------------------------------------------------
cdf <- out[[2]] 
cdf %>% 
  select(-p_value, -nobs, -mean_p, -var_p) %>% 
  slice_max(affinity_score, n=10) %>%
  knitr::kable(digits=3)

## ----reduction2---------------------------------------------------------------
g <- gfilter(g = g_ppi, use_ppi=FALSE, cache = NULL, n = 2000, igraph_method = igraph::degree, val_name = "degree")
g <- gfilter(g=g, use_ppi = FALSE, n = 500, method = "val", val_name = "expression", val = exp)
igraph::gsize(g)

## ----network_potential--------------------------------------------------------
dnp <- node_repression(g=g, v_rm = names(igraph::V(g)), exp = exp)
dnp <- Matrix::colSums(dnp)
dnp <- sort(dnp, decreasing = TRUE)
names(dnp)[1:5]

