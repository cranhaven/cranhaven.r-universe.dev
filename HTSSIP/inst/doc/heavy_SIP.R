## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(ggplot2)
library(HTSSIP)

## ----set_params----------------------------------------------------------
# adjusted P-value cutoff 
padj_cutoff = 0.1

## ------------------------------------------------------------------------
physeq_S2D2_l

## ------------------------------------------------------------------------
physeq = physeq_S2D2_l[[1]]
physeq

## ------------------------------------------------------------------------
physeq %>% sample_data %>% .$Substrate %>% table

## ----heavy_SIP_simple, message=FALSE-------------------------------------
df_res = heavy_SIP(physeq, ex="Substrate=='12C-Con'", 
                   comparison='H-v-H', hypo_test='binary')
df_res %>% head(n=3)

## ------------------------------------------------------------------------
df_res$statistic %>% table

## ------------------------------------------------------------------------
physeq_rep3

## ------------------------------------------------------------------------
physeq_rep3 %>% sample_data %>% head(n=3)

## ----heavy_SIP_ttest, message=FALSE--------------------------------------
df_res = heavy_SIP(physeq_rep3, ex="Treatment=='12C-Con'", 
                   comparison='H-v-H', hypo_test='t-test')
df_res %>% head(n=3)

## ----heavy_SIP_ttest_incorp, message=FALSE-------------------------------
df_res %>%
  filter(padj < padj_cutoff) %>%
  nrow

## ----heavy_SIP_ttest_pval, message=FALSE---------------------------------
df_res$p %>% summary

## ----heavy_SIP_mann, message=FALSE---------------------------------------
df_res = heavy_SIP(physeq_rep3, ex="Treatment=='12C-Con'", 
                   comparison='H-v-H', hypo_test='wilcox')
df_res %>% head(n=3)

## ----heavy_SIP_mann_pval, message=FALSE----------------------------------
df_res$p %>% summary %>% print
df_res$padj %>% summary %>% print

## ------------------------------------------------------------------------
sessionInfo()

