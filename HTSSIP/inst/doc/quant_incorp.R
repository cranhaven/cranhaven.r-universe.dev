## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(ggplot2)
library(HTSSIP)

## ------------------------------------------------------------------------
# HTS-SIP data
physeq_rep3

## ------------------------------------------------------------------------
# qPCR data (list object)
physeq_rep3_qPCR %>% names

## ------------------------------------------------------------------------
# qPCR data (list object)
physeq_rep3_qPCR_sum = physeq_rep3_qPCR$summary
physeq_rep3_qPCR_sum %>% head(n=4)

## ------------------------------------------------------------------------
# transforming OTU counts
physeq_rep3_t = OTU_qPCR_trans(physeq_rep3, physeq_rep3_qPCR_sum)

# calculating atom fraction excess
atomX = qSIP_atom_excess(physeq_rep3_t,
                         control_expr='Treatment=="12C-Con"',
                         treatment_rep='Replicate')
atomX %>% names

## ------------------------------------------------------------------------
atomX$A %>% head(n=4)

## ------------------------------------------------------------------------
# calculating bootstrapped CI values
df_atomX_boot = qSIP_bootstrap(atomX, n_boot=100)
df_atomX_boot %>% head(n=4)

## ------------------------------------------------------------------------
df_dBD = delta_BD(physeq_rep3, control_expr='Treatment=="12C-Con"')
df_dBD %>% head(n=4)

## ------------------------------------------------------------------------
# checking & joining data 
stopifnot(nrow(df_atomX_boot) == nrow(df_dBD))
df_j = dplyr::inner_join(df_atomX_boot, df_dBD, c('OTU'='OTU'))
stopifnot(nrow(df_atomX_boot) == nrow(df_j))

# formatting data for plotting
df_j = df_j %>%
  dplyr::mutate(OTU = reorder(OTU, -delta_BD))

## ---- fig.height=3, fig.width=7------------------------------------------
# plotting BD shift (Z)
ggplot(df_j, aes(OTU)) +
  geom_point(aes(y=Z), color='blue') +
  geom_point(aes(y=delta_BD), color='red') +
  geom_hline(yintercept=0, linetype='dashed', alpha=0.5) +
  labs(x='OTU', y='BD shift (Z)') +
  theme_bw() +
  theme(
    axis.text.x = element_blank()
  )

## ---- fig.height=3, fig.width=3------------------------------------------
# plotting BD shift (Z): boxplots

## formatting the table
df_j_g = df_j %>%
  dplyr::select(OTU, Z, delta_BD) %>%
  tidyr::gather(Method, BD_shift, Z, delta_BD) %>%
  mutate(Method = ifelse(Method == 'Z', 'q-SIP', 'delta-BD'))

## plotting 
ggplot(df_j_g, aes(Method, BD_shift)) +
  geom_boxplot() +
  geom_hline(yintercept=0, linetype='dashed', alpha=0.5) +
  labs(x='Method', y='BD shift (Z)') +
  theme_bw() 

## ------------------------------------------------------------------------
sessionInfo()

