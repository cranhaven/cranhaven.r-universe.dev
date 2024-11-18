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
physeq_rep3_qPCR$raw %>% head(n=3)
physeq_rep3_qPCR$summary %>% head(n=3)

## ------------------------------------------------------------------------
phyloseq::otu_table(physeq_rep3) %>% .[1:5, 1:5]

## ------------------------------------------------------------------------
physeq_rep3_t = OTU_qPCR_trans(physeq_rep3, physeq_rep3_qPCR)
phyloseq::otu_table(physeq_rep3_t) %>% .[1:5, 1:5]

## ------------------------------------------------------------------------
# The 'Treatment' column designates treatment vs control
# The 'Replicate' column designates treatment replicates
data.frame(sample_data(physeq_rep3)) %>%
  dplyr::select(Treatment, Replicate) %>%
  distinct

## ------------------------------------------------------------------------
atomX = qSIP_atom_excess(physeq_rep3_t,
                         control_expr='Treatment=="12C-Con"',
                         treatment_rep='Replicate')
atomX %>% names

## ------------------------------------------------------------------------
df_atomX_boot = qSIP_bootstrap(atomX, n_boot=20)
df_atomX_boot %>% head

## ------------------------------------------------------------------------
CI_threshold = 0
df_atomX_boot = df_atomX_boot %>%
  mutate(Incorporator = A_CI_low > CI_threshold,
                OTU = reorder(OTU, -A))

## ------------------------------------------------------------------------
n_incorp = df_atomX_boot %>%
  filter(Incorporator == TRUE) %>%
  nrow
cat('Number of incorporators:', n_incorp, '\n')

## ---- fig.height=3, fig.width=7------------------------------------------

ggplot(df_atomX_boot, aes(OTU, A, ymin=A_CI_low, ymax=A_CI_high, color=Incorporator)) +
  geom_pointrange(size=0.25) +
  geom_linerange() +
  geom_hline(yintercept=0, linetype='dashed', alpha=0.5) +
  labs(x='OTU', y='Atom fraction excess') +
  theme_bw() +
  theme(
    axis.text.x = element_blank()
  )

## ------------------------------------------------------------------------
sessionInfo()

