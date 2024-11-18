## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(ggplot2)
library(HTSSIP)

## ------------------------------------------------------------------------
physeq_S2D2_l

## ----set_params----------------------------------------------------------
# adjusted P-value cutoff 
padj_cutoff = 0.1
# number of cores for parallel processing (increase depending on your computational hardware)
ncores = 2

## ------------------------------------------------------------------------
physeq = physeq_S2D2_l[[1]]
physeq

## ------------------------------------------------------------------------
physeq %>% sample_data %>% .$Substrate %>% table

## ----HRSIP_run, message=FALSE--------------------------------------------
df_l2fc = HRSIP(physeq,
                design = ~Substrate,
                padj_cutoff = padj_cutoff,
                sparsity_threshold = c(0,0.15,0.3))   # just using 3 thresholds to reduce time
df_l2fc %>% head(n=3)

## ------------------------------------------------------------------------
df_l2fc %>% 
  filter(padj < padj_cutoff) %>%
  group_by() %>%
  summarize(n_incorp_OTUs = OTU %>% unique %>% length)

## ----HRSIP_res_plotting, fig.width=5, fig.height=3.5---------------------
# summarizing
df_l2fc_s = df_l2fc %>% 
  filter(padj < padj_cutoff) %>%
  mutate(Rank2 = gsub('^__', '', Rank2)) %>%
  group_by(Rank2) %>%
  summarize(n_incorp_OTUs = OTU %>% unique %>% length) %>%
  ungroup()

# plotting
ggplot(df_l2fc_s, aes(Rank2, n_incorp_OTUs)) +
    geom_bar(stat='identity') +
    labs(x='Phylum', y='Number of incorporators') +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=45, hjust=1)
    )

## ------------------------------------------------------------------------
# Number of comparisons
physeq_S2D2_l %>% length

## ----HRSIP_parallel_run, message=FALSE-----------------------------------
# Running in parallel; you may need to change the backend for your environment.
# Or you can just set .parallel=FALSE. 
doParallel::registerDoParallel(ncores)

df_l2fc = plyr::ldply(physeq_S2D2_l, 
                      HRSIP, 
                      design = ~Substrate, 
                      padj_cutoff = padj_cutoff,
                      sparsity_threshold = c(0,0.15,0.3),  # just using 3 thresholds to reduce run time
                      .parallel=TRUE)
df_l2fc %>% head(n=3)

## ------------------------------------------------------------------------
df_l2fc %>% .$.id %>% unique

## ------------------------------------------------------------------------
df_l2fc = df_l2fc %>%
  mutate(.id = gsub(' \\| ', '\n', .id))
df_l2fc %>% .$.id %>% unique

## ------------------------------------------------------------------------
df_l2fc %>% 
  filter(padj < padj_cutoff) %>%
  group_by(.id, sparsity_threshold) %>%
  summarize(n_incorp_OTUs = OTU %>% unique %>% length) %>%
  as.data.frame

## ----HRSIP_parallel_res_plotting, fig.width=7, fig.height=7--------------
# summarizing
df_l2fc_s = df_l2fc %>% 
  filter(padj < padj_cutoff) %>%
  mutate(Rank2 = gsub('^__', '', Rank2)) %>%
  group_by(.id, Rank2) %>%
  summarize(n_incorp_OTUs = OTU %>% unique %>% length) %>%
  ungroup()

# plotting
ggplot(df_l2fc_s, aes(Rank2, n_incorp_OTUs)) +
    geom_bar(stat='identity') +
    labs(x='Phylum', y='Number of incorporators') +
    facet_wrap(~ .id, scales='free') +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=55, hjust=1)
    )

## ------------------------------------------------------------------------
windows = data.frame(density_min=c(1.70, 1.72, 1.74), 
                     density_max=c(1.73, 1.75, 1.77))
windows

## ----MWHRSIP_run, message=FALSE------------------------------------------
doParallel::registerDoParallel(ncores)

df_l2fc = plyr::ldply(physeq_S2D2_l, 
                      HRSIP, 
                      density_windows = windows,
                      design = ~Substrate, 
                      padj_cutoff = padj_cutoff,
                      sparsity_threshold = c(0,0.15,0.3), # just using 3 thresholds to reduce run time
                      .parallel = TRUE)
df_l2fc %>% head(n=3)

## ------------------------------------------------------------------------
df_l2fc %>% .$.id %>% unique

## ------------------------------------------------------------------------
df_l2fc %>% 
  filter(padj < padj_cutoff) %>%
  group_by(.id, sparsity_threshold) %>%
  summarize(n_incorp_OTUs = OTU %>% unique %>% length) %>%
  as.data.frame

## ----MWHRSIP_plotting_densWin, fig.width=7, fig.height=4.5---------------
# summarizing
df_l2fc_s = df_l2fc %>% 
  mutate(.id = gsub(' \\| ', '\n', .id)) %>%
  filter(padj < padj_cutoff) %>%
  mutate(density_range = paste(density_min, density_max, sep='-')) %>% 
  group_by(.id, sparsity_threshold, density_range) %>%
  summarize(n_incorp_OTUs = OTU %>% unique %>% length) 

#plotting
ggplot(df_l2fc_s, aes(.id, n_incorp_OTUs, fill=density_range)) +
    geom_bar(stat='identity', position='fill') +
    labs(x='Control-treatment comparision', y='Fraction of incorporators') +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=55, hjust=1)
    )

## ----MWHRSIP_plotting_incorp, fig.width=7, fig.height=7------------------
# summarizing
df_l2fc_s = df_l2fc %>% 
  mutate(.id = gsub(' \\| ', '\n', .id)) %>%
  filter(padj < padj_cutoff) %>%
  mutate(Rank2 = gsub('^__', '', Rank2)) %>%
  group_by(.id, Rank2) %>%
  summarize(n_incorp_OTUs = OTU %>% unique %>% length) %>%
  ungroup()

# plotting
ggplot(df_l2fc_s, aes(Rank2, n_incorp_OTUs)) +
    geom_bar(stat='identity') +
    labs(x='Phylum', y='Number of incorporators') +
    facet_wrap(~ .id, scales='free') +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=55, hjust=1)
    )

## ------------------------------------------------------------------------
sessionInfo()

