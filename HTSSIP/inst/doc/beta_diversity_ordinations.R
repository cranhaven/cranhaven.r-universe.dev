## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(ggplot2)
library(HTSSIP)

## ---- message=FALSE, warning=FALSE---------------------------------------
physeq_S2D2

## ------------------------------------------------------------------------
params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'))
params

## ------------------------------------------------------------------------
params = dplyr::filter(params, Substrate!='12C-Con')
params

## ------------------------------------------------------------------------
ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)
physeq_S2D2_l

## ---- message=FALSE, warning=FALSE---------------------------------------
# running in parallel
doParallel::registerDoParallel(2)
physeq_S2D2_l_df = SIP_betaDiv_ord(physeq_S2D2_l, parallel=TRUE)
physeq_S2D2_l_df %>% head(n=3)

## ---- message=FALSE, warning=FALSE---------------------------------------
physeq_S2D2_l_df %>% .$phyloseq_subset %>% unique

## ------------------------------------------------------------------------
physeq_S2D2_l_df = physeq_S2D2_l_df %>%
  dplyr::mutate(phyloseq_subset = gsub(' \\| ', '\n', phyloseq_subset),
                phyloseq_subset = gsub('\'3\'', '\'03\'', phyloseq_subset))
physeq_S2D2_l_df %>% .$phyloseq_subset %>% unique

## ---- fig.height=6, fig.width=7.5----------------------------------------
phyloseq_ord_plot(physeq_S2D2_l_df)

## ------------------------------------------------------------------------
sessionInfo()

