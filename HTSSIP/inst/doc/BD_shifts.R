## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(HTSSIP)

## ---- message=FALSE, warning=FALSE---------------------------------------
physeq_S2D2

## ----phyloseq_subset-----------------------------------------------------
params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'))
params = dplyr::filter(params, Substrate!='12C-Con')
ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)
physeq_S2D2_l

## ----BD_shift_wmean------------------------------------------------------
wmean1 = BD_shift(physeq_S2D2_l[[2]], nperm=10)
cat('Subset:', names(physeq_S2D2_l)[2], '\n')
wmean1 %>% head(n=3)

## ----plot_wmean, fig.height=3.5, fig.width=7-----------------------------
x_lab = bquote('Buoyant density (g '* ml^-1*')')
y_lab = 'Weighted mean of\nweighted-Unifrac distances'
ggplot(wmean1, aes(BD_min.x, wmean_dist)) +
  geom_line(alpha=0.7) +
  geom_point() +
  labs(x=x_lab, y=y_lab, title='Beta diversity of 13C-treatment relative to 12C-Con') +
  theme_bw() 

## ----wmean_m-------------------------------------------------------------
wmean1_m = wmean1 %>%
  mutate(BD_shift = wmean_dist > wmean_dist_CI_high) %>%
  arrange(BD_min.x) %>%
  mutate(window = (BD_shift == TRUE & lag(BD_shift) == TRUE & lag(BD_shift, 2) == TRUE) |
                  (BD_shift == TRUE & lag(BD_shift) == TRUE & lead(BD_shift) == TRUE) |
                  (BD_shift == TRUE & lead(BD_shift) == TRUE & lead(BD_shift, 2) == TRUE),
         BD_shift = BD_shift == TRUE & window == TRUE,
         BD_shift = ifelse(is.na(BD_shift), FALSE, BD_shift))

wmean1_m %>% head(n=3)

## ----wmean_m_plot, fig.height=3.5, fig.width=7---------------------------
x_lab = bquote('Buoyant density (g '* ml^-1*')')
y_lab = 'Weighted mean of\nweighted-Unifrac distances'
ggplot(wmean1_m, aes(BD_min.x, wmean_dist)) +
  geom_line(alpha=0.7) +
  geom_linerange(aes(ymin=wmean_dist_CI_low,
                     ymax=wmean_dist_CI_high),
                 alpha=0.3) +
  geom_point(aes(color=BD_shift)) +
  scale_color_discrete('Gradient\nfraction\nin BD shift\nwindow?') +
  labs(x=x_lab, y=y_lab, title='Beta diversity of 13C-treatment relative to 12C-Con') +
  theme_bw() 

## ----wmean---------------------------------------------------------------
wmean = plyr::ldply(physeq_S2D2_l, BD_shift, nperm=5)
wmean %>% head(n=3)

## ----shift_plot, fig.height=5, fig.width=7-------------------------------
# formatting the treatment names to look a bit better as facet labels
wmean = wmean %>%
  mutate(Substrate = gsub('.+(13C-[A-z]+).+', '\\1', .id),
         Day = gsub('.+Day ==[ \']*([0-9]+).+', 'Day \\1', .id),
         Day = Day %>% reorder(gsub('Day ', '', Day) %>% as.numeric))

# calculating BD shift windows
wmean = wmean %>%
  mutate(BD_shift = wmean_dist > wmean_dist_CI_high) %>%
  arrange(Substrate, BD_min.x) %>%
  group_by(Substrate) %>%
  mutate(window = (BD_shift == TRUE & lag(BD_shift) == TRUE & lag(BD_shift, 2) == TRUE) |
                  (BD_shift == TRUE & lag(BD_shift) == TRUE & lead(BD_shift) == TRUE) |
                  (BD_shift == TRUE & lead(BD_shift) == TRUE & lead(BD_shift, 2) == TRUE),
         BD_shift = BD_shift == TRUE & window == TRUE,
         BD_shift = ifelse(is.na(BD_shift), FALSE, BD_shift)) %>%
  ungroup()

# plotting, with facetting by 13C-treatment
ggplot(wmean, aes(BD_min.x, wmean_dist)) +
  geom_line(alpha=0.7) +
  geom_linerange(aes(ymin=wmean_dist_CI_low,
                     ymax=wmean_dist_CI_high),
                 alpha=0.3) +
  geom_point(aes(color=BD_shift)) +
  labs(x=x_lab, y=y_lab, 
       title='Beta diversity of 13C-treatments relative to 12C-Con') +
  facet_grid(Day ~ Substrate) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## ------------------------------------------------------------------------
sessionInfo()

