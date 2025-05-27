## ----vig_settings, include=F--------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.width=8, fig.height=5
)
options(rmarkdown.html_vignette.check_title=F)

## ----cran_install-------------------------------------------------------------
# install.packages('spatialGE')

## ----install_devtools---------------------------------------------------------
# if("devtools" %in% rownames(installed.packages()) == FALSE){
#   install.packages("devtools")
# }

#devtools::install_github("fridleylab/spatialGE")

## ----load_spatialGE, message=F------------------------------------------------
library('spatialGE')

## ----extrenal_figs1, echo=FALSE, fig.align='center'---------------------------
knitr::include_graphics("img/dir_structure.png")

## ----data_download1-----------------------------------------------------------
lk='https://github.com/FridleyLab/spatialGE_Data/raw/refs/heads/main/tnbc_bassiouni.zip?download='

## ----check-internet, echo=FALSE, message=FALSE, include=FALSE-----------------
net_check = curl::has_internet()

## ----data_download2, eval=net_check-------------------------------------------
visium_tmp = tempdir()
unlink(visium_tmp, recursive=TRUE)
dir.create(visium_tmp)
download.file(lk, destfile=paste0(visium_tmp, '/', 'tnbc_bassiouni.zip'), mode='wb')
zip_tmp = list.files(visium_tmp, pattern='tnbc_bassiouni.zip$', full.names=TRUE)
unzip(zipfile=zip_tmp, exdir=visium_tmp)

## ----extrenal_figs2, echo=FALSE, fig.align='center', eval=net_check-----------
knitr::include_graphics("img/metadata_file.png")

## ----data_fpaths, eval=net_check----------------------------------------------
visium_folders <- list.dirs(paste0(visium_tmp, '/tnbc_bassiouni'), full.names=TRUE, recursive=FALSE)

## ----data_fpaths2, eval=net_check---------------------------------------------
clin_file <- list.files(paste0(visium_tmp, '/tnbc_bassiouni'), full.names=TRUE, recursive=FALSE, pattern='clinical')

## ----create_stlist, warning=F, eval=net_check---------------------------------
tnbc <- STlist(rnacounts=visium_folders, samples=clin_file, cores=2)

## ----call_stlist, eval=net_check----------------------------------------------
tnbc

## ----count_stats, eval=net_check----------------------------------------------
summarize_STlist(tnbc)

## ----count_dstr, eval=net_check-----------------------------------------------
cp <- distribution_plots(tnbc, plot_type='violin', plot_meta='total_counts')
cp[['total_counts']]

## ----filter_chunk, eval=net_check---------------------------------------------
tnbc <- filter_data(tnbc, spot_minreads=5000, spot_mingenes=1000, spot_maxreads=150000)

cp2 <- distribution_plots(tnbc, plot_type='violin', plot_meta='total_counts')
cp2[['total_counts']]

## ----pseudobulk, eval=net_check-----------------------------------------------
tnbc <- pseudobulk_samples(tnbc, max_var_genes=5000)

## ----pca_chunk, fig.align='center', eval=net_check----------------------------
pseudobulk_dim_plot(tnbc, plot_meta='patient_id')

## ----heatmap_chunk, fig.align='center', eval=net_check------------------------
hm_p <- pseudobulk_heatmap(tnbc, plot_meta='patient_id', hm_display_genes=30)

## ----norm_chunk, eval=net_check-----------------------------------------------
tnbc <- transform_data(tnbc, method='log', cores=2)

## ----genequilt_chunk, eval=net_check------------------------------------------
quilts1 <- STplot(tnbc, 
                  genes=c('NDRG1', 'IGKC'), 
                  samples='sample_094c', 
                  color_pal='BuRd', 
                  ptsize=0.8)

## ----genequilt_chunk2, fig.align='center', eval=net_check---------------------
ggpubr::ggarrange(plotlist=quilts1, nrow=1, ncol=2, legend='bottom')

## ----genekrige_chunk, eval=net_check------------------------------------------
tnbc <- gene_interpolation(tnbc, 
                           genes=c('NDRG1', 'IGKC'),
                           samples=c('sample_094c', 'sample_117e'), cores=2)

## ----plotkrige_chunk1, fig.align='center', eval=net_check---------------------
kriges1 <- STplot_interpolation(tnbc,
                                genes=c('NDRG1', 'IGKC'),
                                samples='sample_094c')
#ggpubr::ggarrange(plotlist=kriges1, nrow=1, ncol=2, common.legend=TRUE, legend='bottom')

## ----plot_tissue, eval=net_check----------------------------------------------
ip = plot_image(tnbc, samples='sample_094c')

ip[['image_sample_094c']]

## ----clustespots_chunk, warning=F, message=F, eval=net_check------------------
tnbc <- STclust(tnbc, 
                ks='dtc', 
                ws=c(0, 0.025, 0.05, 0.2), cores=2)

## ----plotclustspots_chunk, fig.align='center', eval=net_check-----------------
cluster_p <- STplot(tnbc, 
                    samples='sample_094c', 
                    ws=c(0, 0.025, 0.05, 0.2),
                    color_pal='highcontrast')
ggpubr::ggarrange(plotlist=cluster_p, nrow=2, ncol=2, legend='right')

## ----sthet_chunk, eval=net_check----------------------------------------------
tnbc <- SThet(tnbc, 
              genes=c('NDRG1', 'IGKC'),
              method='moran', cores=2)

## ----clinplot_chunk, fig.align='center', eval=net_check-----------------------
p <- compare_SThet(tnbc, 
                   samplemeta='overall_survival_days', 
                   color_by='patient_id',
                   gene=c('NDRG1', 'IGKC'), 
                   color_pal='muted',
                   ptsize=3)

p

## ----stats_slot, eval=net_check-----------------------------------------------
get_gene_meta(tnbc, sthet_only=TRUE)

## ----sphet_table, echo=F, eval=net_check--------------------------------------
library('magrittr') # Use of pipe operator
sphet_info <- tibble::tibble(
  "Statistic"=c('Moran’s I', 'Geary’s C'),
  "Clustered expression"=c('Closer to 1', 'Closer to 0'),
  "No expression pattern"=c('Closer to 0', 'Closer to 1'),
  "Uniform expression"=c('Closer to -1', 'Closer to 2')
  )

kbl_t = kableExtra::kbl(sphet_info, align='c', centering=TRUE) %>%
  kableExtra::kable_styling(position="center", full_width=FALSE)
kableExtra::add_footnote(kbl_t, 
                         c("Note: The boundaries indicated for each statistic are reached when the number of spots/cells is very large."),
                         notation="none")

## ----load_simpkg, message=F, eval=net_check-----------------------------------
library('rpart')
library('spatstat')
library('scSpatialSIM')
library('tidyverse')
library('janitor')

## ----tissue_sim_1, warning=F, message=F, eval=net_check-----------------------
wdw <- owin(xrange=c(0, 3), yrange=c(0, 3))
sim_visium <- CreateSimulationObject(sims=1, cell_types=1, window=wdw)

## ----tissue_sim_2, warning=F, fig.height=4, fig.width=6, eval=net_check-------
# Generate point process
# Then, simulate tissue compartments
set.seed(12345)
sim_visium <- GenerateSpatialPattern(sim_visium, gridded=TRUE, overwrite=TRUE) %>% 
  GenerateTissue(k=1, xmin=1, xmax=2, ymin=1, ymax=2, sdmin=1, sdmax=2, overwrite=TRUE)

PlotSimulation(sim_visium, which=1, what="tissue points") + 
  scale_shape_manual(values=c(19, 1))

## ----tissue_sim_3, warning=F, eval=net_check----------------------------------
# Extract tissue assignments from the `SpatSimObj` object
# Simulate expression of 'gene_1'
sim_visium_df <- sim_visium@`Spatial Files`[[1]] %>%
  clean_names() %>% 
  mutate(gene_1=case_when(tissue_assignment == 'Tissue 1' ~ 1, TRUE ~ 0.1))

# Generate expression patter of "gene_2"
for(i in 1:nrow(sim_visium_df)){
  if(i%%2 == 0){
    sim_visium_df[i, 'gene_2'] = 1
  } else{
    sim_visium_df[i, 'gene_2'] = 0.1
  }
}

# Generate expression of "gene_3"
# Set seed for resproducibility
set.seed(12345)
sim_visium_df[['gene_3']] <- sample(sim_visium_df[['gene_2']])

## ----tissue_sim_4, warning=F, fig.height=4, eval=net_check--------------------
# Extract simulated expression data
sim_expr <- sim_visium_df %>% 
  add_column(libname=paste0('spot', seq(1:nrow(.)))) %>%
  select(c('libname', 'gene_1', 'gene_2', 'gene_3')) %>%
  column_to_rownames('libname') %>% t() %>% 
  as.data.frame() %>% rownames_to_column('genename')

# Extract simulated spot locations
sim_xy <- sim_visium_df %>% 
  add_column(libname=paste0('spot', seq(1:nrow(.)))) %>%
  select(c('libname', 'y', 'x'))

# The `STlist` function can take a list of data frames
simulated <- STlist(rnacounts=list(sim_visium=sim_expr), 
                    spotcoords=list(sim_visium=sim_xy), cores=2)

# Plot expression
ps <- STplot(simulated, genes=c('gene_1', 'gene_2', 'gene_3'), data_type='raw', color_pal='sunset', ptsize=1)
ggpubr::ggarrange(plotlist=ps, ncol=3)

## ----tissue_sim_5, warning=F, message=F, eval=net_check-----------------------
# The `SThet` function requires normalized data
simulated <- transform_data(simulated, cores=2)

# Run `SThet`
simulated <- SThet(simulated, genes=c('gene_1', 'gene_2', 'gene_3'), method=c('moran', 'geary'))

# Extract results
get_gene_meta(simulated, sthet_only=TRUE)

## -----------------------------------------------------------------------------
sessionInfo()

