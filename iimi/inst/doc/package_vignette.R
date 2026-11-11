## ----warning=FALSE, message=FALSE, eval=FALSE---------------------------------
#  # install iimi
#  install.packages(c("iimi", "dplyr", "httr"))
#  
#  # install Biostrings
#  if (!require("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  
#  BiocManager::install("Biostrings")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(iimi)

library(Biostrings)

library(dplyr)

library(httr)

# this is the extracted DNAStringSet data from the Virtool virus database GitHub. It contains both version 1.4.0 and version 1.5.0
if (interactive()) {
  url <- "https://github.com/virtool/iimi/raw/refs/heads/main/data/virus_segments.rda"
  load(url(get_url))
} else {
  set.seed(11723)
  random_seq_func <- function(n) {
    paste0(sample(c("A", "C", "G", "T"), size = n, replace = TRUE), collapse = "")
  }
  # generate sequences and create DNAStringSet
  virus_segments <- DNAStringSet(vapply(c(5834, 1575), random_seq_func, FUN.VALUE = character(1)))
  names(virus_segments) <- c("82np2784", "m0kacxse")
}

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  path_to_bamfiles <- list.files(
#    path = "path/to/your/BAM/files/folder",
#    pattern = "bam$",
#    full.names = TRUE,
#    include.dirs = TRUE
#  )

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  example_cov <- convert_bam_to_rle(bam_file = "path_to_bamfiles")

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  # Using default settings (recommended)
#  df <- convert_rle_to_df(example_cov)
#  
#  # Disabling unreliable region processing
#  df <-
#    convert_rle_to_df(example_cov, unreliable_region_enabled = FALSE)
#  
#  # Using custom unreliable regions
#  # Refer to section 3.3 for details
#  custom_regions <- create_custom_unreliable_regions()
#  df <-
#    convert_rle_to_df(example_cov, unreliable_region_df = custom_regions)

## ----message=FALSE, warning=FALSE, results='hide', eval=FALSE-----------------
#  prediction_default <- predict_iimi(newdata = df, method = "xgb")

## ----eval=FALSE---------------------------------------------------------------
#  # preparing the train and test data
#  
#  # spliting into 80-20 train and test data set with the three plant samples
#  set.seed(123)
#  train_names <- sample(levels(as.factor(df$sample_id)),
#                        length(unique(df$sample_id)) * 0.8)
#  
#  # trian data
#  train_x = df[df$sample_id %in% train_names,]
#  # test data
#  test_x = df[df$sample_id %in% train_names == F,]
#  
#  # preparing labels
#  train_y = c()
#  for (ii in 1:nrow(train_x)) {
#    train_y = append(train_y, example_diag[train_x$seg_id[ii],
#                                           train_x$sample_id[ii]])
#  }

## ----message=FALSE, warning=FALSE, results='hide', eval=FALSE-----------------
#  fit <- train_iimi(train_x = train_x, train_y = train_y)

## ----eval=FALSE---------------------------------------------------------------
#  prediction_customized <-
#    predict_iimi(newdata = test_x,
#                 trained_model = fit)

## -----------------------------------------------------------------------------
# An example of the provided unreliable regions
unreliable_regions %>% group_by(Categories) %>% sample_n(2)

## ----eval=FALSE---------------------------------------------------------------
#  # if you would like to keep unmappable regions that can be mapped to other
#  # viruses or the host genome separate into two data frames, you may use the
#  # following code:
#  
#  # input your own path that you would want to store regions on a virus that can
#  # be mapped to another virus
#  # you can customize the name of this type of mappability profile
#  mappability_profile_virus <-
#    create_mappability_profile("path/to/bam/files/folder/virus", category = "Unmappable region (virus)", virus_info = virus_segments)
#  
#  # input your own path that you would want to store regions on a virus that can
#  # be mapped to the host genome
#  # you can customize the name of this type of mappability profile
#  mappability_profile_host <-
#    create_mappability_profile("path/to/bam/files/folder/host", category = "Unmappable region (host)", virus_info = virus_segments)
#  
#  # if you would like to keep everything in the same data frame, you may use the
#  # following code:
#  mappability_profile <-
#    create_mappability_profile("path/to/bam/files/folder/of/both/types/", category = "Unmappable region", virus_info = virus_segments)

## ----eval=FALSE---------------------------------------------------------------
#  high_nucleotide_regions <-
#    create_high_nucleotide_content(gc = 0.6, a = 0.45, virus_info = virus_segments)

## ----fig.width=7, fig.height=5------------------------------------------------

oldpar <- par(mfrow = c(1, 2))

## if you wish to plot all segments of one sample, you can try:
# plot_cov(covs = example_cov["S1"])

## if you wish to plot all segments from all samples, you can try:
# plot_cov(covs = example_cov)

## if you wish to plot certain segments from one sample, you can try:
segs = c("82np2784", "m0kacxse")
covs_selected = list()
covs_selected$`305S` <-
  example_cov$`305S`[segs]

## if you have many segments that you would want to plot, you can try the following code with the numbers changed
## to find the index of your desired segments:

# covs_selected$`305S` <-
#   example_cov$`305S`[names(example_cov$`305S`)[c(8, 15)]]

par(mar = c(2, 4, 1, 1))
layout(matrix(c(1, 1, 2, 3, 3, 4), nrow = 3))
plot_cov(covs = covs_selected, virus_info = virus_segments)

par(oldpar)


