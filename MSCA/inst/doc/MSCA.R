## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RcppParallel)
RcppParallel::setThreadOptions(numThreads = 1)

## ----setup--------------------------------------------------------------------
library(MSCA)
library(dplyr)

data(EHR)
head(EHR)
EHR %>%
  nrow()

## -----------------------------------------------------------------------------
EHR %>%
  group_by( reg ) %>%
  tally


## -----------------------------------------------------------------------------
s_mat <- make_state_matrices(
  data = EHR,
  id = "link_id",
  ltc = "reg",
  aos = "aos",
  l = 111,
  fail_code = "death",
  cens_code = "cens"
)
dim( s_mat )

## -----------------------------------------------------------------------------
library( cluster )
library( fastcluster )
# Compute the jaccard distance
d_mat <- fast_jaccard_dist( s_mat , as.dist = TRUE )

# Get a hierachical clustering using the built in hclust function
h_mat <- hclust(d = d_mat , method = 'ward.D2' )
h_mat

# Get a typology

ct_mat_8 <- cutree( h_mat , k = 8 )
table( ct_mat_8 )


## -----------------------------------------------------------------------------
# Get a data frame with patient id and cluster assignation 
df1 <- data.frame( link_id = names(ct_mat_8) , cl = paste0('cl_',ct_mat_8)) 
head(df1)  

# Merge with primary data
EHR_cl <- EHR %>%
  left_join( df1 )

# Get cluster sequences by cluster
dt_seq <- get_cluster_sequences(
  dt =  EHR_cl ,
  cl_col = "cl",
  id_col = "link_id",
  event_col = "reg",
  k = 2
)

# Get basic stats by cluster
sequence_stats(
  seq_data = dt_seq ,
  min_seq_freq = 0.03,
  min_conditional_prob = 0,
  min_relative_risk = 0
)

