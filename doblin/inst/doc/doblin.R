## ----include = FALSE----------------------------------------------------------

knitr::opts_knit$set(
              self.contained = TRUE)
knitr::opts_chunk$set(
  #collapse = TRUE,
  dpi = 55,
  fig.retina = 1,
  comment = "#>"
  )
  

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# Rscript ./demo/main.R -t 0.0005 -o ~/Documents/doblin/ -n test
# -i ~/Documents/input.csv -c 12

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# Processing the command line...
# Step 0: Processing CSV file...
# Do you want to plot the dynamics of your dataset?(y/n): y

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# Step 1: Plotting the dynamics...
# 1.1 Reshaping input file into long-format dataframe...
# 1.2 Retrieving the first 1000 barcodes with the highest maximum frequencies...
# 1.3 Assigning colors to lineages having reached the minimum frequency threshold
# among the 1000 most dominant barcoded lines...

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# Do you want to plot a log-scale model, a linear-scale model or both?
#   (logarithmic/linear/both): both
# Plotting in progress...
# Rendering linear-scale area plot. This may take a few minutes...

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# Do you want to plot the diversity of your dataset?(y/n): y
# 2.1 Calculating the diversity...
# 2.2 Plotting the diversity...

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# Step 3: Clustering...
# Specify a minimum mean frequency below which lineages are not taken into account during
# clustering (ex: 0.00005): 0.00005
# 3.1 Filtering the input data...

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# 3.2 Clustering the filtered data...
# Enter an agglomeration method (refer to stats::hclust() R documentation): average
# Enter the metric to be used to measure similarity between two time-series (pearson/dtw) :
#   pearson
# Enter a method for computing covariances in the presence of missing values.
# Please refer to stats::cor() R documentation (ex: pairwise.complete.obs) :
#   pairwise

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# 3.2 Clustering the filtered data...
# Enter an agglomeration method (refer to stats::hclust() R documentation): average
# Enter the metric to be used to measure similarity between two time-series (pearson/dtw):
#   dtw
# Enter the norm for the local distance calculation
# ('L1' for Manhattan or 'L2' for (squared) Euclidean): L2

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# 3.2.1 Computing the relative clusters for ALL thresholds between 0.1 and maximum
# height of hierarchical clustering...
# 3.2.2 Filtering the hierarchical clustering results...
# Enter the minimum number of members per cluster for test : 8
# Enter the minimum number of members per cluster for test : 8
# Enter the minimum average frequency to rescue small clusters: 0.001
# Warning message:
#   By ignoring clusters with fewer than 8 members, you are potentially ignoring
#   dominant clusters.

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# 3.2.3 Quantifying the hierarchical clustering...
# 3.2.4 Enter the chosen threshold for the clustering of test : 0.3

## ----eval=FALSE, collapse=TRUE------------------------------------------------
# 3.2.5 Plotting the resulting clusters...
# DONE

