---
output:
  html_document: default
  pdf_document: default
---
# NEWS
## Version 0.1.0.1

   2024-05-10
  1. Update the test cases to be consistent with testthat (>= 3.0.0).
   
   
   2024-04-17
   1. Created version 0.1.0.1 from 0.1.0.
   1. Fixed a testthat testing issue caused by a new upcoming version of the 'dqrng' package, as notified by the author of the package.
   
## Version 0.1.0

   2022-01-25
   
  1.  When cluster_method = "Ball+BIC", if Mclust discretization was unsuccessful, discretize.jointly will now add jitter noise (of increasing 'amount') to 'data'.
  
  2022-01-18
  
  1. Updated CITATION, REFERENDCES.bib, and README.md.


   2022-01-17
      
  1. Fixed a bug in computing weights for MultiChannel.WUC when cluster IDs are not consecutive numbers after Ball clustering by Mclust (descrtize_jointly.R:198-204)
  2. Fixed function cluster(data, k, method) where the method was not included into the return object.
  3. Examples in the vignettes have been updated.


   2022-01-11
  
  1. Added k=Inf option, that incrementally increases the number of clusters to
  choose until either an increment is not required or each data point is
  its own cluster.
  2. Added an example showing the use of k=Inf option.

  
   2021-10-26
  
  1.  Added an additional parameter 'cluster_method', could be "Ball+BIC",
  "kmeans+silhouette" or "PAM",
  2. Added clustering method Mclust
  3. Changed the default cluster method from Kmeans to Mclust, which is "Ball+BIC"
  4. Added an additional parameter 'grid_method', could be "Sort+split" or 
  "MultiChannel.WUC", "MultiChannel.WUC" is a discretize method Multichannel.MUC
  from Ckmeans.1d.dp. The default option is still "Sort+split" which is the 
  same as previous version. As the "MultiChannel.WUC" option is experimental, it is not recommended.
  5. Added general plotting function plot(), old plot function still available
  6. Changed the default value of min_level from 2 to 1. We expanded min_level argument
  to provide a vector of integers specifying the minimum level for each dimension.
  
## Version 0.0.8.1
  
   2021-10-06

  1. Removed the requirement for the number of points in the data

## Version 0.0.8
  
  
   2020-09-13
  
  1. Added function plotGOCpatterns to plot the continuous data along with the
  cluster preserving grid.
  2. Created a manual for the plotGOCpatterns() function.
  3. Updated the code for the Examples vignette to use plotGOCpatterns.

  
   2020-08-10
  
  1. Created version 0.0.8 from 0.0.7.
  2. Added an additional parameter 'min_level' to denote the minimum number of 
  discretization levels required for each dimension. 
  3. Updated the manual of discrete.jointly() function.
  4. Added an entry in reference and citation.
  5. Updated README.md with badges.

## Version 0.0.7
  
  
   2020-04-03
  
  1. Tidied up code for the Examples vignette.
  2. Updated the manual of discrete.jointly() function.
  3. Made minor editorial changes in DESCRIPTION and README.md.
  4. Resolved signed/unsigned mismatches.

  
   2020-03-31
  
  1. Created version 0.0.7 from 0.0.6.
  2. Fixed memory leak in Clusters.cpp when calculating median.


## Version 0.0.6
  
   2020-03-26
  
  1. Created version 0.0.6 from 0.0.5.
  2. Rewrote Prep_Index() to work in between two consecutive points, 
  rather than on top of a single point.
  3. Using distance() in Prep_Index() to calculate the distance for two 
  iterators.
  4. Using "ceil" in Binary_Index_Searching() to consider even/odd cases
  when determining grid lines.
  5. Fixed potential overflow issues.

## Version 0.0.5 (not released to the public)

  
   2020-03-25
  
  1. Fixed a bug in the prep_index() function.
  2. Fixed prep_index() (lines 120 and 125) such that grid lines are
  put at the midpoint between two conseuctive points, instead of on
  one of the points.
  3. Updated vignette. Example 2 seems always correct now.
  
  
   2020-03-24

  1. Created version 0.0.5 from 0.0.4.
  2. Function discretize.jointly() now returns cluster labels of each
  observation and a similarity score (ARI) between the joint
  discretization and the cluster labels of each observation. 
  3. The class Cluster has a new constructor that takes cluster
  labels and the input data to compute median for each dimension.
  4. Find_grid() is now based on median.
  5. Using 'dqrng' in test cases to avoid RNG issue in testing.
  6. Rewrote multiple functions in Joint_Grid.cpp to avoid push_back().
  7. New visualization code in vignette now shows cluster labels for
  each observation.

## Version 0.0.4 (not released to the public)

   2020-03-20
  
  1. Created version 0.0.4 from 0.0.3.
  2. Fixed typos in DESCRIPTION and README files.

## Version 0.0.3

   2020-03-17
  
  1. Created version 0.0.3 from 0.0.2. Package renamed to GridOnClusters
  2. Function joint.grid.discretize.R() renamed to discretize.jointly()
  3. Return values of function discretize.jointly() changed to include
  both the discretized data and the grid
  4. Manual for discretize.jointly() updated.
  5. Line 104, 105 in Joint_Grid.cpp commented out
  6. Rewrote Find_Grid() to avoid push_back() in Joint_Grid.cpp
  7. Created a vignette to include examples.
  
## Version 0.0.2 (not released to the public)

   2020-03-14
  
  1. Created the initial version 0.0.1. Package renamed to QNJGD

## Version 0.0.1 (not released to the public)

   2020-03-09
  
  1. Created the initial version 0.0.1. Package named JointGridDiscr
