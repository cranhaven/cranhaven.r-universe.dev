# sarp.snowprofile.alignment 2.0.0

  - Improved computation speeds of profile alignments by 
      - making bottlenecks more efficient
      - adding functionality for parallel computations in `distanceSP`, `averageSP` and `clusterSP`
  - Added extended functionality to cluster snow profiles, see `clusterSP`, `plot.clusterSP`
  - Added the vignette *Clustering snow profiles*
  - Consolidated existing distance functions
      - `distMatSP` is now `distanceSPlayers`
      - `distanceSP` now works on both profile pairs and entire
        snowprofileSets to compute pairwise distance matrices

# sarp.snowprofile.alignment 1.2.2

  - Fixed a seldom bug in the `averageSPalongSeason` rescaling routine that would cause the function to fail without recovery
  - Make `averageSPalongSeason` more user-friendly wrt providing precomputed averages as initial condition
  - Add `concat_avgSP_timeseries` to combine two return objects from `averageSPalongSeason` where one holds the season history and the other holds the recent average. This is useful if the routine is applied in an operational setting where the average timeseries is update day-by-day.

# sarp.snowprofile.alignment 1.2.1

  - Added the vignette *Averaging of profiles and retrieval of distributions*
    --> `browseVignettes(package = "sarp.snowprofile.alignment")`

# sarp.snowprofile.alignment 1.2.0

  - Added density and optical grain size (ogs) as warping variables for alignment
  - Averaging of profiles:
      - include slab_rhogs, p_unstable, bdate, scalingFactor, ppu_all in `warpSP` and all `dba` routines
      - fix minor bugs for averaging shallow profiles and backtracking layers
      - add more detailed documentation (also for future developers)
  - Robustify `medoidSP`

# sarp.snowprofile.alignment 1.1.4

  - Implemented functionality to align profiles *without* the requirement to re-scale them first! see examples in `?dtwSP`
  - `simSP` 
      - includes several different approaches of computing snow profile similarities
      - can handle variable similarity values for non-matched layers
      - can apply penalty based on thickness of non-matched layers
  - Implementation of *Dynamic Time Warping Barycenter Averaging* (`dbaSP`) enables to 
      - average snow profiles. see also `averageSP` and `averageSPalongSeason`
      - connect layers within a profile set (`backtrackLayers`) to conveniently compute summary statistics and distributions of layers
  
  - Minor scale changes:
      - Implemented ability to configure the absolute warping window (instead of relative one only)
      - Included first systematic tests (of various different alignment scenarios)
      - Fix bug in `warpSP`, which was introduced due to changes in the `sarp.snowprofile::snowprofile` data class (i.e., introduction of `unobservedBasalLayer`s)
      - package shiny is not imported anymore, but only suggested --> enhances usability on clusters

# sarp.snowprofile.alignment 1.0.0

  - Updated license (GPL >= 3)
  - Two new vignettes
  - Adjust to upstream changes in `sarp.snowprofile`: new class (incl. methods) `snowprofileSet`  
  (i.e., *not backwards-compatible*)
  - Bug fixes:
    - rename remaining deprecated field names (e.g., `grain_type` to `gtype`, `deposition_date` to `ddate`, etc.)
    - make documentation available
    - interactive alignment shiny app is fixed
    - 'globalAlignment' has been made directional (i.e., bottom-up, top-down)

# sarp.snowprofile.alignment 0.1.0
  
  - first version tag
  - main version 0.x to accompany paper submission
