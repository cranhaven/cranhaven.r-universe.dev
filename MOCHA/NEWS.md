# MOCHA 1.1.0
* New Functions:
  - Sharing MOCHA objects between file systems
    - packMOCHA
    - unpackMOCHA
  - Exporting for genome browsers (bigwig, bigbed)
    - exportCoverage
    - exportDifferentials
    - exportMotifs
    - exportOpenTiles
    - exportSmoothedInsertions
  - Getters 
    - getCellTypeTiles
    - getCellTypes
    - getPromoterGenes
    - getSampleCellTypeMetadata
  - Other
    - mergeTileResults
    - plotIntensityDistribution
    - renameCellTypes

* Deprecated Functions:
  - runLMEM, pilotLMEM, runZIGLMM, pilotZIGLMM, IndividualZIGLMM, getModelValues, varZIGLMM, processModelOutputs
  
* Updates test data to consistently use TxDb hg19 references.

* Various minor bug and documentation fixes.

# MOCHA 1.0.2
* Addressed check errors in "donttest" examples.

# MOCHA 1.0.1

* Deprecates `testCoAccessibilityChromVAR()` and `testCoAccessibilityRandom()` in favor of `testCoAccessibility()`
* Updates maintainer email

# MOCHA 1.0.0

* Adopting semantic versioning starting with this version, versioning reflects breaking changes compared to previous CRAN release.
* Includes test improvements
* New functions bulkDimReduction, bulkUMAP, MotifEnrichment, MotifSetEnrichmentAnalysis, pilotLMEM, runLMEM, pilotZIGLMM, runZIGLMM, combineSampleTileMatrix, getCoverage
* Improvements to metadata carried by output objects

# MOCHA 0.2.5

* patches a bug (rounding error) found in getDifferentialAccessibleTiles (#125), and reverts to using mclapply parallelization forgetDifferentialAccessibleTiles.
* adds conditional tests (and snapshot tests) on the COVID dataset to ensure reproducibility with results in the MOCHA manuscript
* updates the COVID vignette through differentials to reflect the latest usage.

# MOCHA 0.2.4

* Fixes bug in callOpenTiles where "Clusters" was hardcoded in the step computing fragment counts table.
* Parallelization overhaul to address memory leaks when using parLapply. parLapply is now passed a helper function directly and a single object input to that function, where the input object is a list containing all variables needed in the helper function.

# MOCHA 0.2.3

* MOCHA MultiAssayExperiment and MOCHA SummarizedExperiment objects now contain new metadata 
* CallOpenTiles now only only accepts the database package names (strings) for Genome, OrgDb, and TxDb, and not the in-memory R objects for those database packages
* Downstream functions where Genome, OrgDb, and TxDb were previously inputs now check the input MOCHA object's metadata to load the relevant databases.

# MOCHA 0.2.2

* getCoAccessibleLinks
* testCoAccessibilityChromVar and testCoAccessibilityRandom
* combineSampleTileMatrix

# MOCHA 0.2.1

* This release adds additional test coverage with new test data, covering edge cases in callOpenTiles.

* Removes option log2Intensity from getSampleTileMatrix (done by default in getDifferentialAccessibleTiles)

# MOCHA 0.2.0

* This includes the initial release of package on CRAN, adds updated requirements for R >= 4.1.0 and plyranges >1.14.0

# MOCHA 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* MOCHA is submitted to CRAN as an initial release.
