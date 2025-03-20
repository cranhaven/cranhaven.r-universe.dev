# We can subset a tileResults object by celltypes

    A MultiAssayExperiment object of 1 listed
     experiment with a user-defined name and respective class.
     Containing an ExperimentList class object of length 1:
     [1] C3: RaggedExperiment with 50000 rows and 3 columns
    Functionality:
     experiments() - obtain the ExperimentList instance
     colData() - the primary/phenotype DataFrame
     sampleMap() - the sample coordination DataFrame
     `$`, `[`, `[[` - extract colData columns, subset, or experiment
     *Format() - convert into a long or wide DataFrame
     assays() - convert ExperimentList to a SimpleList of matrices
     exportClass() - save data to flat files

---

    class: SummarizedExperiment 
    dim: 1 3 
    metadata(0):
    assays(16): CellCounts FragmentCounts ... DoubletEnrichment
      BlacklistRatio
    rownames(1): C3
    rowData names(0):
    colnames(3): scATAC_BMMC_R1 scATAC_CD34_BMMC_R1 scATAC_PBMC_R1
    colData names(2): Sample PassQC

# We can subset a tileResults object by Sample grouping

    A MultiAssayExperiment object of 2 listed
     experiments with user-defined names and respective classes.
     Containing an ExperimentList class object of length 2:
     [1] C2: RaggedExperiment with 50000 rows and 1 columns
     [2] C3: RaggedExperiment with 50000 rows and 1 columns
    Functionality:
     experiments() - obtain the ExperimentList instance
     colData() - the primary/phenotype DataFrame
     sampleMap() - the sample coordination DataFrame
     `$`, `[`, `[[` - extract colData columns, subset, or experiment
     *Format() - convert into a long or wide DataFrame
     assays() - convert ExperimentList to a SimpleList of matrices
     exportClass() - save data to flat files

---

    class: SummarizedExperiment 
    dim: 2 1 
    metadata(0):
    assays(16): CellCounts FragmentCounts ... DoubletEnrichment
      BlacklistRatio
    rownames(2): C2 C3
    rowData names(0):
    colnames(1): scATAC_CD34_BMMC_R1
    colData names(2): Sample PassQC

