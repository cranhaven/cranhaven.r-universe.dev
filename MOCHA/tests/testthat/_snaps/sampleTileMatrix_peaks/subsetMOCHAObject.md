# We can subset a sampleTileMatrix object - and peaks - by celltypes

    class: RangedSummarizedExperiment 
    dim: 11360 3 
    metadata(6): summarizedData Genome ... Directory History
    assays(1): C3
    rownames(11360): chr10:100080500-100080999 chr10:100147000-100147499
      ... chrY:2709500-2709999 chrY:2803000-2803499
    rowData names(2): C2 C3
    colnames(3): scATAC_BMMC_R1 scATAC_CD34_BMMC_R1 scATAC_PBMC_R1
    colData names(2): Sample PassQC

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

