# extractRegion works on a 3 sample test dataset

    class: SummarizedExperiment 
    dim: 80009 4 
    metadata(7): summarizedData Genome ... History Type
    assays(1): C3.All
    rownames(80009): 1 2 ... 80008 80009
    rowData names(0):
    colnames(4): chr Locus Counts Groups
    colData names(0):

---

    class: SummarizedExperiment 
    dim: 1001 4 
    metadata(7): summarizedData Genome ... History Type
    assays(1): C3.All
    rownames(1001): 1 2 ... 1000 1001
    rowData names(0):
    colnames(4): chr Locus Counts Groups
    colData names(0):

# extractRegion works on a 3 sample test dataset with sampleSpecific=TRUE

    class: SummarizedExperiment 
    dim: 80009 4 
    metadata(7): summarizedData Genome ... History Type
    assays(2): C3.All.scATAC_BMMC_R1 C3.All.scATAC_CD34_BMMC_R1
    rownames(80009): 1 2 ... 80008 80009
    rowData names(0):
    colnames(4): chr Locus Counts Groups
    colData names(0):

---

    class: SummarizedExperiment 
    dim: 1001 4 
    metadata(7): summarizedData Genome ... History Type
    assays(1): C3.All
    rownames(1001): 1 2 ... 1000 1001
    rowData names(0):
    colnames(4): chr Locus Counts Groups
    colData names(0):

