# getSampleTileMatrices works on a 1 sample test dataset

    {
      "type": "integer",
      "attributes": {},
      "value": [25112, 1]
    }

---

    $summarizedData
    class: SummarizedExperiment 
    dim: 2 1 
    metadata(0):
    assays(18): CellCounts FragmentCounts ... ReadsInPeaks FRIP
    rownames(2): C2 C5
    rowData names(0):
    colnames(1): PBMCSmall
    colData names(2): Sample PassQC
    
    $Genome
    [1] "hg19"
    
    $TxDb
    $TxDb$pkgname
    [1] "TxDb.Hsapiens.UCSC.hg19.knownGene"
    
    $TxDb$metadata
                                           name
    1                                   Db type
    2                        Supporting package
    3                               Data source
    4                                    Genome
    5                                  Organism
    6                               Taxonomy ID
    7                                UCSC Table
    8                              Resource URL
    9                           Type of Gene ID
    10                             Full dataset
    11                         miRBase build ID
    12                          transcript_nrow
    13                                exon_nrow
    14                                 cds_nrow
    15                            Db created by
    16                            Creation time
    17 GenomicFeatures version at creation time
    18         RSQLite version at creation time
    19                          DBSCHEMAVERSION
                                              value
    1                                          TxDb
    2                               GenomicFeatures
    3                                          UCSC
    4                                          hg19
    5                                  Homo sapiens
    6                                          9606
    7                                     knownGene
    8                       http://genome.ucsc.edu/
    9                                Entrez Gene ID
    10                                          yes
    11                                       GRCh37
    12                                        82960
    13                                       289969
    14                                       237533
    15    GenomicFeatures package from Bioconductor
    16 2015-10-07 18:11:28 +0000 (Wed, 07 Oct 2015)
    17                                      1.21.30
    18                                        1.0.0
    19                                          1.1
    
    
    $OrgDb
    $OrgDb$pkgname
    [1] "org.Hs.eg.db"
    
    $OrgDb$metadata
                     name                                                 value
    1     DBSCHEMAVERSION                                                   2.1
    2             Db type                                                 OrgDb
    3  Supporting package                                         AnnotationDbi
    4            DBSCHEMA                                              HUMAN_DB
    5            ORGANISM                                          Homo sapiens
    6             SPECIES                                                 Human
    7        EGSOURCEDATE                                            2021-Sep13
    8        EGSOURCENAME                                           Entrez Gene
    9         EGSOURCEURL                  ftp://ftp.ncbi.nlm.nih.gov/gene/DATA
    10          CENTRALID                                                    EG
    11              TAXID                                                  9606
    12       GOSOURCENAME                                         Gene Ontology
    13        GOSOURCEURL http://current.geneontology.org/ontology/go-basic.obo
    14       GOSOURCEDATE                                            2021-09-01
    15     GOEGSOURCEDATE                                            2021-Sep13
    16     GOEGSOURCENAME                                           Entrez Gene
    17      GOEGSOURCEURL                  ftp://ftp.ncbi.nlm.nih.gov/gene/DATA
    18     KEGGSOURCENAME                                           KEGG GENOME
    19      KEGGSOURCEURL                  ftp://ftp.genome.jp/pub/kegg/genomes
    20     KEGGSOURCEDATE                                            2011-Mar15
    21       GPSOURCENAME             UCSC Genome Bioinformatics (Homo sapiens)
    22        GPSOURCEURL                                                      
    23       GPSOURCEDATE                                            2021-Jul20
    24       ENSOURCEDATE                                            2021-Apr13
    25       ENSOURCENAME                                               Ensembl
    26        ENSOURCEURL               ftp://ftp.ensembl.org/pub/current_fasta
    27       UPSOURCENAME                                               Uniprot
    28        UPSOURCEURL                               http://www.UniProt.org/
    29       UPSOURCEDATE                              Wed Sep 15 18:21:59 2021
    
    
    $Directory
    [1] "/Users/imran.mcgrath/Documents/projects/PBMCSmall/MOCHA"
    
    $History
    $History[[1]]
    [1] "callOpenTiles 1.0.2"
    
    $History[[2]]
    [1] "getSampleTileMatrix 1.0.2"
    
    

---

    DataFrame with 1 row and 2 columns
                   Sample    PassQC
              <character> <numeric>
    PBMCSmall   PBMCSmall         1

