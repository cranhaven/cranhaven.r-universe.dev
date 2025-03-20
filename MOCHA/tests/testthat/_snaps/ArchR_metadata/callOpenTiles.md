# We can call peaks by sample from an ArchR project

    Code
      tiles@metadata
    Output
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
      [1] "TxDb.Hsapiens.UCSC.hg38.refGene"
      
      $TxDb$metadata
                                             name
      1                                   Db type
      2                        Supporting package
      3                               Data source
      4                                    Genome
      5                                  Organism
      6                               Taxonomy ID
      7                                UCSC Table
      8                                UCSC Track
      9                              Resource URL
      10                          Type of Gene ID
      11                             Full dataset
      12                         miRBase build ID
      13                        Nb of transcripts
      14                            Db created by
      15                            Creation time
      16 GenomicFeatures version at creation time
      17         RSQLite version at creation time
      18                          DBSCHEMAVERSION
                                                value
      1                                          TxDb
      2                               GenomicFeatures
      3                                          UCSC
      4                                          hg38
      5                                  Homo sapiens
      6                                          9606
      7                                       refGene
      8                                   NCBI RefSeq
      9                       http://genome.ucsc.edu/
      10                               Entrez Gene ID
      11                                          yes
      12                                         <NA>
      13                                        88816
      14    GenomicFeatures package from Bioconductor
      15 2021-04-28 16:30:46 +0000 (Wed, 28 Apr 2021)
      16                                       1.41.3
      17                                        2.2.6
      18                                          1.2
      
      
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
      
      
      $History
      $History[[1]]
      [1] "callOpenTiles 1.1.0"
      
      

