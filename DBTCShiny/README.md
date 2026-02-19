# Dada-BLAST-Taxon Assign-Condense Shiny Application (DBTCShiny)
DBTCShiny is an R shiny implementation of the [DBTC](https://github.com/rgyoung6/DBTC) Metabarcode analysis pipeline.

# Description

This package contains the DBTCShiny functions located at [rgyoung6/DBTCShiny](https://github.com/rgyoung6/DBTCShiny) . The Dada-BLAST-Taxon Assign-Condense Shiny package contains the foundational [DBTC](https://github.com/rgyoung6/DBTC) functions (able to be run through the command line) which have been wrapped in a Shiny application for easy user interface. The [DBTC](https://github.com/rgyoung6/DBTC) functions have four main outcomes...

  - [Fastq](https://en.wikipedia.org/wiki/FASTQ_format) file processing using Dada in R
  - Using the Basic Local Alignment Search Tool ([BLAST](https://en.wikipedia.org/wiki/BLAST_(biotechnology))), amplicon sequence variants ([ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant)) can be searched against local NCBI or custom sequence databases
  - Assign taxa to the unique reads using NCBI taxon database (obtain the database using [taxonomizr website](https://CRAN.R-project.org/package=taxonomizr))
  - Condense the resulting [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) taxonomic assignment tables to unique taxa with the ability to combine datasets (using different sequence databases for the same reads, or results from the same samples for different molecular regions) into a combined results table

**NOTE:** While the [DBTC](https://github.com/rgyoung6/DBTC) package has been built for the analysis of high-throughput sequencing results, the BLAST and taxonomic assignment, taxonomic condense can be utilized with single specimen Sanger sequencing data.

# Table of Contents  
- [Installation](#installation)
- [Package Dependencies](#package-dependencies)
- [DBTC Function Descriptions](#dbtc-function-descriptions)
- [Naming Convention Rules](#naming-convention-rules)
- [DBTCShiny Function Details](#dbtcshiny-function-details)
- [Mapping Dashboard](#mapping-dashboard)
  * [Mapping](#mapping)
  * [Data Import](#data-import)
  * [Data Filtering](#data-filtering)
  * [Data Table](#data-table) 
- [Citation](#citation)

# Installation 

DBTCShiny can be installed three ways.

## 1. Install from CRAN

install.packages('DBTCShiny’)

## 2. Install via GitHub
Run the following commands in your R terminal...<br/>
```
if(!require(devtools)) install.packages('devtools')
library('devtools')
devtools::install_github('rgyoung6/DBTCShiny')
library('DBTCShiny')
```

## 3. Install through download from GitHub
Navigate to the [DBTCShiny](https://github.com/rgyoung6/DBTCShiny) GitHub page. Download the files associated with this page to your local computer and place them somewhere in the main file folder named DBTCShiny. Then run the following command pointing to that location on your local computer by replacing the HERE with the path in the below command...<br/>
```
library("DBTCShiny", lib.loc="HERE")
```

([Back to Top](#table-of-contents))

***

# Package Dependencies
 
There are several dependencies necessary for the DBTCShiny package. Most notable is the [DBTC](https://github.com/rgyoung6/DBTC) package. This package requires several bioconductor, CRAN and external packages, programs, and database resources. The installation guide for [DBTC](https://github.com/rgyoung6/DBTC) should be consulted before installing DBTCShiny.

In addition to the [DBTC](https://github.com/rgyoung6/DBTC) dependencies, DBTCShiny has a number of CRAN dependencies. These are listed below...
```
install.packages(c('DBTC',
                   'DT',
                   'ggplot2',
                   'leaflet',
                   'leaflet.extras',
                   'magrittr',
                   'shiny',
                   'shinycssloaders',
                   'shinydashboard',
                   'shinyWidgets'))
library(c('DBTC',
         'DT',
         'ggplot2',
         'leaflet',
         'leaflet.extras',
         'magrittr',
         'shiny',
         'shinycssloaders',
         'shinydashboard',
         'shinyWidgets'))
```

([Back to Top](#table-of-contents))
***

# Run DBTCShiny

After [DBTCShiny installation](#installation) and all of its dependencies (including [DBTC](https://github.com/rgyoung6/DBTC) and all of its dependencies) you need to load the package and then run the Shiny Graphical User Interface (GUI) using the following commands...

```
library(DBTCShiny)
launchDBTCShiny()
```

***

# [DBTC](https://github.com/rgyoung6/DBTC) Function Descriptions

## [dada_implement()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#dada-implement)
The dada_implement() function takes [fastq](https://en.wikipedia.org/wiki/FASTQ_format) files as input, analyses them and produces amplicon sequence variant ([ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant)) files. This function requires a main directory containing folder(s) representing sequencing runs which in-turn contains [fastq](https://en.wikipedia.org/wiki/FASTQ_format) files (the location of one of the [fastq](https://en.wikipedia.org/wiki/FASTQ_format) files in one of the sequencing run folders is used as an input argument). **A run is a group of results processed at the same time on the same machine representing the same molecular methods.** All sequencing folders in the main directory need to represent data from sequencing runs that have used the same primers and protocols. Output from this function includes all processing files and final main output files in the form of [fasta](https://en.wikipedia.org/wiki/FASTA_format) files and [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) tables. 
    
## [combine_dada_output()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#combine-dada-output)
DBTC dada_implement() uses [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) output files ('YYYY_MM_DD_HH_MM_UserInputRunName_Merge' and/or 'YYYY_MM_DD_HH_MM_UserInputRunName_MergeFwdRev') and combines them into a single [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) table and creates an accompanying [fasta](https://en.wikipedia.org/wiki/FASTA_format) file. This function also produces a file containing the processing information for the function. The main input argument for this function is the location of a file in a folder containing all [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) tables wanting to be combined. Output files are generated with the naming convention 'YYYY_MM_DD_HH_MM_combinedDada'.

## [make_BLAST_DB()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#make-blast-db)
This function takes a [fasta](https://en.wikipedia.org/wiki/FASTA_format) file with headers in the MACER format (Young et al. 2021) and establishes a database upon which a BLAST search can be completed. However, if a NCBI sequence database is desired, it is advisable to use, where applicable, NCBI preformatted databases and skip the make_BLAST_DB() function (https://www.ncbi.nlm.nih.gov/books/NBK62345/#blast_ftp_site.The_blastdb_subdirectory). The outcome of the function is a folder with a BLASTable NCBI formatted sequence database.

The MACER [fasta](https://en.wikipedia.org/wiki/FASTA_format) header format

```>GenBankAccessionOrBOLDID|GenBankAccession|Genus|species|UniqueID|Marker```

## [seq_BLAST()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#sequence-blast)
[Fasta](https://en.wikipedia.org/wiki/FASTA_format) file(s) are used as input along with a user selected NCBI formatted database upon which query sequences will be searched using BLAST. The outcome of the function are two files, a BLAST run file and a single file containing all of the BLAST results in tab delimited format. There are no headers in the BLAST results file but the columns (in order left to right) are: query sequence ID, search sequence ID, search taxonomic ID, query to sequence coverage, percent identity, search scientific name, search common name, query start, query end, search start, search end, e-value.

## [taxon_assign()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#taxon-assignment)
This function takes a BLAST result file and associated [fasta](https://en.wikipedia.org/wiki/FASTA_format) files (either on their own or with accompanying [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) files generated from the dada_implement() function) and collapses the multiple BLAST results into as single result for each query sequence. When an [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) table is present the taxonomic results will be combined with the [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) table.

## [combine_assign_output()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#combine-assignment-output)
The combine_assign_output() function takes a file selection and then uses all DBTC [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) taxon assign files ('_taxaAssign_YYYY_MM_DD_HHMM.tsv') in a selected directory and combines them into a single output 'YYYY_MM_DD_HHMM_taxaAssignCombined.tsv' file. The files being combined should represent different samples but representing data that have all come from analysis using the same molecular methods, the same analysis arguments, and the same molecular sequence databases.

## [reduce_taxa()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#reduce-taxa)
To reduce [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) results to unique taxa per file the reduce_taxa() function takes a file selection and then uses all '_taxaAssign_YYYY_MM_DD_HHMM.tsv' and/or 'YYYY_MM_DD_HHMM_taxaAssignCombined.tsv' files in that directory. This function then reduces all [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) with the same taxonomic assignment into a single result and places these results in a '_taxaReduced_YYYY_MM_DD_HHMM.tsv' file for each of the target files in the directory.

## [combine_reduced_output()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#combine-reduced-output)
This function takes a file selection and then uses all '_taxaReduced_YYYY_MM_DD_HHMM.tsv' files in that directory and combines them into a single 'YYYY_MM_DD_HHMM_CombineTaxaReduced.txt' taxa table file with presence absence results. The files being combined should represent the same biological samples but with different molecular marker information. The output [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) can include read numbers or be reduced to presence absence results.

([Back to Top](#table-of-contents))

***

# Naming convention rules

WARNING - NO WHITESPACE!

When running [DBTC](https://github.com/rgyoung6/DBTC) functions the paths for the files selected cannot have white space! File folder locations should be as short as possible (close to the [root](https://en.wikipedia.org/wiki/Root_directory) directory) as some functions do not process long naming conventions. 

Also, when using [DBTC](https://github.com/rgyoung6/DBTC) functions naming conventions need to carefully considered. Special characters should be avoided (including question mark, number sign, exclamation mark). It is recommended that dashes be used for separations in naming conventions while retaining underscores for use as information delimiters (this is how [DBTC](https://github.com/rgyoung6/DBTC) functions use underscore).  

There are several key character strings used in the [DBTC](https://github.com/rgyoung6/DBTC) pipeline, the presence of these strings in file or folder names will cause errors when running [DBTC](https://github.com/rgyoung6/DBTC) functions. 
The following strings are those used in [DBTC](https://github.com/rgyoung6/DBTC) and should not be used in file or folder naming:
  - _BLAST
  - _combinedDada
  - _taxaAssign
  - _taxaAssignCombined
  - _taxaReduced
  - _CombineTaxaReduced

([Back to Top](#table-of-contents))

***

# DBTCShiny Function Details

DBTCShiny uses buttons to select files necessary when running analyses. These buttons will bring up a dialog window (referred to as an 'Open' dialog in Mac OS or an 'Open File' dialog in Windows and a 'File Picker' dialog in Linux and referred to as a 'select file dialog window').

In addition to buttons which bring up select file dialog windows there are fillable fields and option buttons. 

All of these input elements are used to submit user variables and function options to [DBTC](https://github.com/rgyoung6/DBTC) functions via the DBTCShiny graphical user interface.

For package function details please see the [DBTC](https://github.com/rgyoung6/DBTC) descriptions and documentation.
- [dada_implement()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#dada-implement)
- [combine_dada_output()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#combine-dada-output)
- [make_BLAST_DB()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#make-blast-db)
- [seq_BLAST()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#sequence-blast)
- [taxon_assign()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#taxon-assignment)
- [combine_assign_output()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#combine-assignment-output)
- [reduce_taxa()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#reduce-taxa)
- [combine_reduced_output()](https://github.com/rgyoung6/DBTC/tree/main?tab=readme-ov-file#combine-reduced-output)

In addition to the implementation of [DBTC](https://github.com/rgyoung6/DBTC) core functions, DBTCShiny also provides an interactive mapping option for [DBTC](https://github.com/rgyoung6/DBTC) [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) files (see the next 'Mapping Dashboard' section below).

([Back to Top](#table-of-contents))

***

# Mapping Dashboard
DBTCShiny has interactive mapping functions. The following four sections provide information about the use of the mapping functions of DBTCShiny.

## Mapping

![image](https://github.com/rgyoung6/DBTCShiny/assets/60077841/0a645c6f-576c-4617-9960-e6b246e6b2f2)

([Back to Top](#table-of-contents))

## Data Import
Data import buttons to load [ASV](https://en.wikipedia.org/wiki/Amplicon_sequence_variant) data files generated by the DBTCShiny pipeline along with provenance data to visualize on the map (See below for an image of the graphical user interface and the formats of the files necessary for the mapping option).

![image](https://github.com/rgyoung6/DBTCShiny/assets/60077841/872c6676-1737-4669-bca9-23bd6b42e181)

**Provenance Data**

|Campaign|Sample|Run|Lab|Type|Date|West|North| 
|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|
|NationalPark2024|A001|Run1A|GuelphHanner|Sample|YYYY-MM-DD|43.5327|-80.2262|

**ASV Data Information Headers (These will be followed by columns of sample read data, in the below example there is a single sample 'A001')**

|superkingdom|phylum|class|order|family|genus|species|Top_BLAST|Final_Common_Names|Final_Rank|Final_Taxa|Result_Code|RepSequence|Number_ASV|Average_ASV_Length|Number_Occurrences|Average_ASV_Per_Sample|Median_ASV_Per_Sample|Results|A001|
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|Eukaryota(51.1,97,95.2,1e-76)|Chordata(51.1,97,95.2,1e-76)|Mammalia(51.1,97,95.2,1e-76)|Rodentia(51.1,97,95.2,1e-76)|Cricetidae(51.1,97,95.2,1e-76)|Microtus(51.1,97,95.2,1e-76)|Microtus pennsylvanicus(51.1,97,95.2,1e-76)|Microtus pennsylvanicus(100,97.143,6.96e-94)|meadow vole|species|Microtus pennsylvanicus(51.1,97,95.2,1e-76)|SFAT|AGCT|20|196.25|31|374.2580645|108|Merged|100|

([Back to Top](#table-of-contents))

## Data Filtering

The 'Data Filtering' tab provides options to filter out the visible data on the 'Mapping' tab and the data present in the 'Data Table' tab. 

![image](https://github.com/rgyoung6/DBTCShiny/assets/60077841/b650afe7-fab7-41aa-a4dc-b10b89237750)

([Back to Top](#table-of-contents))

## Data Table

A tabular display of the data loaded and filtered which is also being visualized on the map.

![image](https://github.com/rgyoung6/DBTCShiny/assets/60077841/54925e44-ef86-46d2-8d56-af67e7374b37)

([Back to Top](#table-of-contents))

***

# Citation
Young RG, et al., Hanner RH (2024) A Scalable, Open Source, Cross Platform, MetaBarcode Analysis Method using Dada2 and BLAST. Biodiversity Data Journal (In progress)

([Back to Top](#table-of-contents))
