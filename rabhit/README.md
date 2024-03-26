# RAbHIT: R Antibody Haplotype Infrence Tool 

Analysis of antibody repertoires by high throughput sequencing is of major importance in understanding adaptive immune responses. Our knowledge of variations in the genomic loci encoding antibody genes is incomplete, mostly due to technical difficulties in aligning short reads to these highly repetitive loci. The partial knowledge results in conflicting V-D-J gene assignments between different algorithms, and biased genotype and haplotype inference. Previous studies have shown that haplotypes can be inferred by taking advantage of IGHJ6 heterozygosity, observed in approximately one third of the population.

**RAbHIT is a haplotype infrence tool based on a robust novel method for determining V-D-J haplotypes by adapting a Bayesian framework**. Our method extends haplotype inference to IGHD, IGHV, IGKJ, IGKV, and IGLV based analysis, thereby enabling inference of complex genetic events like deletions and copy number variations in the entire population. Based on this method we developed an R package, which implements the method on sequences from naive B-cells, for both the heavy and the light chains. The package offers a haplotype and single chromosome deletion inference based on an anchor gene.  The inferred haplotypes and deletion patterns may have clinical implications for genetic predispositions to diseases. 


## Core Abilities ##

* Haplotype inference
* Single chromosome deletion detection
* Two chromosome deletion detection

## Required Input ##

* Pre-processed antibody repertoire sequencing data with heterozygosity in at least one gene. Antibody repertoire sequencing data is in a data frame format. Each row represents a unique observation and columns represent data about that observation. The names of the required columns are provided below along with a short description.
* Database of germline gene sequences

| Column name   | Description                                                       |
| ------------- |-------------------------------------------------------------------|
| Subject name  | Subject name                                                      |
| V_CALL        | (Comma separated) name(s) of the nearest V allele(s) (IMGT format)|
| D_CALL        | (Comma separated) name(s) of the nearest D allele(s) (IMGT format)|
| J_CALL        | (Comma separated) name(s) of the nearest J allele(s) (IMGT format)|



## Installation ##

RAbHIT is available for installion either from CRAN or from the development version.

### RAbHIT CRAN installation ###

```R
install.packages("rabhit")
```

### RAbHIT repository installation ###

To build from the source code, first install the build dependencies:  

```R
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown", "plotly"))
```

To install the latest version via devtools:

```R
library(devtools)
install_bitbucket("yaarilab/rabhit")
```

Note, installing from bitbucket does not generate the documentation.
To generate them, first clone the repository and then build:

```R
library(devtools)
install_deps()
document()
build()
install()
```

## Documentation ##

A complete documentation of RAbHIT is available at: https://yaarilab.bitbucket.io/RAbHIT/ or in your local repository at: ./vignettes/RAbHIT-vignette.html


## Contact ##

For help, questions, or suggestions, please contact:

* [Ayelet Peres](mailto:peresay@biu.ac.il)
* [Moriah Gidoni](mailto:moriah.cohen@biu.ac.il)
* [Gur Yaari](mailto:gur.yaari@biu.ac.il)
* [Issue tracker](https://bitbucket.org/yaarilab/rabhit/issues?status=new&status=open)


## Copying ##

RAbHIT is free for use under the [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/legalcode)
