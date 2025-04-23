# PIgLET - Program for Ig clusters R package
PIgLET is a suite of computational tools that improves genotype inference and downstream AIRR-seq data analysis. The package as two main tools. The first is Allele Clusters, this tool is designed to reduce the ambiguity within the IGHV alleles. The ambiguity is caused by duplicated or similar alleles which are shared among different genes. The second tool is an allele based genotype, that determined the presence of an allele based on a threshold derived from a naive population. 


## Core Abilities ##

**Allele Similarity Cluster** provides the functions that support the main tool of creating the allele similarity cluster form an IGHV germline set.
**Allele based genotype** provides the functions to infer the IGHV genotype using the allele based method and the allele clusters thresholds.

## Required Input ##

For the allele similarity cluster, a reference set of IUIS/IMGT alleles. The sequences should be gapped.

For the genotype inference, an AIRR-seq format data set.

## Installation ##

PIgLET is available for installation either from the development version.

### PIgLET repository installation ###

To build from the source code, first install the build dependencies:  

```R
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown", "plotly"))
```

To install the latest version via devtools:

```R
library(devtools)
install_bitbucket("yaarilab/piglet")
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

## Contact ##

For help, questions, or suggestions, please contact:

* [Ayelet Peres](mailto:peresay@biu.ac.il)
* [Gur Yaari](mailto:gur.yaari@biu.ac.il)

## Copying ##

RAbHIT is free for use under the [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/legalcode)
