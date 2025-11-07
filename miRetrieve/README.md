# miRetrieve

miRetrieve is designed for microRNA text mining in abstracts. 
By extracting, counting, and analyzing miRNA names from literature, miRetrieve 
aims at providing biological insights from a large amount of text within a short 
period of time.

## Getting Started

An online version with the most important functions of miRetrieve is available 
under https://miretrieve.shinyapps.io/miRetrieve/.

To install miRetrieve from CRAN, run

```
install.packages("miRetrieve")
```

Alternatively, you can also install miRetrieve from GitHub by running

```
install.packages("devtools")

devtools::install_github("JulFriedrich/miRetrieve",
        dependencies = TRUE,
        repos = "https://cran.r-project.org/")
```

miRetrieve is built around the idea of using field-specific PubMed
abstracts from [PubMed](https://pubmed.ncbi.nlm.nih.gov/) to characterize and 
analyze microRNAs in disease-related fields (e.g. "miRNAs in diabetes").

To get started, download a microRNA-related abstract from PubMed via 
*Save - Format: PMID - Create file* and load it into R using

```
df <- miRetrieve::read_pubmed("PubMed_file.txt")
```

and subsequently extract all microRNAs with 

```
df <- extract_mir_df(df)
```

An extensive Vignette with the underlying mechanism, functions, and a 
complete workflow is available under

https://julfriedrich.github.io/miRetrieve/articles/miRetrieve.html

## Authors

**Julian Friedrich**, **Hans-Peter Hammes**, **Guido Krenning**

## License

miRetrieve is published under the GPL-3 license.

## Publication

miRetrieve and its functions are presented in a manuscript,
currently under review.

Supplementary Files referenced in the manuscript are located
in a different repository, freely available under

https://github.com/JulFriedrich/miRetrieve-paper

## Reference

## Acknowledgments

* `join_mirtarbase` is based on the latest miRTarBase version 8.0
(http://miRTarBase.cuhk.edu.cn/). If you use miRetrieve to visualize
miRNA-mRNA interactions based on miRTarBase, please make sure to cite
*Hsi-Yuan Huang, Yang-Chi-Dung Lin, Jing Li, et al.,
miRTarBase 2020: updates to the experimentally validated microRNA–target
interaction database, Nucleic Acids Research, Volume 48, Issue D1,
08 January 2020, Pages D148–D154, https://doi.org/10.1093/nar/gkz896.*

* `compare_mir_terms_log2()`, `compare_mir_count_log2()`, and 
`compare_mir_terms_scatter()` are greatly inspired by 
“tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” by
Silge and Robinson (https://www.tidytextmining.com/). In addition, "tidytext"
provides a valuable resource of general text mining in R.

* Key packages for miRetrieve are *tidytext*, *topicmodels*, 
and the packages included in the *tidyverse* (see Vignette).
