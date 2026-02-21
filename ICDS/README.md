# ICDS
Identification of Cancer Dysfunctional Subpathway by Integrating DNA Methylation, Copy Number Variation, and Gene Expression Data

>Identify Cancer Dysfunctional Sub-pathway by integrating gene expression, DNA methylation and copy number variation, and pathway topological information. 1)We firstly calculate the gene risk scores by integrating three kinds of data: DNA methylation, copy number variation, and gene expression. 2)Secondly, we perform a greedy search algorithm to identify the key dysfunctional sub-pathways within the pathways for which the discriminative scores were locally maximal. 3)Finally, the permutation test was used to calculate statistical significance level for these key dysfunctional sub-pathways.

### how to install

```R
Installation method：

1. library(devtools); 
   install_github("hanjunwei-lab/ICDS")
2. install.packages("ICDS")

Use：
library(ICDS)
```

Please cite the following article when using `ICDS`:

Liu, S., B. Zheng, Y. Sheng, Q. Kong, Y. Jiang, Y. Yang, X. Han, L. Cheng, Y. Zhang, and J. Han, Identification of Cancer Dysfunctional Subpathways by Integrating DNA Methylation, Copy Number Variation, and Gene-Expression Data. Front Genet, 2019. 10: p. 441.
