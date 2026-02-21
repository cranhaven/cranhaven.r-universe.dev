\name{envData}
\alias{envData}
\alias{exp_data}
\alias{meth_data}
\alias{cnv_data}
\alias{amp_gene}
\alias{del_gene}
\alias{zzz}
\alias{exp.p}
\alias{meth.p}
\alias{cnv.p}
\alias{label1}
\alias{label2}
\alias{subpathdata}
\alias{opt_subpathways}
\docType{data}

\title{The variables in the environment include an example expression profile,an methylation profile,an copy number variation data,amplified genes,deleted genes,A numeric vector of z_scores,p-values,A vector of 0/1s, indicating the class of samples,interested subpathways,Optimized subpathway,and the  statistical significance p value and FDR for these optimal subpathways
}
\description{
Identify Cancer Dysfunctional Subpathway by integrating gene expression, DNA methylation and copy number variation, and pathway topological information. 1)We firstly calculate the gene risk scores by integrating three kinds of data: DNA methylation, copy number variation, and gene expression.  2)Secondly, we perform a greedy search algorithm to identify the key dysfunctional subpathways within the pathways for which the discriminative scores were locally maximal. 3)Finally, the permutation test was used to calculate statistical significance level for these key dysfunctional subpathways.
  }
\details{
The environment variable includes the variable \code{exp_data}, \code{meth_data},\code{cnv_data},\code{amp_gene},\code{del_gene},\code{zzz},\code{exp.p},\code{meth.p},\code{cnv.p},\code{label1},\code{label2},\code{subpathdata},\code{opt_subpathways}
}

\format{An environment variable}
\author{Junwei Han\email{hanjunwei1981@163.com},Baotong Zheng\email{btzheng1116@163.com},Siyao Liu \email{liusiyao29@163.com}}

\keyword{data}
