#' @title Simulated Dataset
#'
#' @description The dataset consists of \eqn{N = 240} individuals and \eqn{R =
#'   4} data views with different data types. Two of the data views are
#'   continuous, one is count, and one is binary. The \emph{true} number of
#'   clusters was set to \eqn{K = 4}, and the cluster proportions were set at \eqn{\pi_1
#'   = 0.25, \pi_2 = 0.25, \pi_3 = 0.25, \pi_4 = 0.25}, such that we have
#'   balanced cluster proportions. Each of the data views had \eqn{p_r = 500}
#'   features, \eqn{r = 1, \dots, 4}, but only 50, or 10\%, were relevant
#'   features that contributed to the clustering, and the rest were noise
#'   features that did not contribute to the clustering. In total, there were
#'   \eqn{p = \sum_{r=1}^4 = 2000} features.
#'
#'   For data view 1 (continuous), relevant features were generated from the
#'   following normal distributions: \eqn{\text{N}(10, 1)} for Cluster 1,
#'   \eqn{\text{N}(5, 1)} for Cluster 2, \eqn{\text{N}(-5, 1)} for Cluster 3,
#'   and \eqn{\text{N}(-10, 1)} for Cluster 4, while noise features were
#'   generated from \eqn{\text{N}(0, 1)}. For data view 2 (continuous), relevant
#'   features were generated from the following normal distributions:
#'   \eqn{\text{N}(-10, 1)} for Cluster 1,    \eqn{\text{N}(-5, 1)} for Cluster
#'   2, \eqn{\text{N}(5, 1)} for Cluster 3, and \eqn{\text{N}(10, 1)} for
#'   Cluster 4, while noise features were generated from \eqn{\text{N}(0, 1)}.
#'   For data view 3 (binary), relevant features were generated from the
#'   following Bernoulli distributions: \eqn{\text{Bernoulli}(0.05)} for Cluster
#'   1,    \eqn{\text{Bernoulli}(0.2)} for Cluster 2,
#'   \eqn{\text{Bernoulli}(0.4)} for Cluster 3, and \eqn{\text{Bernoulli}(0.6)}
#'   for Cluster 4, while noise features were generated from
#'   \eqn{\text{Bernoulli}(0.1)}. For data view 4 (count), relevant features
#'   were generated from the following Poisson distributions:
#'   \eqn{\text{Poisson}(50)} for Cluster 1, \eqn{\text{Poisson}(35)} for
#'   Cluster 2, \eqn{\text{Poisson}(20)} for Cluster 3, and
#'   \eqn{\text{Poisson}(10)} for Cluster 4, while noise features were generated
#'   from \eqn{\text{Poisson}(2)}.
#'
#' @docType data
#' @keywords datasets
#' @name sim_data
#' @usage data(sim_data)
#' @format A list containing four datasets, and other elements of interest.
#'
NULL




#' @title LAML (Acute Myeloid Leukemia)  Data
#'
#' @description This is a subset of the LAML (Acute Myeloid Leukemia) data
#'   (TCGA, 2013). The Acute Myeloid Leukemia (laml_tcga) datasets were download
#'   using the cBioPortal for Cancer Genomics tool (Cerami et al., 2012; Gao et
#'   al., 2013). The 170 samples with gene expression data and mutation data
#'   were included. Only a subset of the genes was selected,
#'   as desribed below. To access the data containing all the genes, please
#'   visit: https://github.com/AbdalkarimA/iClusterVB
#'
#' @docType data
#' @name laml
#' @aliases laml.cli laml.exp laml.mut
#' @usage data(laml)
#' @return Within the data file, there is:
#'   \item{\code{laml.cli}:}{ A dataframe
#'   of clinical information for the 170 samples.}
#'   \item{\code{laml.exp}:}{ A
#'   matrix of 170 samples and the gene expression values of the 500 genes
#'   chosen by Zainul Abidin and Westhead (2016) based on having the  highest ranked-based
#'   coefficients of variation and standard deviation across the samples.
#'   Some names may have been updated or corrected from the
#'   supplementary material.}
#'   \item{\code{laml.mut}:}{ A
#'   matrix of 170 samples and the mutation status of 156 genes that had >=2
#'   mutations. 1 indicates the presence of mutation, and 0 indicates the
#'   absence of mutation.}
#'
#'
#' @references Cancer Genome Atlas Research Network, Ley, T. J., Miller, C.,
#'   Ding, L., Raphael, B. J., Mungall, A. J., Robertson, A., Hoadley, K.,
#'   Triche, T. J., Jr, Laird, P. W., Baty, J. D., Fulton, L. L., Fulton, R.,
#'   Heath, S. E., Kalicki-Veizer, J., Kandoth, C., Klco, J. M., Koboldt, D. C.,
#'   Kanchi, K. L., Kulkarni, S., … Eley, G. (2013). Genomic and epigenomic
#'   landscapes of adult de novo acute myeloid leukemia. The New England journal
#'   of medicine, 368(22), 2059–2074. https://doi.org/10.1056/NEJMoa1301689
#'
#'   Cerami, E., Gao, J., Dogrusoz, U., Gross, B. E., Sumer, S. O., Aksoy, B.
#'   A., Jacobsen, A., Byrne, C. J., Heuer, M. L., Larsson, E., Antipin, Y.,
#'   Reva, B., Goldberg, A. P., Sander, C., & Schultz, N. (2012). The cBio
#'   cancer genomics portal: an open platform for exploring multidimensional
#'   cancer genomics data. Cancer discovery, 2(5), 401–404.
#'   https://doi.org/10.1158/2159-8290.CD-12-0095
#'
#'   Gao, J., Aksoy, B. A., Dogrusoz, U., Dresdner, G., Gross, B., Sumer, S. O.,
#'   Sun, Y., Jacobsen, A., Sinha, R., Larsson, E., Cerami, E., Sander, C., &
#'   Schultz, N. (2013). Integrative analysis of complex cancer genomics and
#'   clinical profiles using the cBioPortal. Science signaling, 6(269), pl1.
#'   https://doi.org/10.1126/scisignal.2004088
#'
#'   Zainul Abidin, F. N., & Westhead, D. R. (2017). Flexible model-based
#'   clustering of mixed binary and continuous data: application to genetic
#'   regulation and cancer. Nucleic acids research, 45(7), e53.
#'   https://doi.org/10.1093/nar/gkw1270
NULL
