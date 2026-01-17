#' Genotype and gene expression data from the GEUVADIS project
#'
#' The GEUVADIS (Genetic European Variation in Disease) project (Lappalainen et
#' al., 2013) measured the gene expression in Lymphoblastoid Cell Lines (LCLs)
#' from a subset of individuals from the 1,000 Genomes Project (Auton et al.,
#' 2015), which measured the genotypes of individuals from multiple ethnicities.
#' This data set contains genotype and gene expression data from 373 European
#' individuals. The data are divided into 46 sets which each contain data from
#' an expression quantitative trait locus (eQTL) and its associated genes.
#'
#' @docType data
#'
#' @usage data(geuvadis)
#'
#' @format An object of class \code{"list"} containing 46 eQTL-gene sets. Each
#' element of the list is a matrix with the observations down the rows and the
#' eQTL and genes across the columns. The first column in the matrix contains
#' the genotype (eQTL) data and the remaining columns contain the gene
#' expression data.
#'
#' @references Lappalainen, T., Sammeth, M., Friedländer, M. et al.
#' Transcriptome and genome sequencing uncovers functional variation in humans.
#' Nature 501, 506–511 (2013) doi:10.1038/nature12531
#'
#' Auton, A., Abecasis, G., Altshuler, D. et al. A global reference for human
#' genetic variation. Nature 526, 68–74 (2015) doi:10.1038/nature15393
#'
#' @source \url{https://www.ebi.ac.uk/arrayexpress/files/E-GEUV-1/analysis_
#' results/}
#'
#' @examples
#' # Load the data.
#' data(geuvadis)
#'
#' # Display the first 6 rows of the eQTL-gene set Q8.
#' head(geuvadis$Q8)
#'
'geuvadis'

#' Tissue type and transcription factor binding data during Drosophila mesoderm
#' development
#'
#' Zinzen et al. (2009) measured in vivo transcription factor binding for five
#' key transcription factors using ChIP-chip assays at two hour intervals during
#' drosophila mesoderm development. The five transcription factors are: Twist
#' (Twi), Tinman (Tin), Myocyte enhancing factor 2 (Mef2), Bagpipe (Bap), and
#' Biniou (Bin). Both Twi and Tin were assayed from 2-8 hours, Mef2 from 2-12
#' hours, Bap from 6-8 hours, and Bin from 6-12 hours. In addition, Zinzen et
#' al. identified six tissue types based on tissue specific expression: mesoderm
#' (Meso), somatic muscle (SM), visceral muscle (VM), cardiac muscle (CM),
#' mesoderm and somatic muscle (Meso&SM), and somatic and visceral muscle
#' (SM&VM). All data are binary and measured at 310 cis-regulatory modules.
#'
#' @docType data
#'
#' @usage data(drosophila)
#'
#' @format An object of class \code{"list"} containing tissue type and
#' transcription factor binding data. The first element in the list is a matrix
#' which contains the data in the original form. The second element in the list
#' is a matrix that contains the binary data. For this matrix any value greater
#' than zero to changed to a one. For both data sets the tissue type data
#' appears in the first six columns of the data matrix and the remaining 15
#' columns contain the transcription factor binding data.
#'
#' @references Zinzen, R., Girardot, C., Gagneur, J. et al. Combinatorial
#' binding predicts spatio-temporal cis-regulatory activity. Nature 462, 65–70
#' (2009) doi:10.1038/nature08531
#'
#' @source \url{https://www.nature.com/articles/nature08531#Sec22}
#'
#' @examples
#' # Load the data.
#' data(drosophila)
#'
#' # Display the first 5 rows and 8 columns of the continuous data matrix.
#' drosophila$continuous[1:5, 1:8]
#'
#' # Display the first 5 rows and 8 columns of the discrete data matrix.
#' drosophila$discrete[1:5, 1:8]
#'
'drosophila'
