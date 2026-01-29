#' DEGRE package
#'
#' Genes that are differentially expressed (DEGs) between two or more experimental conditions can be found in investigations using RNA-Seq data. The gene reads matrix is a count matrix that is used to infer DEGs, and fixed effects are often utilized in this inference. A high biological variability may impact the discovery of DEGs once it may be divergent between the fixed effects. This variability can be covered by the random effects. The DEGRE R package was designed to identify DEGs in RNA-Seq experiments considering fixed and the random effects on individuals. These effects are identified earlier in the experimental design matrix. DEGRE has the implementation of preprocessing procedures to clean the count matrix of the gene reads, and it fits a regression for each gene using the Generalized Linear Mixed Model with the negative binomial distribution,followed by a Wald test to test to assess the regression coefficients.
#'
#' @docType package
#'
#' @author Douglas Terra Machado \email{dougterra@gmail.com}, Otávio José Bernardes Brustolini, Yasmmin Côrtes Martins, Marco Antonio Grivet Mattoso Maia, Ana Tereza Ribeiro de Vasconcelos
#'
#' @name DEGRE_package
NULL
