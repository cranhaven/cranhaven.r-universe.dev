\name{gse1992pheno}
\alias{gse1992pheno}
\docType{data}
\title{
Phenotype data of GSE1992

}
\description{
Data such as patients' information, tumor characteristics, samples information related to GSE1992

}
\usage{data("gse1992pheno")}
\format{
  A data frame with 124 observations on the following 24 variables.
  \describe{
    \item{\code{GEO_array_names}}{a character vector}
    \item{\code{Array_Name}}{a character vector}
    \item{\code{Hu_et_al.._BMC_Genomics_7.96_.27Apr2006.}}{a character vector}
    \item{\code{intrinsic_pairs_from_Hu_et_al._2006._exact_RNA_replicates._or_cell_line_experiments}}{a character vector}
    \item{\code{Oh_et_al.._Journal_of_Clinical_Oncology._Apr_10._24.11..1656.64_.2006.}}{a character vector}
    \item{\code{Weigelt_et_al.._Cancer_Research._65.20..9155.8_.2005..}}{a character vector}
    \item{\code{Perreard_et_al.._Breast_Cancer_Research._Apr_20.8.2..R23_.2006.}}{a character vector}
    \item{\code{Herschkowitz_et_al.._Genome_Biol._2007_May_10.8.5..R76}}{a character vector}
    \item{\code{Mullins_et_al.._Clinical_Chemistry_.27._53.7..1273.9}}{a character vector}
    \item{\code{Hoadley_et_al.._BMC_Genomics_8.258_.2007.}}{a character vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{ER__.1.positive._0.negative.}}{a numeric vector}
    \item{\code{Node_status_.1.positive.1_or_more_nodes.._0.negative.}}{a numeric vector}
    \item{\code{Grade}}{a numeric vector}
    \item{\code{Size_.1._..2cm._2._.2cm_to_..5cm._3..5cm._4.any_size_with_direct_extension_to_chest_wall_or_skin.}}{a numeric vector}
    \item{\code{RFS_event_.0.no_relapse.1.relapsed_at_any_site_or_died_of_disease.}}{a numeric vector}
    \item{\code{RFS_months}}{a numeric vector}
    \item{\code{Overall_Survival_Event_.0.alive.1.DOD_or_DOC.}}{a numeric vector}
    \item{\code{Overall_suvival_months}}{a numeric vector}
    \item{\code{Singe_Sample_Predictor_Subtype_.S.SSP_Subtype_assigned_according_to_Hu_et_al._2005._except_for_claudin.low_samples_that_were_identified_by_clustering_analysis}}{a character vector}
    \item{\code{Hoadley_et_al._5.class_Subtype}}{a character vector}
    \item{\code{GEO_individual_microarray_Accession_number}}{a character vector}
    \item{\code{Target_Source_2_.Cy5.}}{a character vector}
    \item{\code{Target_source_1_.Cy3.}}{a character vector}
  }
}
\source{
\url{https://genome.unc.edu/pubsup/breastGEO/UNC-GEO-9-Papers-for-Website-12-23-2009.xls}
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}
\seealso{
 \code{\link{gse1992}} 
}
\examples{
data(gse1992pheno)
}
