\name{TCGA_survivalData}
\alias{TCGA_survivalData}
\docType{data}
\title{
TCGA data for survival analysis
}
\description{
TCGA data is a clinical and microarray dataset consisting of 572 samples
(observations) in row and 162 variables (20 clinical variables such as age, sex
month, status,race, genotype and 142 DE genes)
}
\usage{data("TCGA_survivalData")}
\format{
  A data frame with 572 observations on the following 162 variables.
  \describe{
    \item{\code{sample}}{a character vector}
    \item{\code{sampleID}}{a numeric vector}
    \item{\code{status}}{a numeric vector}
    \item{\code{vital_status.demographic}}{a character vector}
    \item{\code{day}}{a numeric vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{geneder}}{a character vector}
    \item{\code{sex}}{a numeric vector}
    \item{\code{race.demographic}}{a character vector}
    \item{\code{race}}{a numeric vector}
    \item{\code{year_of_birth.demographic}}{a numeric vector}
    \item{\code{year_of_death.demographic}}{a numeric vector}
    \item{\code{age_at_diagnosis.diagnoses}}{a numeric vector}
    \item{\code{days_to_death.demographic}}{a numeric vector}
    \item{\code{egfr_mutation_performed}}{a character vector}
    \item{\code{eml4_alk_translocation_performed}}{a character vector}
    \item{\code{followup_treatment_success}}{a character vector}
    \item{\code{additional_pharmaceutical_therapy}}{a character vector}
    \item{\code{additional_radiation_therapy}}{a character vector}
    \item{\code{SLC25A13}}{a numeric vector}
    \item{\code{PSMC4}}{a numeric vector}
    \item{\code{MRE11A}}{a numeric vector}
    \item{\code{PTCD2}}{a numeric vector}
    \item{\code{STYK1}}{a numeric vector}
    \item{\code{GOLGA5}}{a numeric vector}
    \item{\code{IARS2}}{a numeric vector}
    \item{\code{KIF2A}}{a numeric vector}
    \item{\code{HLTF}}{a numeric vector}
    \item{\code{C14orf166}}{a numeric vector}
    \item{\code{GEMIN2}}{a numeric vector}
    \item{\code{CDC6}}{a numeric vector}
    \item{\code{MSH2}}{a numeric vector}
    \item{\code{FKBP3}}{a numeric vector}
    \item{\code{PSMC6}}{a numeric vector}
    \item{\code{PSMB5}}{a numeric vector}
    \item{\code{CSNK2A1}}{a numeric vector}
    \item{\code{KNOP1}}{a numeric vector}
    \item{\code{AVL9}}{a numeric vector}
    \item{\code{DNAJC2}}{a numeric vector}
    \item{\code{RPA3}}{a numeric vector}
    \item{\code{TMEM106B}}{a numeric vector}
    \item{\code{COA1}}{a numeric vector}
    \item{\code{GAPDH}}{a numeric vector}
    \item{\code{FAF2}}{a numeric vector}
    \item{\code{UCHL5}}{a numeric vector}
    \item{\code{CBX3}}{a numeric vector}
    \item{\code{HNRNPA2B1}}{a numeric vector}
    \item{\code{CALU}}{a numeric vector}
    \item{\code{GAD1}}{a numeric vector}
    \item{\code{PDLIM4}}{a numeric vector}
    \item{\code{TIMM17A}}{a numeric vector}
    \item{\code{NAA35}}{a numeric vector}
    \item{\code{GOLM1}}{a numeric vector}
    \item{\code{SULF1}}{a numeric vector}
    \item{\code{FANCI}}{a numeric vector}
    \item{\code{CLTC}}{a numeric vector}
    \item{\code{SDHC}}{a numeric vector}
    \item{\code{MRPL9}}{a numeric vector}
    \item{\code{ACP1}}{a numeric vector}
    \item{\code{FBXO28}}{a numeric vector}
    \item{\code{PDIA6}}{a numeric vector}
    \item{\code{SIGMAR1}}{a numeric vector}
    \item{\code{ADAM12}}{a numeric vector}
    \item{\code{XRCC4}}{a numeric vector}
    \item{\code{PLOD2}}{a numeric vector}
    \item{\code{TMEM87B}}{a numeric vector}
    \item{\code{BUB3}}{a numeric vector}
    \item{\code{DPY19L4}}{a numeric vector}
    \item{\code{HLCS}}{a numeric vector}
    \item{\code{PSMC2}}{a numeric vector}
    \item{\code{SLBP}}{a numeric vector}
    \item{\code{ORC5}}{a numeric vector}
    \item{\code{SGPL1}}{a numeric vector}
    \item{\code{GREM1}}{a numeric vector}
    \item{\code{VKORC1}}{a numeric vector}
    \item{\code{ZNF146}}{a numeric vector}
    \item{\code{CHAF1A}}{a numeric vector}
    \item{\code{TK1}}{a numeric vector}
    \item{\code{FEN1}}{a numeric vector}
    \item{\code{FRMD5}}{a numeric vector}
    \item{\code{DPY19L1}}{a numeric vector}
    \item{\code{CKAP5}}{a numeric vector}
    \item{\code{ZNF93}}{a numeric vector}
    \item{\code{SEMA4B}}{a numeric vector}
    \item{\code{PRMT3}}{a numeric vector}
    \item{\code{ERO1L}}{a numeric vector}
    \item{\code{DNAJC9}}{a numeric vector}
    \item{\code{FMO3}}{a numeric vector}
    \item{\code{FHL1}}{a numeric vector}
    \item{\code{DAPK2}}{a numeric vector}
    \item{\code{PREX2}}{a numeric vector}
    \item{\code{VPS13D}}{a numeric vector}
    \item{\code{TGFBR3}}{a numeric vector}
    \item{\code{NDST1}}{a numeric vector}
    \item{\code{LIMS2}}{a numeric vector}
    \item{\code{GLI2}}{a numeric vector}
    \item{\code{ACACB}}{a numeric vector}
    \item{\code{PIK3C3}}{a numeric vector}
    \item{\code{PCM1}}{a numeric vector}
    \item{\code{RUNX1T1}}{a numeric vector}
    \item{\code{NCOA1}}{a numeric vector}
    \item{\code{TTC28}}{a numeric vector}
    \item{\code{CBX7}}{a numeric vector}
    \item{\code{CPED1}}{a numeric vector}
    \item{\code{PTGDS}}{a numeric vector}
    \item{\code{SPOCK2}}{a numeric vector}
    \item{\code{GAB1}}{a numeric vector}
    \item{\code{FOXP1}}{a numeric vector}
    \item{\code{RAP1A}}{a numeric vector}
    \item{\code{TCF21}}{a numeric vector}
    \item{\code{WDR35}}{a numeric vector}
    \item{\code{WBP4}}{a numeric vector}
    \item{\code{SORBS3}}{a numeric vector}
    \item{\code{TBX2}}{a numeric vector}
    \item{\code{ITIH5}}{a numeric vector}
    \item{\code{PZP}}{a numeric vector}
    \item{\code{ATP1B2}}{a numeric vector}
    \item{\code{CBFA2T3}}{a numeric vector}
    \item{\code{GSTM5}}{a numeric vector}
    \item{\code{GSTM3}}{a numeric vector}
    \item{\code{ADAMTS8}}{a numeric vector}
    \item{\code{GYPC}}{a numeric vector}
    \item{\code{ARHGAP24}}{a numeric vector}
    \item{\code{PDE5A}}{a numeric vector}
    \item{\code{FBLN5}}{a numeric vector}
    \item{\code{IQSEC1}}{a numeric vector}
    \item{\code{SNCA}}{a numeric vector}
    \item{\code{HMBOX1}}{a numeric vector}
    \item{\code{NFIB}}{a numeric vector}
    \item{\code{PAMR1}}{a numeric vector}
    \item{\code{SAP18}}{a numeric vector}
    \item{\code{IGSF10}}{a numeric vector}
    \item{\code{ANGPT1}}{a numeric vector}
    \item{\code{JAM2}}{a numeric vector}
    \item{\code{PTH1R}}{a numeric vector}
    \item{\code{NFASC}}{a numeric vector}
    \item{\code{DLC1}}{a numeric vector}
    \item{\code{FIGF}}{a numeric vector}
    \item{\code{PKNOX2}}{a numeric vector}
    \item{\code{NDRG2}}{a numeric vector}
    \item{\code{FRAT1}}{a numeric vector}
    \item{\code{ANAPC16}}{a numeric vector}
    \item{\code{TNXB}}{a numeric vector}
    \item{\code{SFTPC}}{a numeric vector}
    \item{\code{KCNK3}}{a numeric vector}
    \item{\code{SYNPO}}{a numeric vector}
    \item{\code{ID4}}{a numeric vector}
    \item{\code{PER1}}{a numeric vector}
    \item{\code{SLIT3}}{a numeric vector}
    \item{\code{FOXO4}}{a numeric vector}
    \item{\code{GJA4}}{a numeric vector}
    \item{\code{PCBP2}}{a numeric vector}
    \item{\code{ADARB1}}{a numeric vector}
    \item{\code{PARVA}}{a numeric vector}
    \item{\code{CFD}}{a numeric vector}
    \item{\code{ARHGEF15}}{a numeric vector}
    \item{\code{GPR20}}{a numeric vector}
    \item{\code{HSBP1}}{a numeric vector}
    \item{\code{RPS28}}{a numeric vector}
    \item{\code{TNXA}}{a numeric vector}
    \item{\code{FXYD1}}{a numeric vector}
  }
}

\source{
https://www.cancer.gov/ccg/access-data
}

\examples{
data(TCGA_survivalData)
}
\keyword{datasets}
