## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, out.width='60%', fig.cap="*Figure 1*. Example lineage-associated mutation file."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig1.jpg")

## ----echo=FALSE, out.width='60%', fig.cap="*Figure 2*. Example location/date file."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig2.jpg")

## ----echo=FALSE, out.width='60%', fig.cap="*Figure 3*. Example features (bed) input file."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig3.jpg")

## ----echo=FALSE, out.width='60%', fig.cap="*Figure 4*. Example lineage-associated mutation file."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig1.jpg")

## ----echo=FALSE, out.width='60%', fig.cap="*Figure 5*. Example location/date file."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig2.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 6*. Example of the initial *MixviR* reference object. Two non-genic positions and 6 genic positions, representing 2 amino acids, are shown."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig6.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 7*. *MixviR* reference object with sequencing depths for the sample being analyzed added in the 'DP' column. 'NA' in this column indicates the position was not included in the input vcf file."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig7.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 8*. Example set of variants from a sample being analyzed after filtering for the default minimum allele frequency of 1%. Deletions have length >1bp in the REF column, while insertions have length >1bp in the 'ALT' column. Variants are merged with the reference to create a “sample genome” that is translated to identify amino acid changes."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig8.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 9*. Example set of SNP-based mutations. Synonymous and non-synonymous mutations in two different genes, and a mutation in a non-genic region, are included."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig9.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 10*. Example insertions and deletions called from a sample during a *MixviR* anaysis. Deletions are indicated with 'del' in the name, and insertions with 'ins' - see *samp_identity* column for examples."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig10.jpg")

## ---- eval = FALSE------------------------------------------------------------
#  samp_mutations <- call_mutations(sample.dir = system.file("extdata", "vcfs",
#                                                            package = "MixviR"),
#                                   reference = "Wuhan",
#                                   name.sep = "_")
#  

## ----echo=FALSE, out.width='80%', fig.cap="*Figure 11.* The *samp_mutations* data frame is the primary output of the `call_mutations()` function."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig11.jpg")

## ---- eval = FALSE------------------------------------------------------------
#  explore_mutations(muts.df = samp_mutations)

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 12*. 'View Mutations' RShiny tab."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig12.jpg")

## ---- eval = FALSE------------------------------------------------------------
#  explore_mutations(muts.df = samp_mutations,
#                    dates = system.file("extdata",
#                                        "example_location_date.csv",
#                                        package = "MixviR"))

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 13*. 'New Mutations' RShiny tab. The displayed mutations were first observed on or after the selected date."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig13.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 14*. 'Mutation Frequencies' RShiny tab displaying frequencies of specific mutations over time."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig14.jpg")

## ---- eval = FALSE------------------------------------------------------------
#  explore_mutations(muts.df = samp_mutations,
#                    dates = system.file(muts.df = samp_mutations,
#                                        "extdata",
#                                        "example_location_date.csv",
#                                        package = "MixviR"),
#                    lineage.muts = system.file("extdata",
#                                               "example_lineage_muts.csv",
#                                               package = "MixviR"))

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 15*. 'Variants Present' RShiny tab identifying lineages present in samples and estimating their frequencies."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig15.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 16*. 'Variants Present' RShiny tab after adjusting the threshold for calling a lineage as present, and now only identifying P.1 as present in the 2021-05-02 sample."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig16.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 17*. Example of a tooltip on Lineages Present plot."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig17.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 18*. Example tooltop on lineage frequency plot."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig18.jpg")

## ---- eval = FALSE------------------------------------------------------------
#  samp_mutations <- call_mutations(sample.dir = system.file("extdata",
#                                                            "vcfs",
#                                                            package = "MixviR"),
#                 reference = "Wuhan",
#                 name.sep = "_",
#                 write.all.targets = TRUE,
#                 lineage.muts = system.file("extdata",
#                                            "example_lineage_muts.csv",
#                                            package = "MixviR"))
#  

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 19*. Example of the *samp_mutations* data frame when the *write.all.muts* option is used to include sequencing depths for mutations not observed in the sample."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig19.jpg")

## ----echo=FALSE, out.width='100%', fig.cap="*Figure 20*. Example tooltip in `explore_mutations()` run with *all.target.muts* set to TRUE."----
knitr::include_graphics("https://raw.githubusercontent.com/mikesovic/MixviR/main/MixviR_v3.5.0/inst/images/Fig20.jpg")

## ---- eval = FALSE, echo = FALSE----------------------------------------------
#  #Note: Example data are from...
#  #0PD00000 April 18, May 2, Aug 15

