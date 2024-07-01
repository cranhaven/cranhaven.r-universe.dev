## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=TRUE, message=FALSE---------------------------------------------
library(taxonbridge)
sample <- load_sample()
dim(sample)

## ---- include=TRUE------------------------------------------------------------
lineages <- get_lineages(sample)

## ---- include= TRUE-----------------------------------------------------------
kingdom <- get_validity(lineages, rank = "kingdom", valid = FALSE)
family <- get_validity(lineages, rank = "family", valid = FALSE)
candidates <- list(kingdom, family)

## ---- include= TRUE-----------------------------------------------------------
get_inconsistencies(candidates, uninomials = FALSE)

## ---- include= TRUE-----------------------------------------------------------
lineages[lineages$canonicalName=="Attheya septentrionalis", "taxonomicStatus"]

## ---- include= TRUE-----------------------------------------------------------
lineages <- get_status(get_lineages(sample), status = "accepted")
kingdom <- get_validity(lineages, rank = "kingdom", valid = FALSE)
family <- get_validity(lineages, rank = "family", valid = FALSE)
candidates <- list(kingdom, family)
get_inconsistencies(candidates, uninomials = FALSE)

## ---- include=TRUE, message=FALSE---------------------------------------------
library(taxonbridge)
sample <- load_sample()
decapoda <- get_taxa(sample, order = "decapoda")

## ---- include=TRUE, message=FALSE---------------------------------------------
swimming_crabs <- get_taxa(sample, family = "portunidae")

## ---- include=TRUE, message=FALSE---------------------------------------------
decapoda <- annotate(decapoda, names = swimming_crabs$canonicalName, 
                     new_column = "swimming_crabs", present = "1")

## ---- include=TRUE, message=TRUE----------------------------------------------
colnames(decapoda)

## ---- include=TRUE, message=TRUE----------------------------------------------
decapoda[!is.na(decapoda$swimming_crabs),"canonicalName"]

## ---- include=TRUE, message=TRUE, fig.show="hold"-----------------------------
GBIF_dist <- prepare_rank_dist(decapoda, GBIF = TRUE)
NCBI_dist <- prepare_rank_dist(decapoda, NCBI = TRUE)
plot_mdb(GBIF_dist)
plot_mdb(NCBI_dist)

## ---- include=TRUE, message=TRUE----------------------------------------------
GBIF_dist
NCBI_dist 

## ---- include=TRUE, message=TRUE, fig.show="hold"-----------------------------
lineages <- get_lineages(decapoda)
GBIF_dist <- prepare_rank_dist(lineages, GBIF = TRUE)
NCBI_dist <- prepare_rank_dist(lineages, NCBI = TRUE)
plot_mdb(GBIF_dist)
plot_mdb(NCBI_dist)

## ---- include=TRUE, message=TRUE----------------------------------------------
get_validity(lineages, valid = FALSE)

## ---- include=TRUE, message=TRUE----------------------------------------------
decapoda <- annotate(decapoda, get_validity(lineages, valid = FALSE)$canonicalName,
                    new_column = "family_inconsistencies", present = 1)
colnames(decapoda)
decapoda[!is.na(decapoda$family_inconsistencies),"canonicalName"]

## ---- include=TRUE,eval = FALSE-----------------------------------------------
#  library(taxonbridge)
#  
#  #Retrieve and merge NCBI and GBIF data. INSERT PATH TO YOUR TAXONKIT INSTALLATION.
#  custom_taxonomy <- load_taxonomies(download_gbif(),
#                                     download_ncbi(taxonkitpath = "/path/to/taxonkit"))
#  
#  #Create a custom taxonomy of all gastropods
#  custom_taxonomy <- get_taxa(custom_taxonomy, class = "gastropoda")
#  
#  #Use fuzzy_search to find occurrences of the names within the custom taxonomy
#  search_result <- c()
#  sp_names <- c("Natica sp", "Conus sp")
#  for (i in sp_names) {
#    iter <- fuzzy_search(custom_taxonomy, i, allow_term_removal = TRUE)
#    search_result <- c(search_result, iter)
#  }
#  exact_names <- c("Polinices mammillata", "Cymatium pileare",
#                   "Chicoreus ramosus", "Murex tenuirostris","Vasum turbinellum",
#                   "Oliva amethystina", "Mitra mitra", "Nassa serta",
#                   "Phos senticosus")
#  for (i in exact_names) {
#    iter <- fuzzy_search(custom_taxonomy, i, allow_term_removal = FALSE)
#    search_result <- c(search_result, iter)
#  }
#  
#  #Annotate the custom taxonomy
#  custom_taxonomy <- annotate(custom_taxonomy, names = search_result, new_column = "cone_snails")
#  
#  #Filter on the annotation
#  custom_taxonomy <- custom_taxonomy[!is.na(custom_taxonomy$cone_snails),]
#  
#  #De-duplicate the custom taxonomy
#  custom_taxonomy <- dedupe(custom_taxonomy, ranked = TRUE)
#  
#  #Create a subset of relevant data
#  custom_taxonomy_short <- custom_taxonomy[, c(1,17,2,3,20,7)]
#  colnames(custom_taxonomy_short) <- c("GBIF_id", "NCBI_id", "species_name",
#                                       "GBIF_rank", "NCBI_rank", "taxonomic_status")
#  
#  #Print the results to the terminal
#  print(custom_taxonomy_short, n=100)

