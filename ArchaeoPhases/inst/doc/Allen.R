## ----basic_relations----------------------------------------------------------
## Load ArchaeoPhases
library(ArchaeoPhases)
## Plot the basic Allen relations
allen_illustrate("basic") 

## ----analyze_stratigraphy-----------------------------------------------------
## Load ArchaeoPhases
library(ArchaeoPhases)
## Plot lattice for two contexts on the same line of a Harris matrix
allen_analyze("m", "m", "Composite relation of two contexts on the same line") 

## ----illustrate_stratigraphy--------------------------------------------------
## Load ArchaeoPhases
library(ArchaeoPhases)
## Illustrate composite relations in a stratigraphic sequence
allen_illustrate("sequence") 

## ----identity_relation--------------------------------------------------------
## Load ArchaeoPhases
library(ArchaeoPhases)
## Load the Anglo Saxon burials dataset
library(ArchaeoPhases.dataset)
library(ggplot2)
data(AngloSaxonBurials)
## Identify the burials with bead BE1-Dghnt
be1.dghnt <- c("UB-4503 (Lec148)", "UB-4506 (Lec172/2)",
"UB-6038 (CasD183)", "UB-4512 (EH091)", "UB-4501 (Lec014)",
"UB-4507 (Lec187)", "UB-4502 (Lec138)", "UB-4042 (But1674)",
"SUERC-39100 (ERL G266)")
chains <- list(list("BE1-Dghnt" = be1.dghnt,
                    "BE1-Dghnt" = be1.dghnt))
allen_observe(data = AngloSaxonBurials,
              chains = chains) 

## ----relation_and_converse----------------------------------------------------
## Load ArchaeoPhases
library(ArchaeoPhases)
## Load the Anglo Saxon burials dataset
library(ArchaeoPhases.dataset)
data(AngloSaxonBurials)
## Identify the burials with bead BE1-Dghnt
be1.dghnt <- c("UB-4503 (Lec148)", "UB-4506 (Lec172/2)",
"UB-6038 (CasD183)", "UB-4512 (EH091)", "UB-4501 (Lec014)",
"UB-4507 (Lec187)", "UB-4502 (Lec138)", "UB-4042 (But1674)",
"SUERC-39100 (ERL G266)")
## Identify the burials with bead BE1-CylRound
be1.cylround <- c("UB-4965 (ApD117)", "UB-4735 (Ber022)", "UB-4739 (Ber134/1)", "UB-6473 (BuD250)", "UB-6476 (BuD339)", "UB-4729 (MH068)", "UB-4835 (ApD134)", "UB-4708 (EH083)", "UB-4733 (MH095)", "UB-4888 (MelSG089)", "UB-4963 (SPTip208)", "UB-4890 (MelSG075)", "UB-4732 (MH094)", "SUERC-51539 (ERL G353)", "SUERC-51551 (ERL G193)")
chains <- list(list("BE1-Dghnt" = be1.dghnt, "BE1-CylRound" = be1.cylround), list("BE1-CylRound" = be1.cylround, "BE1-Dghnt" = be1.dghnt))
allen_observe(data = AngloSaxonBurials,
              chains = chains) 

## ----observe_frequency--------------------------------------------------------
## Load ArchaeoPhases
library(ArchaeoPhases)
## Load the Anglo Saxon burials dataset
library(ArchaeoPhases.dataset)
data(AngloSaxonBurials)
## Identify the burials with bead BE1-Dghnt
be1.dghnt <- c("UB-4503 (Lec148)", "UB-4506 (Lec172/2)",
"UB-6038 (CasD183)", "UB-4512 (EH091)", "UB-4501 (Lec014)",
"UB-4507 (Lec187)", "UB-4502 (Lec138)", "UB-4042 (But1674)",
"SUERC-39100 (ERL G266)")
## Identify the burials with bead BE1-CylRound
be1.cylround <- c("UB-4965 (ApD117)", "UB-4735 (Ber022)", "UB-4739 (Ber134/1)", "UB-6473 (BuD250)", "UB-6476 (BuD339)", "UB-4729 (MH068)", "UB-4835 (ApD134)", "UB-4708 (EH083)", "UB-4733 (MH095)", "UB-4888 (MelSG089)", "UB-4963 (SPTip208)", "UB-4890 (MelSG075)", "UB-4732 (MH094)", "SUERC-51539 (ERL G353)", "SUERC-51551 (ERL G193)")
chains <- list("BE1-Dghnt" = be1.dghnt, "BE1-CylRound" = be1.cylround)
res <- allen_observe_frequency(AngloSaxonBurials, chains, "oFD")
res$rounded.percentage 

