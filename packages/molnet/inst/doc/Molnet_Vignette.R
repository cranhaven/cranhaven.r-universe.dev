## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(molnet)

## ---- echo=TRUE, warning=FALSE, eval=FALSE------------------------------------
#  molnet::install_python_dependencies()

## ----Load data----------------------------------------------------------------
data("mrna_data")
data("protein_data")
data("phosphoprotein_data")
data("metabolite_data")
data("metabolite_protein_interaction")

## -----------------------------------------------------------------------------
head(protein_data$group1$identifiers)
head(mrna_data$group1$identifiers)

## ----Create layers------------------------------------------------------------
number_of_genes <- 100 # set for subsetting

# Create individual layers
mrna_layer <- make_layer(name="mrna",
                         data_group1=mrna_data$group1$data[,1:number_of_genes],
                         data_group2=mrna_data$group2$data[,1:number_of_genes],
                         identifier_group1=data.frame(gene_name=mrna_data$group1$identifiers[1:number_of_genes,]),
                         identifier_group2=data.frame(gene_name=mrna_data$group2$identifiers[1:number_of_genes,])
                         )

protein_layer <- make_layer(name="protein",
                         data_group1=protein_data$group1$data[,1:number_of_genes],
                         data_group2=protein_data$group2$data[,1:number_of_genes],
                         identifier_group1=protein_data$group1$identifiers[1:number_of_genes,],
                         identifier_group2=protein_data$group2$identifiers[1:number_of_genes,]
                         )

## ----echo=FALSE---------------------------------------------------------------


phosphoprotein_layer <- make_layer(name="phosphoprotein",
                         data_group1=phosphoprotein_data$group1$data[,1:number_of_genes],
                         data_group2=phosphoprotein_data$group2$data[,1:number_of_genes],
                         identifier_group1=phosphoprotein_data$group1$identifiers[1:number_of_genes,],
                         identifier_group2=phosphoprotein_data$group2$identifiers[1:number_of_genes,]
                         )

metabolite_layer <- make_layer(name="metabolite",
                         data_group1=metabolite_data$group1$data[,1:number_of_genes],
                         data_group2=metabolite_data$group2$data[,1:number_of_genes],
                         identifier_group1=metabolite_data$group1$identifiers[1:number_of_genes,],
                         identifier_group2=metabolite_data$group2$identifiers[1:number_of_genes,]
                         )

## ----Make layers list---------------------------------------------------------
layers <- list(
  mrna_layer,
  protein_layer,
  phosphoprotein_layer,
  metabolite_layer
)

## ----Inter-layer connections--------------------------------------------------
inter_layer_connections = list(
  make_connection(from = 'mrna', to = 'protein', connect_on = 'gene_name', weight = 1),
  make_connection(from = 'protein', to = 'phosphoprotein', connect_on = 'gene_name', weight = 1),
  make_connection(from = 'protein', to = 'metabolite', connect_on = metabolite_protein_interaction, weight = 'combined_score')
)

## ----Make drug-target interaction---------------------------------------------
drug_target_interaction <- make_drug_target(target_molecules='protein',
                                            interaction_table=drug_gene_interactions,
                                            match_on='gene_name')

## -----------------------------------------------------------------------------
return_errors(check_input(layers = layers, inter_layer_connections = inter_layer_connections, drug_target_interaction = drug_target_interaction))

## ----Settings-----------------------------------------------------------------
settings <- molnet_settings(
  handling_missing_data = list(
    default = "pairwise.complete.obs",
    mrna = "all.obs"
  ),
  save_individual_graphs = FALSE,
  save_combined_graphs = FALSE,
  save_drug_targets = FALSE,
  python_executable = "python3"
)
# disable multi-threading for example run; 
# not recommended for actual data processing
WGCNA::disableWGCNAThreads()


## ----Run pipeline, eval=FALSE-------------------------------------------------
#  start_pipeline(layers, inter_layer_connections, drug_target_interaction, settings)

## ----Individual Graphs, message=FALSE, results='hide'-------------------------
individual_graphs <- generate_individual_graphs(layers = layers, settings = settings)

## ----Combine Graphs-----------------------------------------------------------
combined_graphs <- generate_combined_graphs(individual_graphs[["graphs"]], individual_graphs[["annotations"]], inter_layer_connections, settings)

## ----Drug Targets-------------------------------------------------------------
drug_targets <- determine_drug_targets(combined_graphs[["graphs"]], combined_graphs[["annotations"]], drug_target_interaction, settings)

## ----Calculate interaction score, eval = FALSE--------------------------------
#  interaction_score_graphs <- interaction_score(combined_graphs[["graphs"]], drug_target_edgelists=drug_targets[["edgelist"]], settings=settings)

## ----Calculate differential score---------------------------------------------
data("interaction_score_graphs_vignette")
differential_score_graph <- differential_score(interaction_score_graphs_vignette)

## ----Drug response------------------------------------------------------------
drug_response_score <- get_drug_response_score(differential_score_graph,
                                               drug_targets[["targets"]],
                                               drug_target_interaction$interaction_table)

## ----Result Output------------------------------------------------------------
head(dplyr::filter(drug_response_score, !is.na(drug_response_score)))

