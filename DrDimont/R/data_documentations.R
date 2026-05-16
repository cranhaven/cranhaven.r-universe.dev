### Exemplary intermediate output of DrDimont ###

#' Formatted layers object
#'
#' Exemplary intermediate pipeline output containing a correctly formatted layers list.
#'
#' List containing four layer items created by \code{\link[DrDimont]{make_layer}}.
#' Each layer contains `data` and `identifiers` stratified by group and a `name`
#' element giving the layer name. The data contained in this example refers to mRNA,
#' protein, phosphosite, and metabolite layers. The mRNA, protein, and phosphosite
#' data was adapted and reduced from Krug et al. (2020), containing data from the
#' Clinical Proteomic Tumor Analysis Consortium (CPTAC). The metabolite data was
#' sampled randomly to generate distributions similar to those reported, e.g., in
#' Terunuma et al. (2014). The `data` elements contain the raw data with samples as columns
#' and molecular entities as rows. The `identifiers` elements contain layer-specific identifiers
#' for the molecular entities, e.g, gene_name.
#'
#' @format A list with 4 items. Each layer list contains 2 groups and a `name` element. Each group
#' contains `data` and `identifiers`. The structure for one individual layer:
#' \describe{
#'   \item{groupA}{Data associated with `groupA`}
#'   \describe{
#'   \item{data}{Raw data. Components (e.g. genes or proteins) in columns, samples in rows}
#'   \item{identifiers}{Dataframe containing one column per ID}
#'   }
#'   \item{groupB}{Data associated with `groupB`}
#'   \describe{
#'   \item{data}{see above}
#'   \item{identifiers}{see above}
#'   }
#'   \item{name}{Name of the layer}
#' }
#' @source Terunuma, Atsushi et al. “MYC-driven accumulation of 2-hydroxyglutarate
#' is associated with breast cancer prognosis.” The Journal of Clinical Investigation
#' vol. 124,1 (2014): 398-412. doi:10.1172/JCI71180
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis
#' and Targeted Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"layers_example"


#' Correlation matrices
#'
#' Exemplary intermediate pipeline output: Correlation matrices example data built by
#' \code{\link[DrDimont]{compute_correlation_matrices}} using \link[DrDimont]{layers_example}
#' data and settings:
#'
#' \code{settings <- drdimont_settings(
#'                          handling_missing_data=list(
#'                              default="pairwise.complete.obs",
#'                              mrna="all.obs"))}
#'
#' A subset of the original data from Krug et al. (2020) and randomly sampled metabolite
#' data in \link[DrDimont]{layers_example} was used to generate the correlation
#' matrices. They were created from data stratified by estrogen receptor (ER) status:
#' `groupA` contains data of ER+ patients and `groupB` of ER- patients.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{correlation_matrices}{A named list with two groups.}
#'   \describe{
#'     \item{groupA}{Correlation matrices associated with `groupA`}
#'     \describe{
#'     \item{mrna}{Correlation matrix}
#'     \item{protein}{Correlation matrix}
#'     \item{phosphosite}{Correlation matrix}
#'     \item{metabolite}{Correlation matrix}
#'     }
#'     \item{groupB}{same structure as `groupA`}
#'     }
#'   \item{annotations}{A named list containing dataframes of mappings of assigned node IDs to the
#'   user-provided component identifiers for nodes in `groupA` or `groupB` and all nodes}
#'   \describe{
#'   \item{groupA}{Annotations associated with `groupA`}
#'     \describe{
#'     \item{mrna}{Dataframe}
#'     \item{protein}{Dataframe}
#'     \item{phosphosite}{Dataframe}
#'     \item{metabolite}{Dataframe}
#'     }
#'   \item{groupB}{same structure as `groupA`}
#'   \item{both}{same structure as `groupA`}
#'   }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and
#' Targeted Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"correlation_matrices_example"


#' Individual graphs
#'
#' Exemplary intermediate pipeline output: Individual graphs example data built by
#' \code{\link[DrDimont]{generate_individual_graphs}}. Graphs were created from
#' \link[DrDimont]{correlation_matrices_example} and
#' reduced by the `pickHardThreshold` reduction method. Used settings were:
#'
#' \code{settings <- drdimont_settings(
#'                         reduction_method=list(default="pickHardThreshold"),
#'                         r_squared=list(
#'                             default=0.8,
#'                             groupA=list(metabolite=0.45),
#'                             groupB=list(metabolite=0.15)),
#'                         cut_vector=list(
#'                             default=seq(0.3, 0.7, 0.01),
#'                             metabolite=seq(0.1, 0.65, 0.01)))}
#'
#' A subset of the original data by Krug et al. (2020) and randomly sampled metabolite
#' data from \code{\link[DrDimont]{layers_example}} was used to generate the correlation
#' matrices and individual graphs. They were created from data stratified by estrogen
#' receptor (ER) status: `groupA` contains data of ER+ patients and `groupB` of
#' ER- patients.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{graphs}{A named list with two groups.}
#'   \describe{
#'     \item{groupA}{Graphs associated with `groupA`}
#'     \describe{
#'     \item{mrna}{Graph}
#'     \item{protein}{Graph}
#'     \item{phosphosite}{Graph}
#'     \item{metabolite}{Graph}
#'     }
#'     \item{groupB}{same structure as `groupA`}
#'     }
#'   \item{annotations}{A named list containing dataframes of mappings of assigned node IDs to the
#'   user-provided component identifiers for nodes in `groupA` or `groupB` and all nodes}
#'   \describe{
#'   \item{groupA}{Annotations associated with `groupA`}
#'     \describe{
#'     \item{mrna}{Dataframe}
#'     \item{protein}{Dataframe}
#'     \item{phosphosite}{Dataframe}
#'     \item{metabolite}{Dataframe}
#'     }
#'   \item{groupB}{same structure as `groupA`}
#'   \item{both}{same structure as `groupA`}
#'   }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and
#' Targeted Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"individual_graphs_example"


#' Combined graphs
#'
#' Exemplary intermediate pipeline output: Combined graphs example data built by
#' \code{\link[DrDimont]{generate_combined_graphs}}. Combined graphs were built
#' using the \link[DrDimont]{individual_graphs_example} and:
#'
#' \code{inter_layer_connections = list(
#'          make_connection(from='mrna', to='protein', connect_on='gene_name', weight=1),
#'          make_connection(from='protein', to='phosphosite', connect_on='gene_name', weight=1),
#'          make_connection(from='protein', to='metabolite', connect_on=metabolite_protein_interactions, weight='combined_score'))}
#'
#' A subset of the original data by Krug et al. (2020) and randomly sampled metabolite
#' data from \code{\link[DrDimont]{layers_example}} was used to generate the correlation
#' matrices, individual graphs, and combined graphs. They were created from data
#' stratified by estrogen receptor (ER) status: `groupA` contains data of ER+
#' patients and `groupB` of ER- patients.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{graphs}{A named list with two groups.}
#'   \describe{
#'     \item{groupA}{Graph associated with `groupA`}
#'     \item{groupB}{Graph associated with `groupB`}
#'     }
#'   \item{annotations}{A dataframe of mappings of assigned node IDs to the
#'   user-provided component identifiers for all nodes in `groupA` and `groupB` together
#'   and all layers}
#'   \describe{
#'   \item{both}{Dataframe}
#'   }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and
#' Targeted Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"combined_graphs_example"


#' Drug target nodes in combined network
#'
#' Exemplary intermediate pipeline output: Drug targets detected in the combined
#' graphs. A named list with elements `targets` and `edgelists`. This was created with
#' \code{\link[DrDimont]{determine_drug_targets}} using the \link[DrDimont]{combined_graphs_example}
#'  and:
#'
#' \code{drug_target_interactions <- make_drug_target(target_molecules='protein',
#'              interaction_table=drug_gene_interactions,
#'              match_on='gene_name')}
#'
#' Drug-gene interactions to calculate this output were used from
#' The Drug Gene Interaction Database.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{targets}{A named list}
#'   \describe{
#'   \item{target_nodes}{dataframe with column `node_id` (unique node IDs in the graph targeted by
#'    drugs) and columns `groupA` and `groupB` (bool values specifying whether the node is
#'    contained in the combined graph of the group)}
#'   \item{drugs_to_target_nodes}{Element `drugs_to_target_nodes` contains a named list mapping drug
#'    names to a vector of their target node IDs.}
#'   }
#'   \item{edgelists}{Contains elements `groupA` and `groupB` containing each a dataframe of edges
#'   adjacent to drug target nodes each. Each edgelist dataframe contains columns `from`, `to`, and
#'   `weight`.}
#' }
#' @source The Drug Gene Interaction Database: \url{https://dgidb.org/}
#'
"drug_target_edges_example"


#' Interaction score graphs
#'
#' Exemplary intermediate pipeline output: Interaction score graphs example data built by
#' \code{\link[DrDimont]{generate_interaction_score_graphs}} using \link[DrDimont]{combined_graphs_example}
#' and \link[DrDimont]{drug_target_edges_example}.
#' A named list (elements `groupA` and `groupB`). Each element contains an iGraph
#' object containing edge attributes: the correlation values as `weight` and the
#' interaction score as `interactionweight`.
#'
#' A subset of the original data by Krug et al. (2020) and randomly sampled metabolite
#' data from \code{\link[DrDimont]{layers_example}} was used to generate the correlation
#' matrices, individual graphs, and combined graphs. They were created from data
#' stratified by estrogen receptor (ER) status: `groupA` contains data of ER+
#' patients and `groupB` of ER- patients. Drug-gene interactions were used from
#' The Drug Gene Interaction Database.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{groupA}{iGraph graph object containing the interaction score as weight for groupA.}
#'   \item{groupB}{}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
#' @source The Drug Gene Interaction Database: \url{https://dgidb.org/}
"interaction_score_graphs_example"


#' Differential graph
#'
#' Exemplary intermediate pipeline output: Differential score graph example data built by
#' \code{\link[DrDimont]{generate_differential_score_graph}} using the
#' \link[DrDimont]{interaction_score_graphs_example}.
#' Consists of one graph containing edge attributes: the differential correlation values as
#' `differential_score` and the differential interaction score as `differential_interaction_score`.
#'
#' A subset of the original data by Krug et al. (2020) and randomly sampled metabolite
#' data from \code{\link[DrDimont]{layers_example}} was used to generate the correlation
#' matrices, individual graphs, and combined graphs. They were created from data
#' stratified by estrogen receptor (ER) status: `groupA` contains data of ER+
#' patients and `groupB` of ER- patients.
#'
#' @format An iGraph graph object.
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"differential_graph_example"


#' Drug response score
#'
#' Exemplary final pipeline output: Drug response score dataframe. This contains drugs and the
#' calculated differential drug response score. The score was calculated by
#' \code{\link[DrDimont]{compute_drug_response_scores}} using
#' \link[DrDimont]{differential_graph_example}, \link[DrDimont]{drug_target_edges_example} and
#'
#' \code{drug_target_interaction <- make_drug_target(target_molecules='protein',
#'              interaction_table=drug_gene_interactions,
#'              match_on='gene_name')}
#'
#' A subset of the original data by Krug et al. (2020) and randomly sampled metabolite
#' data from \code{\link[DrDimont]{layers_example}} was used to generate the correlation
#' matrices, individual graphs, combined graphs, interaction score graphs, and differential
#' score graph. They were created from data stratified by estrogen receptor (ER) status:
#' `groupA` contains data of ER+ patients and `groupB` of ER- patients. Drug-gene
#' interactions were used from The Drug Gene Interaction Database.
#'
#' @format Dataframe with two columns
#' \describe{
#'   \item{drug_name}{Names of drugs}
#'   \item{drug_response_scores}{Associated differential drug response scores}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
#' @source The Drug Gene Interaction Database: \url{https://dgidb.org/}
"drug_response_scores_example"


### Raw data  ###

#' mRNA expression data
#'
#' mRNA analysis of breast cancer patient data from Krug et al. (2020) (data from the Clinical
#' Proteomic Tumor Analysis Consortium (CPTAC)). The data is stratified by estrogen receptor (ER)
#' expression status (`groupA` = ER+, `groupB` = ER-). The data was reduced to 50 genes.
#' For each group, a dataframe is given containing the raw data with the mRNA/gene as rows and the
#' samples as columns. The first column contains the gene identifiers (gene_name).
#'
#' @format
#' \describe{
#'   \item{groupA}{ER+ data; data.frame: first column contains mRNA/gene identifier gene_name;
#'                 other columns are samples containing the quantified mRNA data per gene}
#'   \item{groupB}{ER- data; data.frame: first column contains mRNA/gene identifier gene_name;
#'                 other columns are samples containing the quantified mRNA data per gene}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"mrna_data"


#' Protein data
#'
#' Protein analysis of breast cancer patients data from Krug et al. (2020) (data from the Clinical
#' Proteomic Tumor Analysis Consortium (CPTAC)). The data is stratified by estrogen receptor (ER)
#' expression status (`groupA` = ER+, `groupB` = ER-). The data was reduced to 50 genes.
#' For each group a dataframe is given containing the raw data with the proteins as rows and the
#' samples as columns. The first two columns contain the protein identifiers
#' (ref_seq and gene_name).
#'
#' @format
#' \describe{
#'   \item{groupA}{ER+ data; data.frame: first two columns contain protein identifiers ref_seq and gene_name;
#'                 other columns are samples containing the quantified proteomics data per protein}
#'   \item{groupB}{ER- data; data.frame: first two columns contain protein identifiers ref_seq and gene_name;
#'                 other columns are samples containing the quantified proteomics data per protein}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#' Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"protein_data"


#' Phosphosite data
#'
#' Phosphosite analysis of breast cancer patient data from Krug et al. (2020) (data from the Clinical
#' Proteomic Tumor Analysis Consortium (CPTAC)). The data is stratified by estrogen receptor (ER)
#' expression status (`groupA` = ER+, `groupB` = ER-). The data was reduced to 50 genes.
#' For each group, a dataframe is given containing the raw data with the phosphosites as rows and the
#' samples as columns. The first three columns contain the phosphosite and protein identifiers
#' (site_id, ref_seq and gene_name).
#'
#' @format
#' \describe{
#'   \item{groupA}{ER+ data; data.frame: first three columns contain phosphosite and protein identifiers
#'                 site_id, ref_seq and gene_name;
#'                 other columns are samples containing the quantified phosphosite data per phosphosite}
#'   \item{groupB}{ER- data; data.frame: first three columns contain phosphosite and protein identifiers
#'                 site_id, ref_seq and gene_name;
#'                 other columns are samples containing the quantified phosphosite data per phosphosite}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"phosphosite_data"


#' Metabolomics data
#'
#' Metabolomics analysis of breast cancer patient data sampled randomly to
#' generate distributions similar to those reported (e.g., in Terunuma et al. (2014)).
#' The data is stratified by estrogen receptor (ER) expression status (`groupA` = ER+, `groupB` = ER-). 
#' The data was reduced to 50 metabolites. 
#' For each group, a dataframe is given containing the raw data with the metabolites as rows and the
#' samples as columns. The first three columns contain the metabolite identifiers
#' (biochemical_name, metabolon_id and pubchem_id).
#'
#' @format
#' \describe{
#'   \item{groupA}{ER+ data; data.frame: first three columns contain metabolite identifiers
#'                 biochemical_name, metabolon_id and pubchem_id;
#'                 other columns are samples containing the quantified metabolite data per metabolite}
#'   \item{groupB}{ER- data; data.frame: first three columns contain metabolite identifiers
#'                 biochemical_name, metabolon_id and pubchem_id;
#'                 other columns are samples containing the quantified metabolite data per metabolite}
#' }
#' @source Terunuma, Atsushi et al. “MYC-driven accumulation of 2-hydroxyglutarate is associated
#' with breast cancer prognosis.” The Journal of Clinical Investigation 
#' vol. 124,1 (2014): 398-412. doi:10.1172/JCI71180
#' @source \url{https://www.metabolon.com}
#' @source Pubchem IDs: \url{https://pubchem.ncbi.nlm.nih.gov}
#' @source MetaboAnalyst: \url{https://www.metaboanalyst.ca/faces/upload/ConvertView.xhtml}
#'
"metabolite_data"


#' Metabolite protein interaction data
#'
#' @description Dataframe providing interactions of metabolites and proteins. The data was taken
#' from the STITCH Database.
#'
#' @format A dataframe with 3 columns.
#' \describe{
#'   \item{pubchem_id}{Pubchem IDs defining interacting metabolites}
#'   \item{gene_name}{gene names defining interacting proteins}
#'   \item{combined_score}{Score describing the strength of metabolite-protein interaction}
#' }
#' @source STITCH DB: \url{https://stitch-db.org/}
#' @source Pubchem IDs: \url{https://pubchem.ncbi.nlm.nih.gov}
#' @source STRING DB: \url{https://string-db.org/}
"metabolite_protein_interactions"


#' Drug-gene interactions
#'
#' @description Dataframe providing interactions of drugs with genes. The data was downloaded from
#' The Drug Gene Interaction Database.
#'
#' @format A dataframe with 4 columns.
#' \describe{
#'   \item{gene_name}{Gene names of targeted protein-coding genes.}
#'   \item{drug_name}{Drug-names with known interactions.}
#'   \item{drug_chembl_id}{ChEMBL ID of drugs.}
#' }
#' @source The Drug Gene Interaction Database: \url{https://dgidb.org/}
#' @source ChEMBL IDs: \url{https://www.ebi.ac.uk/chembl}
"drug_gene_interactions"
