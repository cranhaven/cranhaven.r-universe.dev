# Exemplary intermediate output

#' Formatted layers object
#'
#' Exemplary intermediate pipeline output containing a correctly formatted layers list.
#'
#' List containing four layer items created by \code{\link{make_layer}}. Each layer contains 'data'
#' and 'identifiers' stratified by group and a 'name' element giving the layer name. The data
#' contained in this example refers to mRNA, protein, phosphoprotein and metabolite layers. The
#' data on mRNA, protein and phosphoproteins in taken from Krug et al., 2020 containing data from
#' the Clinical Proteomic Tumor Analysis Consortium (CPTAC). The metabolite data was sampled
#' randomly to generate distributions similar to those reported (e.g., in Terunuma et al., 2014).
#'
#' @format A list with 4 items. Each layer list contains 2 groups and a 'name' element. Each group
#' contains 'data' and 'identifiers'. The structure for one individual layer:
#' \describe{
#'   \item{group1}{Data associated with group1}
#'   \describe{
#'   \item{data}{Raw data. Components (e.g. genes) in columns, samples in rows}
#'   \item{identifiers}{Data frame containing one column per ID}
#'   }
#'   \item{group2}{Data associated with group2}
#'   \describe{
#'   \item{data}{see above}
#'   \item{identifiers}{see above}
#'   }
#'   \item{name}{Name of the layer}
#' }
#' @source Terunuma, Atsushi et al. “MYC-driven accumulation of 2-hydroxyglutarate is associated
#' with breast cancer prognosis.”
#' The Journal of clinical investigation vol. 124,1 (2014): 398-412. doi:10.1172/JCI71180
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and
#' Targeted Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"layers_example"

#' Individual graphs
#'
#' Exemplary intermediate pipeline output: Individual graphs example data built by
#' \code{\link{generate_individual_graphs}}. Graphs were created by correlation computation and
#' reduced by the 'p_value' reduction method (default settings of \code{\link{molnet_settings}}. A
#' subset of the original data by Krug et al., 2020 and randomly sampled metabolite data
#' (\code{\link{layers_example}}) was used to generate graphs. They were created from data
#' tratified by estrogen receptor (ER) status: group1 contains data of ER+ patients and group2 of
#' ER- patients.
#'
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{graphs}{A named list with two groups.}
#'   \describe{
#'     \item{group1}{Graphs associated with group1}
#'     \describe{
#'     \item{mrna}{}
#'     \item{protein}{}
#'     \item{phosphoprotein}{}
#'     \item{metabolite}{}
#'     }
#'     \item{group2}{same structure as above}
#'     }
#'   \item{annotations}{A named list containing data frames of mappings of assigned node IDs to the
#'   user-provided component identifiers for nodes in group1 or group2 and all nodes}
#'   \describe{
#'   \item{group1}{}
#'   \item{group2}{}
#'   \item{all}{}
#'   }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and
#' Targeted Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"individual_graphs_example"

#' Combined graphs
#'
#' Exemplary intermediate pipeline output: Combined graphs example data built by
#' \code{\link{generate_combined_graphs}}. Graphs were created with default settings of
#' \code{\link{molnet_settings}}.  A subset of the original data by Krug et al., 2020 and randomly
#' sampled metabolite data was used to generate graphs \code{\link{layers_example}}. They were
#' created from data stratified by estrogen receptor (ER) status: group1 contains data of ER+
#' patients and group2 of ER- patients.
#'
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{graphs}{A named list with two groups.}
#'   \describe{
#'     \item{group1}{Graph associated with group1}
#'     \item{group2}{Graph associated with group2}
#'     }
#'   \item{annotations}{A named list containing data frames of mappings of assigned node IDs to the
#'   user-provided component identifiers for nodes in group1 or group2 and all nodes}
#'   \describe{
#'   \item{group1}{}
#'   \item{group2}{}
#'   \item{all}{}
#'   }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
#'
"combined_graphs_example"

#' Drug target nodes in combined network
#'
#' Exemplary intermediate pipeline output: Drug targets detected in the combined graphs. A named
#' list with elements `drug_targets` and `edgelist`. This was created from
#' \code{\link{determine_drug_targets}} using the default settings given by
#' \code{\link{molnet_settings}}. Drug-gene interactions to calculate this output were used from
#' The Drug Gene Interaction Database.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{targets}{A named list}
#'   \describe{
#'   \item{target_nodes}{data frame with column `node_id` (unique node IDs in the graph targeted by
#'    drugs) and columns `group1` and `group2` (boolean values specifying whether the node is
#'    contained in the combined graph of the group)}
#'   \item{drugs_to_target_nodes}{Element `drugs_to_target_nodes` contains a named list mapping drug
#'    names to a vector of their target node IDs.}
#'   }
#'   \item{edgelist}{Contains elements `group1` and `group2` containing each a data frame of edges
#'   adjacent to drug target nodes each. Each edgelist data frame contains columns `from`, `to` and
#'   `weight`.}
#' }
#' @source The Drug Gene Interaction Database: \url{https://www.dgidb.org/}
#'
"drug_targets_example"

#' Interaction score graphs
#'
#' Exemplary intermediate pipeline output: Interaction score graphs example data built by
#' \code{\link{interaction_score}}. A named list (elements `group1` and `group2`). Each element
#' contains an iGraph object containing the interaction score as weight. This was computed using a
#' subset of the data published by Krug et al., 2020 and randomly sampled metabolite data
#' \code{\link{layers_example}}.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{group1}{iGraph graph object containing the interaction score as weight for group1.}
#'   \item{group2}{}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"interaction_score_graphs_example"

#' Interaction score graphs for vignette
#'
#' Exemplary intermediate pipeline output used in the vignette.
#'
#' @format A named list with 2 items.
#' \describe{
#'   \item{group1}{iGraph graph object containing the interaction score as weight for group1.}
#'   \item{group2}{}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#' Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"interaction_score_graphs_vignette"

#' Differential graph
#'
#' Exemplary intermediate pipeline output: Differential score graph example data built by
#' \code{\link{differential_score}}. Contains one graph carrying the differential interaction score
#' as weight. This was computed using a subset of the data published by Krug et al., 2020 and
#' randomly sampled metabolite data \code{\link{layers_example}}.
#'
#' @format An iGraph graph object.
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#' Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
#'
#'
"differential_score_graph_example"

#' Drug response score
#'
#' Exemplary final pipeline output: Drug response score data frame. This contains drugs and the
#' calculated differential drug response score. The score was calculated by
#' \code{\link{get_drug_response_score}}.
#'
#'  The original data used to compute this object was data published by Krug et al., 2020 and
#' randomly sampled metabolite data \code{\link{layers_example}}.. Drug-gene interactions to
#' calculate this output were used from The Drug Gene Interaction Database.
#'
#' @format Data frame with two columns
#' \describe{
#'   \item{drug_name}{Names of drugs}
#'   \item{drug_response_score}{Associated differential drug response scores}
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
#' @source The Drug Gene Interaction Database: \url{https://www.dgidb.org/}
"drug_response_score_example"

# Raw data

#' Metabolite protein interaction data
#'
#' @description Data frame providing interactions of metabolites and proteins. The data was taken
#' from the STITCH Database.
#'
#' @format A data frame with 3 columns.
#' \describe{
#'   \item{pubchemID}{Pubchem IDs defining interacting metabolites}
#'   \item{STRING_id}{STRING IDs defining interacting proteins}
#'   \item{combined_score}{Score describing the strength of metabolite-protein interaction}
#' }
#' @source STITCH DB: \url{http://stitch.embl.de/}
#' @source Pubchem IDs: \url{https://pubchem.ncbi.nlm.nih.gov}
#' @source STRING DB: \url{https://string-db.org/}
"metabolite_protein_interaction"

#' Drug-gene interactions
#'
#' Data frame providing interactions of drugs with genes. The data was downloaded from The Drug Gene
#'  Interaction Database.
#'
#' @format A data frame with 4 columns.
#' \describe{
#'   \item{gene_name}{Gene names of targeted protein-coding genes.}
#'   \item{ncbi_id}{NCBI IDs of targeted protein-coding genes.}
#'   \item{drug_name}{Drug-names with known interactions.}
#'   \item{drug_chembl_id}{ChEMBL ID of drugs.}
#' }
#' @source The Drug Gene Interaction Database: \url{https://www.dgidb.org/}
#' @source ChEMBL IDs: \url{https://www.ebi.ac.uk/chembl}
"drug_gene_interactions"

#' Protein data
#'
#'
#' Protein analysis of breast cancer patients data from Krug et al., 2020 (data from the Clinical
#' Proteomic Tumor Analysis Consortium (CPTAC)). The data is stratified by estrogen receptor (ER)
#' expression status (group1 = ER+, group2 = ER-). Each group is given as a sub-list containing
#' 'data' (raw data, proteins in columns and samples in rows) and 'identifiers' (one column per
#' identifier, rows in the same order as the protein order in 'data').
#'
#' @format
#' \describe{
#'   \item{group1}{ER+ data}
#'   \describe{
#'     \item{data}{raw data, protein in columns and samples in rows}
#'     \item{identifiers}{one column per identifier, rows in the same order as the protein order in
#'     'data', identifiers: RefSeq ID, gene name}
#'     }
#'   \item{group2}{ER- data}
#'   \describe{
#'     \item{data}{see above}
#'     \item{identifiers}{see above}
#'     }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#' Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"protein_data"

#' mRNA expression data
#'
#' mRNA analysis of breast cancer patients data from Krug et al., 2020 (data from the Clinical
#' Proteomic Tumor Analysis Consortium (CPTAC)). The data is stratified by estrogen receptor (ER)
#' expression status (group1 = ER+, group2 = ER-). Each group is given as a sub-list containing
#' 'data' (raw data, mRNAs in columns and samples in rows) and 'identifiers' (one column per
#' identifier, rows in the same order as the mRNA order in 'data').
#'
#' @format
#' \describe{
#'   \item{group1}{ER+ data}
#'   \describe{
#'     \item{data}{raw data, mRNA in columns and samples in rows}
#'     \item{identifiers}{one column per identifier, rows in the same order as the mRNA order in
#'     'data', identifiers: gene name}
#'     }
#'   \item{group2}{ER- data}
#'   \describe{
#'     \item{data}{see above}
#'     \item{identifiers}{see above}
#'     }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"mrna_data"

#' Phosphosite data
#'
#' Phosphosite analysis of breast cancer patients data from Krug et al., 2020 (data from the
#' Clinical Proteomic Tumor Analysis Consortium (CPTAC)). The data is stratified by estrogen
#' receptor (ER) expression status (group1 = ER+, group2 = ER-). Each group is given as a sub-list
#' containing 'data' (raw data, phosphosites in columns and samples in rows) and 'identifiers'
#' (one column per identifier, rows in the same order as the phosphosite order in 'data').
#'
#' @format
#' \describe{
#'   \item{group1}{ER+ data}
#'   \describe{
#'     \item{data}{raw data, phosphosites in columns and samples in rows}
#'     \item{identifiers}{one column per identifier, rows in the same order as the phoshosite order
#'     in 'data', identifiers: Phosphosite ID, RefSeq ID, gene name}
#'     }
#'   \item{group2}{ER- data}
#'   \describe{
#'     \item{data}{see above}
#'     \item{identifiers}{see above}
#'     }
#' }
#' @source Krug, Karsten et al. “Proteogenomic Landscape of Breast Cancer Tumorigenesis and Targeted
#'  Therapy.” Cell vol. 183,5 (2020): 1436-1456.e31. doi:10.1016/j.cell.2020.10.036
"phosphoprotein_data"

#' Metabolomics data
#'
#' Metabolomics analysis of breast cancer patients data sampled randomly to generate distributions
#' similar to those reported e.g. in (Terunuma et al., 2014). The data is stratified by estrogen
#' receptor (ER) expression status (group1 = ER+, group2 = ER-). Each group is given as a sub-list
#' containing 'data' (raw data, metabolites in columns and samples in rows) and 'identifiers'
#' (one column per identifier, rows in the same order as the metabolites order in 'data').
#'
#'
#'
#'
#' @format
#' \describe{
#'   \item{group1}{ER+ data}
#'   \describe{
#'     \item{data}{raw data, metabolites in columns and samples in rows}
#'     \item{identifiers}{one column per identifier, rows in the same order as the metabolite order
#'     in 'data', identifiers: biochemical name, METABOLON ID, Pubchem ID}
#'     }
#'   \item{group2}{ER- data}
#'   \describe{
#'     \item{data}{see above}
#'     \item{identifiers}{see above}
#'     }
#' }
#' @source Terunuma, Atsushi et al. “MYC-driven accumulation of 2-hydroxyglutarate is associated
#' with breast cancer prognosis.”
#' The Journal of clinical investigation vol. 124,1 (2014): 398-412. doi:10.1172/JCI71180
#' @source \url{https://www.metabolon.com}
#' @source Pubchem IDs: \url{https://pubchem.ncbi.nlm.nih.gov}
#' @source MetaboAnalyst: \url{https://www.metaboanalyst.ca/faces/upload/ConvertView.xhtml}
#'
"metabolite_data"

#' Drug target interaction example data
#'
#' @format A named list with 3 items.
#' \describe{
#'   \item{target_molecules}{Name of layer containing the drug targets. This name has to match the
#'   corresponding named item in the list of layers supplied to \code{\link{start_pipeline}}.}
#'   \item{interaction_table}{Table giving drug-gene-interactions.}
#'   \describe{
#'   \item{gene_name}{Gene names of targeted protein-coding genes.}
#'   \item{ncbi_id}{NCBI IDs of targeted protein-coding genes.}
#'   \item{drug_name}{Drug-names with known interactions.}
#'   \item{drug_chembl_id}{ChEMBL ID of drugs.}
#'   }
#'  \item{match_on}{Column name of the data frame supplied in `interaction_table` that is used for
#'  matching drugs and target nodes in the graph (e.g. `ncbi_id`).}
#' }
#' @source Terunuma, Atsushi et al. “MYC-driven accumulation of 2-hydroxyglutarate is associated
#' with breast cancer prognosis.”
#' The Journal of clinical investigation vol. 124,1 (2014): 398-412. doi:10.1172/JCI71180
#' @source \url{https://www.metabolon.com}
#' @source Pubchem IDs: \url{https://pubchem.ncbi.nlm.nih.gov}
#' @source MetaboAnalyst: \url{https://www.metaboanalyst.ca/faces/upload/ConvertView.xhtml}
#'
"drug_target_interaction_example"
