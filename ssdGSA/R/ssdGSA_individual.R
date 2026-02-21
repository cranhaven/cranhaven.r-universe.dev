#' @title Single Sample Directional Gene Set Analysis Using Individual Weighted Scores
#'
#' @description This function is to calculate single sample directional (disease weighted) gene set scores for a given disease using individual weighted scores.
#'
#'
#' @param Data Data matrix of gene expressions with gene ensembl ID as row names and columns corresponding to different samples.
#' @param Gene_sets A list of gene sets with gene set names as component names, and each component is a vector of gene entrez ID.
#' @param Direction_matrix Matrix containing directionality information for each gene, such as effect size, t statistics,
#' p value of summary statistics. Each row of the direction matrix is for one gene, and there should be at least two columns
#' (with the 1st column containing gene entrez ID, and 2nd column containing directionality information).
#'
#'
#' @return Matrix of directional gene set scores with rows corresponding to gene sets and columns corresponding to different
#' samples will be return.
#'
#' @importFrom dplyr %>% filter mutate mutate_at summarise across pull left_join rename_at group_by starts_with join_by
#' @importFrom utils stack
#' @importFrom tibble rownames_to_column column_to_rownames tibble
#' @importFrom tidyselect last_col
#'
#' @details Single sample directional gene set analysis using individual weighted scores inherits the standard gene set variation
#' analysis(GSVA) method, but also provides the option to use summary statistics from any analysis (disease vs healthy, lesional side vs nonlesional side, etc..)
#' input to define the direction of gene sets used for directional gene set score calculation for a given disease. This function is
#' specific for using individual weighted scores.
#'
#' @seealso ssdGSA
#'
#' @examples
#'
#' ssdGSA_individual(Data = data_matrix_entrezID,
#'                   Gene_sets = gene_sets[c(1,2,4)],
#'                   Direction_matrix = direction_matrix
#'                   )
#'
#'
#' @keywords gene set variation analysis
#'
#' @export


# Main ssdGSA_individual function
ssdGSA_individual <- function(Data, # Input the data matrix
                              Gene_sets, # Input the gene sets
                              Direction_matrix # Input the direction matrix
){
  #0. Format the input data
  Direction_matrix <- Direction_matrix %>% mutate(across(c(1), as.character)) %>% dplyr::rename(ES = c(2))


  #1.1 Check if the row names of data matrix (Data), gene sets (Gene_sets), and direction matrix (Direction_matrix) are using the same GeneID
  genes_in_Gene_sets <- unlist(Gene_sets) %>% as.vector
  genes_in_Data <- rownames(Data) %>% as.vector
  genes_in_Direction_matrix <- Direction_matrix$gene %>% as.vector

  check_gene_name_match(genes_in_Gene_sets,
                        genes_in_Data,
                        genes_in_Direction_matrix)

  #1.2 Report the percent of genes in each gene set that do not have information in the data matrix and direction matrix
  check_genes_missing(Gene_sets, Data, Direction_matrix)


  #2. Split the gene.sets based on the directionality of genes from direction_matrix
  ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
  ES <- NULL

  up_genes <- Direction_matrix %>% filter(ES > 0) %>% pull(gene)
  down_genes <- Direction_matrix %>% filter(ES < 0) %>% pull(gene)

  Gene_sets_pov <- lapply(Gene_sets, intersect, y = up_genes)
  Gene_sets_neg <- lapply(Gene_sets, intersect, y = down_genes)

  #2.1 Check whether there are gene sets that contain both positively and negatively correlated genes missing in data matrix
  if ((!is.null(check_genes_missing_total(Gene_sets_pov, Data)$Total_missing_in_data_matrix))&
      (!is.null(check_genes_missing_total(Gene_sets_pov, Data)$Total_missing_in_data_matrix))){

    Missing_Genes <- intersect(check_genes_missing_total(Gene_sets_pov, Data)$Total_missing_in_data_matrix,
                               check_genes_missing_total(Gene_sets_neg, Data)$Total_missing_in_data_matrix)
    if (length(Missing_Genes)>0){
      message(paste0("Both positively and negatively correlated genes in gene set(s): ", Missing_Genes, " are missing in data matrix, so these gene sets will not be calculated in the following analysis.", sep = ""))
      Gene_sets_pov <- Gene_sets_pov[names(Gene_sets_pov)[!(names(Gene_sets_pov) %in% Missing_Genes)]]
      Gene_sets_neg <- Gene_sets_neg[names(Gene_sets_neg)[!(names(Gene_sets_neg) %in% Missing_Genes)]]
    }
  }


  #3. Calculate final score for each gene in each gene set
  # Add a note to show the running of gsva
  message(paste0("Note: Running GSA for ", length(names(Gene_sets)), " gene sets using individual weights.", sep = ""))
  #
  ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
  GS_pov <- NULL

  suppressMessages(GS_pov_temp <- Gene_sets_pov %>%
                     utils::stack() %>%
                     rename_at(1, ~"gene") %>%
                     rename_at(2, ~"GS_pov" ) %>%
                     left_join(Direction_matrix) %>%
                     group_by(GS_pov))

  ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
  GS_neg <- NULL

  suppressMessages(GS_neg_temp <- Gene_sets_neg %>%
                     utils::stack() %>%
                     rename_at(1, ~"gene") %>%
                     rename_at(2, ~"GS_neg" ) %>%
                     left_join(Direction_matrix) %>%
                     group_by(GS_neg) )


  ## Summarized individual ES-weighted scores
  ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
  . <- ES <- gene <- rowname <- NULL

  pov_weighted_score_sum <- GS_pov_temp %>%
    dplyr::select(GS_pov, gene, ES) %>%
    left_join(., as.data.frame(Data) %>% tibble::rownames_to_column(), join_by(gene == rowname)) %>%
    replace(is.na(.), 0)  %>%
    mutate(across(3:last_col(), ~.*ES)) %>%
    dplyr::select(-ES, -gene) %>%
    group_by(GS_pov) %>%
    summarise(across(1:last_col(), ~sum(.x,na.rm = TRUE))) %>%
    tibble::column_to_rownames(var = "GS_pov")

  suppressMessages(pov_weighted_score_sum2 <- tibble(GS = names(Gene_sets_pov)) %>%
                     left_join(., pov_weighted_score_sum %>% as.data.frame()%>% tibble::rownames_to_column(var="GS"), by = "GS") %>%
                     replace(is.na(.), 0) %>%
                     tibble::column_to_rownames(var = "GS"))

  neg_weighted_score_sum <- GS_neg_temp %>%
    dplyr::select(GS_neg, gene, ES) %>%
    left_join(., as.data.frame(Data) %>% tibble::rownames_to_column(), join_by(gene == rowname)) %>%
    replace(is.na(.), 0)  %>%
    mutate(across(3:last_col(), ~.*ES)) %>%
    dplyr::select(-ES, -gene) %>%
    group_by(GS_neg) %>%
    summarise(across(1:last_col(), ~sum(.x,na.rm = TRUE))) %>%
    tibble::column_to_rownames(var = "GS_neg")

  suppressMessages(neg_weighted_score_sum2 <- tibble(GS = names(Gene_sets_neg)) %>%
                     left_join(., neg_weighted_score_sum %>% as.data.frame()%>% tibble::rownames_to_column(var="GS"), by = "GS") %>%
                     replace(is.na(.), 0) %>%
                     tibble::column_to_rownames(var = "GS"))

  ## Summarized individual ES
  pov_ES_sum <- GS_pov_temp %>%
    dplyr::select(GS_pov, gene, ES) %>%
    left_join(., as.data.frame(Data) %>% tibble::rownames_to_column(), join_by(gene == rowname)) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-ES, -gene) %>%
    mutate(across(3:last_col(), abs)) %>%
    group_by(GS_pov) %>%
    summarise(across(1:last_col(), ~sum(.x,na.rm = TRUE))) %>%
    tibble::column_to_rownames(var = "GS_pov")

  suppressMessages(pov_ES_sum2 <- tibble(GS = names(Gene_sets_pov)) %>%
                     left_join(., pov_ES_sum %>% as.data.frame()%>% tibble::rownames_to_column(var="GS"), by = "GS") %>%
                     replace(is.na(.), 0) %>%
                     tibble::column_to_rownames(var = "GS"))

  neg_ES_sum <- GS_neg_temp %>%
    dplyr::select(GS_neg, gene, ES) %>%
    left_join(., as.data.frame(Data) %>% tibble::rownames_to_column(), join_by(gene == rowname)) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-ES, -gene) %>%
    mutate(across(3:last_col(), abs)) %>%
    group_by(GS_neg) %>%
    summarise(across(1:last_col(), ~sum(.x,na.rm = TRUE))) %>%
    tibble::column_to_rownames(var = "GS_neg")

  suppressMessages(neg_ES_sum2 <- tibble(GS = names(Gene_sets_neg)) %>%
                     left_join(., neg_ES_sum %>% as.data.frame()%>% tibble::rownames_to_column(var="GS"), by = "GS") %>%
                     replace(is.na(.), 0) %>%
                     tibble::column_to_rownames(var = "GS"))

  #
  GS_final = (pov_weighted_score_sum2 - neg_weighted_score_sum2)/(pov_ES_sum2 + neg_ES_sum2)

  return(GS_final)

}

