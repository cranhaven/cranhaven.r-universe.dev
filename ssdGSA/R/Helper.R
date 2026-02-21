# Helper functions

#' @title Function to Transform ENSEMBL ID to ENTREZ ID
#'
#' @description This function is to uniform gene ID types in data matrices, i.e., from ENSEMBL ID to ENTREZ ID.
#'
#' @param Data Data matrix of gene expressions with gene ensembl ID as row names and columns corresponding to different samples.
#'
#' @return Data matrix of gene expressions with ENSEMBL ID as row names and columns corresponding to samples will be return.
#'
#' @importFrom dplyr %>% filter select as_tibble mutate across pull left_join rename_at group_by rename_with
#' @importFrom stringr word
#' @importFrom clusterProfiler bitr
#' @importFrom tibble column_to_rownames
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#'
#' @details Since gene IDs in data matrices from different sources may be in different formats (ensembl ID or entrez ID),
#' this function is to transform the gene IDs in the data matrix from ensembl ID to entrez ID, to assist the following
#' single sample directional gene set analysis.
#'
#'
#' @examples
#' transform_ensembl_2_entrez(Data = data_matrix)
#'
#'
#' @export

transform_ensembl_2_entrez <- function(Data){

  gene_ids <- rownames(Data)
  from_ID_type = "ENSEMBL"
  to_ID_type = "ENTREZID"

  #check if ENSEMBL ID contain ".", if yes, split the ID by "." and only take the first part for matching
  if(grepl("\\.",Data %>% rownames) %>% all & from_ID_type == "ENSEMBL") {
    gene_ids = gene_ids %>% stringr::word( sep = '\\.')
  }

  gene_name <- gene_ids %>%
    clusterProfiler::bitr(fromType = from_ID_type, toType = to_ID_type, OrgDb = org.Hs.eg.db) %>%
    filter(!duplicated(across(from_ID_type))) %>%
    filter(!duplicated(across(to_ID_type))) %>%
    dplyr::rename_with(~"gene",from_ID_type)

  ## Quiets concerns of R CMD check re: the gene's that appear in pipelines
  gene <- NULL

  suppressMessages(logcpmx <- Data %>%
                     as_tibble(rownames = "gene") %>%
                     mutate(gene = stringr::word(gene, sep = '\\.')) %>%
                     filter(gene %in% gene_name$gene) %>%
                     left_join(gene_name) %>%
                     dplyr::select(-gene) )

  logcpmxx <- logcpmx %>%
    tibble::column_to_rownames(var = "ENTREZID") %>%
    as.matrix()

  return(logcpmxx)
}



#' @title Function to Check if Gene IDs in the Input Gene Data Match Well
#'
#' @description This function is to check if the gene IDs in the gene sets, data matrix and direction matrix match well.
#'
#'
#' @param genes_in_Gene_sets A list of gene names from the gene sets.
#' @param genes_in_Data A list of gene names from the data matrix.
#' @param genes_in_Direction_matrix A list of gene names from the direction matrix.
#'
#' @return If there are more than 10\% of gene names in gene sets do not match those in the data matrix or direction matrix,
#' the single sample directional gene set analysis would stop.
#'
#' @details Before single sample directional gene set analysis, it is necessary to check whether the gene ID types in the
#' gene sets, data matrix and direction matrix have the same gene ID type. If not, the ssdGSA and ssdGSA_individual would stop,
#' and users should double check to make gene ID types in different parts match one another.
#'
#'
#'
check_gene_name_match <- function(genes_in_Gene_sets,
                                  genes_in_Data,
                                  genes_in_Direction_matrix){
  pct_overlap_gene_sets <- length(intersect(genes_in_Gene_sets, genes_in_Data))/length(genes_in_Gene_sets)
  pct_overlap_direction_matrix <- length(intersect(genes_in_Direction_matrix, genes_in_Gene_sets))/length(genes_in_Gene_sets)

  if(pct_overlap_gene_sets < 0.1){
    stop("The gene ids in the gene sets and the data matrix do not match well. Please check if data has the same gene id type as the gene sets.")}
  else if(pct_overlap_direction_matrix < 0.1){
    stop("The gene ids in the gene sets and the direction matrix do not match well. Please check if direction_matrix has the same gene id type as the gene sets.")}

}



#' @title Function to Check if Genes in Gene Sets Have Missing Information in Data Matrix and Direction Matrix
#'
#' @description This function is to check if genes in gene sets to be analyzed have missing information in data matrix and direction matrix.
#'
#' @param Gene_sets  A list of gene sets to be analyzed, with gene set names as component names, and each component is a vector of gene entrez ID.
#' @param Data Data matrix of gene expressions with gene ensembl ID as row names and columns corresponding to different samples.
#' @param Direction_matrix Matrix containing directionality information for each gene, such as effect size, p value of summary statistics.
#' Each row of the matrix is for one gene, and there should be at least two columns (with the 1st column containing
#' gene entrez ID, and 2nd column containing directionality information).
#'
#' @return When at least one gene in the gene sets have information missing in data matrix or direction matrix, warning messages will be given, as
#' well as the percentages (missing number/total number) of gene sets. If less than 10 gene sets have missing information, percentages
#' (missing number/total number) of genes in each gene set that have missing information in data matrix and direction matrix will also be reported.
#' However, if more than 10 gene sets have missing information, no detailed individual gene set missing information will be reported. Also note that
#' if a gene set has 100% information missing in the data or direction matrix, the name of the gene set will be notated.
#'
#'
#' @importFrom vctrs list_drop_empty
#' @importFrom dplyr %>%
#'
#' @details Before single sample directional gene set analysis, it is necessary to check if genes in the gene sets have missing information in
#' data matrix and direction matrix. If not, warning messages would be given such that users can double check whether the gene set analysis results
#' are reliable.
#'
#'
#'
#'
check_genes_missing <- function(Gene_sets, Data, Direction_matrix){
  check_in_fun <- function(x, y){
    return(x[!(x %in% y)])
  }
  PKnames <- names(Gene_sets)
  Gene_sets_data_missing <- lapply(Gene_sets[PKnames], check_in_fun, y = rownames(Data))
  Gene_sets_direction_missing <- lapply(Gene_sets[PKnames], check_in_fun, y = Direction_matrix$gene)
  pct_missingsets_data <- round(length(vctrs::list_drop_empty(Gene_sets_data_missing))/length(PKnames)*100, 2)
  pct_missingsets_direction <- round(length(vctrs::list_drop_empty(Gene_sets_direction_missing))/length(PKnames)*100, 2)
  message(paste0("Warning messages: \n", pct_missingsets_data, "% (", length(vctrs::list_drop_empty(Gene_sets_data_missing)),"/", length(PKnames),") of gene sets have information missing in the data matrix for at least one gene; \n",
                    pct_missingsets_direction, "% (", length(vctrs::list_drop_empty(Gene_sets_direction_missing)),"/", length(PKnames),") of gene sets have information missing in the direction matrix for at least one gene. \n", sep = ""))

  Gene_sets_data_missing <- vctrs::list_drop_empty(Gene_sets_data_missing)
  PKnames_data_missing <- names(Gene_sets_data_missing)
  Gene_sets_direction_missing <- vctrs::list_drop_empty(Gene_sets_direction_missing)
  PKnames_direction_missing <- names(Gene_sets_direction_missing)

  if ((length(Gene_sets_data_missing)>0)&(length(Gene_sets_data_missing)<10)){
    pct_missing_data <- round(lengths(Gene_sets_data_missing)/lengths(Gene_sets[PKnames_data_missing])*100,2)
    message(paste0(PKnames_data_missing, ": \n ", pct_missing_data, "% of genes (",
                      lengths(Gene_sets_data_missing), "/", lengths(Gene_sets[PKnames_data_missing]),
                      ") in this gene set have information missing in the data matrix; \n", sep = ""))
    if (length((names(pct_missing_data)[pct_missing_data == 100]))>0){
      message(paste0("Gene set: '", names(pct_missing_data)[pct_missing_data == 100], "' has 100% information missing in data matrix.", sep = ""))
    }
  } else if (length(Gene_sets_data_missing)>10) {
    message("Note since more than 10 gene sets have missing information in the data matrix, no detailed individual gene set missing information will be reported.")
  }

  if ((length(Gene_sets_direction_missing)>0)&(length(Gene_sets_direction_missing)<10)){
    pct_missing_direction <- round(lengths(Gene_sets_direction_missing)/lengths(Gene_sets[PKnames_direction_missing])*100,2)
    message(paste0(PKnames_direction_missing, ": \n ", pct_missing_direction,
                      "% of genes (", lengths(Gene_sets_direction_missing), "/", lengths(Gene_sets[PKnames_direction_missing]),
                      ") in this gene set have information missing in the direction matrix; \n", sep = ""))
    if (length((names(pct_missing_direction)[pct_missing_direction == 100]))>0){
      message(paste0("Gene set: '", names(pct_missing_direction)[pct_missing_direction == 100], "' has 100% information missing in direction matrix.", sep = ""))
    }
  } else if (length(Gene_sets_direction_missing)>10){
    message("Note since more than 10 gene sets have missing information in the direction matrix, no detailed individual gene set missing information will be reported.")
  }
}

#' @title Function to Calculate the Average of Gene Expressions
#'
#' @description This function is to calculate the average of gene expressions for genes in the given gene sets.
#'
#'
#' @param Data Data matrix of gene expressions with gene ensembl ID as row names and columns corresponding to different samples.
#' @param pathway.db A list of gene sets.
#'
#' @return Matrix of average gene expression in each gene set with rows corresponding to gene sets and columns corresponding to
#' samples will be returned.
#'
#'
#' @importFrom dplyr %>% as_tibble filter select bind_rows
#' @importFrom purrr map
#' @importFrom tibble column_to_rownames
#'
#' @details Within the ssdGSA function, when GSA_method = "avg.exprs", this function is used to calculate the average of gene
#' expressions for genes in the given gene sets.
#'
#'
#'
## AVG cpm method
avg_expression <- function(Data, pathway.db){
  cal.avg.logcpm <- function(pathway.set, df){
    ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
    gene <- NULL

    dat.sub <- Data %>%
      as_tibble(rownames = "gene") %>%
      dplyr::filter(gene %in% pathway.set) %>%
      dplyr::select(-gene) %>%
      colMeans() # use colMeans() for column-wise means of a matrix
    out <- c(names(pathway.set), dat.sub)
  }
  result <- purrr::map(pathway.db, cal.avg.logcpm, df = Data) %>%
    bind_rows(.id = "pathway") %>%
    tibble::column_to_rownames(var = "pathway")
  return(result)
}



#' @title Function to Calculate Median of Gene Expressions
#'
#' @description This function is to calculate the median of gene expressions for genes in the given gene sets.
#'
#' @param Data Data matrix of gene expressions with gene ensembl ID as row names and columns corresponding to different samples.
#' @param pathway.db A list of gene sets.
#'
#' @return Matrix of average gene expression in each gene set with rows corresponding to gene sets and columns corresponding to
#' samples will be returned.
#'
#' @importFrom dplyr %>% as_tibble filter select bind_rows
#' @importFrom purrr map
#' @importFrom tibble column_to_rownames
#'
#' @details Within the ssdGSA function, when GSA_method = "median.exprs", this function is used to calculate the average of
#' gene expressions for genes in the given gene sets.
#'
#'
## MEDIAN cpm method
median_expression <- function(Data, pathway.db){
  cal.med.logcpm <- function(pathway.set, df){
    ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
    gene <- . <- NULL

    dat.sub <- Data %>%
      as_tibble(rownames = "gene") %>%
      filter(gene %in% pathway.set) %>%
      dplyr::select(-gene) %>%
      apply(., 2, median)
      # as.matrix() %>%
      # robustbase::colMedians() # use colMedians() for column-wise medians of a matrix
    out <- c(names(pathway.set), dat.sub)
  }
  result <- purrr::map(pathway.db, cal.med.logcpm, df = Data) %>%
    bind_rows(.id = "pathway") %>%
    tibble::column_to_rownames(var = "pathway")
  return(result)
}



#' @title Function to Check if Gene IDs in the Input Gene Data Match Well When Direction Matrix Is Missing
#'
#' @description This function is to check if the gene IDs in the gene sets and data matrix match well.
#'
#'
#' @param genes_in_Gene_sets A list of gene names from the gene sets.
#' @param genes_in_Data A list of gene names from the data matrix.
#'
#' @return If there are more than 10\% of gene names in gene sets do not match those in the data matrix or direction matrix,
#' the single sample directional gene set analysis would stop.
#'
#' @details Before single sample directional gene set analysis, it is necessary to check whether the gene ID types in the in the
#' gene sets, data matrix and direction matrix have the same ID type. If not, users should double check to make gene ID types
#' match one another.
#'
#'
#'
check_gene_name_match_noDir <- function(genes_in_Gene_sets,
                                        genes_in_Data){
  pct_overlap_gene_sets <- length(intersect(genes_in_Gene_sets, genes_in_Data))/length(genes_in_Gene_sets)

  if(pct_overlap_gene_sets < 0.1){
    stop("The gene ids in the gene sets and the data matrix do not match well. Please check if data has the same gene id type as the gene sets.")
    }
}



#' @title Function to Check if Genes in Gene Sets Have Missing Information in Data Matrix
#'
#' @description This function is to check if genes in gene sets to be analyzed have missing information in data matrix.
#'
#' @param Gene_sets  A list of gene sets to be analyzed, with gene set names as component names, and each component is a vector
#' of gene entrez ID.
#' @param Data Data matrix of gene expressions with gene ensembl ID as row names and columns corresponding to different samples.
#'
#' @return When at least one gene in the gene sets have information missing in data matrix, warning messages will be given, as
#' well as the percentages (missing number/total number) of gene sets. If less than 10 gene sets have missing information, percentages
#' (missing number/total number) of genes in each gene set that have missing information in data matrix and direction matrix will also be reported.
#' However, if more than 10 gene sets have missing information, no detailed individual gene set missing information will be reported. Also note that
#' if a gene set has 100% information missing in the data matrix, the name of the gene set will be notated.
#'
#'
#' @importFrom vctrs list_drop_empty
#' @importFrom dplyr %>%
#'
#' @details Before single sample directional gene set analysis, it is necessary to check if genes in the gene sets have missing information in
#' data matrix. If not, warning messages would be given such that users can double check whether the gene set analysis results
#' are reliable.
#'
#'
#'
check_genes_missing_noDir <- function(Gene_sets, Data){
  check_in_fun <- function(x, y){
    return(x[!(x %in% y)])
  }
  PKnames <- names(Gene_sets)
  Gene_sets_data_missing <- lapply(Gene_sets[PKnames], check_in_fun, y = rownames(Data))
  pct_missingsets_data <- round(length(vctrs::list_drop_empty(Gene_sets_data_missing))/length(PKnames)*100, 2)
  message(paste0("Warning messages: \n", pct_missingsets_data, "% (", length(vctrs::list_drop_empty(Gene_sets_data_missing)),"/", length(PKnames),") of gene sets have information missing in the data matrix for at least one gene.", sep = ""))

  Gene_sets_data_missing <- vctrs::list_drop_empty(Gene_sets_data_missing)
  PKnames_data_missing <- names(Gene_sets_data_missing)

  if ((length(Gene_sets_data_missing)>0)&(length(Gene_sets_data_missing)<10)){
    pct_missing_data <- round(lengths(Gene_sets_data_missing)/lengths(Gene_sets[PKnames_data_missing])*100,2)
    message(paste0(PKnames_data_missing, ": \n ", pct_missing_data, "% of genes (",
                      lengths(Gene_sets_data_missing), "/", lengths(Gene_sets[PKnames_data_missing]),
                      ") in this gene set have information missing in the data matrix; \n", sep = ""))
    if (length((names(pct_missing_data)[pct_missing_data == 100]))>0){
      message(paste0("Gene set: '", names(pct_missing_data)[pct_missing_data == 100], "' has 100% information missing in data matrix.", sep = ""))
    }
  }else if (length(Gene_sets_data_missing)>10){
    message("Note since more than 10 gene sets have missing information in the data matrix, no detailed individual gene set missing information will be reported.")
  }
}


#' @title Function to Check if 100% of Genes in Gene Sets Have Missing Information in Data Matrix
#'
#' @description This function is to check if 100% of genes in gene sets to be analyzed have missing information in data matrix.
#'
#' @param Gene_sets  A list of gene sets to be analyzed, with gene set names as component names, and each component is a vector
#' of gene entrez ID.
#' @param Data Data matrix of gene expressions with gene ensembl ID as row names and columns corresponding to different samples.
#'
#' @return A list 'Total_missing_in_data_matrix' with names of the gene sets that have 100% of genes that have information missing in the data matrix will be
#' returned. If there are no such gene sets, NULL list will be returned.
#'
#'
#' @importFrom vctrs list_drop_empty
#' @importFrom dplyr %>%
#'
#' @details Before single sample directional gene set analysis, it is necessary to check if genes in the gene sets have missing information in
#' data matrix. If a gene set has 100% information missing in the data matrix, the name of the gene set will be returned as a list named
#' 'Total_missing_in_data_matrix'; If no such gene sets exist, nothing will be returned.
#'
#'
#'
#'
check_genes_missing_total <- function(Gene_sets, Data){
  check_in_fun <- function(x, y){
    return(x[!(x %in% y)])
  }
  PKnames <- names(Gene_sets)
  Gene_sets_data_missing <- lapply(Gene_sets[PKnames], check_in_fun, y = rownames(Data))

  Gene_sets_data_missing <- vctrs::list_drop_empty(Gene_sets_data_missing)
  PKnames_data_missing <- names(Gene_sets_data_missing)
  Gene_names_data_100 <- c()
  if (length(Gene_sets_data_missing)>0){
    pct_missing_data <- round(lengths(Gene_sets_data_missing)/lengths(Gene_sets[PKnames_data_missing])*100,2)
   if (length((names(pct_missing_data)[pct_missing_data == 100]))>0){
          Gene_names_data_100 <- names(pct_missing_data)[pct_missing_data == 100]
   }
  }
  return(list(Total_missing_in_data_matrix = Gene_names_data_100))
}


## ssGSA

#' @title Function to Calculate Single Sample Gene Set Scores without Direction Matrix
#'
#' @description This function is to calculate traditional single sample gene set scores without considering the direction
#' of each gene.
#'
#' @param Data Data matrix of gene expressions with gene ID as row names and columns corresponding to different samples.
#' @param Gene_sets A list of gene sets with gene set names as component names, and each component is a vector of gene ID.
#' @param GSA_weight Method to calculate weight in GSA. By default this is set to "group_weighted". Other option is "equal_weighted".
#' @param GSA_weighted_by When "group_weighted" is chosen to calculate GSA_weight, further specifications are need to specify how
#' group weights are calculated. By default this is set to "avg.ES" (average of group ES). Other options are "sum.ES" (sum of group ES)
#' and "median.ES" (median of group ES).
#' @param GSA_method Method to employ in the estimation of gene-set enrichment scores per sample. By default this is set to "gsva"
#' (Hanzelmann et al, 2013). Other options are "ssgsea" (Barbie et al, 2009), "zscore" (Lee et al, 2008), "avg.exprs" (average value
#' of gene expressions in the gene set), and "median.exprs" (median of gene expressions in the gene set).
#' @param min.sz GSVA parameter to define the minimum size of the resulting gene sets. By default this is set to 1.
#' @param max.sz GSVA parameter to define the maximum size of the resulting gene sets. By default this is set to 2000.
#' @param mx.diff  GSVA parameter to offer two approaches to calculate the enrichment statistic from the KS random walk statistic.
#' mx.diff = FALSE: enrichment statistic is calculated as the maximum distance of the random walk from 0. mx.diff=TRUE (default):
#' enrichment statistic is calculated as the magnitude difference between the largest positive and negative random walk deviations.
#'
#'
#' @return Matrix of gene set scores (without considering directionality information of each gene) with rows corresponding to gene sets
#' and columns corresponding to different samples will be return.
#'
#'
#' @importFrom dplyr %>% filter mutate across pull left_join rename_at group_by
#' @importFrom utils stack
#' @importFrom GSVA gsva gsvaParam ssgseaParam zscoreParam
#' @importFrom stats median
#'
#' @details Single sample directional gene set analysis inherits the standard gene set variation analysis(GSVA) method, but also provides
#' the option to use summary statistics from any analysis (disease vs healthy, LS vs NL, etc..) input to define the direction of gene
#' sets used for directional gene set score calculation for a given disease or directional function. However, when the directionality
#' information is missing for genes, gene set scores from traditional single sample gene set analysis will be returned.
#'
#'
#' @references Xingpeng Li, Qi Qian. ssdGSA - Single sample direction gene set analysis tool.
#' @references Barbie, D.A. et al. Systematic RNA interference reveals that oncogenic KRAS-driven cancers require TBK1. Nature, 462(5):108-112, 2009.
#' @references Hanzelmann, S., Castelo, R. and Guinney, J. GSVA: Gene set variation analysis for microarray and RNA-Seq data. BMC Bioinformatics, 14:7, 2013.
#' @references Lee, E. et al. Inferring pathway activity toward precise disease classification. PLoS Comp Biol, 4(11):e1000217, 2008.
#' @references Tomfohr, J. et al. Pathway level analysis of gene expression using singular value decomposition. BMC Bioinformatics, 6:225, 2005.
#'
#' @seealso ssdGSA, ssdGSA_individual
#'
#'
#' @keywords gene set variation analysis
#'

## Main ssGSA function
ssGSA <- function(Data, # Input data matrix here
                  Gene_sets, # Input gene sets here
                  GSA_weight = "equal_weighted", # Options are: "equal_weighted" or "group_weighted"
                  GSA_weighted_by = "sum.ES", # Options are: "sum.ES", "avg.ES" or "median.ES"
                  GSA_method = "gsva", # Options are: "gsva","ssgsea","zscore","avg.exprs", or "median.exprs"
                  min.sz = 1,  # GSVA parameter
                  max.sz = 2000, # GSVA parameter
                  mx.diff = TRUE # GSVA parameter
){
    #0. Check parameter input:
    #Check input and see if character input is correct
    #Check GSA_method
    if(!(GSA_method %in% c("gsva","ssgsea","zscore","avg.exprs", "median.exprs"))){
      stop("The parameter input 'GSA_method' is not correct. Please choose from 'gsva','ssgsea','zscore','avg.exprs', 'median.exprs'.")}
    #Check GSA_weight
    if(!(GSA_weight %in% c("equal_weighted","group_weighted"))){
      stop("The parameter input 'GSA_weight' is not correct. Please choose from 'equal_weighted','group_weighted'.")}
    #Check GSA_weighted_by
    if(!(GSA_weighted_by %in% c("sum.ES","avg.ES","median.ES"))){
      stop("The parameter input 'GSA_weighted_by' is not correct. Please choose one from 'sum.ES','avg.ES','median.ES'.")}


    #1.1 Check if the row names of data matrix (Data) and gene sets (Gene_sets) are using the same gene ID
    genes_in_Gene_sets <- unlist(Gene_sets) %>% as.vector
    genes_in_Data <- rownames(Data) %>% as.vector

    check_gene_name_match_noDir(genes_in_Gene_sets,
                          genes_in_Data)

    #1.2 Report the percent of genes in each gene set that do not have information in the data matrix
    check_genes_missing_noDir(Gene_sets, Data)



    #2. Run GSVA for each gene set

    # Add a message to state the running of gsva
    message(paste0("Note: Running GSVA for ", length(names(Gene_sets)), " gene sets using '", GSA_method, "' method with '", GSA_weight, "' weights.", sep = ""))
      if(GSA_method == "gsva"){

        # "gsva", "ssgsea", "zscore"
        suppressMessages(GS_final <- gsva(gsvaParam(exprData = Data,
                                                 geneSets = Gene_sets,
                                                 minSize = min.sz,
                                                 maxSize = max.sz,
                                                 maxDiff = mx.diff)))
        # "ssgsea"
      }else if(GSA_method == "ssgsea"){
        suppressMessages(GS_final <- gsva(ssgseaParam(exprData = Data,
                                                   geneSets = Gene_sets,
                                                   minSize = min.sz,
                                                   maxSize = max.sz)))

        # "zscore"
      }else if(GSA_method == "zscore"){
        suppressMessages(GS_final <- gsva(zscoreParam(exprData = Data,
                                                   geneSets = Gene_sets,
                                                   minSize = min.sz,
                                                   maxSize = max.sz)))


      # "avg.exprs"
    }else if(GSA_method == "avg.exprs"){
      GS_final <- avg_expression(Data, Gene_sets)

      # "median.exprs"
    }else if(GSA_method == "median.exprs"){
      GS_final <- median_expression(Data, Gene_sets)
    }
  return(GS_final)
}


