#' @title Single Sample Directional Gene Set Analysis (ssdGSA)
#'
#' @description This function is to calculate directional (disease weighted) gene set scores by incorporating each gene's
#' correlation to a disease or pathway in the gene set.
#'
#' @param Data Data matrix of gene expressions with gene ID as row names and columns corresponding to different samples.
#' @param Gene_sets A list of gene sets with gene set names as component names, and each component is a vector of gene ID.
#' @param Direction_matrix Matrix containing directionality information for each gene, such as effect size, t statistics,
#' p value of summary statistics. Each row of the direction matrix is for one gene, and there should be at least two columns
#' (with the 1st column containing gene entrez ID, and 2nd column containing directionality information). Note that the default is
#' "Direction_matrix = NULL", meaning that no direction matrix is inputted, then the classic single sample gene set scores without
#' direction information would be calculated and returned.
#' @param GSA_weight Method to calculate weight in GSA. By default this is set to "group_weighted". Other option is "equal_weighted".
#' @param GSA_weighted_by When "group_weighted" is chosen to calculate GSA_weight, further specifications are needed to specify how
#' group weights are calculated. By default, this is set to "avg.ES" (average of group ES). Other options are "sum.ES" (sum of group ES)
#' and "median.ES" (median of group ES).
#' @param GSA_method Method to employ in the estimation of gene set enrichment scores per sample. By default this is set to "gsva"
#' (Hanzelmann et al, 2013). Other options are "ssgsea" (Barbie et al, 2009), "zscore" (Lee et al, 2008), "avg.exprs" (average value
#' of gene expressions in the gene set), and "median.exprs" (median of gene expressions in the gene set).
#' @param min.sz GSVA parameter to define the minimum size of the resulting gene sets. By default this is set to 1.
#' @param max.sz GSVA parameter to define the maximum size of the resulting gene sets. By default this is set to 2000.
#' @param mx.diff  GSVA parameter to offer two approaches to calculate the enrichment statistic from the KS random walk statistic.
#' mx.diff = FALSE: enrichment statistic is calculated as the maximum distance of the random walk from 0. mx.diff=TRUE (default):
#' enrichment statistic is calculated as the magnitude difference between the largest positive and negative random walk deviations.
#'
#'
#' @return Matrix of directional gene set scores with rows corresponding to gene sets and columns corresponding to different
#' samples will be return.
#'
#'
#' @importFrom dplyr %>% filter mutate across pull left_join rename_at group_by
#' @importFrom tibble tibble rownames_to_column column_to_rownames
#' @importFrom utils stack
#' @importFrom GSVA gsva gsvaParam ssgseaParam zscoreParam
#' @importFrom stats median
#'
#' @details Single sample directional gene set analysis inherits the standard gene set variation analysis(GSVA) method, but also provides
#' the option to use summary statistics from any analysis (disease vs healthy, lesional side vs nonlesional side, etc..) input to define the direction of gene
#' sets used for directional gene set score calculation for a given disease or directional function. This function is specific for using group weighted scores.
#'
#'
#' @references Xingpeng Li, Qi Qian. ssdGSA - Single sample directional gene set analysis tool.
#' @references Barbie, D.A. et al. Systematic RNA interference reveals that oncogenic KRAS-driven cancers require TBK1. Nature, 462(5):108-112, 2009.
#' @references Hanzelmann, S., Castelo, R. and Guinney, J. GSVA: Gene set variation analysis for microarray and RNA-Seq data. BMC Bioinformatics, 14:7, 2013.
#' @references Lee, E. et al. Inferring pathway activity toward precise disease classification. PLoS Comp Biol, 4(11):e1000217, 2008.
#' @references Tomfohr, J. et al. Pathway level analysis of gene expression using singular value decomposition. BMC Bioinformatics, 6:225, 2005.
#'
#' @seealso ssdGSA_individual
#'
#' @examples
#'
#' ssdGSA(Data = data_matrix_entrezID,
#'        Gene_sets = gene_sets[c(1,2,4)],
#'        Direction_matrix = direction_matrix,
#'        GSA_weight = "group_weighted",
#'        GSA_weighted_by = "sum.ES",
#'        GSA_method = "gsva",
#'        min.sz = 1,
#'        max.sz = 2000,
#'        mx.diff = TRUE
#'        )
#'
#'
#' @keywords gene set variation analysis
#'
#' @export


## Main ssdGSA function
ssdGSA <- function(Data, # Input data matrix here
                   Gene_sets, # Input gene sets here
                   Direction_matrix = NULL, # Input direction matrix here
                   GSA_weight = "equal_weighted", # Options are: "equal_weighted" or "group_weighted"
                   GSA_weighted_by = "sum.ES", # Options are: "sum.ES", "avg.ES" or "median.ES"
                   GSA_method = "gsva", # Options are: "gsva","ssgsea","zscore","avg.exprs", or "median.exprs"
                   min.sz = 1,  # GSVA parameter
                   max.sz = 2000, # GSVA parameter
                   mx.diff = TRUE # GSVA parameter
){

  #check if Data and Gene_sets are formated
  if(!is.matrix(Data)){stop("Please check the format of the data matrix.")}
  if(!is.list(Gene_sets)){stop("Please check the format of the gene sets.")}

  #the case when Direction_matrix is empty
  if(is.null(Direction_matrix)){
    GS_final = ssGSA(Data,
                     Gene_sets,
                     GSA_weight,
                     GSA_weighted_by,
                     GSA_method,
                     min.sz,
                     max.sz,
                     mx.diff)
  }else{

  # Format the input data
  Direction_matrix <- Direction_matrix %>%
                          mutate(across(c(1), as.character)) %>%
                          dplyr::rename(gene = c(1), ES = c(2)) ### name gene!! as.character(first column)

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


  #1.1 Check if the row names of data matrix (Data), gene sets (Gene_sets), and direction matrix (Direction_matrix) are using the same gene ID
  genes_in_Gene_sets <- unlist(Gene_sets) %>% as.vector
  genes_in_Data <- rownames(Data) %>% as.vector
  genes_in_Direction_matrix <- Direction_matrix$gene %>% as.vector

  check_gene_name_match(genes_in_Gene_sets,
                        genes_in_Data,
                        genes_in_Direction_matrix)

  #1.2 Report the percent of genes in each gene set that do not have information in the data matrix or direction matrix
  check_genes_missing(Gene_sets, Data, Direction_matrix)


  #2. Split the gene sets based on the directionality of genes with information in the direction matrix
  ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
  ES <- gene <- NULL

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

  #3. Run GSA for each gene set when not all pov or neg are empty
  if ((sum(lengths(Gene_sets_pov)) != 0)&(sum(lengths(Gene_sets_neg)) != 0)){

  # Add a message to state the running of gsva
  message(paste0("Note: Running GSA for ", length(names(Gene_sets)), " gene sets using '", GSA_method, "' method with '", GSA_weight, "' weights.", sep = ""))
  if(GSA_method == "gsva"){

    # "gsva", "ssgsea", "zscore"
    suppressMessages(GS_up <- gsva(gsvaParam(exprData = Data,
                                             geneSets = Gene_sets_pov,
                                             minSize = min.sz,
                                             maxSize = max.sz,
                                             maxDiff = mx.diff)))



    suppressMessages(GS_down <- gsva(gsvaParam(exprData = Data,
                                               geneSets = Gene_sets_neg,
                                               minSize = min.sz,
                                               maxSize = max.sz,
                                               maxDiff =  mx.diff)))

    # "ssgsea"
  }else if(GSA_method == "ssgsea"){
    suppressMessages(GS_up <- gsva(ssgseaParam(exprData = Data,
                                             geneSets = Gene_sets_pov,
                                             minSize = min.sz,
                                             maxSize = max.sz)))



    suppressMessages(GS_down <- gsva(ssgseaParam(exprData = Data,
                                               geneSets = Gene_sets_neg,
                                               minSize = min.sz,
                                               maxSize = max.sz)))

    # "ssgsea"
  }else if(GSA_method == "zscore"){
    suppressMessages(GS_up <- gsva(zscoreParam(exprData = Data,
                                               geneSets = Gene_sets_pov,
                                               minSize = min.sz,
                                               maxSize = max.sz)))



    suppressMessages(GS_down <- gsva(zscoreParam(exprData = Data,
                                                 geneSets = Gene_sets_neg,
                                                 minSize = min.sz,
                                                 maxSize = max.sz)))

    # "avg.exprs"
  }else if(GSA_method == "avg.exprs"){
    GS_up <- avg_expression(Data, Gene_sets_pov)
    GS_down <- avg_expression(Data, Gene_sets_neg)

    # "median.exprs"
  }else if(GSA_method == "median.exprs"){
    GS_up <- median_expression(Data, Gene_sets_pov)
    GS_down <- median_expression(Data, Gene_sets_neg)
  }


  # Check if the dimension of GS_up and GS_down is the same
  if ((dim(GS_up)[1] != dim(GS_down)[1])|(dim(GS_up)[2] != dim(GS_down)[2]))
    warning("Please check if there are enough genes in the up-regulated or down-regulated gene sets")

  #4. Calculate final score for each gene set

  if(GSA_weight == "equal_weighted"){
    ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
    . <- NULL

    suppressMessages(GS_up <- tibble(GS = names(Gene_sets_pov)) %>%
      left_join(., GS_up %>% as.data.frame()%>% tibble::rownames_to_column(var="GS"), by = "GS") %>%
      replace(is.na(.), 0) %>%
      tibble::column_to_rownames(var = "GS"))

    suppressMessages(GS_down <- tibble(GS = names(Gene_sets_neg)) %>%
      left_join(., GS_down %>% as.data.frame()%>% tibble::rownames_to_column(var="GS"), by = "GS") %>%
      replace(is.na(.), 0) %>%
      tibble::column_to_rownames(var = "GS"))

    GS_final = GS_up - GS_down

  }else if(GSA_weight == "group_weighted"){
    # function to get the sum/avg/median of ES
    ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
    GS_pov <- GS_neg <- NULL

    suppressMessages(GS_pov_temp <- Gene_sets_pov %>%
                       utils::stack() %>%
                       rename_at(1, ~"gene") %>%
                       rename_at(2, ~"GS_pov" ) %>%
                       left_join(Direction_matrix) %>%
                       group_by(GS_pov))


    suppressMessages(GS_neg_temp <- Gene_sets_neg %>%
                       utils::stack() %>%
                       rename_at(1, ~"gene") %>%
                       rename_at(2, ~"GS_neg" ) %>%
                       left_join(Direction_matrix) %>%
                       group_by(GS_neg))

    if(GSA_weighted_by == "sum.ES"){
      GS_pov <- GS_pov_temp %>% summarise(cal_ES = sum(ES))
      GS_neg <- GS_neg_temp %>% summarise(cal_ES = sum(ES))
    } else if(GSA_weighted_by == "avg.ES") {
      GS_pov <- GS_pov_temp %>% summarise(cal_ES = mean(ES))
      GS_neg <- GS_neg_temp %>% summarise(cal_ES = mean(ES))
    } else if(GSA_weighted_by == "median.ES") {
      GS_pov <- GS_pov_temp %>% summarise(cal_ES = median(ES))
      GS_neg <- GS_neg_temp %>% summarise(cal_ES = median(ES))
    }


    # If there are missing information in the direction matrix, we need to modify that. In this case, we assign ES 0 to those
    # gene sets which have missing values in direction matrix.

    ## Quiets concerns of R CMD check re: the .'s that appear in pipelines
    . <- cal_ES <- NULL

    suppressMessages(GS_pov2 <- tibble(GS_pov = names(Gene_sets_pov)) %>%
      left_join(., GS_pov) %>%
      replace(is.na(.), 0) )

    suppressMessages(GS_neg2 <- tibble(GS_neg = names(Gene_sets_neg)) %>%
      left_join(., GS_neg) %>%
      replace(is.na(.), 0) )

    # To modify the case when length(Gene_sets_pov)!= dim(GS_up)[1]. In this case, we assign scores 0 to those
    # gene sets which have not been calculated GSA scores.
    GS_up2 <- GS_up %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "GS_pov") %>%
      left_join(GS_pov2,., by = "GS_pov") %>%
      replace(is.na(.), 0) %>%
      dplyr::select(-cal_ES) %>%
      as.data.frame() %>%
      tibble::column_to_rownames(., "GS_pov")

    GS_down2 <- GS_down %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "GS_neg") %>%
      left_join(GS_neg2,., by = "GS_neg") %>%
      replace(is.na(.), 0) %>%
      dplyr::select(-cal_ES) %>%
      as.data.frame() %>%
      tibble::column_to_rownames(., "GS_neg")

    GS_final = 2 * (abs(GS_pov2$cal_ES) * GS_up2 - abs(GS_neg2$cal_ES) * GS_down2) / (abs(GS_pov2$cal_ES) + abs(GS_neg2$cal_ES))

      }
  } else if(sum(lengths(Gene_sets_pov)) == 0){
    warning("There are no genes in the up-regulated gene sets, classic single sample gene set analysis will be executed.")
    GS_final = ssGSA(Data,
                     Gene_sets_neg,
                     GSA_weight,
                     GSA_weighted_by,
                     GSA_method,
                     min.sz,
                     max.sz,
                     mx.diff)

  } else if (sum(lengths(Gene_sets_neg)) == 0){
    warning("There are no genes in the down-regulated gene sets, classic single sample gene set analysis will be executed.")
    GS_final = ssGSA(Data,
                     Gene_sets_pov,
                     GSA_weight,
                     GSA_weighted_by,
                     GSA_method,
                     min.sz,
                     max.sz,
                     mx.diff)
    }


  }# where direction matrix is NULL

  return(GS_final)
}

