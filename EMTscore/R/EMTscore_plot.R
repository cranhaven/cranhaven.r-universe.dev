############################# Execution of Plot function

## Function Input:  gene expression matrix and cell type information

## Function Output: Figure of EMT score
#' present EMT score result
#' 
#' @param Methods can select nnPCA, ssGSEA, AUCell, SCSE
#' @param cell_annotation_file cell annotation file in data frame, The column name for cell type is "celltype_annotation" and the column name for cell sampleID is "name".
#' @param geneExp gene expression matrix
#' @param M_signature_gene_sets M signature gene list in dataframe, we supply example list such as geneList_M, M_signature_for_cancer, M_signature_for_cell
#' @param E_signature_gene_sets E signature gene list in dataframe, we supply example list such as geneList_E, E_signature_for_cancer, E_signature_for_cell
#'
#' @return Figure represent EMT scores and cell type
#' @export
#'
#' @examples
#' library(nsprcomp)
#' library(ggplot2)
#' library(dplyr)
#' library(gridExtra)
#' library(curl)
#' Methods = "nnPCA"
#' data(cell_annotation_file)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_E_signature)
#' data(Panchy_et_al_M_signature)
#' 
#' 
#' Execute_E_M_plot(cell_annotation_file, 
#' Methods, 
#' geneExp, 
#' M_signature_gene_sets = Panchy_et_al_M_signature, 
#' E_signature_gene_sets = Panchy_et_al_E_signature
#' )


Execute_E_M_plot <- function(cell_annotation_file, Methods, geneExp, M_signature_gene_sets, E_signature_gene_sets)
{
  merge.all <- function(x, ..., by = "row.names") {
    L <- list(...)
    for (i in seq_along(L)) {
      x <- merge(x, L[[i]], by = by)
      rownames(x) <- x$Row.names
      x$Row.names <- NULL
    }
    return(x)
  }
  rownames(cell_annotation_file) = cell_annotation_file$name
  if (Methods == "nnPCA"){
    nnPCA_Escore <- Execute_nnPCA(geneExp, E_signature_gene_sets, dimension=1,score_names='Escore')
    nnPCA_Mscore <- Execute_nnPCA(geneExp, M_signature_gene_sets, dimension=1,score_names='Mscore')
    
    data_for_plot <- merge.all(cell_annotation_file, nnPCA_Escore, nnPCA_Mscore)
  }
  if (Methods == "ssGSEA"){
    ssGSEA_Escore <- Execute_ssGSEA(geneExp, E_signature_gene_sets, colnames = 'Escore')
    ssGSEA_Mscore <- Execute_ssGSEA(geneExp, M_signature_gene_sets, colnames = 'Mscore')
    data_for_plot <- merge.all(cell_annotation_file, ssGSEA_Escore, ssGSEA_Mscore)
  }
  if (Methods == "AUCell"){
    AUCell_Escore <- Execute_AUCell(geneExp, E_signature_gene_sets, colnames = "Escore")
    AUCell_Mscore <- Execute_AUCell(geneExp, M_signature_gene_sets, colnames = 'Mscore')
    data_for_plot <- merge.all(cell_annotation_file, AUCell_Escore, AUCell_Mscore)
  }
  if (Methods == "SCSE"){
    SCSE_Escore <- Execute_SCSE(geneExp, E_signature_gene_sets, colnames = 'Escore')
    SCSE_Mscore <- Execute_SCSE(geneExp, M_signature_gene_sets, colnames = 'Mscore')
    data_for_plot <- merge.all(cell_annotation_file, SCSE_Escore, SCSE_Mscore)
  }
  
  
  
  colors = c("#F87189", "#CE9031", "#A48CF5", "#97A430", "#39A7D0", "#E57D5F", 
             "#84C7B9", "#E1AF64", "#C26CCF", "#B0BF43", "#57C3E8", "#F29D9E", "#92AAE6")
  
  p = ggplot(data_for_plot, aes(x = Escore, y = Mscore, fill =celltype_annotation)) +
    geom_point(size = 2.5, alpha = 0.3 , aes(color = celltype_annotation)) +
    scale_colour_manual(values=colors) +
    stat_density2d(aes(x = Escore, y = Mscore), bins = 10, alpha=0.2, geom="polygon")
  
  sbg <- data_for_plot %>% 
    group_by(celltype_annotation) %>% 
    summarise(count = n(),
              mE = mean(Escore), 
              sdE = sd(Escore), 
              mM = mean(Mscore), 
              sdM = sd(Mscore))
  
  p2 = p + geom_errorbar(data = sbg, 
                         mapping = aes(x = mE, y = mM,
                                       ymin = mM - 0.8*sdM, 
                                       ymax = mM + 0.8*sdM), 
                         width = 0,size = 2) +
    geom_errorbarh(data = sbg, 
                   mapping = aes(x = mE, y = mM,
                                 xmin = mE - 0.8*sdE,
                                 xmax = mE + 0.8*sdE),
                   height = 0,size = 2) + 
    geom_point(data = sbg, aes(x = mE, 
                               y = mM,
                               fill = celltype_annotation), 
               color = "white", shape = 21, size =6,
               alpha = 1, show.legend = FALSE,stroke = 1.5) + 
    scale_colour_manual(values=colors) +
    scale_fill_manual(values=colors) +
    theme(panel.background = element_rect(fill = "white", colour = "black"))
  
  
  p3 = p2  + theme(panel.border = element_rect(color = "black", size = 1.5, fill = NA))  +
    theme(title=element_text(size=12,color="black",
                             face="italic",hjust=0.5,lineheight=0.2),
          axis.text=element_text(size=12,color="black") ) +
    theme(axis.ticks.length.x = unit(0.15,'cm'), 
          axis.ticks.length.y = unit(0.15,'cm')) + 
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
    )
  
  return (p3)
}


############################# Execution of Plot function

## Function Input:  gene expression matrix and cell type information

## Function Output: Figure of M dimension score
#' present M dimension score result
#' 

#' @param cell_annotation_file cell annotation file in data frame, The column name for cell type is "celltype_annotation" and the column name for cell sampleID is "name".
#' @param geneExp gene expression matrix
#' @param M_signature_gene_sets M signature gene list in dataframe, we supply example list such as geneList_M, M_signature_for_cancer, M_signature_for_cell
#'
#' @return Figure represent M dimension 1 and dimension 2 scores and cell type
#' @export
#'
#' @examples
#' library(nsprcomp)
#' library(ggplot2)
#' library(dplyr)
#' library(gridExtra)
#' library(curl)
#' data(cell_annotation_file)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_M_signature)
#' 
#' 
#' Execute_M_dimension_plot(
#' cell_annotation_file ,
#' geneExp, 
#' M_signature_gene_sets = Panchy_et_al_M_signature
#' )  # only for nnPCA method


Execute_M_dimension_plot <- function(cell_annotation_file, geneExp, M_signature_gene_sets)
{
  merge.all <- function(x, ..., by = "row.names") {
    L <- list(...)
    for (i in seq_along(L)) {
      x <- merge(x, L[[i]], by = by)
      rownames(x) <- x$Row.names
      x$Row.names <- NULL
    }
    return(x)
  }
  rownames(cell_annotation_file) = cell_annotation_file$name
  nnPCA_Mscore <- Execute_nnPCA(geneExp, M_signature_gene_sets, dimension=2,score_names=c('M1_score','M2_score'))
    
  data_for_plot <- merge.all(cell_annotation_file, nnPCA_Mscore)
  
  
  colors = c("#F87189", "#CE9031", "#A48CF5", "#97A430", "#39A7D0", "#E57D5F", 
             "#84C7B9", "#E1AF64", "#C26CCF", "#B0BF43", "#57C3E8", "#F29D9E", "#92AAE6")
  
  p = ggplot(data_for_plot, aes(x = M1_score, y = M2_score, fill =celltype_annotation)) +
    geom_point(size = 2.5, alpha = 0.3 , aes(color = celltype_annotation)) +
    scale_colour_manual(values=colors) +
    stat_density2d(aes(x = M1_score, y = M2_score), bins = 10, alpha=0.2, geom="polygon")
  
  sbg <- data_for_plot %>% 
    group_by(celltype_annotation) %>% 
    summarise(count = n(),
              mE = mean(M1_score), 
              sdE = sd(M1_score), 
              mM = mean(M2_score), 
              sdM = sd(M2_score))
  
  p2 = p + geom_errorbar(data = sbg, 
                         mapping = aes(x = mE, y = mM,
                                       ymin = mM - 0.8*sdM, 
                                       ymax = mM + 0.8*sdM), 
                         width = 0,size = 2) +
    geom_errorbarh(data = sbg, 
                   mapping = aes(x = mE, y = mM,
                                 xmin = mE - 0.8*sdE,
                                 xmax = mE + 0.8*sdE),
                   height = 0,size = 2) + 
    geom_point(data = sbg, aes(x = mE, 
                               y = mM,
                               fill = celltype_annotation), 
               color = "white", shape = 21, size =6,
               alpha = 1, show.legend = FALSE,stroke = 1.5) + 
    scale_colour_manual(values=colors) +
    scale_fill_manual(values=colors) +
    theme(panel.background = element_rect(fill = "white", colour = "black"))
  
  
  p3 = p2  + theme(panel.border = element_rect(color = "black", size = 1.5, fill = NA))  +
    theme(title=element_text(size=12,color="black",
                             face="italic",hjust=0.5,lineheight=0.2),
          axis.text=element_text(size=12,color="black") ) +
    theme(axis.ticks.length.x = unit(0.15,'cm'), 
          axis.ticks.length.y = unit(0.15,'cm')) + 
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
    )
  
  return (p3)
}





############################# Execution of Plot function

## Function Input:  gene expression matrix and cell type information

## Function Output: Figure of EM score and M dimension score for nnPCA
#' present EM score and M dimension score result
#' 

#' @param cell_annotation_file A data frame containing cell annotation, with columns `celltype_annotation` and `name`.
#' @param geneExp A gene expression matrix.
#' @param gene_lists A list of M and E signature gene sets to be used for the nnPCA analysis.
#' @param fig_title The title of the figure
#' @return Figure represent two plots: 1): EM score 2): M dimension 1 and dimension 2 scores and cell type
#' @export
#'
#' @examples
#' 
#' library(ggpubr)
#' library(ggplot2)
#' library(nsprcomp)
#' library(dplyr)
#' library(gridExtra)
#' data(cell_annotation_file)
#' library(curl)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_M_signature)
#' data(Panchy_et_al_E_signature)
#' 
#' 
#' gene_lists <- list(Panchy_et_al = list(Panchy_et_al_M_signature, Panchy_et_al_E_signature))
#' 
#' Execute_nnPCA_plot(
#' cell_annotation_file, 
#' geneExp, 
#' gene_lists, 
#' fig_title = 'RPM'
#' )  # only for nnPCA method


Execute_nnPCA_plot <- function(cell_annotation_file, geneExp, gene_lists, fig_title){
  # Initialize list to store plots
  plots <- list(EM_plots = list(), MM_plots = list())
  
  # Loop through each 'major' list and create EvM and M1vsM2 plot
  for (name in names(gene_lists)){
    # Get E and M genes from each major list
    M_genes <- gene_lists[[name]][[1]]
    E_genes <- gene_lists[[name]][[2]]
    
    # Create plots
    EM_plot <- Execute_E_M_plot(cell_annotation_file, 'nnPCA',
                                geneExp, M_genes, E_genes) +
      ggtitle(paste(name))
    MM_plot <- Execute_M_dimension_plot(cell_annotation_file,
                                        geneExp, M_genes)
    
    # Store each plot with name
    plots[[1]][[paste0(name, '_EM_plot')]] <- EM_plot
    plots[[2]][[paste0(name, '_MM_plot')]] <- MM_plot
  }
  
  # Arrange rows of plot
  row1 <- plots[[1]]
  row2 <- plots[[2]]
  
  # Add labels to each row separately
  row1_labeled <- annotate_figure(ggarrange(plotlist = row1, ncol = length(row1), legend = 'none'),
                                  left = text_grob("E vs M", rot = 90, face = "bold", vjust = 0.5))
  
  row2_labeled <- annotate_figure(ggarrange(plotlist = row2, ncol = length(row2), common.legend = T, legend = 'bottom'),
                                  left = text_grob("M1 vs M2", rot = 90, face = "bold", vjust = 0.5))
  
  # Combine both labeled rows into a single arranged plot
  arranged_plot <- ggarrange(row1_labeled, row2_labeled, ncol = 1, nrow = 2)
  
  # Add main title to the arranged plot
  arranged_plot <- annotate_figure(arranged_plot, top = text_grob(fig_title, face = "bold"))
  
  return(arranged_plot)
}





############################# Execution of Plot function

## Function Input:  gene expression matrix and cell type information

## Function Output: Figure of heat map
#' present result of heat map of genes that contribute to M score of dimension 1 and disension 2
#' 
#' @param geneExp gene expression matrix
#' @param geneList_M M signature gene list in dataframe, we supply example list such as geneList_M, M_signature_for_cancer, M_signature_for_cell
#'
#' @return Figure represent EMT score for different methods and gene sets
#' @export
#'
#' @examples
#' 
#' library(ggpubr)
#' library(ggplot2)
#' library(dplyr)
#' library(nsprcomp)
#' library(pheatmap)
#' library(grid)
#' library(gridExtra)
#' library(circlize)
#' library(paletteer)
#' library(ggthemes)
#' library(ComplexHeatmap)

#' library(curl)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_M_signature)
#' geneList_M = Panchy_et_al_M_signature
#' 
#' 
#' plot_heatmap_function( geneExp, geneList_M)

plot_heatmap_function <- function(geneExp,geneList_M)
{
  
  geneExp_M <- geneExp[, colnames(geneExp) %in% geneList_M$GeneName]
  pc_feature_M <- nsprcomp(as.matrix(geneExp_M), nneg=TRUE, ncomp=2)
  
  # Get the top genes based on pcs
  result = data.frame(pc_feature_M$rotation)
  PC1_gname = rownames(result[order(-result$PC1),])[1:10]
  PC2_gname = rownames(result[order(-result$PC2),])[1:10]
  
  # Extract expression data for top genes
  PC1_gname_Exp = geneExp[, colnames(geneExp) %in% PC1_gname]
  PC2_gname_Exp = geneExp[, colnames(geneExp) %in% PC2_gname]
  
  # Arrange data for heatmap
  new_df1 <- t(PC1_gname_Exp)[PC1_gname, ]
  new_df2 <- t(PC2_gname_Exp)[PC2_gname, ]
  
  result_plot = rbind(new_df1, new_df2)
  
  # Generate M scores for each component
  M_score = data.frame(pc_feature_M$x)
  colnames(M_score) = c("M_PC1_score","M_PC2_score")
  
  # Reorder expression data to match M scores
  sorted_data_M = M_score[colnames(result_plot), ]

  
  # PCs and annotations
  PC_gname = append(PC1_gname, PC2_gname)
  result_plot2 = result[PC_gname, ]
  
  # Define color palettes using paletteer
  # This will set white as the minimum at the darker color as the max
  
  expr_palette = "grDevices::RdBu"
  pc1_palette = "ggthemes::Classic Blue"
  pc2_palette = "ggthemes::Red"
  m_pc1_palette = "ggthemes::Classic Green"
  m_pc2_palette = "ggthemes::Orange"
  label_palette = "ggsci::hallmarks_light_cosmic"
  
  expr_colors <- paletteer::paletteer_c(expr_palette, n = 50)
  pc1_colors <- colorRamp2(c(0, max(result_plot2$PC1, na.rm = TRUE)), 
                           c("white", paletteer::paletteer_c(pc1_palette, n = 2, direction = -1)[1]))
  pc2_colors <- colorRamp2(c(0, max(result_plot2$PC2, na.rm = TRUE)), 
                           c("white", paletteer::paletteer_c(pc2_palette, n = 2, direction = -1)[1]))
  m_pc1_colors <- colorRamp2(c(min(sorted_data_M$M_PC1_score), max(sorted_data_M$M_PC1_score)),
                             c("white", paletteer::paletteer_c(m_pc1_palette, n = 2, direction = -1)[1]))
  m_pc2_colors <- colorRamp2(c(min(sorted_data_M$M_PC2_score), max(sorted_data_M$M_PC2_score)), 
                             c("white", paletteer::paletteer_c(m_pc2_palette, n = 2, direction = -1)[1]))
  
  # Categorical palette
  label_colors <- paletteer::paletteer_d(label_palette, n = 2)
  names(label_colors) <- c("M_PC1", "M_PC2")
  
  # Define annotations
  row_ha = rowAnnotation(PC1 = result_plot2$PC1 , PC2 = result_plot2$PC2,
                         col = list(
                           PC1 = pc1_colors,
                           PC2 = pc2_colors
                         ))
  
  
  
  # Define categorical color 
  row_labels <- c(rep("M_PC1", length(PC1_gname)), rep("M_PC2", length(PC2_gname)))
  row_label_annotation <- rowAnnotation(
    Label = row_labels,
    col = list(Label = label_colors)
  )
  
  # Define continuous color map for M scores
  ha <- HeatmapAnnotation(
    M_PC1_score = sorted_data_M$M_PC1_score,
    M_PC2_score = sorted_data_M$M_PC2_score,
    col = list(
      M_PC1_score = m_pc1_colors,
      M_PC2_score = m_pc2_colors
    )
  )
  
  # Generate heatmap with specified color schemes
  p <- Heatmap(
    result_plot, 
    col = expr_colors, 
    cluster_rows = FALSE,
    top_annotation = ha,
    right_annotation = row_ha,
    left_annotation = row_label_annotation, 
    show_column_names = FALSE,
    name = "Gene Expr",
  )
  return (p)
}

