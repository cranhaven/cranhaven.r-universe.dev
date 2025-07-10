#' @title ssGSEA
#'
#' @description Single-sample Gene Set Enrichment Analysis. This kind of analysis is recommended when there are too few samples. The function will generate an enrichment score for each sample and associated visualizations.
#' @description The function ssGSEA is implemented on top of GSVA package. It will produce an heatmap and a violin plot. If chosen, it will produce also the enrichment score table. All the necessary data are already provided with the autoGO package (as the MSigDB gene sets). It is possible to choose whether to perform ssGSEA with all the MSigDB (database) or with a sub-group.
#' @param norm_data Path of the normalized matrix. By default it is the "deseq_vst_data.txt" computed with the function "deseq_analysis()", but other normalized counts matrix (like TPMs) can be passed to the function. Requirements: first column gene names.
#' @param gene_id_type One of c("gene_symbol", "entrez_gene", "ensembl_gene"). The notation for gene names in the matrix.
#' @param write_enrich_tables Default = FALSE. Set to TRUE to save the matrix with enrichment scores.
#' @param group A matrix for the annotation on the heatmap and for grouping in the statistical analysis. Can be the path to an annotation matrix or dataframe.
#' @param outfolder The name to assign to the folder for output saving. (Default = "ssgsea/"). NOTE: please add "/" at the end.
#' @param full_names Default = FALSE, Terms full names are codified for visualization purposes. Set to TRUE to plot full names instead.
#' @param tpm_norm (Default = FALSE). Set to TRUE to perform a TPM normalization on counts matrix before the analysis.
#' @param categories Default = NULL, all the gene sets are considered. The user can specify one or more categories, like c("C1", "C2", "C3").
#' @export

# FIXME: decide what to do with the "norm_data" - by default using a deseq-generated file might not make sense. Make a decision, and update both code & param documentation above


ssgsea_wrapper <- function(norm_data = "results/deseq_vst_data.txt",
                           gene_id_type = c("gene_symbol", "entrez_gene", "ensembl_gene"),
                           write_enrich_tables = FALSE,
                           group = NULL,
                           outfolder = "./ssgsea",
                           full_names = FALSE,
                           tpm_norm = FALSE,
                           categories = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "H")) {
  if (grepl(".tsv", norm_data)[1]) {
    norm_data <- read_tsv(norm_data, col_types = cols())
  } else if (grepl(".csv", norm_data)[1]) {
    norm_data <- read_csv(norm_data, col_types = cols())
  } else if (grepl(".rds", norm_data)[1]) {
    norm_data <- readRDS(norm_data)
  } else if (grepl(".txt", norm_data)[1]) {
    norm_data <- read_delim(norm_data, delim = "\t", col_types = cols())
  } else if (is.data.frame(norm_data)) {
    norm_data <- norm_data
  } else {
    stop("Provide a file .tsv, .csv, .rds or .txt tab-separated")
  }

  norm_data <- as.data.frame(norm_data)
  rownames(norm_data) <- norm_data[, 1]
  norm_data[, 1] <- NULL

  if (tpm_norm) {
    conversion_ensembl_unique <- conversion_ensembl[!duplicated(conversion_ensembl$external_gene_name), ]

    common_gene_names <- intersect(conversion_ensembl_unique$external_gene_name, gene_length$external_gene_name)

    gene_length <- gene_length %>%
      dplyr::filter(.data$external_gene_name %in% common_gene_names)

    gene_length$ensembl_gene_id <- conversion_ensembl_unique %>%
      dplyr::filter(.data$external_gene_name %in% common_gene_names) %>%
      dplyr::select(.data$ensembl_gene_id) %>%
      dplyr::pull()

    gene_length <- gene_length %>%
      dplyr::filter(.data$ensembl_gene_id %in% rownames(norm_data))

    norm_data <- norm_data %>%
      dplyr::filter(rownames(norm_data) %in% gene_length$ensembl_gene_id)

    gene_lengths <- gene_length %>%
      dplyr::select(width) %>%
      dplyr::pull()

    tpm <- function(counts, len) {
      x <- counts / len
      return(t(t(x) * 1e6 / colSums(x)))
    }
    norm_data <- tpm(norm_data, gene_lengths) %>% as.data.frame()
  }

  if (length(gene_id_type) > 1 || !(gene_id_type %in% c("gene_symbol", "entrez_gene", "ensembl_gene"))) {
    stop("gene_id_type parameter should be only one of 'gene_symbol', 'entrez_gene' or 'ensembl_gene'.")
  }

  if (!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

  for (gs in categories) {
    result_name <- paste0("ssgsea_", gs)

    msigdbr_df <- msigdbr::msigdbr(species = "Homo sapiens", category = gs)
    gset_list <- split(x = msigdbr_df[gene_id_type] %>% pull(), f = msigdbr_df$gs_name)

    matrix_data <- as.matrix(norm_data)
    gsva_data <- ssgseaParam(matrix_data, gset_list)

    result <- gsva(gsva_data, verbose = FALSE)

    if (write_enrich_tables) {
      if (!dir.exists(file.path(outfolder, "tables"))) {
        dir.create(file.path(outfolder, "tables"), recursive = T)
      }

      write.table(result, file.path(outfolder, "tables", paste0(result_name, "_EnrichmentScore.tsv")), sep = "\t", quote = F, col.names = NA, row.names = T)
    }

    if (!is.null(group)) {
      if (!is.data.frame(group)[1] & grepl(".txt", group)[1]) {
        group <- read_delim(group, delim = "\t", col_types = cols())
      } else if (!is.data.frame(group)) {
        stop("Provide a data.frame or a .txt file for group variable.")
      }

      group_class <- split.data.frame(group, group[[2]])

      group1 <- result[, group_class[[1]][[1]]]
      group2 <- result[, group_class[[2]][[1]]]

      table <- data.frame()
      for (x in rownames(result)) {
        w_res <- wilcox.test(as.numeric(group1[x, ]), as.numeric(group2[x, ]))
        t_res <- t.test(as.numeric(group1[x, ]), as.numeric(group2[x, ]))
        tb <- data.frame(
          GeneSet = x, wilcoxon = w_res$p.value, t_test = t_res$p.value,
          mean_group1 = t_res$estimate[["mean of x"]],
          mean_group2 = t_res$estimate[["mean of y"]]
        )
        table <- rbind.data.frame(table, tb)
      }
      table$w_adj <- p.adjust(table$wilcoxon, method = "BH")
      table$t_adj <- p.adjust(table$t_test, method = "BH")

      colnames(table)[grep("group1", colnames(table))] <- paste0("mean_", names(group_class)[1])
      colnames(table)[grep("group2", colnames(table))] <- paste0("mean_", names(group_class)[2])

      table <- table %>% dplyr::arrange(.data$w_adj)

      write.table(table, file.path(outfolder, "tables", paste0(
        gsub("ssgsea_", "", gs), "_stats_",
        names(group_class)[1], "_vs_", names(group_class)[2], ".txt"
      )),
      quote = F, sep = "\t", row.names = F
      )
      sign <- table$GeneSet[(table$wilcoxon <= 0.05 | table$t_test <= 0.05)]
      if (length(sign) >= 20) {
        # sign <- table$GeneSet[(table$wilcoxon <= 0.01 | table$t_test <= 0.01)]
        sign <- table$GeneSet[1:20]
      }

      group1 <- group1 %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "geneset") %>%
        tidyr::pivot_longer(cols = -.data$geneset, names_to = "pat") %>%
        dplyr::mutate(group = names(group_class)[1])

      group2 <- group2 %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "geneset") %>%
        tidyr::pivot_longer(cols = -.data$geneset, names_to = "pat") %>%
        dplyr::mutate(group = names(group_class)[2])

      df <- rbind.data.frame(group1, group2) %>%
        dplyr::filter(.data$geneset %in% sign)

      if (!dir.exists(file.path(outfolder, "plots"))) dir.create(file.path(outfolder, "plots"), recursive = T)

      if (!full_names) {
        cod <- data.frame(Term = unique(df$geneset), Term_ID = paste0("TERM_", 1:length(unique(df$geneset))))
        write.table(cod, file.path(outfolder, "plots", paste0("codified_term_", gsub("ssgsea_", "", gs), ".txt")), sep = "\t", quote = F, row.names = F)
        df <- df %>%
          dplyr::left_join(cod, by = c("geneset" = "Term")) %>%
          dplyr::mutate(Term_ID = factor(.data$Term_ID, levels = paste0("TERM_", 1:length(unique(df$geneset)))))
      } else if (full_names) {
        df <- df %>%
          dplyr::mutate(Term_ID = .data$geneset) %>%
          dplyr::select(-.data$geneset)
      }

      mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(unique(df$Term_ID)))
      p <- ggplot(data = df, aes(x = .data$Term_ID, y = .data$value)) +
        geom_violin(aes(fill = .data$Term_ID), show.legend = F, trim = F, scale = "width") +
        stat_summary(fun = median, geom = "point", size = 1, color = "black", shape = 18) +
        theme_bw() +
        labs(x = "", y = "Enrichment Score", title = paste0("Distribution of significative genesets for ", toupper(gsub("ssgsea_", "", gs)))) +
        theme(
          legend.position = "top", legend.margin = margin(0, 0, 0, 0, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_fill_manual(values = mycolors)
      png(file.path(outfolder, "plots", paste0("distrib_", gsub("ssgsea_", "", gs), ".png")), width = 4000, height = 2500, res = 300)
      print(p)
      dev.off()

      # heatmap
      group <- group %>% dplyr::arrange(1)

      if (!full_names) {
        filt <- result[sign, group[[1]]] %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "geneset") %>%
          dplyr::left_join(cod, by = c("geneset" = "Term")) %>%
          dplyr::mutate(Term_ID = factor(.data$Term_ID, levels = paste0("TERM_", 1:length(unique(df$geneset))))) %>%
          textshape::column_to_rownames(loc = "Term_ID") %>%
          dplyr::select(-.data$geneset)
      } else if (full_names) {
        filt <- result[sign, group[[1]]] %>%
          as.data.frame()
      }

      z_filt <- t(scale(t(filt)))
      head(z_filt)

      Var <- setNames(c("#446455", "#FDD262"), unique(group[[2]]))

      ha <- HeatmapAnnotation(
        df = group[[2]], show_annotation_name = F,
        annotation_legend_param = list(df = list(title = " ")),
        col = list(df = Var)
      )
      ht <- Heatmap(z_filt,
        top_annotation = ha,
        column_title = paste0("Distribution of significative genesets for ", gsub("ssgsea_", "", gs)),
        col = RColorBrewer::brewer.pal(9, "PuRd"),
        column_names_rot = 45, heatmap_legend_param = list(title_position = "leftcenter-rot"),
        name = "zscore(ES)", show_row_dend = F, rect_gp = gpar(col = "white", lwd = 0.5)
      )

      png(file.path(outfolder, "plots", paste0("heatmap_", gsub("ssgsea_", "", gs), ".png")), width = 3000, height = 3000, res = 300)
      draw(ht, heatmap_legend_side = "left", annotation_legend_side = "left", merge_legends = T)
      dev.off()
    } else {
      print("Plots will be generated with the top 15 TERMS ordered by mean for sample.")
      ordered <- apply(result, 1, mean) %>% as.data.frame()
      colnames(ordered) <- "Mean"
      ordered <- ordered %>% dplyr::arrange(dplyr::desc(.data$Mean))

      result <- result %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "geneset") %>%
        dplyr::filter(.data$geneset %in% rownames(ordered)[1:15])

      if (write_enrich_tables) {
        if (!dir.exists(file.path(outfolder, "tables"))) {
          dir.create(file.path(outfolder, "tables"), recursive = T)
        }

        write.table(result, file.path(outfolder, "tables", paste0(result_name, "_EnrichmentScore_filtered.tsv")), sep = "\t", quote = F, row.names = F)
      }

      if (!dir.exists(file.path(outfolder, "plots"))) dir.create(file.path(outfolder, "plots"), recursive = T)

      if (!full_names) {
        cod <- data.frame(Term = unique(result$geneset), Term_ID = paste0("TERM_", 1:length(unique(result$geneset))))
        write.table(cod, file.path(outfolder, "plots", paste0("codified_term_", gsub("ssgsea_", "", gs), ".txt")), sep = "\t", quote = F, row.names = F)
        df <- result %>%
          dplyr::left_join(cod, by = c("geneset" = "Term")) %>%
          dplyr::mutate(Term_ID = factor(.data$Term_ID, levels = paste0("TERM_", 1:length(unique(result$geneset))))) %>%
          dplyr::select(-.data$geneset) %>%
          tidyr::pivot_longer(cols = -.data$Term_ID)
      } else if (full_names) {
        df <- result %>%
          dplyr::mutate(Term_ID = .data$geneset) %>%
          dplyr::select(-.data$geneset) %>%
          tidyr::pivot_longer(cols = -.data$Term_ID)
      }

      mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(unique(df$Term_ID)))
      p <- ggplot(data = df, aes(x = .data$Term_ID, y = .data$value)) +
        geom_violin(aes(fill = .data$Term_ID), show.legend = F, trim = F, scale = "width") +
        stat_summary(fun = median, geom = "point", size = 1, color = "black", shape = 18) +
        theme_bw() +
        labs(x = "", y = "Enrichment Score", title = paste0("Distribution of significative genesets for ", toupper(gsub("ssgsea_", "", gs)))) +
        theme(
          legend.position = "top", legend.margin = margin(0, 0, 0, 0, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_fill_manual(values = mycolors)
      png(file.path(outfolder, "plots", paste0("distrib_", gsub("ssgsea_", "", gs), ".png")), width = 4000, height = 2500, res = 300)
      print(p)
      dev.off()

      if (!full_names) {
        data <- result %>%
          as.data.frame() %>%
          dplyr::left_join(cod, by = c("geneset" = "Term")) %>%
          dplyr::mutate(Term_ID = factor(.data$Term_ID, levels = paste0("TERM_", 1:length(unique(result$geneset))))) %>%
          textshape::column_to_rownames(loc = "Term_ID") %>%
          dplyr::select(-.data$geneset)
      } else if (full_names) {
        data <- result %>%
          as.data.frame() %>%
          textshape::column_to_rownames(loc = "geneset")
      }

      z_data <- t(scale(t(data)))

      if (grepl("HALLMARK_", rownames(z_data)[1])) {
        rownames(z_data) <- gsub("HALLMARK_", "", rownames(z_data), ignore.case = T)
      }

      fontsize <- 10

      ht <- Heatmap(z_data,
        column_title = paste0("Distribution of significative genesets for ", gsub("ssgsea_", "", gs)),
        col = RColorBrewer::brewer.pal(9, "PuRd"),
        column_names_rot = 45, heatmap_legend_param = list(title_position = "leftcenter-rot"),
        name = "zscore(ES)", show_row_dend = F, rect_gp = gpar(col = "white", lwd = 0.5),
        row_names_max_width = max_text_width(rownames(z_data), gp = gpar(fontsize = fontsize)),
        row_names_gp = gpar(fontsize = fontsize), column_names_gp = gpar(fontsize = fontsize)
      )

      png(file.path(outfolder, "plots", paste0("heatmap_", gsub("ssgsea_", "", gs), ".png")), width = 3800, height = 3000, res = 300)
      draw(ht, heatmap_legend_side = "left")
      dev.off()
    }
  }
}
