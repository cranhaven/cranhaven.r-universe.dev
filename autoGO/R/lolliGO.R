#' @title lolliGO
#'
#' @description The function lolliGO.R implement the lollipop plot of the first 20 enrichment terms.
#' @description For each enrichment result table a lollipop plot is produced. Results are stored in the "enrichment_plots" subfolder for each comparison.
#' @param enrich_tables Dataframe containing the enrichment results or a path to your .tsv file containing the enrichment results. Columns 'Term' and 'Adjusted.P.Value' are required.
#' @param title Default to NULL, only specify if from_autoGO is FALSE. When enrich_tables is not from autoGO and thus from read_enrich_tables, the user can specify title and subtitle of the plot as 2-element character vector, for example: c("This is the title", "this is the subtitle")
#' @param outfolder Default to NULL, only specify if from_autoGO is FALSE. The name to assign to the folder for output saving.
#' @param outfile Default to "lolliGO.png", is ignored if from_autoGO is TRUE. The name of the lolli filename.
#' @param from_autoGO Default is TRUE, set to FALSE if the enrichment tables you want to use are not from a differential expression analysis.
#' @return No return value. Files will be produced as part of normal execution.
#' @examples
#' \dontrun{
#' lolliGO(
#'   enrich_tables = enrich_tables,
#'   title = NULL,
#'   outfolder = NULL,
#'   outfile = NULL,
#'   from_autoGO = TRUE
#' )
#' }
#' @export


lolliGO <- function(enrich_tables,
                    title = NULL,
                    outfolder = NULL,
                    outfile = "lolliGO.png",
                    from_autoGO = TRUE) {
  if (!is.data.frame(enrich_tables) && is.list(enrich_tables)) {
    # a list of data frames, with each containing an enrich table.
    # this is the preferred option and the name of the list
    # is used to derive the output path.

    if (from_autoGO && (!is.null(title) || !is.null(outfolder))) {
      stop("when providing a list of dataframes generated from autoGO pipeline, title as well as outfolder and outfile will be derived from the path and any user-supplied values, will be ignored.")
    } else if (!from_autoGO && (is.null(title) || is.null(outfolder) || is.null(outfile))) {
      stop("when providing a list of dataframes not generated from autoGO pipeline, title as well as outfolder and outfile must be specified")
    }

    invisible(lapply(
      names(enrich_tables),
      function(df_metadata) {
        # for each data frame of the list, run lolliGO separately.
        # the results will be put in separated directories according
        # to the `names` as stored in the list

        if (from_autoGO) {
          # example of a path:
          # "./results/MCF7.3D.2p_vs_MCF7.3D.SM/filtered_DE_thFC0_thPval0.05/down_genes/enrichment_tables/KEGG_2021_Human"
          # PS: don't hate us :'(

          my_comparison <- basename(dirname(dirname(dirname(dirname(df_metadata)))))
          db <- basename(df_metadata)
          outfolder <- file.path(dirname(dirname(df_metadata)), "enrichment_plots")

          # the path must include information about up/down regulated genes
          # example of a title:
          # GO Biological Process 2021 for Up Regulated Genes (db and gene set)
          # H460 2D vs H460 3D (comparison)
          if (grepl("/down_genes", df_metadata)) {
            title <- paste0(gsub("_", " ", db), " for Down Regulated Genes")
            subtitle <- ifelse(is.na(gsub("_", " ", my_comparison)), "", gsub("_", " ", my_comparison))
          } else if (grepl("/up_genes", df_metadata)) {
            title <- paste0(gsub("_", " ", db), " for Up Regulated Genes")
            subtitle <- ifelse(is.na(gsub("_", " ", my_comparison)), "", gsub("_", " ", my_comparison))
          } else if (grepl("/up_down_genes", df_metadata)) {
            title <- paste0(gsub("_", " ", db), " for all DE Genes")
            subtitle <- ifelse(is.na(gsub("_", " ", my_comparison)), "", gsub("_", " ", my_comparison))
          } else {
            title <- ifelse(is.na(gsub("_", " ", db)), "", gsub("_", " ", db))
            subtitle <- ifelse(is.na(gsub("_", " ", my_comparison)), "", gsub("_", " ", my_comparison))
          }

          title <- c(title, subtitle)

          outfile <- paste0("lolliGO_", db, ".png")
        }

        enrich_table <- enrich_tables[[df_metadata]]
        do_lolliGO(
          enrich_table = enrich_table,
          title = title,
          outfolder = outfolder,
          outfile = outfile
        )
      }
    ))
  } else {
    if (!is.data.frame(enrich_tables) && !is.character(enrich_tables)) {
      stop("enrich_tables can only be a list of dataframes or a dataframe or a character vector representing a file path")
    } else if (from_autoGO) {
      stop("can only be from_autoGO if enrich_tables are produced by the read_enrich_tables function")
    } else if ((is.null(title) || is.null(outfolder) || is.null(outfile))) {
      stop("when providing a single dataframe not generated from the autoGO pipeline, title as well as outfolder and outfile must be specified")
    }

    do_lolliGO(
      enrich_table = enrich_tables,
      title = title,
      outfolder = outfolder,
      outfile = outfile
    )
  }
}

do_lolliGO <- function(enrich_table, title, outfolder, outfile) {
  if (is.character(enrich_table) &&
    file.exists(enrich_table) &&
    tools::file_ext(enrich_table) == "tsv") {
    # load file and overwrite variable

    enrich_table <- read_delim(enrich_table, delim = "\t", col_types = cols())
  }

  enrich_table <- enrich_table %>%
    dplyr::arrange(.data$Adjusted.P.value, .data$Term) %>%
    dplyr::slice(1:20) %>%
    tidyr::extract(.data$Overlap, into = c("gene_counts", "gene_total"), regex = "([0-9]+)\\/([0-9]+)") %>%
    type_convert(col_types = cols(gene_counts = col_double(), gene_total = col_double())) %>%
    dplyr::mutate(
      Term = gsub("\\(GO.*", "", .data$Term),
      `-log10(Adjusted.P.value)` = -log10(.data$`Adjusted.P.value`),
      percent = round(.data$gene_counts / .data$gene_total, digits = 2),
      percent = ifelse(.data$percent > 0.4, 0.4, .data$percent)
    )

  breaks <- round(seq(min(enrich_table$gene_counts), max(enrich_table$gene_counts), length.out = 6))

  ggplot(enrich_table, aes(x = .data$`-log10(Adjusted.P.value)`, reorder(.data$Term, .data$`Adjusted.P.value`))) +
    ggtitle(label = title[1], subtitle = title[2]) +
    geom_segment(aes(xend = 0, yend = .data$Term)) +
    geom_point(aes(color = .data$percent, size = .data$gene_counts)) +
    scale_color_viridis_c(guide = guide_colorbar(reverse = TRUE), option = "plasma", breaks = seq(0, 0.4, 0.1), limits = c(0, 0.4), labels = c("0 %", "10 %", "20 %", "30 %", "> 40 %")) +
    scale_size_continuous(range = c(5, 12), breaks = breaks) +
    theme_minimal() +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
    labs(x = "-log10(adjp_val)", y = "") +
    geom_vline(xintercept = -log10(0.05), size = 1, colour = "#e09696", linetype = "longdash") +
    theme(
      title = element_text(size = 23), plot.background = element_rect(fill = "#ffffff"),
      axis.text = element_text(size = 18), axis.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0))
    ) +
    guides(
      colour = guide_colourbar(title = "Percentage", order = 1, title.position = "top", title.theme = element_text(size = 15), label.vjust = 0.5, label.theme = element_text(size = 12), ticks.colour = "black"),
      size = guide_legend(title = "Counts", order = 2, title.position = "top", title.theme = element_text(size = 15), reverse = T, label.theme = element_text(size = 12))
    )

  if (!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

  ggsave(filename = file.path(outfolder, outfile), plot = last_plot(), width = unit(20, "cm"), height = unit(10, "cm"))
}
