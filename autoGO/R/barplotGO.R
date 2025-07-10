#' @title barplotGO
#'
#' @description The function barplotGO.R implement the barplot of the first 15 enrichment terms.
#' @description For each enrichment result table a barplot is produced. Results are stored in the "enrichment_plots" subfolder for each comparison.
#' @param enrich_tables Dataframe containing the enrichment results or a path to your .tsv file containing the enrichment results. Columns 'Term' and 'Adjusted.P.Value' are required.
#' @param title Default to NULL, only specify if from_autoGO is FALSE. When enrich_tables is not from autoGO and thus from read_enrich_tables, the user can specify title and subtitle of the plot as 2-element character vector, for example: c("This is the title", "this is the subtitle")
#' @param outfolder Default to NULL, only specify if from_autoGO is FALSE. The name to assign to the folder for output saving.
#' @param outfile Default to "barplotGO.png", is ignored if from_autoGO is TRUE. The name of the barplot filename.
#' @param from_autoGO Default is TRUE, set to FALSE if the enrichment tables you want to use are not from a differential expression analysis.
#' @return No return value. Files will be produced as part of normal execution.
#' @examples
#' \dontrun{
#' barplotGO(
#'   enrich_tables = enrich_tables,
#'   title = NULL,
#'   outfolder = NULL,
#'   outfile = NULL,
#'   from_autoGO = TRUE
#' )
#' }
#' @export


barplotGO <- function(enrich_tables,
                      title = NULL,
                      outfolder = NULL,
                      outfile = "barplotGO.png",
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
        # for each data frame of the list, run barplot separately.
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

          outfile <- paste0("barplotGO_", db, ".png")
        }

        enrich_table <- enrich_tables[[df_metadata]]
        do_barplotGO(
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

    do_barplotGO(
      enrich_table = enrich_tables,
      title = title,
      outfolder = outfolder,
      outfile = outfile
    )
  }
}

do_barplotGO <- function(enrich_table, title, outfolder, outfile) {
  if (is.character(enrich_table) &&
    file.exists(enrich_table) &&
    tools::file_ext(enrich_table) == "tsv") {
    # load file and overwrite variable

    enrich_table <- read_delim(enrich_table, delim = "\t", col_types = cols())
  }

  enrich_table <- enrich_table %>%
    dplyr::arrange(.data$Adjusted.P.value, .data$Term) %>%
    dplyr::slice(1:15) %>%
    dplyr::mutate(
      Term = gsub("\\(GO.*", "", .data$Term),
      `-log10(Adjusted.P.value)` = -log10(.data$`Adjusted.P.value`)
    )

  ggplot(data = enrich_table, aes(x = reorder(.data$Term, .data$`-log10(Adjusted.P.value)`), y = .data$`-log10(Adjusted.P.value)`)) +
    geom_bar(stat = "identity", fill = "#91bbdb", width = 0.5) +
    theme_minimal() +
    ggtitle(title[1], subtitle = title[2]) +
    geom_hline(yintercept = -log10(0.05), size = 1, colour = "#e09696", linetype = "longdash") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(y = "-log10(adjp_val)", x = "") +
    theme(
      title = element_text(size = 23, ), plot.background = element_rect(fill = "#ffffff"),
      axis.text = element_text(size = 18), axis.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0))
    )

  if (!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

  ggsave(filename = file.path(outfolder, outfile), plot = last_plot(), width = unit(25, "cm"), height = unit(10, "cm"))
}
