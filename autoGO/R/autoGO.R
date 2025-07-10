#' @title autoGO
#'
#' @description Perform enrichment analysis on all the desired gene lists. This function take advantage of the 'enrichR' package.
#' @param list_of_genes it can be a list of dataframes containing gene names (i.e. from the output of read_gene_lists()), a single dataframe or character vector of gene names, or a path to a .txt file containing one gene name per row.
#' @param dbs Databases over which the enrichment will be performed, based on the enrichR libraries. Default are GO_Molecular_Function_2021, GO_Cellular_Component_2021, GO_Biological_Process_2021, KEGG_2021_Human. Run choose_database() to see all the possible databases.
#' @param my_comparison Name of the comparison (or the analysis) the user would like to inspect. Ignored if list_of_genes is a list. taken from the path.
#' @param ensembl (Default = FALSE). Set to TRUE if the provided gene list contains Ensembl IDs. A conversion to HGNC will be performed.
#' @param excel (Default = FALSE). Set to TRUE if you want to save output tables in .xlsx format.
#' @param outfolder Default to NULL. The name to assign to the folder in which outputs are saved. Ignored if list_of_genes is a list, taken from the path.
#' @return No return value. Files will be produced as part of normal execution.
#' @examples
#' \dontrun{
#' autoGO(
#'   list_of_genes = gene_lists,
#'   dbs = c("GO_Molecular_Function_2021", "GO_Biological_Process_2021", "KEGG_2021_Human"),
#'   my_comparison = NULL,
#'   ensembl = F,
#'   excel = F,
#'   outfolder = NULL
#' )
#' }
#' @export


autoGO <- function(list_of_genes,
                   dbs = c(
                     "GO_Molecular_Function_2021",
                     "GO_Biological_Process_2021",
                     "KEGG_2021_Human"
                   ),
                   my_comparison = NULL,
                   ensembl = FALSE,
                   excel = FALSE,
                   outfolder = NULL) {
  if (!is.data.frame(list_of_genes) && is.list(list_of_genes)) {
    # a list of data frames, with each containing gene names.
    # this is the preferred option and the name of the list
    # is used to derive the output path.

    if (!is.null(my_comparison) || !is.null(outfolder)) {
      warning("when providing a list of dataframes from read_gene_lists, my_comparison as well as outfolder will be derived from the path and any user-supplied values, will be ignored.")
    }

    invisible(lapply(
      names(list_of_genes),
      function(df_metadata) {
        # for each data frame of the list, run autoGO separately.
        # the results will be put in separated directories according
        # to the `names` as stored in the list
        set_of_genes <- list_of_genes[[df_metadata]]
        do_autogo(
          list_of_genes = set_of_genes,
          dbs = dbs,
          my_comparison = "",
          ensembl = ensembl,
          excel = excel,
          outfolder = dirname(df_metadata)
        )
      }
    ))
  } else {
    invisible(do_autogo(
      list_of_genes = list_of_genes,
      dbs = dbs,
      my_comparison = my_comparison,
      ensembl = ensembl,
      excel = excel,
      outfolder = outfolder
    ))
  }
}

do_autogo <- function(list_of_genes,
                      dbs,
                      my_comparison,
                      ensembl,
                      excel,
                      outfolder) {
  if (is.data.frame(list_of_genes)) {
    if (is.null(outfolder)) {
      stop("The output folder must be specified, unless the output of read_gene_lists is used")
    }
    # a data frame of gene names
    list_of_genes <- list_of_genes %>% dplyr::pull()
  } else if (is.character(list_of_genes) & !grepl(".txt", list_of_genes)[1]) {
    if (is.null(outfolder)) {
      stop("The output folder must be specified, unless the output of read_gene_lists is used")
    }
    # a character vector of gene names
    list_of_genes <- list_of_genes
  } else if (grepl(".txt", list_of_genes)) {
    if (is.null(outfolder)) {
      stop("The output folder must be specified, unless the output of read_gene_lists is used")
    }
    # a file, tab-delimited, where each row contains a gene name
    list_of_genes <- read_delim(list_of_genes,
      delim = "\t",
      col_types = cols(),
      col_names = FALSE
    ) %>% dplyr::pull()
  } else {
    stop("list_of_genes parameter not valid: should be either a list of data.frames, a data.frame, a character vector or a .txt file")
  }

  if (ensembl) {
    all_genes_conversion <- conversion_ensembl
    list_of_genes <- as.data.frame(list_of_genes) %>%
      dplyr::inner_join(all_genes_conversion, by = c("list_of_genes" = "ensembl_gene_id")) %>%
      dplyr::pull()
  }

  enriched <- enrichr(list_of_genes, dbs)
  lapply(seq_along(enriched), function(i) {
    enriched[[i]] <- enriched[[i]][!is.na(enriched[[i]]$Term), ]
    enriched[[i]]$`-log10(Adjusted.P.value)` <- -log10(enriched[[i]]$Adjusted.P.value)
    enriched[[i]] <- enriched[[i]][order(enriched[[i]]$Adjusted.P.value), ]
  })

  my_path <- file.path(outfolder, my_comparison, "enrichment_tables")
  if (!dir.exists(my_path)) dir.create(my_path, recursive = T)

  invisible(lapply(seq_along(enriched), function(ind) {
    if (dim(enriched[[ind]])[1] > 0) {
      if (excel) openxlsx::write.xlsx(enriched[[ind]], file = file.path(my_path, paste0(names(enriched)[ind], ".xlsx")), row.names = F)
      write.table(enriched[[ind]], sep = "\t", quote = F, file = file.path(my_path, paste0(names(enriched)[ind], ".tsv")), row.names = F)
    }
  }))
}
