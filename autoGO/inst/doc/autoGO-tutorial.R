## ----setup_package, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = T, error = TRUE, eval = F)

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  library(devtools)
#  install_github("mpallocc/auto-go")
#  library(autoGO)

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  data(counts, groups, comparisons)

## -----------------------------------------------------------------------------
#  deseq_analysis(counts,
#                 groups,
#                 comparisons,
#                 padj_threshold = 0.05,
#                 log2FC_threshold = 0,
#                 pre_filtering = T,
#                 save_excel = F,
#                 outfolder = "./results",
#                 del_csv = ",")

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
groups <- data.frame(sample=c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6"),
                     group=c("CTRL", "CTRL", "TREAT_A", "TREAT_A", "TREAT_B", "TREAT_B"))

comparisons <- data.frame(treatment = c("TREAT_A", "TREAT_B", "TREAT_A"),
                          control = c("CTRL", "CTRL", "TREAT_B"))
knitr::kable(list(groups, comparisons), booktabs = TRUE, valign = 't', caption = "Groups and comparisons example tables")

## -----------------------------------------------------------------------------
#  filtering_DE(padj_threshold = 0.05,
#               log2FC_threshold = 1,
#               outfolder = "./results",
#               save_excel = F)

## -----------------------------------------------------------------------------
#  filename <- "./results/H460.2D_vs_H460.3D.2p/DE_H460.2D_vs_H460.3D.2p_allres.tsv"
#  volcanoplot(DE_results = filename,
#              my_comparison = "H460.2D_vs_H460.3D.2p",
#              log2FC_thresh = 0,
#              padj_thresh = 0.05,
#              highlight_genes = NULL,
#              del_csv = ",",
#              outfolder = "./results")
#  
#  filename <- "./results/H460.2D_vs_H460.3D.2p/DE_H460.2D_vs_H460.3D.2p_allres.tsv"
#  volcanoplot(DE_results = filename,
#              my_comparison = "H460.2D_vs_H460.3D.2p",
#              log2FC_thresh = 1,
#              padj_thresh = 0.05,
#              highlight_genes = NULL,
#              del_csv = ",",
#              outfolder = "./results")

## -----------------------------------------------------------------------------
#  all_path_res <- list.files(path = "results", pattern = "_allres.tsv", recursive = T, full.names = T)
#  res_lists <- lapply(all_path_res, function (x) read_tsv(x, col_types = cols()))
#  names(res_lists) <- gsub("results/|/DE_.*", "", all_path_res)
#  
#  invisible(lapply(names(res_lists), function (i) volcanoplot(res_lists[[i]], my_comparison = i)))

## ---- fig.show='hold', echo=FALSE, out.width = '47%',  eval=TRUE--------------
knitr::include_graphics("volcano1.png")
knitr::include_graphics("volcano2.png")


## -----------------------------------------------------------------------------
#  choose_database(db_search = "KEGG")

## -----------------------------------------------------------------------------
#  gene_lists_path <- "./results"
#  gene_lists <- read_gene_lists(gene_lists_path = gene_lists_path,
#                 log2FC_threshold = 0,
#                 padj_threshold = 0.05,
#                 which_list = "down_genes",
#                 from_autoGO = T,
#                 files_format = NULL)
#  names(gene_lists)

## -----------------------------------------------------------------------------
#  autoGO(list_of_genes = gene_lists,
#        dbs = c("GO_Molecular_Function_2021", "GO_Biological_Process_2021", "KEGG_2021_Human"),
#        my_comparison = NULL,
#        ensembl = F,
#        excel = F,
#        outfolder = NULL)

## -----------------------------------------------------------------------------
#  autoGO(list_of_genes = gene_lists[[1]],
#        dbs = c("GO_Molecular_Function_2021", "GO_Biological_Process_2021", "KEGG_2021_Human"),
#        my_comparison = "my_comparison_2022",
#        ensembl = F,
#        excel = F,
#        outfolder = "./results")

## -----------------------------------------------------------------------------
#  enrich_table_path <- "./results"
#  enrich_tables <- read_enrich_tables(
#                    enrich_table_path = enrich_table_path,
#                    log2FC_threshold = 0,
#                    padj_threshold = 0.05,
#                    which_list = "down_genes",
#                    from_autoGO = T,
#                    files_format = NULL)
#  names(enrich_tables)

## -----------------------------------------------------------------------------
#  barplotGO(enrich_tables = enrich_tables,
#            title = NULL,
#            outfolder = NULL,
#            outfile = NULL,
#            from_autoGO = TRUE)

## ---- echo=FALSE, out.width = '98%',  eval=TRUE-------------------------------
knitr::include_graphics("barplotGO_GO_Molecular_Function_2021.png")

## -----------------------------------------------------------------------------
#  enrich_table <- enrich_tables[[1]]
#  barplotGO(enrich_tables = enrich_table,
#            title = c("Title of my barplot", "and subtitle"),
#            outfolder = "./results/my_comparison_2022/enrichment_plots",
#            outfile = "barplot_myDB.png",
#            from_autoGO = FALSE)

## -----------------------------------------------------------------------------
#  lolliGO(enrich_tables = enrich_tables,
#          title = NULL,
#          outfolder = NULL,
#          outfile = NULL,
#          from_autoGO = TRUE)

## ---- echo=FALSE, out.width = '98%',  eval=TRUE-------------------------------
knitr::include_graphics("lolliGO_GO_Molecular_Function_2021.png")

## -----------------------------------------------------------------------------
#  enrich_table <- enrich_tables[[1]]
#  lolliGO(enrich_tables = enrich_table,
#          title = c("Title of my barplot", "and subtitle"),
#          outfolder = "./results/my_comparison_2022/enrichment_plots",
#          outfile = "lolli_myDB.png",
#          from_autoGO = FALSE)

## -----------------------------------------------------------------------------
#  heatmapGO(db = "GO_Biological_Process_2021",
#            outfolder = "./results",
#            log2FC_threshold = 0,
#            padj_threshold = 0.05,
#            min_term_per_row = 3,
#            which_list = "down_genes")

## ---- echo=FALSE, out.width = '80%', fig.align='center', eval=TRUE------------
knitr::include_graphics("down_genes_GO_Biological_Process_2021_min3.png")

## -----------------------------------------------------------------------------
#  dbs <- c("GO_Molecular_Function_2021", "GO_Biological_Process_2021", "KEGG_2021_Human")
#  lapply(dbs, function (i) heatmapGO(db = i, which_list = "down_genes", min_term_per_row = 3))

