## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(mLLMCelltype)

## -----------------------------------------------------------------------------
# # Set API keys as environment variables
# Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-api-key")  # For Claude models
# Sys.setenv(OPENAI_API_KEY = "your-openai-api-key")        # For GPT models
# Sys.setenv(GEMINI_API_KEY = "your-gemini-api-key")        # For Gemini models
# Sys.setenv(OPENROUTER_API_KEY = "your-openrouter-api-key") # For OpenRouter models

## -----------------------------------------------------------------------------
# results <- annotate_cell_types(
#   input = markers,
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = "your-anthropic-api-key",  # Direct API key
#   top_gene_count = 10
# )

## -----------------------------------------------------------------------------
# # Example marker data frame
# markers_df <- data.frame(
#   cluster = c(0, 0, 0, 1, 1, 1),
#   gene = c("CD3D", "CD3E", "CD2", "CD14", "LYZ", "CST3"),
#   avg_log2FC = c(2.5, 2.3, 2.1, 3.1, 2.8, 2.5),
#   p_val_adj = c(0.001, 0.001, 0.002, 0.0001, 0.0002, 0.0005)
# )

## -----------------------------------------------------------------------------
# # Assuming you have a Seurat object named 'seurat_obj'
# library(Seurat)
# all_markers <- FindAllMarkers(seurat_obj, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)

## -----------------------------------------------------------------------------
# # Path to your CSV file
# markers_file <- "path/to/markers.csv"

## -----------------------------------------------------------------------------
# # Example marker list
# markers_list <- list(
#   "0" = c("CD3D", "CD3E", "CD2", "IL7R", "LTB"),
#   "1" = c("CD14", "LYZ", "CST3", "MS4A7", "FCGR3A")
# )

## -----------------------------------------------------------------------------
# # Example marker data
# markers <- data.frame(
#   cluster = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
#   gene = c("CD3D", "CD3E", "CD2", "IL7R", "LTB", "CD14", "LYZ", "CST3", "MS4A7", "FCGR3A"),
#   avg_log2FC = c(2.5, 2.3, 2.1, 1.8, 1.7, 3.1, 2.8, 2.5, 2.2, 2.0),
#   p_val_adj = c(0.001, 0.001, 0.002, 0.003, 0.005, 0.0001, 0.0002, 0.0005, 0.001, 0.002)
# )
# 
# # Run annotation with a single model
# results <- annotate_cell_types(
#   input = markers,
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#   top_gene_count = 10,
#   debug = FALSE  # Set to TRUE for more detailed output
# )
# 
# # Print results
# print(results)

## -----------------------------------------------------------------------------
# # Define models to use
# models <- c(
#   "claude-sonnet-4-6",  # Anthropic
#   "gpt-5.5",                      # OpenAI
#   "gemini-3.1-pro-preview"               # Google
# )
# 
# # API keys for different providers
# api_keys <- list(
#   anthropic = Sys.getenv("ANTHROPIC_API_KEY"),
#   openai = Sys.getenv("OPENAI_API_KEY"),
#   gemini = Sys.getenv("GEMINI_API_KEY")
# )
# 
# # Run annotation with multiple models
# results <- list()
# for (model in models) {
#   provider <- get_provider(model)
#   api_key <- api_keys[[provider]]
# 
#   results[[model]] <- annotate_cell_types(
#     input = markers,
#     tissue_name = "human PBMC",
#     model = model,
#     api_key = api_key,
#     top_gene_count = 10
#   )
# }
# 
# # Create consensus
# consensus_results <- interactive_consensus_annotation(
#   input = markers,
#   tissue_name = "human PBMC",
#   models = models,  # Use all the models defined above
#   api_keys = api_keys,
#   controversy_threshold = 0.7,
#   entropy_threshold = 1.0,
#   consensus_check_model = "claude-sonnet-4-6"
# )

## -----------------------------------------------------------------------------
# # Assuming you have a Seurat object named 'seurat_obj' and consensus results
# library(Seurat)
# 
# # Add consensus annotations to Seurat object
# seurat_obj$cell_type_consensus <- plyr::mapvalues(
#   x = as.character(Idents(seurat_obj)),
#   from = names(consensus_results$final_annotations),
#   to = consensus_results$final_annotations
# )
# 
# # Extract consensus metrics from the consensus results
# # Note: These metrics are available in the consensus_results$initial_results$consensus_results
# consensus_metrics <- lapply(names(consensus_results$initial_results$consensus_results), function(cluster_id) {
#   metrics <- consensus_results$initial_results$consensus_results[[cluster_id]]
#   return(list(
#     cluster = cluster_id,
#     consensus_proportion = metrics$consensus_proportion,
#     entropy = metrics$entropy
#   ))
# })
# 
# # Convert to data frame for easier handling
# metrics_df <- do.call(rbind, lapply(consensus_metrics, data.frame))
# 
# # Add consensus proportion to Seurat object
# seurat_obj$consensus_proportion <- plyr::mapvalues(
#   x = as.character(Idents(seurat_obj)),
#   from = metrics_df$cluster,
#   to = metrics_df$consensus_proportion
# )
# 
# # Add entropy to Seurat object
# seurat_obj$entropy <- plyr::mapvalues(
#   x = as.character(Idents(seurat_obj)),
#   from = metrics_df$cluster,
#   to = metrics_df$entropy
# )

## -----------------------------------------------------------------------------
# # Plot UMAP with cell type annotations
# DimPlot(seurat_obj, group.by = "cell_type_consensus", label = TRUE, repel = TRUE) +
#   ggtitle("Cell Type Annotations") +
#   theme(plot.title = element_text(hjust = 0.5))

## -----------------------------------------------------------------------------
# # Set OpenRouter API key
# Sys.setenv(OPENROUTER_API_KEY = "your-openrouter-api-key")
# 
# # Use a free model
# free_results <- annotate_cell_types(
#   input = markers,
#   tissue_name = "human PBMC",
#   model = "meta-llama/llama-4-maverick:free",  # Note the :free suffix
#   api_key = Sys.getenv("OPENROUTER_API_KEY"),
#   top_gene_count = 10
# )
# 
# # Print results
# print(free_results)

