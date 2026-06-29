## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(mLLMCelltype)
# 
# results <- annotate_cell_types(
#   input,                # Marker gene data (data frame, list, or file path)
#   tissue_name,          # Tissue name (e.g., "human PBMC", "mouse brain")
#   model,                # LLM model to use
#   api_key = NA,         # API key (if not set in environment, NA returns prompt only)
#   top_gene_count = 10,  # Number of top genes per cluster to use
#   debug = FALSE         # Whether to print debugging information
# )

## ----eval=FALSE---------------------------------------------------------------
# consensus_results <- interactive_consensus_annotation(
#   input,                # Original marker gene data (Seurat FindAllMarkers result or list of genes)
#   tissue_name = NULL,   # Optional tissue name
#   models = c("claude-sonnet-4-6", "gpt-5.5", "gemini-3.1-pro-preview"),  # Models to use
#   api_keys,             # Named list of API keys
#   top_gene_count = 10,  # Number of top genes to use
#   controversy_threshold = 0.7,  # Threshold for identifying controversial clusters
#   entropy_threshold = 1.0,  # Entropy threshold for controversial clusters
#   max_discussion_rounds = 3,  # Maximum discussion rounds
#   consensus_check_model = NULL,  # Model to use for consensus checking (see recommendations below)
#   log_dir = "logs",     # Directory for logs
#   cache_dir = NULL,  # Uses default system cache directory
#   use_cache = TRUE      # Whether to use cache
# )

## -----------------------------------------------------------------------------
# # Load example data
# library(Seurat)
# data("pbmc_small")
# 
# # Find markers
# pbmc_markers <- FindAllMarkers(pbmc_small, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
# 
# # Run annotation with a single model
# results <- annotate_cell_types(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#   top_gene_count = 10
# )
# 
# # Add annotations to Seurat object
# pbmc_small$cell_type_claude <- plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = names(results),
#   to = results
# )
# 
# # Visualize
# DimPlot(pbmc_small, group.by = "cell_type_claude", label = TRUE)

## -----------------------------------------------------------------------------
# # Define multiple models to use
# models <- c(
#   "claude-sonnet-4-6",  # Anthropic
#   "gpt-5.5",                      # OpenAI
#   "gemini-3.1-pro-preview",              # Google
#   "grok-4.3"                       # X.AI
# )
# 
# # API keys for different providers
# api_keys <- list(
#   anthropic = Sys.getenv("ANTHROPIC_API_KEY"),
#   openai = Sys.getenv("OPENAI_API_KEY"),
#   gemini = Sys.getenv("GEMINI_API_KEY"),
#   grok = Sys.getenv("GROK_API_KEY")
# )
# 
# # Run annotation with multiple models
# results <- list()
# for (model in models) {
#   provider <- get_provider(model)
#   api_key <- api_keys[[provider]]
# 
#   results[[model]] <- annotate_cell_types(
#     input = pbmc_markers,
#     tissue_name = "human PBMC",
#     model = model,
#     api_key = api_key,
#     top_gene_count = 10
#   )
# }
# 
# # Create consensus
# consensus_results <- interactive_consensus_annotation(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   models = models,  # Use all the models defined above
#   api_keys = api_keys,
#   controversy_threshold = 0.7,
#   entropy_threshold = 1.0,
#   consensus_check_model = "claude-sonnet-4-6"
# )
# 
# # View consensus results
# # You can access the final annotations with consensus_results$final_annotations
# 
# # Add consensus annotations and metrics to Seurat object
# pbmc_small$cell_type_consensus <- plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = names(consensus_results$final_annotations),
#   to = consensus_results$final_annotations
# )
# 
# # Extract consensus metrics from the consensus results
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
# pbmc_small$consensus_proportion <- plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = metrics_df$cluster,
#   to = metrics_df$consensus_proportion
# )
# 
# # Add entropy to Seurat object
# pbmc_small$shannon_entropy <- plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = metrics_df$cluster,
#   to = metrics_df$entropy
# )

## -----------------------------------------------------------------------------
# # Set OpenRouter API key
# openrouter_api_key <- Sys.getenv("OPENROUTER_API_KEY")
# 
# # Define free OpenRouter models to use
# free_models <- c(
#   "meta-llama/llama-4-maverick:free",                # Meta Llama 4 Maverick (free)
#   "meta-llama/llama-3.3-70b-instruct:free",    # Meta Llama 3.3 70B (free)
#   "deepseek/deepseek-v4-pro:free",             # DeepSeek V4 Pro (free)
#   "meta-llama/llama-3.3-70b-instruct:free"          # Meta Llama 3.3 70B (free)
# )
# 
# # Run annotation with free OpenRouter models
# free_results <- list()
# for (model in free_models) {
#   free_results[[model]] <- annotate_cell_types(
#     input = pbmc_markers,
#     tissue_name = "human PBMC",
#     model = model,  # OpenRouter models are automatically detected by format: 'provider/model-name:free'
#     api_key = openrouter_api_key,
#     top_gene_count = 10
#   )
# }
# 
# # Create consensus with free models
# free_consensus_results <- interactive_consensus_annotation(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   models = free_models,  # Use all the free models defined above
#   api_keys = list("openrouter" = openrouter_api_key),
#   controversy_threshold = 0.7,
#   entropy_threshold = 1.0,
#   consensus_check_model = "meta-llama/llama-4-maverick:free"  # Use a free model for consensus checking
# )
# 
# # View free model consensus results
# # You can access the final annotations with free_consensus_results$final_annotations
# 
# # Add free model consensus annotations to Seurat object
# pbmc_small$free_model_consensus <- plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = names(free_consensus_results$final_annotations),
#   to = free_consensus_results$final_annotations
# )
# 
# # Compare paid vs. free model results
# comparison <- data.frame(
#   cluster = names(consensus_results$final_annotations),
#   paid_models = consensus_results$final_annotations,
#   free_models = free_consensus_results$final_annotations,
#   agreement = consensus_results$final_annotations == free_consensus_results$final_annotations
# )
# print(comparison)

## -----------------------------------------------------------------------------
# # Save markers to CSV
# write.csv(pbmc_markers, "pbmc_markers.csv", row.names = FALSE)
# 
# # Run annotation using the CSV file
# results <- annotate_cell_types(
#   input = "pbmc_markers.csv",
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = Sys.getenv("ANTHROPIC_API_KEY")
# )

## -----------------------------------------------------------------------------
# # Note: The annotate_cell_types function does not have built-in caching.
# # If you need caching, you can implement it separately.
# 
# # Run annotation
# results <- annotate_cell_types(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#   top_gene_count = 10,
#   debug = FALSE
# )
# 
# # If you need custom caching, you can implement it using your own cache manager
# # This is just a conceptual example and not part of the actual package
# # cache_manager <- YourCacheManager$new(cache_dir = "path/to/cache")
# # cache_manager$clear_cache()

## -----------------------------------------------------------------------------
# # Example of using a free model via OpenRouter
# # First, set your OpenRouter API key
# Sys.setenv(OPENROUTER_API_KEY = "your-openrouter-api-key")
# 
# # Then use a free model with the :free suffix
# free_model_results <- annotate_cell_types(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   model = "meta-llama/llama-4-maverick:free",  # Note the :free suffix
#   api_key = Sys.getenv("OPENROUTER_API_KEY")
#   # No need to specify provider - it's automatically detected from the model name format
# )

## -----------------------------------------------------------------------------
# library(Seurat)
# library(mLLMCelltype)
# library(ggplot2)
# 
# # Load data
# data("pbmc_small")
# 
# # Standard Seurat preprocessing
# pbmc_small <- NormalizeData(pbmc_small)
# pbmc_small <- FindVariableFeatures(pbmc_small)
# pbmc_small <- ScaleData(pbmc_small)
# pbmc_small <- RunPCA(pbmc_small)
# pbmc_small <- FindNeighbors(pbmc_small)
# pbmc_small <- FindClusters(pbmc_small, resolution = 0.5)
# pbmc_small <- RunUMAP(pbmc_small, dims = 1:10)
# 
# # Find markers for each cluster
# pbmc_markers <- FindAllMarkers(pbmc_small, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
# 
# # Define models to use
# models <- c(
#   "claude-sonnet-4-6",
#   "gpt-5.5",
#   "gemini-3.1-pro-preview"
# )
# 
# # API keys
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
#     input = pbmc_markers,
#     tissue_name = "human PBMC",
#     model = model,
#     api_key = api_key,
#     top_gene_count = 10
#   )
# 
#   # Add individual model results to Seurat object
#   column_name <- paste0("cell_type_", gsub("[^a-zA-Z0-9]", "_", model))
#   pbmc_small[[column_name]] <- plyr::mapvalues(
#     x = as.character(Idents(pbmc_small)),
#     from = names(results[[model]]),
#     to = results[[model]]
#   )
# }
# 
# # Create consensus
# consensus_results <- interactive_consensus_annotation(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   models = models,  # Use all the models defined above
#   api_keys = api_keys,
#   controversy_threshold = 0.7,
#   entropy_threshold = 1.0,
#   consensus_check_model = "claude-sonnet-4-6"
# )
# 
# # Add consensus results to Seurat object
# pbmc_small$cell_type_consensus <- plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = names(consensus_results$final_annotations),
#   to = consensus_results$final_annotations
# )
# 
# # Extract consensus metrics from the consensus results
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
# pbmc_small$consensus_proportion <- as.numeric(plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = metrics_df$cluster,
#   to = metrics_df$consensus_proportion
# ))
# 
# # Add entropy to Seurat object
# pbmc_small$shannon_entropy <- as.numeric(plyr::mapvalues(
#   x = as.character(Idents(pbmc_small)),
#   from = metrics_df$cluster,
#   to = metrics_df$entropy
# ))
# 
# # Visualize results
# p1 <- DimPlot(pbmc_small, group.by = "cell_type_consensus", label = TRUE, repel = TRUE) +
#   ggtitle("Cell Type Annotations") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# p2 <- FeaturePlot(pbmc_small, features = "consensus_proportion", cols = c("yellow", "green", "blue")) +
#   ggtitle("Consensus Proportion") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# p3 <- FeaturePlot(pbmc_small, features = "shannon_entropy", cols = c("red", "orange")) +
#   ggtitle("Shannon Entropy") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # Combine plots
# p1 | p2 | p3

## -----------------------------------------------------------------------------
# # Using more genes (better for well-characterized tissues)
# results_more_genes <- annotate_cell_types(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#   top_gene_count = 20  # Using more genes
# )
# 
# # Using fewer genes (better for noisy data)
# results_fewer_genes <- annotate_cell_types(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#   top_gene_count = 5   # Using fewer genes
# )

## ----eval=FALSE---------------------------------------------------------------
# # Example of using interactive_consensus_annotation with different controversy thresholds
# # Lower threshold (more clusters will be discussed)
# consensus_results_low_threshold <- interactive_consensus_annotation(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   models = c("claude-sonnet-4-6", "gpt-5.5", "gemini-3-flash-preview"),
#   api_keys = list(
#     "anthropic" = Sys.getenv("ANTHROPIC_API_KEY"),
#     "openai" = Sys.getenv("OPENAI_API_KEY"),
#     "gemini" = Sys.getenv("GEMINI_API_KEY")
#   ),
#   controversy_threshold = 0.3  # Lower threshold - more clusters will be discussed
# )
# 
# # Higher threshold (fewer clusters will be discussed)
# consensus_results_high_threshold <- interactive_consensus_annotation(
#   input = pbmc_markers,
#   tissue_name = "human PBMC",
#   models = c("claude-sonnet-4-6", "gpt-5.5", "gemini-3-flash-preview"),
#   api_keys = list(
#     "anthropic" = Sys.getenv("ANTHROPIC_API_KEY"),
#     "openai" = Sys.getenv("OPENAI_API_KEY"),
#     "gemini" = Sys.getenv("GEMINI_API_KEY")
#   ),
#   controversy_threshold = 0.7  # Higher threshold - fewer clusters will be discussed
# )

