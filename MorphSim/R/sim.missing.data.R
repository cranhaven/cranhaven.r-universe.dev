#' Remove morphological character data
#'
#' @description
#' This function removes characters from a morphological matrix simulated using morphsim
#' @param data A `morpho` object with sequence data.
#' @param seq Character. Which sequence data to use: "tips", "nodes", or "SA".
#' @param method Character. Method for removing data. Options:
#' \itemize{
#'   \item \code{"random"}: removes characters randomly across the entire matrix.
#'   \item \code{"partition"}: removes characters by partition
#'   (one probability per partition).
#'   \item \code{"rate"}: removes characters by rate category
#'   (one probability per rate category, ordered from slowest to fastest).
#'   \item \code{"trait"}: removes characters from specific traits across all taxa.
#'   \item \code{"taxa"}: removes characters from specific taxa across all traits.
#'   \item \code{"extinct"}: removes data from extinct taxa only.
#'   Requires \code{seq = "tips"} as extinct taxa are only present at the tips of the tree.
#'   \item \code{"trait_taxa"}: removes characters at the intersection of specific traits and
#'     specific taxa, reflecting scenarios where certain anatomical regions are less likely
#'     to be preserved in particular lineages.
#' }
#' @param probability Numeric. Probability of missing data (single value or vector depending on method).
#' @param traits Integer vector. Indices of traits to remove. Required when
#'   \code{method = "trait"} or \code{method = "trait_taxa"}.
#' @param taxa Character vector. Names of taxa to remove data from. Required when
#'   \code{method = "taxa"} or \code{method = "trait_taxa"}.
#'
#' @return An object of class morpho.
#'
#' @export
#' @examples
#' # simulate a phylogenetic tree
#' phy <- ape::rtree(10)
#'
#' # simulate characters along the branches of the tree
#' morpho_data <-  sim.morpho(tree = phy,
#'                            k = c(2,3,4),
#'                            trait.num = 20,
#'                            ancestral = TRUE,
#'                            partition = c(10,5,5),
#'                            ACRV = "gamma",
#'                            variable = TRUE,
#'                            ACRV.ncats = 4,
#'                            define.Q = NULL)
#'
#' # randomly remove data
#' missing.data <- sim.missing.data(data = morpho_data,
#'                                   method = "random",
#'                                   seq = "tips",
#'                                   probability = 0.5)
#'
#'
#' # remove data based on the partition
#' missing.data <- sim.missing.data(data = morpho_data,
#'                                  method = "partition",
#'                                  seq = "tips",
#'                                  probability = c(0.7, 0, 0.5))
#'
#' # remove data based on the rate it was simulated under
#' missing.data <- sim.missing.data(data = morpho_data,
#'                                         method = "rate",
#'                                         seq = "tips",
#'                                         probability = c(0,0,0.2,1))
#'
#' # remove characters from specific traits
#' missing.data <- sim.missing.data(data = morpho_data,
#'                                  method = "trait",
#'                                  seq = "tips",
#'                                  probability = 1,
#'                                  traits = c(1,2,5))
#'
#' # remove characters from specific taxa
#' missing.data <- sim.missing.data(data = morpho_data,
#'                                  method = "taxa",
#'                                  seq = "tips",
#'                                  probability = 1,
#'                                  taxa = c("t1", "t2"))
#'
#' # remove specific character traits from specific taxa
#' missing.data <- sim.missing.data(data = morpho_data,
#'                                  method = "trait_taxa",
#'                                  seq = "tips",
#'                                  probability = 0.8,
#'                                  traits = c(1,2,5),
#'                                  taxa = c("t1", "t2"))
sim.missing.data <- function(data = NULL, seq = NULL, method = NULL, probability = NULL,
                             traits = NULL, taxa = NULL){
  #### Checks
  if (is.null(method) || !method %in% c("random", "partition", "rate", "trait", "taxa", "extinct", "trait_taxa")) {
    stop("Error: must specify a `method`: 'random', 'partition', 'rate', 'trait', 'taxa', 'extinct', or 'trait_taxa'.")
  }
  if (is.null(data) || !inherits(data, "morpho")) {
    stop("Error: `data` must be a morpho object.")
  }
  if (is.null(seq) || !seq %in% c("tips", "nodes", "SA")) {
    stop("Error: `seq` must be one of 'tips', 'nodes', or 'SA'.")
  }
  if (is.null(probability)) {
    stop("Error: You must provide a `probability` value.")
  }
  if (!is.numeric(probability) || any(probability < 0 | probability > 1)) {
    stop("Error: `probability` must be between 0 and 1.")
  }
  if(method == "rate" && !is.null(data$combined)){
    stop ("Error: Cannot use rates on data sets that are combined from different models.
          Simulate missing data first and then combine.")
  }
  if (method == "rate" && length(unique(data$model$RateVarTrait[1,])) != length(probability)){
    stop("Error: Number of probabilities must match the number of rate categories used. See data$model$RateVarTrait" )
  }
  ## Create data frame
  x <- t(as.data.frame(data$sequences[[seq]]))
  trait.num  <- length(x[1,])
  taxa.num <-   length(x[,1])
  ## Method: Random
  if (method == "random"){
    if (length(probability) > 1) stop("For method = 'random', provide a single probability.")
    remove <- round((trait.num* taxa.num)* probability, 0)
    total_cells <- taxa.num*trait.num
    all_combinations <- expand.grid(Row = 1:taxa.num, Column = 1:trait.num)
    random_cells<- all_combinations[sample(1:total_cells, remove, replace = FALSE), ]
    for (i in 1:remove){
      x[random_cells$Row[i], random_cells$Column[i]] <- "?"
    }
  }
  ## Method: Rate
  if (method == "rate"){
    rates <- data$model$RateVarTrait
    sorted_cats <- sort(unique(rates[1, ]))  # ascending: cat 1 (slowest) to cat N (fastest)
    if (length(probability) != length(sorted_cats)) {
      stop("Length of `probability` must match the number of rate categories.")
    }
    for (j in seq_along(sorted_cats)){
      traits_per_rate <- which(rates == sorted_cats[j])
      remove <- round((length(traits_per_rate)* taxa.num)* probability[j], 0)
      total_cells <- length(traits_per_rate)*taxa.num
      all_combinations <- expand.grid(Row = 1:taxa.num, Column = traits_per_rate)
      random_cells <- all_combinations[sample(1:total_cells, remove, replace = FALSE), ]
      for (i in 1:remove){
        x[random_cells$Row[i], random_cells$Column[i]] <- "?"
      }
    }
  }
  ## Method: Partition
  if(method == "partition"){
    if (length(probability) != length(data$model$Specified)){
      stop("Vector of probabilities does not match the number of partitions")
    }
    start_col <- 1
    for ( j in 1:length(data$model$Specified)){
      traits_per_partition <-  as.numeric(sub(".*Part:(\\d+).*", "\\1",
                                              data[["model"]][["Specified"]][j]))
      remove <- round((traits_per_partition* taxa.num)* probability[j], 0)
      total_cells <- traits_per_partition*taxa.num
      all_combinations <- expand.grid(Row = 1:taxa.num,
                                      Column =  start_col:(start_col + traits_per_partition -1))
      random_cells <- all_combinations[sample(1:total_cells, remove, replace = FALSE), ]
      for ( i in 1:remove){
        x[random_cells$Row[i], random_cells$Column[i]] <- "?"
      }
      start_col <- start_col + traits_per_partition
    }
  }
  ## Method: Trait
  if ( method == "trait"){
    if (length(probability) > 1) {
      stop("For method = 'trait', provide a single probability.")
    }
    if (is.null(traits)) stop("For method = 'trait', you must specify `traits`.")
    remove <- round((length(traits)* taxa.num)* probability, 0)
    total_cells <- length(traits)*taxa.num
    all_combinations <- expand.grid(Row = 1:taxa.num, Column = traits)
    random_cells <- all_combinations[sample(1:total_cells, remove, replace = FALSE), ]
    for ( i in 1:remove){
      x[random_cells$Row[i], random_cells$Column[i]] <- "?"
    }
  }
  ## Method: Taxa
  if ( method == "taxa"){
    if (length(probability) > 1) {
      stop("For method = 'taxa', provide a single probability.")
    }
    if (is.null(taxa)) stop("For method = 'taxa', you must specify `taxa`.")
    remove <- round((length(taxa)* trait.num)* probability, 0)
    total_cells <- length(taxa)* trait.num
    all_combinations <- expand.grid(Column = 1:trait.num,  Row = taxa)
    random_cells <- all_combinations[sample(1:total_cells, remove, replace = FALSE), ]
    for ( i in 1:remove){
      x[as.character(random_cells$Row[i]), random_cells$Column[i]] <- "?"
    }
  }
  ## Method: Extinct
  if ( method == "extinct"){
    if (length(probability) > 1) {
      stop("For method = 'extinct', provide a single probability.")
    }
    if (seq != "tips") {
      stop("For method = 'extinct', `seq` must be 'tips' as extinct taxa are only present at the tips of the tree.")
    }
    tip_depths <- ape::node.depth.edgelength(data$trees$TimeTree)[1:length(data$trees$TimeTree$tip.label)]
    tree_height <- max(ape::node.depth.edgelength(data$trees$TimeTree))
    tip_ages <-   round(abs(tip_depths - tree_height),3)
    extinct <- data$trees$EvolTree$tip.label[which(tip_ages != 0) ]
    remove <- round((length(extinct)* trait.num)* probability, 0)
    total_cells <- length(extinct)* trait.num
    all_combinations <- expand.grid(Column = 1:trait.num,  Row = extinct)
    random_cells <- all_combinations[sample(1:total_cells, remove, replace = FALSE), ]
    for ( i in 1:remove){
      x[as.character(random_cells$Row[i]), random_cells$Column[i]] <- "?"
    }
  }
  ## Method: Trait_Taxa
  if (method == "trait_taxa"){
    if (length(probability) > 1) stop("For method = 'trait_taxa', provide a single probability.")
    if (is.null(traits)) stop("For method = 'trait_taxa', you must specify `traits`.")
    if (is.null(taxa)) stop("For method = 'trait_taxa', you must specify `taxa`.")
    remove <- round((length(traits) * length(taxa)) * probability, 0)
    total_cells <- length(traits) * length(taxa)
    all_combinations <- expand.grid(Column = traits, Row = taxa)
    random_cells <- all_combinations[sample(1:total_cells, remove, replace = FALSE), ]
    for (i in 1:remove){
      x[as.character(random_cells$Row[i]), random_cells$Column[i]] <- "?"
    }
  }
  ## update morpho object
  tax <- rownames(x)
  for ( i in 1:length(tax)){
    data$sequences[[seq]][[tax[i]]] <- x[tax[i],]
  }
  return(data)
}
