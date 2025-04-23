#' @importFrom utils globalVariables
utils::globalVariables(c("entrezgene_id", 
                         "symbol"))

#' Obtain Reactome pathways
#' 
#' Connects to `reactome.db` \insertCite{reactome}{dnapath}  
#' to obtain a list of pathways for a given species.
#' The pathway list is processed by combining any two pathways that have 
#' substantial overlap (default is over 90% overlap). This output if this 
#' function can be used for the `pathway_list` argument in 
#' \code{\link{dnapath}}.
#'
#' @param species A string, for example "Homo sapiens" or "Mus musculus", 
#' indicating the species to use. 
#' @param overlap_limit (Optional) Any pathways that have an overlap
#' greater than overlap_limit are combined. Set to NULL to disable
#' this option.
#' @param min_size The minimum pathway size. Any Reactome pathways with fewer
#' than `min_size` genes are removed from the list. Defaults to 10. 
#' @param max_size The maximum pathway size. Any Reactome pathways with more
#' than `max_size` genes are removed from the list. Defaults to 50. 
#' @param verbose Set to FALSE to turn off messages.
#' @return A named list of vectors. Each vector corresponds to a Reactome pathway
#' and contains the entrezgene IDs of the genes in that pathway.
#' @references 
#' \insertRef{reactome}{dnapath}
#' @seealso 
#' The genes in the Reactome pathways use entrezgene IDs. These can be converted
#' to gene symbols, if desired, using the \code{\link{entrez_to_symbol}} and
#' \code{\link{rename_genes}} functions.
#' @export
#' @examples 
#' # Obtaining a pathway list for human (Homo sapiens).
#' # In this example, overlapping pathways are not combined (this is
#' # specified by setting overlap_limit to NULL).
#' pathway_list <- get_reactome_pathways("Homo sapiens", overlap_limit = NULL,
#'                                       min_size = 10, max_size = 20)
get_reactome_pathways <- function(species, overlap_limit = 0.9, min_size = 10, 
                                  max_size = 50, verbose = TRUE) {
  if(!requireNamespace("reactome.db", quietly = TRUE)) {
    message("Warning: The `reactome.db` package must be installed to use ",
            "get_reactome_pathways(). Returning the p53_pathway list by default.\n")
    return(dnapath::p53_pathways)
  }
  if(!requireNamespace("AnnotationDbi", quietly = TRUE)) {
    message("Warning: The `AnnotationDbi` package must be installed to use ",
            "get_reactome_pathways(). Returning the p53_pathway list by default.\n")
    return(dnapath::p53_pathways)
  }
  
  
  # If species is "human" or "mouse", change to appropriate name.
  if(tolower(species) == "human") 
    species <- "Homo sapiens"
  if(tolower(species) == "mouse" || tolower(species) == "mice") 
    species <- "Mus musculus"
  
  
  if(verbose)
    cat("Obtaining reactome pathway information for species:", species, "\n")
  # Get list of reactome pathway names
  # Map names to ID
  # Map ID to entrez ID
  # Save results as p by g matrix. (Optional; not performed.)
  
  # library(reactome.db)
  # Map reactome NAME to reactome ID. Subset on pathways for this species.
  reactome_to_id <- reactome.db::reactomePATHNAME2ID
  pathway_for_species <- gsub(":.*", "", AnnotationDbi::keys(reactome_to_id))
  index <- which(tolower(species) == tolower(pathway_for_species))
  if(length(index) == 0) {
    available_species <- unique(pathway_for_species)
    if(tolower(species) %in% tolower(available_species)) {
      message("Warning: Unknown problem occured when accessing `reactome.db`. Please check ",
              "that the packages `AnnotationDbi` and `reactome.db` are up-to-date. ", 
              "Returning the p53_pathway list by default.\n")
      return(dnapath::p53_pathways)
    } else {
      message(species, " is not an available species. Use one of the following:\n\t", 
           paste(available_species[-length(available_species)], collapse = ",\n\t"), 
           ",\n\t", available_species[length(available_species)], ".")
      message("Returning the p53_pathway list by default.")
      return(dnapath::p53_pathways)
    }
  }
  reactome_to_id <- reactome_to_id[index]
  reactome_to_id <- as.list(reactome_to_id)
  
  # Map reactome ID to entrez ID.
  id_to_entrez <- reactome.db::reactomePATHID2EXTID
  
  # Subset on reactome IDs obtained in first step.
  # id_to_entrez <- id_to_entrez[sapply(reactome_to_id, dplyr::first)]
  
  index <- which(AnnotationDbi::keys(id_to_entrez) %in% sapply(reactome_to_id, dplyr::first))
  id_to_entrez <- id_to_entrez[index]
  id_to_entrez <- as.list(id_to_entrez)
  
  # Remove any pathways containing fewer than min_size genes or more than 
  # max_size genes.
  sizes <- sapply(id_to_entrez, function(x) length(unique(x)))
  id_to_entrez <- id_to_entrez[sizes >= min_size & sizes <= max_size]
  
  # Map reactome ID back to reactome NAME.
  id_to_reactome <- reactome.db::reactomePATHID2NAME
  
  # Subset on reactome IDs obtained in previous step.
  index <- which(AnnotationDbi::keys(id_to_reactome) %in% names(id_to_entrez))
  id_to_reactome <- id_to_reactome[index]
  id_to_reactome <- as.list(id_to_reactome)
  
  # Finally, map reactome NAME to entrez ID
  pathway_list <- id_to_entrez
  names(pathway_list) <- sapply(names(id_to_entrez), function(x) {
    dplyr::first(id_to_reactome[[which(names(id_to_reactome) == x)[1]]])
  })
  names(pathway_list) <- gsub("^[ A-Za-z]*: ", "", names(pathway_list))
  
  if(!is.null(overlap_limit)) {
    if(((overlap_limit >= 1) || (overlap_limit <= 0))) {
      warning("`overlap_limit` is not between 0 and 1. Overlapping pathways",
              " are not combined.")
    } else {
      if(verbose)
        cat("Combining pathways that have greater than", overlap_limit * 100,
            "% overlap.\n")
      pathway_list <- combine_overlapping_pathways(pathway_list, overlap_limit)
    }
  }
  
  return(pathway_list)
}

#' Modify a pathway list to combine overlapping pathways.
#'
#' @param pathway_list A list of pathways obtained from
#' \code{\link{get_reactome_pathways}}.
#' @param overlap_limit A percentage between 0 and 1. If two pathways
#' overlap by more than this amount, they are combined into one pathway.
#' @return A modified list with overlapping pathways combined together.
#' @keywords internal
combine_overlapping_pathways <- function(pathway_list, overlap_limit = 0.9) {
  if((overlap_limit >= 1) || (overlap_limit <= 0)) {
    warning("`overlap_limit` is not between 0 and 1. Returning pathway list",
            " unchanged.")
    return(pathway_list)
  }
  
  # Determine which pathways are similar.
  n <- length(pathway_list)
  similar_to <- diag(FALSE, n)
  for(i in 2:n) {
    for(j in 1:(i - 1)) {
      len_intersect <- length(intersect(pathway_list[[i]], pathway_list[[j]]))
      if(len_intersect == 0) {
        similar_to[i, j] <- FALSE
      } else {
        len_union <- length(union(pathway_list[[i]], pathway_list[[j]]))
        if(len_union != 0)
          similar_to[i, j] <- (len_intersect / len_union) > overlap_limit
      }
    }
  }
  pathways_to_remove <- NULL
  for(i in 1:ncol(similar_to)) {
    if(sum(similar_to[, i]) > 0) {
      index <- which(similar_to[, i])
      names(pathway_list)[i] <- paste0(names(pathway_list)[i], " (See also: ",
                                       paste0(names(pathway_list)[index],
                                              collapse = "; "), ")")
      
      pathway_list[[i]] <- unique(unlist(pathway_list[c(i, index)]))
      pathways_to_remove <- c(pathways_to_remove, index)
    }
  }
  
  pathways_to_remove <- unique(pathways_to_remove)
  
  if(length(pathways_to_remove) > 0) {
    pathway_list <- pathway_list[-pathways_to_remove]
  }
  return(pathway_list)
}







#' Obtain gene symbols for entrezgene IDs
#'
#' Uses `biomaRt` \insertCite{biomart}{dnapath} to map entrezgene IDs to gene 
#' symbols for a given species. Obtains MGI symbols for mouse species and
#' HGNC symbols for other species.
#' (Note: this mapping may not work for all species.)
#' The output of this function can be used in \code{\link{rename_genes}}. 
#' 
#' If entrezgene IDs are used in a `dnapath_list` or `dnapath`
#' object, or a pathway list, then \code{\link{get_genes}} can be used to 
#' extract them and used for the `x` argument here.
#' 
#' @param x A vector of entrezgene IDs.
#' @param species The species used to obtain the entrezgene IDs. For example:
#' "Homo sapiens", "m musculus", "C. elegans", or "S cerevisiae".
#' "Human" and "mouse" can also be used and will be converted to the 
#' correct species name.
#' @param symbol_name The type of gene symbol to use. If NULL, then "hgnc_symbol"
#' is used for HGNC symbols, unless `species` is "mmusculus", in which case 
#' @param dir_save The directory to store annotation reference. Future
#' calls to this function will use the stored annotations. This speeds up the
#' operation and allows for reproducibility in the event that the `biomaRt`
#' database is updated. Set to NULL to disable. By default, it uses a
#' temporary directory to store files during the R session.
#' "mgi_symbol" is used.
#' @param verbose Set to FALSE to avoid messages.
#' @return A data frame with two columns: the first contains the original
#' entrezgene IDs, and the second contains the corresponding gene symbols.
#' MGI symbols are returned when `species = "Mus musculus"` and HGNC symbols
#' are returned otherwise.
#' @note
#' Internet connection is required to connect to biomaRt. If unavailable, the
#' default biomart and default species contained in the package is used, but
#' this may not match the desired species.
#' @references 
#' \insertRef{biomart}{dnapath}
#' @seealso 
#' \code{\link{symbol_to_entrez}}, \code{\link{get_genes}}
#' @export
#' @examples 
#' \donttest{
#' data(meso)
#' # The meso gene expression data contains entrezgene IDs. 
#' # These can be converted to gene symbols.
#' gene_mat <- entrez_to_symbol(colnames(meso$gene_expression), species = "human")
#' }
entrez_to_symbol <- function(x, 
                             species,
                             symbol_name = NULL,
                             dir_save = tempdir(),
                             verbose = TRUE) {
  if(is(x, "dnapath") || is(x, "dnapath_list")) {
    stop("Input should be a vector of entrezgene IDs, not a dnapath object. ",
         'Use rename_genes() with `to = "symbol"`')
  }
  
  species <- format_species_name(species)
  
  # Different species will have different names for symbol identifier.
  if(is.null(symbol_name)) {
    if(species == "mmusculus") {
      symbol_name <- "mgi_symbol"
    } else {
      symbol_name <- "hgnc_symbol"
    }
  }
  
  # If the entrezgene IDs are a factor or character object, coerce into numeric.
  if(!is.numeric(x)) {
    if(is.factor(x)) {
      x <- suppressWarnings(as.numeric(as.character(x)))
    } else {
      x <- suppressWarnings(as.numeric(x))
    }
  }
  
  gene_info <- get_biomart_mapping(species, symbol_name, dir_save, verbose) %>%
    dplyr::group_by(entrezgene_id) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), dplyr::first)) 
  
  if(is.null(gene_info)) {
    # get_biomart_mapping() may return NULL if biomaRt is not installed and there
    # is no archived mapping for this species. Return "NA" string for gene symbols.
    
    df <- data.frame(entrezgene_id = x,
                    symbol = "NA", 
                    stringsAsFactors = FALSE)
    colnames(df)[2] <- symbol_name
  } else {
    # If a mapping was obtained, then create the final data frame to return.
    df <- data.frame(entrezgene_id = x)
    df <- dplyr::left_join(df, gene_info, by = "entrezgene_id")
    
    # If any symbols were not found, replace NA value(s) with the entrezgene ID.
    index_na <- which(is.na(df[, 2]))
    if(length(index_na) > 0) {
      df[index_na, 2] <- df[index_na, 1]
    }
  }
  
  return(df)
}




#' Obtain entrezgene IDs for gene symbols
#'
#' Uses `biomaRt` \insertCite{biomart}{dnapath}   
#' to map entrezgene IDs to gene symbols for a given species. The output of
#' this function can be used in \code{\link{rename_genes}}.
#' 
#' If entrezgene IDs are used in a `dnapath_list` or `dnapath`
#' object, or a pathway list, then \code{\link{get_genes}} can be used to 
#' extract them and used for the `x` argument here.
#' 
#' @param x A vector of gene symbols.
#' @param species The species used to obtain the entrezgene IDs. For example:
#' "Homo sapiens", "m musculus", "C. elegans", or "S cerevisiae".
#' "Human" and "mouse" can also be used and will be converted to the 
#' correct species name.
#' @param symbol_name The type of gene symbol to use. If NULL, then "hgnc_symbol"
#' is used for HGNC symbols, unless `species` is "mmusculus", in which case 
#' "mgi_symbol" is used.
#' @param dir_save The directory to store annotation reference. Future
#' calls to this function will use the stored annotations. This speeds up the
#' operation and allows for reproducibility in the event that the `biomaRt`
#' database is updated. Set to NULL to disable. By default, it uses a
#' temporary directory to store files during the R session.
#' @param verbose Set to FALSE to avoid messages.
#' @return A data frame with two columns: the first contains the original
#' gene symbols, and the second contains a corresponding entrezgene ID. If a
#' gene symbol is not mapped to an entrezgene ID, the entrezgene ID is set to -1.
#' @note
#' Internet connection is required to connect to biomaRt. If unavailable, the
#' default biomart and default species contained in the package is used, but
#' this may not match the desired species.
#' 
#' It is assumed that `x` contains MGI symbols when the biomart species is
#' "Mus musculus" and HGNC symbols otherwise.
#' @references 
#' \insertRef{biomart}{dnapath}
#' @seealso 
#' \code{\link{entrez_to_symbol}}, \code{\link{get_genes}} 
#' @export
#' @examples
#' \donttest{
#' # Convert a set of gene symbols to entrezgene IDs.
#' # Note that not all may have mapping (such as "MSX" in this example).
#' gene_mat <- symbol_to_entrez(c("SOX2", "SEMA3E", "COL11A1", "UBB", "MSX"),
#'                              species = "human")
#' }
symbol_to_entrez <- function(x, 
                             species,
                             symbol_name = NULL,
                             dir_save = tempdir(),
                             verbose = TRUE) {
  
  if(is(x, "dnapath") || is(x, "dnapath_list")) {
    stop("Input should be a vector of entrezgene IDs, not a dnapath object.")
  }
  
  # If the gene symbols are a factor, coerce into character.
  if(is.factor(x)) {
    x <- as.character(x)
  }
  
  species <- format_species_name(species)
  
  # Different species will have different names for symbol identifier.
  if(is.null(symbol_name)) {
    if(species == "mmusculus") {
      symbol_name <- "mgi_symbol"
    } else {
      symbol_name <- "hgnc_symbol"
    }
  }
  
  gene_info <- get_biomart_mapping(species, symbol_name, dir_save, verbose) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(symbol_name))) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), dplyr::first)) 
  
  # get_biomart_mapping() may return NULL if biomaRt is not installed or
  # if there is no internet access. Return -2 for entrezgene IDs.
  if(is.null(gene_info)) {
    df <- data.frame(symbol = x,
                     entrezgene_id = -2, 
                     stringsAsFactors = FALSE)
    colnames(df)[1] <- symbol_name
    return(df)
  }
  
  df <- data.frame(symbol = x, stringsAsFactors = FALSE)
  colnames(df) <- symbol_name
  df <- dplyr::left_join(df, gene_info, by = symbol_name)
  
  # If any entrezgenes were not found, replace NA value(s) with -1
  index_na <- which(is.na(df[, 2]))
  if(length(index_na) > 0) {
    df[index_na, 2] <- -1
  }
  
  return(df)
}


#' Internal function for obtaining biomaRt mapping
#'
#' @param species The species to obtain a biomart dataset for. This should be
#' an output from \code{\link{format_species_name}}.
#' @param symbol_name The type of gene symbol to use. HGNC symbols are used by 
#' default (hgnc_symbol), unless `species` is "mmusculus", in which case MGI 
#' symbols are used (mgi_symbol).
#' @param dir_save The directory to store annotation reference. Future
#' calls to this function will use the stored annotations. This speeds up the
#' operation and allows for reproducibility in the event that the `biomaRt`
#' database is updated. Set to NULL to disable. By default, it uses a
#' temporary directory to store files during the R session.
#' @param verbose Set to FALSE to avoid messages.
#' @return A data frame containing a mapping between entrezgene IDs and gene 
#' symbols.
#' @keywords internal
#' @export
get_biomart_mapping <- function(species, symbol_name, dir_save, verbose) {
  if(!requireNamespace("biomaRt", quietly = TRUE)) {
    # If biomaRt package is not installed, return archived mapping or NULL 
    # if one is not available.
    
    message("Warning: the bioconductor package `biomaRt` is not installed.\n")
    if(species == "hsapiens") {
      message("Warning: Using the biomaRt data archived in `dnapath`; this ",
              "mapping may be outdated.")
      gene_info <- dnapath::biomart_hsapiens
    } else {
      message("Error: No default biomaRt data available for ", species, ". ", 
              "Please install the `biomaRt` package.\n")
      gene_info <- NULL
    }
  } else if(!curl::has_internet()) {
    # If no internet access, return archived mapping or NULL if one is not 
    # available.
    
    message("Warning: Internet connection is not available.\n")
    if(species == "hsapiens") {
      message("Warning: Using the biomaRt data archived in `dnapath`; this ",
              "mapping may be outdated.")
      gene_info <- dnapath::biomart_hsapiens
    } else {
      message("Error: No default biomaRt data available for ", species, ". ", 
              "Please establish internet connection and try again.\n")
      gene_info <- NULL
    }
  } else {
    # Access biomart to obtain mapping.
    
    load_file <- NULL
    if(!is.null(dir_save)) {
      # If a save directory is provided, get the path to a .rds file for this species.
      load_file <- file.path(dir_save, paste0("entrez_to_", species, ".rds"))
    }
    
    if(!is.null(dir_save) && file.exists(load_file)) {
      # If the save directory already contains a .rds file for this species, 
      # then load that mapping.
      if(verbose) cat("\t- loading gene info from", load_file, "\n")
      gene_info <- readRDS(load_file)
    } else {
      # If the save directory does not contain a .rds file for this species,
      # then access biomart to create the mapping.
      mart <- init_mart(species)
      if(is.null(mart)) {
        # Failure may occur here due to internet connection issues. In this case,
        # give a warning and default to an archived mapping.
        message("Warning: unable to obtain a valid Mart object. ",
                "Using archived biomart data for hsapiens.")
        gene_info <- dnapath::biomart_hsapiens
      } else {
        gene_info <- biomaRt::getBM(attributes = c("entrezgene_id", symbol_name),
                                    mart = mart)
      }
      
      # Process the mapping:
      # Remove any rows containing NA values for the entrezgene ID.
      gene_info %>%
        dplyr::filter(!is.na(entrezgene_id)) ->
        gene_info
      
      # If biomart was accessed, store the mapping for future reference.
      if(!is.null(mart) && !is.null(dir_save)) {
        save_file <- load_file
        if(verbose)
          cat("\t- saving gene info to", save_file, "\n")
        if(!dir.exists(dir_save)) 
          dir.create(dir_save, recursive = TRUE)
        saveRDS(gene_info, save_file)
      }
    }
  }
  
  return(gene_info)
}

#' Initialize biomaRt for a given species
#'
#' @param species The species to obtain a biomart dataset for. This should be
#' an output from \code{\link{format_species_name}}.
#' @return A biomaRt object.
#' @note Requires internet connection. This function is called by 
#' \code{\link{get_biomart_mapping}}, which checks for internet connection
#' prior to calling this function.
#' @keywords internal
#' @export
init_mart <- function(species) {
  # listMarts(); listAttributes(mart) # Useful functions to obtain more info.
  # listDatasets(useMart('ensembl'));
  
  # Attempt to connect to biomaRt database.
  message("Connecting to biomaRt database...\n")
  mart <- tryCatch(
    biomaRt::useMart(biomart = "ensembl", 
                     dataset = paste0(species, "_gene_ensembl")),
    error = function(e) {
      if(!curl::has_internet()) {
        message("Error: Internet connection is not available.\n")
      } else if(!grepl("curl", e)) {
        message("Error: ", paste0('"', species, '"'), " is not an available species.\n",
                "Example species include: `H. sapiens`, `M. Musculus`, `C. elegans`, etc.\n",
                "Use the following command to see which species are available:\n",
                "> biomaRt::listDatasets(useMart('ensembl'))$dataset\n",
                "Note: biomaRt uses the format `hsapiens` for `Homo sapiens`.\n")
      }
      return(NULL)
    })
  
  return(mart)
}


#' Format sepcies name input.
#' 
#' Internal function used to format species names to be used with biomart.
#' @param species The species to obtain a biomart dataset for. For example:
#' "Homo sapiens", "m musculus", "C. elegans", or "S cerevisiae", and
#' "Human" and "mouse" will be converted to the format required in biomart.
#' @return A string containing the formatted species name.
#' @keywords internal
#' @export
format_species_name <- function(species) {
  # Convert to lowercase. If a vector of names is provided, use only the first.
  species <- tolower(species[1])
  
  if(species == "human") {
    species <- "h sapiens"
  } else if(species == "mouse") {
    species <- "m musculus"
  }
  
  # If species contains a space, like "homo sapiens", "h. sapiens", or "h sapiens", 
  # convert to format "hsapiens".
  if(grepl(" ", species)) {
    first <- substr(species, 1, 1)
    species <- paste0(first, gsub("(.* )", "", species))
  }
  if(grepl("_gene_ensembl", species)) {
    species <- gsub("_gene_ensembl", "", species)
  }
  
  return(species)
}