check_layer <- function(layer) {
    #' @title [INTERNAL] Check layer input
    #'
    #' @description [INTERNAL] Checks if the data used to create a network layer is valid and has the right
    #' format
    #'
    #' @param layer [list] Named list of layers to check. Created by \code{\link[DrDimont]{make_layer}}
    #' @return Character string vector containing error messages.
    #' 
    #' @keywords internal
    #' @noRd
    
    errors <- c()
    for (group in c("groupA", "groupB")) {
    
        ### if group in layer not given skip it
        if (is.null(layer[[group]])){next}
    
        ### check if length of components and length of identifiers match
        if (dim(layer[[group]][['data']])[2] != dim(layer[[group]][['identifiers']])[1]) {
            errors <- c(errors, stringr::str_interp("Layer \"${layer[['name']]}\", group \"${group}\": The number of columns in the supplied data does not equal the number of provided identifiers."))
        }
      
        ### check if data is numerical matrix
        if (!is.numeric(as.matrix(layer[[group]][['data']]))) {
          errors <- c(errors, stringr::str_interp("Layer \"${layer[['name']]}\", group \"${group}\": The supplied data is not numeric. Please check your data. Numerical data is required."))
        }
      
        ### check if the identifiers contain an id called 'layer', layer attribute is assigned in the pipeline
        if ('layer' %in% colnames(layer[[group]][['identifiers']])) {
            errors <- c(errors, stringr::str_interp("Layer \"${layer[['name']]}\", group \"${group}\": Identifiers cannot be named \"layer\". Please choose a different name for the identifier with name \"layer\"."))
        }
        
        ### check for duplicates in identifiers
        if (sum(duplicated(layer[[group]][['identifiers']])) > 0){
          errors <- c(errors, stringr::str_interp("Layer \"${layer[['name']]}\", group \"${group}\": Duplicate entries given in `identifiers` dataframe. This may cause ERRORS!"))
        }
      
        message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("Layer \"${layer[['name']]}\", group \"${group}\" contains "), 
                dim(layer[[group]][['data']])[1], " samples and ", dim(layer[[group]][['data']])[2], " genes/proteins/entities.")
    }
    return(errors)
}


check_connection <- function(connection) {
    #' @title [INTERNAL] Check connection
    #'
    #' @description [INTERNAL] Checks if the data given to create an inter-layer connection is valid and has the
    #' right input format
    #'
    #' @param connection [list] Connection to check. Created by \code{\link[DrDimont]{make_connection}}
    #' @return Character string vector containing error messages.
    #' 
    #' @keywords internal
    #' @noRd

    errors <- c()

    ### check if group argument is one of 'A', 'B' or 'both'
    if (as.character(connection$group) != 'A' & as.character(connection$group) != 'B' & connection$group != 'both') {
        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The `group` argument has to be \"A\", \"B\" or \"both\". Argument `group` is set to: ${connection$group}."))
    }

    ### check if argument 'from' is a string and has length 1
    if (!is.character(connection$from) | length(connection$from) != 1) {
        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The `from` argument has to be a character string and not contain more than one element."))
    }

    ### check if argument 'to' is a string and has length 1
    if (!is.character(connection$to) | length(connection$to) > 1) {
        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The `to` argument has to be a character string and not contain more than one element."))
    }

    ### check if arguments 'connect_on' and 'weight' are in the correct format if connection by 'id' given
    if (connection$by == "id") {
      
        if (!length(connection$connect_on) == 1) {
            errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The argument `connect_on` is a character string and is therefore expected to contain the name of one identifier, but length is not 1. Argument `connect_on` is set to: ${connection$connect_on}."))
        }
      
        if (!is.numeric(connection$weight)) {
            errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": If `connect_on` is a character string, `weight` has to be numeric. Argument `weight` is set to: ${connection$weight}."))
        }
    }
    ### check if arguments 'connect_on' and 'weight' are in the correct format if connection by 'table' given
    else if (connection$by == "table") {

        if (!is.character(connection$weight) || !is.vector(connection$weight) || !length(connection$weight) == 1) {
            errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The argument `weight` is expected to contain the name of one column in the dataframe passed as `connect_on`. Argument `weight` is set to: ${connection$weight}."))
        }
      
        ### check if column names can be retrieved
        tryCatch(colnames(connection$connect_on),
            error = function(e) errors <- c(errors, "Connection \"${connection$from} - ${connection$to}\": Couldn't get column names of `connect_on`. The argument seems malformed. Make sure the argument passed to `connect_on` is a character string or a dataframe.")
        )

        ### check if argument 'weight' can be found in table
        if (!(connection$weight %in% colnames(connection$connect_on))) {
            errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The argument `weight` is expected to contain the name of one column in the dataframe passed as `connect_on`. Argument `weight` is set to: ${connection$weight}. The column names are: ${toString(colnames(connection$connect_on))}."))
        }
        
        else {
          
          ### check if column in 'connect_on' passed by 'weight' is numeric 
          if(!is.numeric(as.matrix(connection$connect_on[ , connection$weight]))) {
            errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The column specified in `weight` does not contain numeric data. Please provide a numeric column."))
          }
          
          ### check if duplicate entries are given in the table passed to 'connect_on'
          if (sum(duplicated(connection$connect_on[, !names(connection$connect_on) %in% c(connection$weight)]))>0){
            errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": Duplicate entries found in the dataframe specified in `connect_on`. This may cause ERRORS!"))
          }
        }
      
    }
    else if (connection$by == "none") {
        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The argument `connect_on` has to be a character string or a dataframe. Argument `connect_on` is set to: ${connection$connect_on}."))
    }

    return(errors)
}


check_sensible_connections <- function(connection, layers) {
    #' @title [INTERNAL] Check connection and layer data
    #'
    #' @description [INTERNAL] Checks if the connection defined in 'connection' makes sense in
    #' context of the defined layers.
    #'
    #' @param connection [list] Connection to check. Created by \code{\link[DrDimont]{make_connection}}
    #' @param layers [list] List of layers to check. Individual layers are created by
    #' \code{\link[DrDimont]{make_layer}} and need to be wrapped in a list.
    #'
    #' @return Character string vector containing error messages.
    #' 
    #' @keywords internal
    #' @noRd
    
    errors <- c()
    layer_names <- c()
    
    for (layer in layers) {
        layer_names <- c(layer_names, layer[['name']])
    }

    if (!connection$from %in% layer_names) {
        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The layer given in the `from` argument cannot be found in the names of the created layers. Argument `from` is set to: ${connection$from}. Layer names are: ${toString(layer_names)}."))
    }

    if (!connection$to %in% layer_names) {
        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The layer given in the `to` argument cannot be found in the names of the created layers. Argument `to` is set to: ${connection$to}. Layer names are: ${toString(layer_names)}."))
    }

    if (connection$to %in% layer_names & connection$from %in% layer_names) {
        
        if (connection$by == "id") {
            
            for (group in c("groupA", "groupB")) {
                
                for (layer in c(connection$from, connection$to)) {
                    
                    ### if group not given for layer then skip it
                    if (is.null(layers[[which(layer_names == layer)]][[group]])){next}
                  
                    ### check if argument passed to 'connect_on' can be found in identifiers of layers and groups
                    identifier_cols <- colnames(layers[[which(layer_names == layer)]][[group]][['identifiers']])
                    if (!connection$connect_on %in% identifier_cols) {
                        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": The identifier specified in `connect_on` was not found in the identifiers of layer \"${layer}\" of group \"${group}\". Argument `connect_on` is set to: ${connection$connect_on}. Identifiers are: ${toString(identifier_cols)}."))
                    }
                }
            }
        }

        if (connection$by == "table") {
            
            for (group in c("groupA", "groupB")) {
                
                for (layer in c(connection$from, connection$to)){
   
                    ### if group not given for layer then skip it
                    if (is.null(layers[[which(layer_names == layer)]][[group]])){next}
                    
                    ### check if column names in table passed to 'connect_on' can be found in identifiers of layers and groups
                    identifier_cols <- colnames(layers[[which(layer_names == layer)]][[group]][['identifiers']])
                    if (!any(colnames(connection$connect_on) %in% identifier_cols)) {
                        errors <- c(errors, stringr::str_interp("Connection \"${connection$from} - ${connection$to}\": None of the column names supplied in `connect_on` dataframe were found in the identifiers of layer \"${layer}\" of group \"${group}\". Column names in `connect_on` are: ${toString(colnames(connection$connect_on))}. Identifiers are: ${toString(identifier_cols)}."))
                    }

                }
            }
          
        }
    }
    return(errors)
}

check_drug_target <- function(drug_target_interactions) {
    #' @title [INTERNAL] Check drug target interaction data
    #'
    #' @description [INTERNAL] Checks if the data used to define the interaction between drugs and
    #' targets is valid and formatted correctly.
    #'
    #' @param drug_target_interactions [list] A named list of the drug interaction data. Created by
    #' \code{\link[DrDimont]{make_drug_target}}
    #'
    #' @return Character string vector containing error messages.
    #' 
    #' @keywords internal
    #' @noRd

    errors <- c()
    
    ### check if argument passed to 'match_on' is in column names of 'interaction_table'
    if(!drug_target_interactions$match_on %in% colnames(drug_target_interactions$interaction_table)) {
        errors <- c(errors, stringr::str_interp("Drug-target interaction: The column name specified in `match_on` cannot be found in the column names of the dataframe supplied in `interaction_table`. Argument `match_on` is set to: ${drug_target_interactions$match_on}. Column names in `interaction_table` are: ${toString(colnames(drug_target_interactions$interaction_table))}."))
    }
    
    ### check if 'drug_name' is in column names of 'interaction_table'
    if(!'drug_name' %in% colnames(drug_target_interactions$interaction_table)) {
      errors <- c(errors, stringr::str_interp("Drug-target interaction: The column name 'drug_name' is required but not found in the column names of the dataframe supplied in `interaction_table`. Column names in `interaction_table` are: ${toString(colnames(drug_target_interactions$interaction_table))}."))
    }
    
    ### check if duplicate entries are given in the table passed to 'interaction_table'
    if (sum(duplicated(drug_target_interactions$interaction_table))>0){
      errors <- c(errors, stringr::str_interp("Drug-target interaction: Duplicate entries found in the dataframe specified in `interaction_table`. This may cause ERRORS!"))
    }

    return(errors)
}

check_drug_targets_in_layers <- function(drug_target_interactions, layers) {
    #' @title [INTERNAL] Check drug target and layer data
    #'
    #' @description [INTERNAL] Checks if the parameters supplied in 'drug_target_interactions' make
    #' sense in the context of the defined layers.
    #' 
    #' @param drug_target_interactions [list] A named list of the drug interaction data. Created by
    #' \code{\link[DrDimont]{make_drug_target}}
    #' @param layers [list] List of layers to check. Individual layers are created by
    #' \code{\link[DrDimont]{make_layer}} and need to be wrapped in a list.
    #'
    #' @return Character string vector containing error messages.
    #' 
    #' @keywords internal
    #' @noRd
    
    layer_names <- c()
    for (layer in layers) {
        layer_names <- c(layer_names, layer[['name']])
    }

    errors <- c()

    if (!drug_target_interactions$target_molecules %in% layer_names) {
        errors <- c(errors, stringr::str_interp("Drug-target interaction: The defined target molecules was not found in the list of layers. Target molecules are set to: ${drug_target_interactions$target_molecules}. Layers are: ${toString(layer_names)}."))
    } else {
      
        for (group in c("groupA", "groupB")) {
            
            ### if group not given for layer then skip it
            if (is.null(layers[[which(layer_names == drug_target_interactions$target_molecules)]][[group]])){next}
        
            identifier_cols <- colnames(layers[[which(layer_names == drug_target_interactions$target_molecules)]][[group]][['identifiers']])
            if(!drug_target_interactions$match_on %in% identifier_cols) {
                errors <- c(errors, stringr::str_interp("Drug-target interaction: The column name specified in `match_on` cannot be found in the column names of the identifiers of the target layer. Argument `match_on` is set to: ${drug_target_interactions$match_on}. Column names in the identifiers of target layer \"${drug_target_interactions$target_molecules}\" of group \"${group}\" are: ${toString(identifier_cols)}."))
            }
        }
    }
    return(errors)
}


check_input <- function(layers, inter_layer_connections, drug_target_interactions) {
    #' @title Check pipeline input data for required format
    #'
    #' @description Checks if input data is valid and formatted correctly. This function is a
    #' wrapper for other check functions to be executed as the first step of the DrDimont pipeline.
    #'
    #' @param layers [list] List of layers to check. Individual layers were created by
    #' \code{\link[DrDimont]{make_layer}} and need to be wrapped in a list.
    #' @param inter_layer_connections [list] A list containing connections between layers. Each
    #' connection was created by \code{\link[DrDimont]{make_connection}} and wrapped in a list.
    #' @param drug_target_interactions [list] A named list of the drug interaction data. Created by
    #' \code{\link[DrDimont]{make_drug_target}}
    #'
    #' @return Character string vector containing error messages.
    #' 
    #' @examples
    #' data(layers_example)
    #' data(metabolite_protein_interactions)
    #' data(drug_gene_interactions)
    #' data
    #' 
    #' all_layers <- layers_example
    #' 
    #' all_inter_layer_connections = list(
    #'     make_connection(from='mrna', to='protein', connect_on='gene_name', weight=1),
    #'     make_connection(from='protein', to='phosphosite', connect_on='gene_name', weight=1),
    #'     make_connection(from='protein', to='metabolite',
    #'     connect_on=metabolite_protein_interactions, weight='combined_score'))
    #' 
    #' all_drug_target_interactions <- make_drug_target(
    #'                                     target_molecules="protein",
    #'                                     interaction_table=drug_gene_interactions,
    #'                                     match_on="gene_name")
    #' 
    #' return_errors(check_input(layers=all_layers,
    #'     inter_layer_connections=all_inter_layer_connections,
    #'     drug_target_interactions=all_drug_target_interactions))
    #' 
    #' @export

    errors <- c()
    # check layers
    n <- length(layers)
    for (i in 1:n) {
        errors <- c(errors, check_layer(layers[[i]]))
    }

    # check inter-layer connections
    for (connection in inter_layer_connections) {
        errors <- c(errors, check_connection(connection))
        errors <- c(errors, check_sensible_connections(connection, layers))
    }

    # check drug target
    errors <- c(errors, check_drug_target(drug_target_interactions))
    errors <- c(errors, check_drug_targets_in_layers(drug_target_interactions, layers))

    return(errors)
}


return_errors <- function(errors) {
    #' @title Return detected errors in the input data
    #'
    #' @description Throws an error in case errors have been passed to the function. Messages
    #' describing the detected errors are printed.
    #'
    #' @param errors [string] Character string vector containing error messages.
    #' 
    #' @return No return value, writes error messages to console
    #' 
    #' @examples
    #' data(layers_example)
    #' data(metabolite_protein_interactions)
    #' data(drug_gene_interactions)
    #' data
    #' 
    #' all_layers <- layers_example
    #' 
    #' all_inter_layer_connections = list(
    #'     make_connection(from='mrna', to='protein', connect_on='gene_name', weight=1),
    #'     make_connection(from='protein', to='phosphosite', connect_on='gene_name', weight=1),
    #'     make_connection(from='protein', to='metabolite',
    #'     connect_on=metabolite_protein_interactions, weight='combined_score'))
    #' 
    #' all_drug_target_interactions <- make_drug_target(
    #'                                     target_molecules="protein",
    #'                                     interaction_table=drug_gene_interactions,
    #'                                     match_on="gene_name")
    #' 
    #' return_errors(check_input(layers=all_layers,
    #'     inter_layer_connections=all_inter_layer_connections,
    #'     drug_target_interactions=all_drug_target_interactions))
    #' 
    #'
    #' @export

    if (!is.null(errors)) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("\n ----- \n${length(errors)} ERROR(s) detected:\n ----- \n"))
        message(paste(paste0(seq(1, length(errors), 1), ". ", errors), "\n", collapse = "\n"))
        stop("ERRORs detected! Details are printed above this error message.\n")
    }
}
