check_layer <- function(layer) {
    #' Check layer input
    #'
    #' (INTERNAL) Checks if the data used to create a network layer is valid and has the right
    #' format
    #'
    #' @param layer layer to check. Created by \code{\link{make_layer}}
    #' @return Character string vector containing error messages.
    #' @export
    #' @examples
    #' data(metabolite_data)
    #' metabolite_layer = make_layer(name="metabolite",
    #' metabolite_data$group1$data,
    #' metabolite_data$group2$data,
    #' metabolite_data$group1$identifiers,
    #' metabolite_data$group2$identifiers)
    #' return_errors(check_layer(metabolite_layer))
    #'
    errors <- c()
    for (j in 1:2) {
        # do length of components and length of identifiers match?
        if (dim(layer[[j]][['data']])[2] != dim(layer[[j]][['identifiers']])[1]) {
            errors <- c(errors, stringr::str_interp("Layer ${layer[['name']]}, group ${j}: The number of columns in the supplied data does not equal the number of provided identifiers."))
        }
        # do the identifiers contain an id called 'layer'? layer attribute is assigned in the pipeline
        if ('layer' %in% colnames(layer[[j]][['identifiers']])) {
            errors <- c(errors, stringr::str_interp("Identifiers cannot be named 'layer'. Please choose a different name for the identifier 'layer' in layer ${layer[['name']]}, group ${j}."))
        }
    }
    return(errors)
}

check_connection <- function(connection) {
    #' Checks connection
    #'
    #' (INTERNAL) Checks if the data given to create an inter-layer connection is valid and has the
    #' right input format
    #'
    #' @param connection Connection to check. Created by \code{\link{make_connection}}
    #' @return Character string vector containing error messages.
    #' @export
    #' @examples
    #' con = make_connection("mrna", "protein", connect_on="gene_name")
    #' return_errors(check_connection(con))
    #'

    errors <- c()

    # are group arguments '1', '2' or 'both'?
    if (as.character(connection$group) != '1' & as.character(connection$group) != '2' & connection$group != 'both') {
        errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): The 'group' argument has to be '1', '2' or 'both'. It was set to ${connection$group}"))
    }

    # is argument 'from' a string and length 1?
    if (!is.character(connection$from) | length(connection$from) != 1) {
        errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): The 'from' argument has to be a character string and not contain more than one element."))
    }

    # is argument 'to' a string and length 1?
    if (!is.character(connection$to) | length(connection$to) > 1) {
        errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): The 'to' argument has to be a character string and not contain more than one element."))
    }

    if (connection$by == "id") {
        if (!length(connection$connect_on) == 1) {
            errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}):
                               The argument 'connect_on' is a character string and is
                               therefore expected to contain the name of exactly
                               one identifier, but length was not 1.
                               'connect_on' was ${connection$connect_on}."))
        }
        if (!is.numeric(connection$weight)) {
            errors <- c(errors, stringr::str_interp(
            "Connection (${connection$from}-${connection$to}):
     If 'connect_on' is a character string, 'weight' has to be numeric."))
        }
    }
    else if (connection$by == "table") {

        if (!is.character(connection$weight) || !is.vector(connection$weight) || !length(connection$weight) == 1) {
            errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}):
                             The argument 'weight' is expected to contain the
                             name of exactly one column in the table passed as
                             'connect_on'.
                             'weight' was ${connection$weight}."))
        }

        tryCatch(colnames(connection$connect_on),
                 error = function(e) errors <- c(errors, "Couldn't get column names of 'connect_on'.
                                      Argument seems malformed. Make sure 'connect_on'
                                      is a character string or a table.")
        )

        if (!(connection$weight %in% colnames(connection$connect_on))) {
            errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}):
                             The argument 'weight' is expected to contain the
                             name of exactly one column in the table passed as
                             'connect_on'.
                             'weight' was ${connection$weight}.
                              Column names were ${toString(colnames(connection$connect_on))}"))
        }

        if(!is.numeric(as.matrix(connection$connect_on[ , connection$weight]))) {
            errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): The column specified in 'weight' does not contain numeric data. Please provide a numeric column."))
        }
    }

    return(errors)
}

check_sensible_connections <- function(connection, layers) {
    #' Check connection and layer data
    #'
    #' @description (INTERNAL) Checks if the connection defined in 'connection' makes sense in
    #' context of the defined layers.
    #'
    #' @param connection Connection to check. Created by \code{\link{make_connection}}
    #' @param layers List of layers to check. Individual layers are created by
    #' \code{\link{make_layer}} and need to be wrapped in a list.
    #'
    #' @return Character string vector containing error messages.
    #' @export
    #' @examples
    #' data(mrna_data)
    #' mrna_layer = make_layer(name="mrna",
    #' mrna_data$group1$data, mrna_data$group2$data,
    #' mrna_data$group1$identifiers,
    #' mrna_data$group2$identifiers)
    #' data(protein_data)
    #' protein_layer = make_layer(name="protein",
    #' protein_data$group1$data,
    #' protein_data$group2$data,
    #' protein_data$group1$identifiers,
    #' protein_data$group2$identifiers)
    #' con = make_connection("mrna", "protein", connect_on="gene_name")
    #' return_errors(check_sensible_connections(con, layers=list(mrna_layer, protein_layer)))
    #'
    errors <- c()
    layer_names <- c()
    for (layer in layers) {
        layer_names <- c(layer_names, layer[['name']])
    }

    if (!connection$from %in% layer_names) {
        errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): The layer given in the 'from' argument cannot be found in the names of the created layers. 'from' was ${connection$from}. Layer names were: ${toString(layer_names)}."))
    }

    if (!connection$to %in% layer_names) {
        errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): The layer given in the 'to' argument cannot be found in the names of the created layers. 'from' was ${connection$to}. Layer names were: ${toString(layer_names)}."))
    }

    if (connection$to %in% layer_names & connection$from %in% layer_names) {
        if (connection$by == 'id') {
            for (i in 1:2) {
                for (layer in c(connection$from, connection$to)) {
                    identifier_cols <- colnames(layers[[which(layer_names == layer)]][[i]][['identifiers']])
                    if (!connection$connect_on %in% identifier_cols) {
                        errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): The identifier specified in 'connect_on' was not found in the identifiers of layer ${layer} of group ${i}. Please correct the 'connect_on' argument or the names of the layer identifiers. 'connect_on' was ${connection$connect_on}. Identifiers were: ${toString(identifier_cols)}"))
                    }
                }
            }
        }

        if (connection$by == 'table') {
            for (i in 1:2) {
                for (layer in c(connection$from, connection$to)) {
                    identifier_cols <- colnames(layers[[which(layer_names == layer)]][[i]][['identifiers']])
                    if (!any(colnames(connection$connect_on) %in% identifier_cols)) {
                        errors <- c(errors, stringr::str_interp("Connection (${connection$from}-${connection$to}): None of the columnnames supplied in 'connect_on' were found in the identifiers of layer ${layer} of group ${i}. Please correct the columnnames of the table passed to 'connect_on' or the names of the layer identifiers. Columnames in 'connect_on' were ${toString(colnames(connection$connect_on))}. Identifiers were: ${toString(identifier_cols)}"))
                    }

                }
            }
        }
    }
    return(errors)
}

check_drug_target <- function(drug_target_interaction) {
    #'Check drug target interaction data
    #'
    #' @description (INTERNAL) Checks if the data used to define interaction between drugs and
    #' targets is valid and formatted correctly.
    #'
    #' @param drug_target_interaction A named list of the drug interaction data. Created by
    #' \code{\link{make_drug_target}}
    #'
    #'@return Character string vector containing error messages.
    #'@examples
    #'data(drug_gene_interactions)
    #'drug_target_interaction <- make_drug_target(target_molecules='protein',
    #'interaction_table=drug_gene_interactions,
    #'match_on='gene_name')
    #'return_errors(check_drug_target(drug_target_interaction))
    #'@export
    #'

    errors <- c()
    if(!drug_target_interaction$match_on %in% colnames(drug_target_interaction$interaction_table)) {
        errors <- c(errors, stringr::str_interp("Drug-target interaction: The columnname specified in 'match_on' cannot be found in the columnnames of the table supplied in 'interaction_table'. 'match_on' is ${drug_target_interaction$match_on}, columnnames in 'interaction_table' are ${toString(colnames(drug_target_interaction$interaction_table))}"))
    }

    return(errors)
}

check_drug_targets_in_layers <- function(drug_target_interaction, layers) {
    #'Check drug target and layer data
    #'
    #'@description (INTERNAL) Checks if the parameters supplied in 'drug_target_interaction' makes
    #' sense in the context of the defined layers.
    #'@param drug_target_interaction A named list of the drug interaction data. Created by
    #'\code{\link{make_drug_target}}
    #'@param layers List of layers to check. Individual layers are created by
    #' \code{\link{make_layer}} and need to be wrapped in a list.
    #'
    #'@return Character string vector containing error messages.
    #'@export
    #'@examples
    #'data(layers_example)
    #'layers <- layers_example
    #'data(drug_gene_interactions)
    #'drug_target_interaction <- make_drug_target(target_molecules='protein',
    #'interaction_table=drug_gene_interactions,
    #'match_on='gene_name')
    #'return_errors(check_drug_targets_in_layers(drug_target_interaction, layers))
    #'
    layer_names <- c()
    for (layer in layers) {
        layer_names <- c(layer_names, layer[['name']])
    }

    errors <- c()

    if (!drug_target_interaction$target_molecules %in% layer_names) {
        errors <- c(errors, stringr::str_interp("Drug-target interaction: The defined target was not found in the list of layers. Targets molecules are ${drug_target_interaction$target_molecules}, layers are ${toString(layer_names)}. Please correct target molecule string or the layer names."))
    } else {
        for (i in 1:2) {
            identifier_cols <- colnames(layers[[which(layer_names == drug_target_interaction$target_molecules)]][[i]][['identifiers']])
            if(!drug_target_interaction$match_on %in% identifier_cols) {
                errors <- c(errors, stringr::str_interp("Drug-target interaction: The columnname specified in 'match_on' cannot be found in the columnnames of the identifiers of the target layer. 'match_on' is ${drug_target_interaction$match_on}, columnnames in the identifiers of target layer ${drug_target_interaction$target_molecules} of group ${i} are ${toString(identifier_cols)}"))
            }
        }
    }
    return(errors)
}

check_input <- function(layers,
                        inter_layer_connections,
                        drug_target_interaction
                        ) {
    #' Check pipeline input data for required format
    #'
    #' @description Checks if input data is valid and formatted correctly. This function is a
    #' wrapper for other check functions to be executed as first step of the molnet pipeline.
    #'
    #' @param layers List of layers to check. Individual layers were created by
    #' \code{\link{make_layer}} and need to be wrapped in a list.
    #' @param inter_layer_connections A list containing connections between layers. Each
    #' connection was created by \code{\link{make_connection}} and wrapped in a list.
    #' @param drug_target_interaction A named list of the drug interaction data. Created by
    #' \code{\link{make_drug_target}}
    #'
    #' @return Character string vector containing error messages.
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
    errors <- c(errors, check_drug_target(drug_target_interaction))
    errors <- c(errors, check_drug_targets_in_layers(drug_target_interaction, layers))

    return(errors)
}

return_errors <- function(errors) {
    #' Return detected errors
    #'
    #' @description Throws an error in case errors have been passed to the function. Messages
    #' describing the detected errors are printed.
    #'
    #' @param errors Character string vector containing error messages.
    #' @export
    #' @examples
    #' layer <- molnet::layers_example[[2]]
    #' return_errors(check_layer(layer))
    #'

    if (!is.null(errors)) {
        message(stringr::str_interp("\n ----- \n${length(errors)} Error(s) detected:\n ----- \n"))
        message(paste(paste0(seq(1, length(errors), 1), ". ", errors), "\n", collapse = "\n"))
        stop("Errors detected! Details are printed above this error message.\n")
    }
}
