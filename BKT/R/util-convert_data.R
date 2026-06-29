# MARK: convert_data
convert_data <- function(data, skill_name, defaults, model_type,
                         gs_refs, resource_refs, return_df, folds) {
    if (is.null(model_type)) {
        multilearn <- multiprior <- multipair <- multigs <- FALSE
    } else {
        multilearn <- model_type[1]
        multiprior <- model_type[2]
        multipair <- model_type[3]
        multigs <- model_type[4]
    }
    if (is.character(skill_name)) {
        if (length(skill_name) > 1) {
            skill_name <- paste(skill_name, collapse = "|")
        }
    }
    df <- data
    # default column names for cognitive tutors
    ct_default <- list(
        order_id = "Row",
        skill_name = "KC(Default)",
        correct = "Correct First Attempt",
        user_id = "Anon Student Id",
        multilearn = "Problem Name",
        multiprior = "Anon Student Id",
        multipair = "Problem Name",
        multigs = "Problem Name",
        folds = "Anon Student Id"
    )
    # integrate custom defaults with default assistments/ct columns if they are still unspecified
    if (is.null(defaults)) {
        defaults <- list()
    } else {
        # stop("not implemented")
        # TBD: check defaults's key
    }

    df_columns <- colnames(df)
    for (key in names(ct_default)) {
        value <- ct_default[[key]]
        if (key %in% df_columns) {
            defaults[[key]] <- key
        } else if (value %in% df_columns) {
            defaults[[key]] <- value
        }
    }
    # change df columns names
    # df_columns <- colnames(df)
    # for (key in names(defaults)) {
    #     value <- defaults[[key]]
    #     df_columns[df_columns == value] <- key
    # }
    # colnames(df) <- df_columns

    # sort df by order_id
    df[[defaults[["order_id"]]]] <- as.numeric(df[[defaults[["order_id"]]]])
    df <- df[order(df[[defaults[["order_id"]]]]), ]

    required_columns <- c("user_id", "correct", "skill_name")
    for (col in required_columns) {
        if (!(defaults[[col]] %in% names(df))) {
            stop(paste("The required column (", col, ") is missing in the dataframe."))
        }
    }
    # order by user_id
    df[[defaults[["user_id"]]]] <- as.character(df[[defaults[["user_id"]]]])
    df <- df[order(ascii_order(df[[defaults[["user_id"]]]]), df[[defaults[["order_id"]]]]), ]
    if ("original" %in% colnames(df)) {
        df <- df[df$original == 1, ]
    }
    df[[defaults[["skill_name"]]]] <- as.character(df[[defaults[["skill_name"]]]])

    tryCatch(
        {
            df[[defaults[["correct"]]]] <- as.integer(df[[defaults[["correct"]]]])
        },
        warning = function(w) {
            stop("Invalid Data In Specified Corrects Column")
        },
        error = function(e) {
            stop("Invalid Data In Specified Corrects Column")
        }
    )

    # handle skills
    datas <- list()
    skill_name <- paste0("^(", skill_name, ")$")
    all_skills <- unique(na.omit(df[[defaults[["skill_name"]]]]))
    all_skills <- as.character(all_skills)
    all_skills <- all_skills[grepl(skill_name, all_skills)]
    if (length(all_skills) == 0) {
        stop("No matching skills")
    }
    for (skill_ in all_skills) {
        if (is.null(resource_refs) || !(skill_ %in% names(resource_refs))) {
            resource_ref <- NULL
        } else {
            resource_ref <- resource_refs[[skill_]][["resource_names"]]
        }
        if (is.null(gs_refs) || !(skill_ %in% names(gs_refs))) {
            gs_ref <- NULL
        } else {
            gs_ref <- gs_refs[[skill_]][["gs_names"]]
        }
        df3 <- df[df[[defaults[["skill_name"]]]] == skill_, ]

        if (nrow(df3) == 0) {
            stop("Incorrect Skill or Dataset Specified")
        }
        stored_index <- rownames(df3)
        multiprior_index <- NULL
        correct_values <- unique(df3[[defaults[["correct"]]]])
        if (!all(correct_values %in% c(-1, 0, 1))) {
            stop("Correctness must be -1 (no response), 0 (incorrect), or 1 (correct)")
        }
        df3[[defaults[["correct"]]]] <- df3[[defaults[["correct"]]]] + 1
        data <- as.matrix(df3[[defaults[["correct"]]]])
        Data <- list()
        user_ids <- df3[[defaults[["user_id"]]]]
        lengths <- as.integer(table(factor(user_ids, levels = unique(user_ids))))

        starts <- integer(length(lengths))
        starts[1] <- 1
        for (i in 2:length(lengths)) {
            starts[i] <- starts[i - 1] + lengths[i - 1]
        }

        if (multipair + multiprior + multilearn > 1) {
            stop("cannot specify more than 1 resource handling (only one of multipair, multiprior or multilearn)")
        }
        if (multipair) {
            if (!"multipair" %in% names(defaults)) {
                stop("multipair default column not specified")
            } else if (!(defaults[["multipair"]] %in% colnames(df3))) {
                stop("specified multipair default column not in data")
            }

            resources <- rep(1L, length(data))

            if (is.null(resource_ref)) {
                new_resource_ref <- list("Default" = 1) # no pair
            } else {
                new_resource_ref <- resource_ref
            }
            for (i in seq_len(nrow(df3))) {
                # for the first entry of a new student, no pair
                if (i == 1 || df3[i, defaults[["user_id"]]] != df3[i - 1, defaults[["user_id"]]]) {
                    resources[i] <- 1
                } else {
                    # each pair is keyed via "[item 1] [item 2]"
                    k <- paste0(as.character(df3[i, defaults[["multipair"]]]), " ", as.character(df3[i - 1, defaults[["multipair"]]]))
                    if (!is.null(resource_ref) && !(k %in% names(resource_ref))) {
                        stop("Pair ", k, " not fitted")
                    }
                    if (!(k %in% names(new_resource_ref))) {
                        # form the resource reference as we iterate through the dataframe, mapping each new pair to a number [1, # total pairs]
                        new_resource_ref[[k]] <- length(new_resource_ref) + 1
                    }
                    resources[i] <- new_resource_ref[[k]]
                }
            }
            if (is.null(resource_ref)) {
                resource_ref <- new_resource_ref
            }
        } else if (multiprior) {
            if (!"multiprior" %in% names(defaults)) {
                stop("multiprior default column not specified")
            } else if (!(defaults[["multiprior"]] %in% colnames(df3))) {
                stop("specified multiprior default column not in data")
            }

            resources <- rep(1L, length(data) + length(starts))
            new_data <- rep(0L, length(data) + length(starts))
            # create new resources [2, #total + 1] based on how student initially responds
            all_priors <- unique(df3[[defaults[["multiprior"]]]])
            all_priors <- sort(all_priors)

            if (is.null(resource_ref)) {
                resource_ref <- list()
                resource_ref[["Default"]] <- 1
                resource_ref <- c(resource_ref, setNames(seq(2, length(all_priors) + 1), all_priors))
            } else {
                for (i in all_priors) {
                    if (!(i %in% names(resource_ref))) {
                        stop("Prior ", i, " not fitted")
                    }
                }
            }

            all_resources <- sapply(df3[[defaults[["multiprior"]]]], function(x) resource_ref[[as.character(x)]])

            # create phantom timeslices with resource 2 or 3 in front of each new student based on their initial response
            for (i in seq_along(starts)) {
                new_data[(i + starts[i]):(i + starts[i] + lengths[i] - 1)] <- data[(starts[i]):(starts[i] + lengths[i] - 1)]
                resources[i + starts[i] - 1] <- all_resources[starts[i]]
                resources[(i + starts[i]):(i + starts[i] + lengths[i] - 1)] <- rep(1L, lengths[i])
                starts[i] <- starts[i] + i - 1
                lengths[i] <- lengths[i] + 1
            }
            multiprior_index <- sapply(seq_along(starts), function(i) starts[i] - 1)
            data <- new_data
        } else if (multilearn) {
            if (!"multilearn" %in% names(defaults)) {
                stop("multilearn default column not specified")
            } else if (!(defaults[["multilearn"]] %in% colnames(df3))) {
                stop("specified multilearn default column not in data")
            }

            all_learns <- unique(df3[[defaults[["multilearn"]]]])
            all_learns <- sort(all_learns)

            if (is.null(resource_ref)) {
                resource_ref <- setNames(1:length(unique(df3[[defaults[["multilearn"]]]])), all_learns)
            } else {
                for (i in all_learns) {
                    if (!(i %in% names(resource_ref))) {
                        stop(paste("Learn rate", i, "not fitted"))
                    }
                }
            }

            resources <- sapply(df3[[defaults[["multilearn"]]]], function(x) resource_ref[[x]])
            resources <- as.integer(resources)
            resources <- matrix(resources, ncol = 1)
        } else {
            resources <- rep(1, length(data))
            resources <- as.integer(unlist(resources))
            resources <- matrix(resources, ncol = 1)
        }

        if (multigs) {
            if (!("multigs" %in% names(defaults))) {
                stop("multigs default column not specified")
            } else if (!(defaults[["multigs"]] %in% colnames(df3))) {
                stop("specified multigs default column not in data")
            }
            all_guess <- unique(df3[[defaults[["multigs"]]]])
            all_guess <- sort(all_guess)

            # map each new guess/slip case to a row [0, # total]
            if (is.null(gs_ref)) {
                gs_ref <- as.list(setNames(seq_along(all_guess), all_guess))
            } else {
                for (i in all_guess) {
                    if (!(i %in% names(gs_ref))) {
                        stop("Guess rate ", i, " not previously fitted")
                    }
                }
            }
            data_ref <- sapply(df3[[defaults[["multigs"]]]], function(x) gs_ref[[x]])
            # make data n-dimensional, fill in corresponding row and make other non-row entries 0
            data_temp <- matrix(0, nrow = length(unique(df3[[defaults[["multigs"]]]])), ncol = nrow(df3))
            for (i in seq_len(ncol(data_temp))) {
                data_temp[data_ref[i], i] <- data[i]
            }
            Data$data <- data_temp
        } else {
            data <- list(data)
            data <- as.integer(unlist(data))
            data_matrix <- matrix(data, nrow = 1)
            Data <- list("data" = data_matrix)
        }

        if (!multilearn && !multipair && !multiprior) {
            resource_ref <- list()
            resource_ref[["default"]] <- 1
        }
        if (!multigs) {
            gs_ref <- list()
            gs_ref[["default"]] <- 1
        }
        Data$starts <- starts
        Data$lengths <- lengths
        Data$resources <- resources
        Data$resource_names <- resource_ref
        Data$gs_names <- gs_ref
        Data$index <- stored_index
        Data$multiprior_index <- multiprior_index
        if (folds) {
            Data[["folds"]] <- as.integer(df3[["folds"]])
        }
        datas[[skill_]] <- Data
    }
    if (return_df) {
        return(list(datas = datas, df = df))
    }
    return(datas)
}

convert_data_path <- function(data_path, skill_name, defaults, model_type,
                              gs_refs, resource_refs, return_df, folds) {
    data <- read.csv(data_path, header = TRUE, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
    if (is.null(data)) {
        stop("Failed to load the CSV file.")
    }
    return(convert_data(data, skill_name, defaults, model_type, gs_refs, resource_refs, return_df, folds))
}

# const require to sort string by ascii
ascii_order <- function(x) {
    sapply(seq_along(x), function(i) {
        paste0(sprintf("%03d", utf8ToInt(substr(x[i], 1, nchar(x[i])))), collapse = "")
    })
}
