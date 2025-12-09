### * None of the functions in this file is exported

### * as.derived.mcmc.list()

#' @keywords internal
#' @noRd

as.derived.mcmc.list <- function(x) {
    if (!"derived.mcmc.list" %in% class(x)) {
        return(structure(x, class = c("derived.mcmc.list", class(x))))
    } else {
        return(x)
    }
}

### * build_uptake_mask

### ** Doc

#' Build a foodweb uptake mask based on links
#'
#' @section Link format:
#' 
#' Links are defined by a string describing connections between groups of
#' compartments and their directions using \code{->} and \code{<-}. The groups
#' being connected can be a single compartment or several compartments. Several
#' compartments can be separated either by commas or by spaces. Connection
#' descriptions can be chained in a single string.
#'
#' Some valid links are for example \code{"NH4, NO3 -> epi"} or
#' \code{"NH4, NO3 -> epi -> lepto, tricor"}.
#'
#' See the Examples section for more details.
#' 
#' @param links A vector of strings defining the trophic links between
#'     compartments (see the examples)
#' 
#' @return An uptake mask matrix with named columns and rows
#'
#' @examples
#' f <- isotracer:::build_uptake_mask(links = "NH4, NO3 -> epi -> pseph, tricor")
#' f
#'
#' # A larger foodweb
#' links <- c("NH4, NO3 -> seston, epi, CBOM, FBOM",
#'           "seston -> lepto", "epi -> petro, pseph",
#'           "CBOM, FBOM -> eudan", "CBOM -> phyllo",
#'           "FBOM -> tricor -> arg, euthy")
#' f2 <- isotracer:::build_uptake_mask(links = links)
#' f2
#'
#' @keywords internal
#' @noRd

### ** Code

build_uptake_mask <- function(links) {
    # Get the compartment names
    compartments <- compartments_from_links(links)
    nComp <- length(compartments)
    # Split the links
    elementaryLinks <- unlist(lapply(links, parse_link_string))
    stopifnot(all(grepl("->|<-", elementaryLinks)))
    pairs <- strsplit(elementaryLinks, "->|<-")
    stopifnot(all(unlist(pairs) %in% compartments))
    # Create the uptake rate mask
    mask <- matrix(0, ncol = nComp, nrow = nComp,
                   dimnames = list(compartments, compartments))
    # Add the links to the uptake rate mask
    for (k in 1:length(pairs)) {
        i <- match(pairs[[k]][1], compartments)
        j <- match(pairs[[k]][2], compartments)
        if (grepl("->", elementaryLinks[k])) {
            mask[j,i] = 1
        } else {
            mask[i,j] = 1
        }
    }
    # Return
    stopifnot(all(diag(mask) == 0)) # No rates to self
    # Return
    return(mask)
}

### * compartments_from_links()

#' Build a vector of unique compartments from link strings
#'
#' @param links A vector of link strings; see the examples for details about
#'     their format
#'
#' @return A string vector containing unique compartments present in the links
#'
#' @examples
#' links <- c("NH4, NO3 -> epi -> tricor, pseph, petro", "tricor -> arg")
#' compartments <- isotracer:::compartments_from_links(links)
#'
#' @keywords internal
#' @noRd

compartments_from_links <- function(links) {
    links <- unlist(lapply(links, parse_link_string))
    return(sort(unique(unlist(strsplit(links, "->|<-")))))
}

### * parse_link_string()

#' Parse a link string into simple links
#'
#' @param link String, see the examples for details about the format
#'
#' @return A vector containing the simple links. All simple links will have
#'     exactly one "->" or "<-" symbol, between two strings, with no
#'     whitespaces.
#'
#' @examples
#' isotracer:::parse_link_string("NH4, NO3 -> epi -> tricor, pseph, petro")
#' isotracer:::parse_link_string("tricor <- epi -> pseph")
#' isotracer:::parse_link_string("NH4 NO3 -> epi seston FBOM CBOM")
#'
#' @keywords internal
#' @noRd

parse_link_string <- function(link) {
    # Split by connections
    links <- unlist(strsplit(link, split = "<-|->"))
    stopifnot(length(links) > 1)
    # Build pairs of groups
    pairs <- list()
    kLinks <- 1
    for (i in 1:(length(links)-1)) {
        LHS <- unlist(strsplit(links[kLinks], split = " |,"))
        LHS <- LHS[LHS != ""]
        stopifnot(length(LHS) > 0)
        RHS <- unlist(strsplit(links[kLinks+1], split = " |,"))
        RHS <- RHS[RHS != ""]
        stopifnot(length(RHS) > 0)
        pairs[[kLinks]] <- list(LHS, RHS)
        kLinks <- kLinks + 1
    }
    # Get directions
    directions <- list()
    kLinkString <- 1
    for (i in 1:(length(links)-1)) {
        kLinkString <- kLinkString + nchar(links[i])
        directions[[i]] <- substr(link, start = kLinkString,
                                  stop = kLinkString + 1)
        kLinkString <- kLinkString + 2
    }
    # Function to trim whitespaces
    ## https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
    trim <- function(x) {
        return(gsub("^\\s+|\\s+$", "", x))
    }
    # Build simple pairs
    out <- list()
    kOut <- 1
    for (i in seq_along(pairs)) {
        for (k in seq_along(pairs[[i]][[1]])) {
            for (l in seq_along(pairs[[i]][[2]])) {
                out[kOut] <- paste0(trim(pairs[[i]][[1]][k]), trim(directions[[i]]),
                                   trim(pairs[[i]][[2]][l]))
                
                kOut <- kOut + 1
            }
        }
    }
    # Return
    return(unlist(out))
}

### * make_init()

### ** Doc

#' Build tibble of initial conditions for network model
#'
#' @param data Data frame
#' @param comp String, name of the column containing the names of the
#'     compartments
#' @param size String, name of the column containing the sizes of the
#'     compartments
#' @param prop String, name of the column containing values for the
#'     proportion of marked material
#' @param group_by Optional, vector of strings giving the name of the columns
#'     to use to group observations (e.g. per stream, per forest, per
#'     experiment, ...)
#' 
#' @return A tibble with one row per grouping level
#'
#' @importFrom stats setNames
#' 
#' @examples
#' library(tibble)
#' data <- tibble(comps = rep(c("NH4", "algae", "daphnia", "NH4", "algae",
#'                              "daphnia"), 2),
#'                sizes = runif(12, 1, 10),
#'                props = runif(12, 0.01, 0.05),
#'                aquariumID = c(rep("aq01", 6), rep("aq02", 6)),
#'                temperature = rep(rep(c("low", "high"), each = 3), 2))
#' data
#'
#' # No grouping variable
#' z <- isotracer:::make_init(data, "comps", "sizes", "props")
#' z$initial
#'
#' # One grouping variable
#' z <- isotracer:::make_init(data, "comps", "sizes", "props", group_by = "aquariumID")
#' z
#'
#' # Several grouping variables
#' z <- isotracer:::make_init(data, "comps", "sizes", "props",
#'                group_by = c("aquariumID", "temperature"))
#' z
#' 
#' @keywords internal
#' @noRd

### ** Code

make_init <- function(data, comp, size, prop, group_by = NULL) {
    # Extract the columns containing initial conditions information
    initial <- tibble::as_tibble(data[, c(comp, size, prop)])
    colnames(initial) <- c("compartment", "size", "proportion")
    initial$compartment <- as.character(initial$compartment)
    # If no grouping variable is given, just return a tibble with one row
    if (is.null(group_by)) {
        out <- tibble::tibble("initial" = list(initial),
                              "group" = list(NULL))
    } else {
        # Else, get group levels
        for (g in group_by) {
            data[[g]] <- as.character(data[[g]])
        }
        group <- as.character(interaction(data[, group_by]))
        groupLevels <- unique(group)
        # Build the initial conditions sets (list of data frames)
        initialSets <- lapply(seq_along(groupLevels), function(i) {
            focalGroup <- groupLevels[i]
            indices <- which(group == focalGroup)
            return(initial[indices, ])
        })
        # Build the grouping variables sets (list of named vectors)
        groupingVariablesSets <- lapply(seq_along(groupLevels), function(i) {
            focalGroup <- groupLevels[i]
            indices <- which(group == focalGroup)
            tmp <- unique(data[indices, group_by])
            stopifnot(nrow(tmp) == 1)
            tmp <- setNames(as.character(tmp), nm = group_by)
            return(tmp)
        })
        # Put initial conditions and groups together
        groups <- tibble::tibble("group" = groupingVariablesSets)
        initials <- tibble::tibble("initial" = initialSets)
        out <- tibble::as_tibble(cbind(initials, groups))
    }
    # Return
    row.names(out) <- NULL
    return(out)
}

### * nm_is_grouped()

#' Is a network model grouped into replicate units?
#'
#' @param nm Network model
#'
#' @return Boolean
#' 
#' @keywords internal
#' @noRd

nm_is_grouped <- function(nm) {
    if (!"group" %in% colnames(nm)) {
        return(FALSE)
    }
    if (nrow(nm) == 0) {
        return(FALSE)
    }
    if (nrow(nm) == 1 && is.null(nm$group[[1]])) {
        return(FALSE)
    }
    return(TRUE)
}

### * compatible_groups()

#' Check that two tibbles have compatible "group" columns for merging
#'
#' @param x First tibble
#' @param y Second tibble
#' @param error Boolean, raise error when incompatibility is encountered?
#'
#' @return TRUE if x and y are compatible. If they are not, return FALSE if
#'     \code{error} is FALSE (the default) or raise an error when \code{error}
#'     is TRUE.
#'
#' @keywords internal
#' @noRd

compatible_groups <- function(x, y, error = FALSE) {
    gx <- nm_get_groups(x)
    gy <- nm_get_groups(y)
    cx <- colnames(gx)
    cy <- colnames(gy)
    # Check that one column set is contained in the other
    if (!(all(cx %in% cy) | all(cy %in% cx))) {
        if (!error) {
            return(FALSE)
        } else {
            stop("One set of grouping variables is not contained within the other.\n",
                 "In x but not in y: ", paste0(cx[!cx %in% cy], collapse = ", "), "\n",
                 "In y but not in x: ", paste0(cy[!cy %in% cx], collapse = ", "))
        }
    }
    # Check that the levels of common columns are compatible
    cs <- intersect(cx, cy)
    for (ics in cs) {
        lcx <- unique(gx[[ics]])
        lcy <- unique(gy[[ics]])
        if (!setequal(lcx, lcy)) {
            if (!error) {
                return(FALSE)
            } else {
                stop("The levels for the grouping variable \"", ics, "\" are not the same ",
                     "across x and y.")
            }
        }
    }
    # Compatible groupings
    return(TRUE)
}

### * nm_get_groups()

#' Return a tibble with the groups of a network model
#'
#' @param nm A network model
#' @param error Boolean, if TRUE raise an error if nm is not grouped, otherwise
#'     return NULL if nm is not grouped.
#'
#' @return A tibble, with rows in the same order as in the input
#'
#' @examples
#' x <- tibble::tribble(~col, ~group,
#'              "red", c(sp = "dog", name = "toto"),
#'              "blue", c(sp = "dog", name = "tata"),
#'              "yellow", c(sp = "cat", name = "felix"))
#' isotracer:::nm_get_groups(x)
#' 
#' @keywords internal
#' @noRd

nm_get_groups <- function(nm, error = TRUE) {
    if (!nm_is_grouped(nm)) {
        if (error) {
            stop("Input is not grouped.")
        } else {
            return(NULL)
        }
    }
    rows <- purrr::map(nm$group, function(g) {
        tidyr::spread(tibble::enframe(g),
                      key = "name", value = "value")
    })
    return(dplyr::bind_rows(rows))
}

### * make_obs()

### ** Doc

#' Build observed data
#'
#' Note that nested grouping variables have to be coded appropriately by the
#' user. For example, if there are two forests, each with three plots, the
#' plots IDs cannot be "1", "2", "3" in both forests, but should be for example
#' "1-1", "1-2", "1-3" in the first forest and "2-1", "2-2" and "2-3" in the
#' second forest.
#'
#' @param data Data frame
#' @param comp String, name of the column containing the names of the
#'     compartments
#' @param size String, name of the column containing the sizes of the
#'     compartments
#' @param prop String, name of the column containing values for the proportion
#'     of heavy tracer
#' @param time String, name of the column containing values for the time
#' @param group_by Optional, vector of strings giving the name of the columns
#'     to use to group observations (e.g. per stream, per forest, per
#'     experiment, ...). The name \code{tracerObservations} is reserved and
#'     should not be used.
#'
#' @return A tibble with one row per grouping level
#'
#' @importFrom stats setNames
#'
#' @examples
#' library(tibble)
#' data <- tibble(comps = rep(c("NH4", "algae", "daphnia", "NH4", "algae",
#'                              "daphnia"), 2),
#'                sizes = runif(12, 1, 10),
#'                props = runif(12, 0.01, 0.05),
#'                time = runif(12, 0, 5),
#'                aquariumID = c(rep("aq01", 6), rep("aq02", 6)),
#'                temperature = rep(rep(c("low", "high"), each = 3), 2))
#' data
#'
#' # No grouping variable
#' z <- isotracer:::make_obs(data, "comps", "sizes", "props", "time")
#' z$observations
#'
#' # One grouping variable
#' z <- isotracer:::make_obs(data, "comps", "sizes", "props", "time",
#'                           group_by = "aquariumID")
#' z
#'
#' # Several grouping variables
#' z <- isotracer:::make_obs(data, "comps", "sizes", "props", "time", 
#'                group_by = c("aquariumID", "temperature"))
#' z
#' 
#' @keywords internal
#' @noRd

### ** Code

make_obs <- function(data, comp, size, prop, time, group_by = NULL) {
    # Extract the columns containing observations
    observations <- tibble::as_tibble(data[, c(comp, size, prop, time)])
    colnames(observations) <- c("compartment", "size", "proportion", "time")
    observations$compartment <- as.character(observations$compartment)
    # If no grouping variable is given, just return a tibble with one column
    # and one row
    if (is.null(group_by)) {
        out <- tibble::tibble("observations" = list(observations),
                              "group" = list(NULL))
    } else {
        # Else, build a tibble with the grouping applied and use dplyr::group_by_
        for (g in group_by) {
            data[[g]] <- as.character(data[[g]])
        }
        group <- as.character(interaction(data[, group_by]))
        groupLevels <- unique(group)
        # Build the observation sets (list of data frames)
        observationSets <- lapply(seq_along(groupLevels), function(i) {
            focalGroup <- groupLevels[i]
            indices <- which(group == focalGroup)
            return(observations[indices, ])
        })
        # Build the grouping variables sets (list of named vectors)
        groupingVariablesSets <- lapply(seq_along(groupLevels), function(i) {
            focalGroup <- groupLevels[i]
            indices <- which(group == focalGroup)
            tmp <- unique(data[indices, group_by])
            stopifnot(nrow(tmp) == 1)
            tmp <- setNames(as.character(tmp), nm = group_by)
            return(tmp)
        })
        # Put observations and groups together
        groups <- tibble::tibble("group" = groupingVariablesSets)
        observations <- tibble::tibble("observations" = observationSets)
        out <- tibble::as_tibble(cbind(observations, groups))
    }
    # Return
    row.names(out) <- NULL
    return(out)
}

### * merge_nm_by_groups()

#' Merge a network model object and a tibble taking grouping into account
#'
#' @param nm The original \code{networkModel} object.
#' @param tib The tibble to merge into \code{nm}.
#' @param destination String, name of the column to merge into the network
#'     model.
#' @param tib_name String, used when an error is raised.
#'
#' @return The updated network model object.
#' 
#' @keywords internal
#' @noRd

merge_nm_by_group <- function(nm, tib, destination, tib_name = "unnamed tib") {
    nm_grouped <- nm_is_grouped(nm)
    tib_grouped <- nm_is_grouped(tib)
    previous_priors <- attr(nm, "priors")
    # nm not grouped, tib not grouped
    if (!nm_grouped & !tib_grouped) {
        stopifnot(nrow(nm) == 1 & nrow(tib) == 1)
        nm[[destination]][[1]] <- tib[[destination]][[1]]
    }
    # nm not grouped, tib grouped
    if (!nm_grouped & tib_grouped) {
        stopifnot(nrow(nm) == 1)
        nm_orig <- nm
        for (j in seq_len(nrow(tib) - 1)) {
            nm <- dplyr::bind_rows(nm, nm_orig)
        }
        nm[[destination]] <- tib[[destination]]
        nm$group <- tib$group
    }
    # nm grouped, tib not grouped
    if (nm_grouped & !tib_grouped) {
        stopifnot(nrow(tib) == 1)
        tib_orig <- tib
        for (j in seq_len(nrow(nm) - 1)) {
            tib <- dplyr::bind_rows(tib,
                                    tib_orig)
        }
        nm[[destination]] <- tib[[destination]]
    }
    # nm grouped and tib grouped
    if (nm_grouped & tib_grouped) {
        if (!compatible_groups(nm, tib)) {
         stop("Network model and ", tib_name, " do not have compatible grouping.")
        }
        # Merge
        nm_g <- nm_get_groups(nm)
        tib_g <- nm_get_groups(tib)
        cols <- intersect(colnames(nm_g), colnames(tib_g))
        allCols <- unique(c(colnames(nm_g), colnames(tib_g)))
        stopifnot(!".myRowNumberNm" %in% colnames(nm_g))
        stopifnot(!".myRowNumberTib" %in% colnames(tib_g))
        nm_g[[".myRowNumberNm"]] <- seq_len(nrow(nm_g))
        tib_g[[".myRowNumberTib"]] <- seq_len(nrow(tib_g))
        merged <- dplyr::full_join(nm_g, tib_g, by = cols)
        out_nm <- nm[merged[[".myRowNumberNm"]], ]
        out_tib <- tib[merged[[".myRowNumberTib"]], ]
        out_nm[[destination]] <- out_tib[[destination]]
        # Update groups
        groups <- merged[, allCols]
        groups <- lapply(seq_len(nrow(groups)), function(i) {
            unlist(groups[i, ])
        })
        out_nm[["group"]] <- groups
        nm <- out_nm
    }
    # Return
    attr(nm, "priors") <- previous_priors
    return(nm)
}

### * get_n_cores()

# https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions

# From https://cran.r-project.org/doc/manuals/r-release/R-ints.html (on 2021-09-13):
#
# "_R_CHECK_LIMIT_CORES_ If set, check the usage of too many cores in package
# parallel. If set to 'warn' gives a warning, to 'false' or 'FALSE' the check
# is skipped, and any other non-empty value gives an error when more than 2
# children are spawned. Default: unset (but 'TRUE' for CRAN submission checks)."

#' Determine the number of cores to use for parallelizable functions
#'
#' @param cores Number of cores to use. Default is \code{NULL}, which means to
#'     use the value stored in \code{options()[["mc.cores"]]} (or 1 if this
#'     value is not set).
#'
#' @return An integer, the number of cores to use for parallel computations.
#'
#' @keywords internal
#' @noRd

get_n_cores <- function(cores = NULL) {
    n_cores_max <- parallel::detectCores()
    n_cores_options <- options()[["mc.cores"]]
    on_cran_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    on_cran_limit <- (nzchar(on_cran_limit) && on_cran_limit == "TRUE")
    if (is.null(cores)) { cores <- n_cores_options }
    if (is.null(cores)) { cores <- 1 }
    # The condition below is a convoluted way to check that cores is an integer
    # (because the simpler e.g. "is.integer(2)" is FALSE).
    # See also ?is.integer and the is.wholenumber() function in the examples.
    if ((!is.numeric(cores)) || (!((as.integer(cores) - cores) == 0)) || !(cores > 0)) {
        stop("`cores` must be an integer > 0.")
    }
    if (cores > n_cores_max) {
        warning(paste0("The provided number of cores (", cores, ") is greater ",
                       "than the number of available cores (", n_cores_max,
                       ").\n"),
                "Using the number of available cores instead.")
        cores <- n_cores_max
    }
    if (cores > 1) {
        if (.Platform$OS.type == "windows") {
            warning("Multiple core implementation not working on Windows; using 1 core.")
            cores <- 1
        }
    }
    if (on_cran_limit) {
        message("CRAN limit for the number of cores was detected. Using cores <- min(cores, 2).")
        cores <- min(cores, 2)
    }
    return(cores)
}
