#' @docType class
#' @title Networks with Unit-Level Attributes (R6 Class)
#' @description
#' The `iglm.data` class is a container for storing, validating, and analyzing
#' unit-level attributes (x_attribute, y_attribute) and connections (z_network).
#' @import R6
#' @import ragg
#' @importFrom igraph graph_from_edgelist layout_with_fr vcount add_vertices V plot.igraph
#' @importFrom grDevices colorRampPalette adjustcolor
#' @importFrom graphics legend plot
#' @importFrom Matrix sparseMatrix spMatrix bdiag diag
#' @export
iglm.data_generator <- R6::R6Class("iglm.data",
  private = list(
    .x_attribute = NULL,
    .y_attribute = NULL,
    .z_network = NULL,
    .neighborhood = NULL,
    .overlap = NULL,
    .fix_z_alocal = NULL,
    .directed = NULL,
    .n_units = NULL,
    .family_x = NULL,
    .family_y = NULL,
    .scale_x = NULL,
    .scale_y = NULL,
    .fix_x = NULL,
    .fix_z = NULL,
    .descriptives = NULL,
    .label_x = "x",
    .label_y = "y",
    .label_z = "z",
    .validate = function() {
      errors <- character()
      # Check labels
      if (!is.character(private$.label_x) || length(private$.label_x) != 1 || is.na(private$.label_x) || nchar(trimws(private$.label_x)) == 0) {
        errors <- c(errors, "'label_x' must be a single non-empty character string.")
      }
      if (!is.character(private$.label_y) || length(private$.label_y) != 1 || is.na(private$.label_y) || nchar(trimws(private$.label_y)) == 0) {
        errors <- c(errors, "'label_y' must be a single non-empty character string.")
      }
      if (!is.character(private$.label_z) || length(private$.label_z) != 1 || is.na(private$.label_z) || nchar(trimws(private$.label_z)) == 0) {
        errors <- c(errors, "'label_z' must be a single non-empty character string.")
      }
      # Check scales
      if (length(private$.scale_x) != 1 || private$.scale_x <= 0) {
        errors <- c(errors, "'scale_x' must be a single positive number.")
      }
      if (length(private$.scale_y) != 1 || private$.scale_y <= 0) {
        errors <- c(errors, "'scale_y' must be a single positive number.")
      }

      # Check types
      valid_families <- c("binomial", "poisson", "normal")
      if (length(private$.family_x) != 1 || !private$.family_x %in% valid_families) {
        errors <- c(errors, "family_x must be one of 'binomial', 'poisson', or 'normal'.")
      } else if (private$.family_x != "normal" && private$.scale_x != 1) {
        warning("family_x is not 'normal', but scale_x is not 1. Setting scale_x to 1.")
        private$.scale_x <- 1
      }
      if (length(private$.family_y) != 1 || !private$.family_y %in% valid_families) {
        errors <- c(errors, "family_y must be one of 'binomial', 'poisson', or 'normal'.")
      } else if (private$.family_y != "normal" && private$.scale_y != 1) {
        warning("family_y is not 'normal', but scale_y is not 1. Setting scale_y to 1.")
        private$.scale_y <- 1
      }

      # Check attribute lengths
      if (length(private$.x_attribute) != private$.n_units) {
        errors <- c(errors, "Length of 'x_attribute' must be equal to 'n_units'.")
      }
      if (length(private$.y_attribute) != private$.n_units) {
        errors <- c(errors, "Length of 'y_attribute' must be equal to 'n_units'.")
      }
      if (!inherits(private$.descriptives, "list")) {
        errors <- c(errors, "'descriptives' must be a list.")
      }

      # Check for missing values in attributes
      if (any(is.na(private$.x_attribute))) {
        errors <- c(errors, "'x_attribute' contains missing (NA/NaN) values.")
      }
      if (any(is.na(private$.y_attribute))) {
        errors <- c(errors, "'y_attribute' contains missing (NA/NaN) values.")
      }

      # Check attribute value constraints
      if (private$.family_x == "binomial" && !any(is.na(private$.x_attribute)) && !all(private$.x_attribute %in% c(0, 1))) {
        errors <- c(errors, "For 'binomial' family, 'x_attribute' must be a binary vector.")
      }
      # browser()
      if (private$.family_x == "poisson" && !any(is.na(private$.x_attribute)) && !all(floor(private$.x_attribute) == private$.x_attribute & private$.x_attribute >= 0)) {
        errors <- c(errors, "For 'poisson' family, 'x_attribute' must be a vector of non-negative integers.")
      }
      if (private$.family_y == "binomial" && !any(is.na(private$.y_attribute)) && !all(private$.y_attribute %in% c(0, 1))) {
        errors <- c(errors, "For 'binomial' family, 'y_attribute' must be a binary vector.")
      }

      if (!is.logical(private$.fix_z_alocal)) {
        stop("`fix_z_alocal` must be a logical value (TRUE or FALSE).", call. = FALSE)
      }
      if (private$.family_y == "poisson" && !any(is.na(private$.y_attribute)) && !all(floor(private$.y_attribute) == private$.y_attribute & private$.y_attribute >= 0)) {
        errors <- c(errors, "For 'poisson' family, 'y_attribute' must be a vector of non-negative integers.")
      }
      # Check z_network format
      if (!is.matrix(private$.z_network) && !inherits(private$.z_network, "Matrix")) {
        errors <- c(errors, "'z_network' must be a matrix or a sparse Matrix object.")
      } else {
        if (ncol(private$.z_network) == 2) {
          if (sum(is.na(private$.z_network)) > 0) {
            errors <- c(errors, "'z_network' edge list contains NA values.")
          }
          if (any(private$.z_network < 1) || any(private$.z_network > private$.n_units)) {
            errors <- c(errors, "'z_network' edge list contains invalid unit indices.")
          }
        } else {
          if (any(is.na(private$.z_network))) {
            errors <- c(errors, "'z_network' contains missing (NA/NaN) values.")
          }
          if (ncol(private$.z_network) != private$.n_units) {
            errors <- c(errors, "'z_network' must be either an edge list with 2 columns or an adjacency matrix of size n_units x n_units.")
          }
        }
      }
      # Check neighborhood format
      if (!is.null(private$.neighborhood)) {
        if (!is.matrix(private$.neighborhood) && !inherits(private$.neighborhood, "Matrix")) {
          errors <- c(errors, "'neighborhood' must be a matrix or a sparse Matrix object.")
        } else {
          if (ncol(private$.neighborhood) == 2) {
            if (sum(is.na(private$.neighborhood)) > 0) {
              errors <- c(errors, "'neighborhood' edge list contains NA values.")
            }
            if (any(private$.neighborhood < 1) || any(private$.neighborhood > private$.n_units)) {
              errors <- c(errors, "'neighborhood' edge list contains invalid unit indices.")
            }
          } else {
            if (any(is.na(private$.neighborhood))) {
              errors <- c(errors, "'neighborhood' contains missing (NA/NaN) values.")
            }
            if (ncol(private$.neighborhood) != private$.n_units) {
              errors <- c(errors, "'neighborhood' must be either an edge list with 2 columns or an adjacency matrix of size n_units x n_units.")
            }
          }
        }
      }
      # Check directed flag
      if (!is.logical(private$.directed) || length(private$.directed) != 1 || is.na(private$.directed)) {
        errors <- c(errors, "'directed' must be a single logical value (TRUE or FALSE).")
      }
      if (!is.logical(private$.fix_z) || length(private$.fix_z) != 1 || is.na(private$.fix_z)) {
        errors <- c(errors, "'fix_z' must be a single logical value (TRUE or FALSE).")
      }
      if (!is.logical(private$.fix_x) || length(private$.fix_x) != 1 || is.na(private$.fix_x)) {
        errors <- c(errors, "'fix_x' must be a single logical value (TRUE or FALSE).")
      }

      if (length(errors) > 0) stop(paste(errors, collapse = "\n"))

      invisible(self)
    }
  ),
  public = list(
    #' @description
    #' Create a new `iglm.data` object, that includes data on two attributes and one network.
    #'
    #' @param x_attribute A numeric vector for the first unit-level attribute.
    #' @param y_attribute A numeric vector for the second unit-level attribute.
    #' @param z_network A matrix representing the network. Can be a 2-column
    #'   edgelist or a square adjacency matrix.
    #' @param neighborhood An optional matrix for the neighborhood representing local dependence.
    #'   Can be a 2-column edgelist or a square adjacency matrix.
    #'   A tie in `neighborhood` between unit i and j indicates that j is in the neighborhood of i,
    #'   implying dependence between the respective units.
    #' @param directed A logical value indicating if `z_network` is directed.
    #'   If `NA` (default), directedness is inferred from the symmetry of
    #'   `z_network`.
    #' @param n_units An integer for the number of units in the system.
    #'   If `NA` (default), `n_units` is inferred from the attributes or
    #'   network matrices.
    #' @param family_x Character string for the family of `x_attribute`.
    #'   Must be one of `"binomial"`, `"poisson"`, or `"normal"`.
    #'   Default is `"binomial"`.
    #' @param family_y Character string for the family of `y_attribute`.
    #'   Must be one of `"binomial"`, `"poisson"`, or `"normal"`.
    #'   Default is `"binomial"`.
    #' @param scale_x A positive numeric value for scaling (e.g., variance
    #'   for "normal" type). Default is 1.
    #' @param scale_y A positive numeric value for scaling (e.g., variance
    #'   for "normal" type). Default is 1.
    #' @param fix_x Logical. If `TRUE`, the `x_attribute` is treated as fixed
    #'   during model estimation and simulation. Default is `FALSE`.
    #' @param fix_z Logical. If `TRUE`, the `z_network` is treated as fixed
    #'  during model estimation and simulation. Default is `FALSE`.
    #' @param fix_z_alocal Logical. If `TRUE` (default), alocal dyads in the neighborhood are fixed.
    #' @param return_neighborhood Logical. If `TRUE` (default) and
    #'   `neighborhood` is `NULL`, a full neighborhood (all dyads) is
    #'   generated implying global dependence. If `FALSE`, no neighborhood is set.
    #' @param file (character) Optional file path to load a saved `iglm.data` object state.
    #' @param label_x Character string for the label/name of `x_attribute`. Default is `"x"`.
    #' @param label_y Character string for the label/name of `y_attribute`. Default is `"y"`.
    #' @param label_z Character string for the label/name of `z_network`. Default is `"z"`.
    #' @param n_actor (Deprecated) An integer for the number of units. Please use \code{n_units} instead.
    #' @return A new `iglm.data` object.
    initialize = function(x_attribute = NULL, y_attribute = NULL, z_network = NULL,
                          neighborhood = NULL, directed = NA, n_units = NA,
                          family_x = "binomial", family_y = "binomial",
                          scale_x = 1,
                          scale_y = 1,
                          fix_x = FALSE,
                          fix_z = FALSE,
                          fix_z_alocal = TRUE,
                          return_neighborhood = TRUE,
                          file = NULL,
                          label_x = "x",
                          label_y = "y",
                          label_z = "z",
                          n_actor = NULL) {
      if (is.na(n_units) && !is.null(n_actor)) {
        warning("'n_actor' is deprecated; please use 'n_units' instead.", call. = FALSE)
        n_units <- n_actor
      }
      # browser()
      if (!is.null(file)) {
        if (!file.exists(file)) {
          stop(paste("File", file, "does not exist."))
        }
        data_loaded <- readRDS(file)
        if (is.list(data_loaded) && !"n_units" %in% names(data_loaded) && "n_actor" %in% names(data_loaded)) {
          data_loaded$n_units <- data_loaded$n_actor
        }
        required_fields <- c(
          "x_attribute", "y_attribute", "z_network",
          "neighborhood", "directed", "n_units",
          "family_x", "family_y", "scale_x",
          "scale_y", "fix_x", "fix_z", "fix_z_alocal"
        )
        if (!is.list(data_loaded) || !all(required_fields %in% names(data_loaded))) {
          stop("File does not contain a valid iglm.data state.", call. = FALSE)
        }
        x_attribute <- data_loaded$x_attribute
        y_attribute <- data_loaded$y_attribute
        z_network <- data_loaded$z_network
        neighborhood <- data_loaded$neighborhood
        directed <- data_loaded$directed
        n_units <- data_loaded$n_units
        type_x <- data_loaded$family_x
        type_y <- data_loaded$family_y
        scale_x <- data_loaded$scale_x
        scale_y <- data_loaded$scale_y
        fix_x <- data_loaded$fix_x
        fix_z <- data_loaded$fix_z
        fix_z_alocal <- data_loaded$fix_z_alocal
        if (!is.null(data_loaded$label_x)) label_x <- data_loaded$label_x
        if (!is.null(data_loaded$label_y)) label_y <- data_loaded$label_y
        if (!is.null(data_loaded$label_z)) label_z <- data_loaded$label_z
      }
      if (!is.null(z_network) && any(is.na(z_network))) {
        if ((is.matrix(z_network) || inherits(z_network, "Matrix")) && ncol(z_network) == 2) {
          stop("'z_network' edge list contains NA values.", call. = FALSE)
        } else {
          stop("'z_network' contains missing (NA/NaN) values.", call. = FALSE)
        }
      }
      if (!is.null(neighborhood) && any(is.na(neighborhood))) {
        if ((is.matrix(neighborhood) || inherits(neighborhood, "Matrix")) && ncol(neighborhood) == 2) {
          stop("'neighborhood' edge list contains NA values.", call. = FALSE)
        } else {
          stop("'neighborhood' contains missing (NA/NaN) values.", call. = FALSE)
        }
      }
      private$.family_x <- as.character(family_x)
      private$.family_y <- as.character(family_y)
      private$.scale_x <- scale_x
      private$.scale_y <- scale_y
      private$.fix_x <- fix_x
      private$.fix_z <- fix_z
      private$.fix_z_alocal <- as.logical(fix_z_alocal)

      private$.label_x <- label_x
      private$.label_y <- label_y
      private$.label_z <- label_z

      private$.descriptives <- list()

      if (return_neighborhood) {
        if (is.null(neighborhood)) {
          if (is.na(n_units)) {
            stop("n_units must be provided if neighborhood is not provided.")
          }
          neighborhood <- expand.grid(1:n_units, 1:n_units)
          neighborhood <- neighborhood[neighborhood$Var1 != neighborhood$Var2, ]
          neighborhood <- as.matrix(neighborhood)
        }
      }
      if (is.null(z_network)) {
        private$.z_network <- matrix(0, nrow = 0, ncol = 2)
      } else {
        private$.z_network <- z_network
      }
      if (ncol(private$.z_network) > 2) {
        if (!directed) {
          private$.z_network[lower.tri(private$.z_network)] <- 0
        }
        private$.z_network <- which(private$.z_network == 1, arr.ind = T)
      }

      if (!directed) {
        wrong_tmp <- private$.z_network[, 1] > private$.z_network[, 2]
        correct_tmp <- private$.z_network[, 1] < private$.z_network[, 2]
        private$.z_network <- rbind(
          private$.z_network[correct_tmp, c(1, 2)],
          private$.z_network[wrong_tmp, c(2, 1)]
        )
        private$.z_network <- private$.z_network[!duplicated(private$.z_network), ]
        private$.z_network <- matrix(private$.z_network, ncol = 2)
      } else {
        private$.z_network <- matrix(private$.z_network, ncol = 2)
      }

      if (is.na(n_units)) {
        if (return_neighborhood) {
          if (ncol(neighborhood) > 2) {
            private$.n_units <- nrow(neighborhood)
          } else {
            private$.n_units <- max(neighborhood)
          }
        } else if (!is.null(x_attribute)) {
          private$.n_units <- length(x_attribute)
        } else if (!is.null(y_attribute)) {
          private$.n_units <- length(y_attribute)
        } else if (ncol(z_network) > 2) {
          private$.n_units <- nrow(z_network)
        } else {
          private$.n_units <- max(z_network)
        }
      } else {
        private$.n_units <- n_units
      }
      private$.fix_x <- fix_x
      private$.fix_z <- fix_z

      if (is.na(private$.n_units)) {
        stop("n_units could not be inferred. Please provide n_units.")
      }
      if (is.null(x_attribute) | (length(x_attribute) != private$.n_units)) {
        private$.x_attribute <- numeric(length = private$.n_units)
      } else {
        private$.x_attribute <- x_attribute
      }
      if (is.null(y_attribute) | (length(y_attribute) != private$.n_units)) {
        private$.y_attribute <- numeric(length = private$.n_units)
      } else {
        private$.y_attribute <- y_attribute
      }
      if (is.na(directed)) {
        if (ncol(private$.z_network) == 2) {
          z_network_tmp <- matrix(0, nrow = private$.n_units, ncol = private$.n_units)
          z_network_tmp[private$.z_network] <- 1
          private$.directed <- !isSymmetric(z_network_tmp)
        } else {
          private$.directed <- !isSymmetric(private$.z_network)
        }
      } else {
        private$.directed <- directed
      }
      if (return_neighborhood) {
        if (ncol(neighborhood) == 2) {
          sp_nb <- spMatrix(
            nrow = private$.n_units, ncol = private$.n_units,
            i = neighborhood[, 1], j = neighborhood[, 2],
            x = rep(1, length(neighborhood[, 2]))
          )
          sp_nb_trans <- sparseMatrix(i = sp_nb@j + 1, j = sp_nb@i + 1, dims = sp_nb@Dim)

          overlap <- sp_nb %*% sp_nb_trans
          overlap <- as(overlap, "TsparseMatrix")
          overlap <- cbind(
            overlap@i + 1,
            overlap@j + 1
          )
          private$.overlap <- overlap[overlap[, 1] != overlap[, 2], ]
          private$.neighborhood <- neighborhood
        } else {
          positions <- which(neighborhood == 1, arr.ind = T)
          sp_nb <- spMatrix(
            nrow = ncol(neighborhood), ncol = ncol(neighborhood),
            i = positions[, 1], j = positions[, 2], x = rep(1, length(positions[, 2]))
          )
          sp_nb_trans <- sparseMatrix(i = sp_nb@j + 1, j = sp_nb@i + 1, dims = sp_nb@Dim)

          overlap <- as.matrix(sp_nb %*% sp_nb_trans > 0)
          diag(overlap) <- 0
          private$.overlap <- which(overlap == 1, arr.ind = T)
          private$.neighborhood <- which(neighborhood == 1, arr.ind = T)
        }
      } else {
        private$.overlap <- matrix(0, nrow = 0, ncol = 2)
      }
      private$.validate()

      invisible(self)
    },
    #' @description
    #' Sets the `z_network` of the `iglm.data` object.
    #' @param z_network A matrix representing the network. Can be a 2-column
    #'  edgelist or a square adjacency matrix.
    #'  @return The `iglm.data` object itself (`self`), invisibly.
    set_z_network = function(z_network) {
      if (ncol(z_network) > 2) {
        if (!private$.directed) {
          z_network[lower.tri(z_network)] <- 0
        }
        z_network <- which(z_network == 1, arr.ind = T)
      }
      if (!private$.directed && ncol(z_network) == 2 && nrow(z_network) > 0) {
        wrong_tmp <- z_network[, 1] > z_network[, 2]
        correct_tmp <- z_network[, 1] < z_network[, 2]
        z_network <- rbind(
          z_network[correct_tmp, c(1, 2), drop = FALSE],
          z_network[wrong_tmp, c(2, 1), drop = FALSE]
        )
        z_network <- z_network[!duplicated(z_network), , drop = FALSE]
        z_network <- matrix(z_network, ncol = 2)
      } else {
        z_network <- matrix(z_network, ncol = 2)
      }
      private$.z_network <- z_network
      private$.descriptives <- list()
      private$.validate()
      invisible(self)
    },

    #' @description
    #' Sets the `family_x` of the `iglm.data` object.
    #' @param family_x A character string for the family of `x_attribute`.
    #'  Must be one of `"binomial"`, `"poisson"`, or `"normal"`.
    #'  @return The `iglm.data` object itself (`self`), invisibly.
    set_family_x = function(family_x) {
      private$.family_x <- as.character(family_x)
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the `family_y` of the `iglm.data` object.
    #' @param family_y A character string for the family of `y_attribute`.
    #' Must be one of `"binomial"`, `"poisson"`, or `"normal"`.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_family_y = function(family_y) {
      private$.family_y <- as.character(family_y)
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the `scale_x` of the `iglm.data` object.
    #' @param scale_x A positive numeric value for scaling (e.g., variance
    #' for "normal" type).
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_scale_x = function(scale_x) {
      private$.scale_x <- scale_x
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the `scale_y` of the `iglm.data` object.
    #' @param scale_y A positive numeric value for scaling (e.g., variance
    #' for "normal" type).
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_scale_y = function(scale_y) {
      private$.scale_y <- scale_y
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the `x_attribute` of the `iglm.data` object.
    #' @param x_attribute A numeric vector for the first unit-level attribute.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_x_attribute = function(x_attribute) {
      private$.x_attribute <- x_attribute
      private$.descriptives <- list()
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the `y_attribute` of the `iglm.data` object.
    #' @param y_attribute A numeric vector for the first unit-level attribute.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_y_attribute = function(y_attribute) {
      private$.y_attribute <- y_attribute
      private$.descriptives <- list()
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the label for the `x_attribute`.
    #' @param label_x A character string for the label of `x_attribute`.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_label_x = function(label_x) {
      if (!is.character(label_x) || length(label_x) != 1 || is.na(label_x) || nchar(trimws(label_x)) == 0) {
        stop("`label_x` must be a single non-empty character string.", call. = FALSE)
      }
      private$.label_x <- as.character(label_x)
      invisible(self)
    },
    #' @description
    #' Sets the label for the `y_attribute`.
    #' @param label_y A character string for the label of `y_attribute`.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_label_y = function(label_y) {
      if (!is.character(label_y) || length(label_y) != 1 || is.na(label_y) || nchar(trimws(label_y)) == 0) {
        stop("`label_y` must be a single non-empty character string.", call. = FALSE)
      }
      private$.label_y <- as.character(label_y)
      invisible(self)
    },
    #' @description
    #' Sets the label for the `z_network`.
    #' @param label_z A character string for the label of `z_network`.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_label_z = function(label_z) {
      if (!is.character(label_z) || length(label_z) != 1 || is.na(label_z) || nchar(trimws(label_z)) == 0) {
        stop("`label_z` must be a single non-empty character string.", call. = FALSE)
      }
      private$.label_z <- as.character(label_z)
      invisible(self)
    },
    #' @description
    #' Gathers the current state of the `iglm.data` object into a list.
    #' This includes all attributes, network, and configuration
    #' details necessary to reconstruct the object later.
    #' @return A list containing the current state of the `iglm.data` object.
    gather = function() {
      data_to_save <- list(
        x_attribute = private$.x_attribute,
        y_attribute = private$.y_attribute,
        z_network = private$.z_network,
        neighborhood = private$.neighborhood,
        directed = private$.directed,
        n_units = private$.n_units,
        family_x = private$.family_x,
        family_y = private$.family_y,
        scale_x = private$.scale_x,
        scale_y = private$.scale_y,
        fix_x = private$.fix_x,
        fix_z = private$.fix_z,
        fix_z_alocal = private$.fix_z_alocal,
        label_x = private$.label_x,
        label_y = private$.label_y,
        label_z = private$.label_z
      )
      return(data_to_save)
    },
    #' @description
    #' Sets the option whether alocal edges are fixed or not.
    #' @param fix_z_alocal A logical value indicating whether alocal edges should be treated as fixed or not.
    set_fix_z_alocal = function(fix_z_alocal) {
      if (!is.logical(fix_z_alocal)) {
        stop("`fix_z_alocal` must be a logical value (TRUE or FALSE).", call. = FALSE)
      }
      private$.fix_z_alocal <- fix_z_alocal
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Deletes isolates from the `z_network` and updates the attributes and neighborhood accordingly.
    #' Isolates are units that do not have any connections in the `z_network`. This method identifies such units, removes them from the attributes and neighborhood, and updates the `z_network` to reflect the new unit indices.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    delete_isolates = function() {
      # browser()
      if (ncol(private$.z_network) == 2) {
        units_in_network <- unique(c(private$.z_network[, 1], private$.z_network[, 2]))
        isolates <- setdiff(1:private$.n_units, units_in_network)
        unit_df <- data.frame(
          id_old = 1:private$.n_units,
          in_network = 1:private$.n_units %in% units_in_network
        )
        unit_df$id_new <- NA
        unit_df$id_new[unit_df$in_network] <- 1:sum(unit_df$in_network)
        if (length(isolates) > 0) {
          private$.x_attribute <- private$.x_attribute[-isolates]
          private$.y_attribute <- private$.y_attribute[-isolates]
          private$.n_units <- length(private$.x_attribute)
          private$.z_network <- private$.z_network[!private$.z_network[, 1] %in% isolates & !private$.z_network[, 2] %in% isolates, , drop = FALSE]
          private$.z_network[, 1] <- unit_df$id_new[private$.z_network[, 1]]
          private$.z_network[, 2] <- unit_df$id_new[private$.z_network[, 2]]
          if (!is.null(private$.neighborhood)) {
            private$.neighborhood <- private$.neighborhood[!private$.neighborhood[, 1] %in% isolates & !private$.neighborhood[, 2] %in% isolates, , drop = FALSE]
            private$.neighborhood[, 1] <- unit_df$id_new[private$.neighborhood[, 1]]
            private$.neighborhood[, 2] <- unit_df$id_new[private$.neighborhood[, 2]]
            private$.overlap <- private$.overlap[!private$.overlap[, 1] %in% isolates & !private$.overlap[, 2] %in% isolates, , drop = FALSE]
            private$.overlap[, 1] <- unit_df$id_new[private$.overlap[, 1]]
            private$.overlap[, 2] <- unit_df$id_new[private$.overlap[, 2]]
          }
        }
      }
      private$.descriptives <- list()
      invisible(self)
    },
    #' @description
    #' Saves the current state of the `iglm.data` object to a specified file path
    #' in RDS format. This includes all attributes, network, and configuration
    #' details necessary to reconstruct the object later.
    #' @param file (character) The file where the object state should be saved. Must have a .rds extension.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    save = function(file) {
      if (missing(file) || !is.character(file) || length(file) != 1) {
        stop("A valid 'file' (character string) must be provided.", call. = FALSE)
      }
      extension <- tools::file_ext(file)
      if (tolower(extension) != "rds") {
        stop("File extension must be .rds", call. = FALSE)
      }

      data_to_save <- self$gather()
      saveRDS(data_to_save, file = file)
      message(paste("Object state saved to:", file))
      invisible(self)
    },
    #' @description
    #' Sets the `fix_x` of the `iglm.data` object.
    #' @param fix_x A logical value indicating if `x_attribute` is fixed or random.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_fix_x = function(fix_x) {
      private$.fix_x <- fix_x
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the `fix_z` of the `iglm.data` object.
    #' @param fix_z A logical value indicating if `z_network` is fixed or random.
    #' @return The `iglm.data` object itself (`self`), invisibly.
    set_fix_z = function(fix_z) {
      private$.fix_z <- fix_z
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Calculates the density of the `z_network`.
    #' @return A numeric value for the network density.
    mean_z = function() {
      if (private$.n_units < 2) {
        return(0)
      }
      m <- nrow(private$.z_network) / (private$.n_units * (private$.n_units - 1) / (2 - private$.directed))
      private$.descriptives$density_z <- m
      return(m)
    },
    #' @description
    #' Calculates the mean of the `x_attribute`.
    #' @return A numeric value for the mean of `x_attribute`.
    mean_x = function() {
      m <- mean(private$.x_attribute)
      private$.descriptives$density_x <- m
      return(m)
    },
    #' @description
    #' Calculates the mean of the `y_attribute`.
    #' @return A numeric value for the mean of `y_attribute`.
    mean_y = function() {
      m <- mean(private$.y_attribute)
      private$.descriptives$density_y <- m
      return(m)
    },
    #' @description
    #' Calculates the distribution of the `x_attribute`.
    #' @param value_range (numeric vector) Optional range of values to consider for the distribution. If `NULL` (default), the range is inferred from the data.
    #' @param prob (logical) If `TRUE` (default), returns probabilities; if `FALSE`, returns frequencies.
    #' @param plot (logical) If `TRUE` (default), plots the distribution using a density plot for continuous data or a bar plot for discrete data.
    #' @return A numeric vector representing the distribution of `x_attribute` (invisible).
    x_distribution = function(value_range = NULL, prob = TRUE, plot = TRUE) {
      if (is.null(value_range)) {
        value_range <- range(private$.x_attribute)
      }
      if (private$.family_x == "normal") {
        tmp_density <- density(private$.x_attribute, from = value_range[1], to = value_range[2])
        names(tmp_density$y) <- tmp_density$x

        if (plot) {
          plot(
            las = 1, tmp_density,
            main = "Density of x_attribute",
            xlab = "x_attribute values", ylab = "Density"
          )
        }
        private$.descriptives$x_distribution <- tmp_density$y
      } else {
        info <- factor(as.numeric(private$.x_attribute),
          levels = seq(from = value_range[1], to = value_range[2])
        )
        info <- table(info)
        if (sum(info) > 0) {
          info <- info / (sum(info) * prob + (!prob))
        }
        private$.descriptives$x_distribution <- info
        if (plot) {
          barplot(info,
            main = "Distribution of x_attribute",
            xlab = "x_attribute values",
            ylim = c(0, max(info) * 1.2),
            ylab = ifelse(prob, "Probability", "Frequency")
          )
        }
      }
      invisible(private$.descriptives$x_distribution)
    },
    #' @description
    #' Short alias for `x_distribution`.
    #' @param value_range (numeric vector) Optional range of values to consider for the distribution. If `NULL` (default), the range is inferred from the data.
    #' @param prob (logical) If `TRUE` (default), returns probabilities; if `FALSE`, returns frequencies.
    #' @param plot (logical) If `TRUE` (default), plots the distribution.
    #' @return A numeric vector representing the distribution of `x_attribute` (invisible).
    x_dist = function(value_range = NULL, prob = TRUE, plot = TRUE) {
      self$x_distribution(value_range = value_range, prob = prob, plot = plot)
    },
    #' @description
    #' Calculates the distribution of the `y_attribute`.
    #' @param value_range (numeric vector) Optional range of values to consider for the distribution. If `NULL` (default), the range is inferred from the data.
    #' @param prob (logical) If `TRUE` (default), returns probabilities; if `FALSE`, returns frequencies.
    #' @param plot (logical) If `TRUE` (default), plots the distribution using a density plot for continuous data or a bar plot for discrete data.
    #' @return A numeric vector representing the distribution of `y_attribute` (invisible).
    y_distribution = function(value_range = NULL, prob = TRUE, plot = TRUE) {
      if (is.null(value_range)) {
        value_range <- range(private$.y_attribute)
      }
      if (private$.family_y == "normal") {
        tmp_density <- density(private$.y_attribute, from = value_range[1], to = value_range[2])
        names(tmp_density$y) <- tmp_density$x
        if (plot) {
          plot(
            las = 1, tmp_density,
            main = "Density of y_attribute",
            xlab = "y_attribute values", ylab = "Density"
          )
        }
        private$.descriptives$y_distribution <- tmp_density$y
      } else {
        info <- factor(as.numeric(private$.y_attribute),
          levels = seq(from = value_range[1], to = value_range[2])
        )
        info <- table(info)
        if (sum(info) > 0) {
          info <- info / (sum(info) * prob + (!prob))
        }
        private$.descriptives$y_distribution <- info
        if (plot) {
          barplot(info,
            main = "Distribution of y_attribute",
            ylim = c(0, max(info) * 1.2),
            xlab = "y_attribute values", ylab = ifelse(prob, "Probability", "Frequency")
          )
        }
      }
      invisible(private$.descriptives$y_distribution)
    },
    #' @description
    #' Short alias for `y_distribution`.
    #' @param value_range (numeric vector) Optional range of values to consider for the distribution. If `NULL` (default), the range is inferred from the data.
    #' @param prob (logical) If `TRUE` (default), returns probabilities; if `FALSE`, returns frequencies.
    #' @param plot (logical) If `TRUE` (default), plots the distribution.
    #' @return A numeric vector representing the distribution of `y_attribute` (invisible).
    y_dist = function(value_range = NULL, prob = TRUE, plot = TRUE) {
      self$y_distribution(value_range = value_range, prob = prob, plot = plot)
    },
    #' @description
    #' Calculates the matrix of edgewise shared partners.
    #' This is a two-path matrix (e.g., $A A^T$ or $A^T A$).
    #'
    #' @param type (character) The type of two-path to calculate for directed
    #'   networks. Ignored if network is undirected.
    #'   Must be one of:
    #'   `"OTP"` (Outgoing Two-Path, \eqn{z_{i,j}\, z_{i,h} \, z_{j,h}} ),
    #'   `"ISP"` (Ingoing Shared Partner, \eqn{z_{i,j}\, z_{h,i} \, z_{j,h}}),
    #'   `"OSP"` (Outgoing Shared Partner, \eqn{z_{i,j}\, z_{i,h} \, z_{j,h}}),
    #'   `"ITP"` (Incoming Two-Path, \eqn{z_{i,j}\, z_{h,i} \, z_{j,h}}),
    #'   `"ALL"` (Any one of the above).
    #'   Default is `"ALL"`.
    #' @param mode (character) Either `"global"` (default) to evaluate across all edges,
    #'   or `"local"` to evaluate only edges with overlapping neighborhoods (from `overlap`).
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A numeric vector of shared partner counts for edges.
    edgewise_shared_partner = function(type = "ALL", mode = "global",
                                       x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL) {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      if (!type %in% c("OTP", "ITP", "ISP", "OSP", "ALL", "symm")) {
        stop("type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")
      }
      if (!private$.directed && type %in% c("OTP", "ITP", "ISP", "OSP")) {
        stop(sprintf("Type '%s' is only for directed networks. For undirected networks, use type = 'ALL'.", type))
      }
      if (private$.directed && type == "symm") {
        stop("Type 'symm' is only for undirected networks.")
      }

      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)

      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }
      has_constraints <- has_i_constr || has_j_constr
      key <- if (mode == "local") paste0(type, "_local") else type

      if (!has_constraints && !is.null(private$.descriptives$edgewise_shared_partner[[key]])) {
        return(private$.descriptives$edgewise_shared_partner[[key]])
      }

      if (is.null(private$.descriptives$dyadwise_shared_partner[[type]])) {
        self$dyadwise_shared_partner(type = type, mode = "global")
      }
      res_dsp <- private$.descriptives$dyadwise_shared_partner[[type]]

      if (nrow(private$.z_network) > 0) {
        edges <- private$.z_network
        if (mode == "local") {
          if (is.null(private$.overlap) || nrow(private$.overlap) == 0) {
            edges <- matrix(numeric(0), ncol = 2)
          } else {
            which_overlap <- check_overlap(edges, private$.overlap)
            edges <- edges[which_overlap, , drop = FALSE]
          }
        }
        if (has_constraints && nrow(edges) > 0) {
          cond_sender <- filter_nodes(private$.x_attribute, x_i, private$.family_x) &
            filter_nodes(private$.y_attribute, y_i, private$.family_y)
          cond_receiver <- filter_nodes(private$.x_attribute, x_j, private$.family_x) &
            filter_nodes(private$.y_attribute, y_j, private$.family_y)
          units_sender <- which(cond_sender)
          units_receiver <- which(cond_receiver)

          if (private$.directed) {
            qualifies <- (edges[, 1] %in% units_sender) & (edges[, 2] %in% units_receiver)
          } else {
            if (has_i_constr && has_j_constr) {
              qualifies <- ((edges[, 1] %in% units_sender) & (edges[, 2] %in% units_receiver)) |
                ((edges[, 1] %in% units_receiver) & (edges[, 2] %in% units_sender))
            } else {
              qualifies <- (edges[, 1] %in% units_sender) | (edges[, 2] %in% units_sender)
            }
          }
          edges <- edges[qualifies, , drop = FALSE]
        }
        res <- if (nrow(edges) > 0) as.numeric(res_dsp[edges]) else numeric(0)
        res <- res[!is.na(res)]
      } else {
        res <- numeric(0)
      }

      if (!has_constraints) {
        if (is.null(private$.descriptives$edgewise_shared_partner)) {
          private$.descriptives$edgewise_shared_partner <- list()
        }
        private$.descriptives$edgewise_shared_partner[[key]] <- res
      }
      return(res)
    },
    #' @description
    #' Sets the neighborhood and overlap matrices.
    #' @param neighborhood A matrix for a secondary neighborhood.
    #'  Can be a 2-column edgelist or a square adjacency matrix.
    #' @param overlap A matrix for the overlap network.
    #'  Can be a 2-column edgelist or a square adjacency matrix.
    #' @return None. Updates the internal neighborhood and overlap matrices.
    set_neighborhood_overlap = function(neighborhood, overlap) {
      if (!is.matrix(neighborhood)) {
        stop("'neighborhood' must be a matrix or a sparse Matrix object.")
      }
      if (ncol(neighborhood) > 2) {
        private$.neighborhood <- which(neighborhood == 1, arr.ind = T)
      } else {
        private$.neighborhood <- neighborhood
      }

      if (!is.matrix(overlap)) {
        stop("'overlap' must be a matrix or a sparse Matrix object.")
      }
      if (ncol(overlap) > 2) {
        private$.overlap <- which(overlap == 1, arr.ind = T)
      } else {
        private$.overlap <- overlap
      }
      private$.descriptives <- list()
      invisible(self)
    },
    #' @description
    #' Calculates the matrix of dyadwise shared partners.
    #'
    #' @param type (character) The type of two-path to calculate for directed
    #'   networks. Ignored if network is undirected.
    #'   Must be one of:
    #'   `"OTP"` (Outgoing Two-Path, \eqn{z_{i,h} \, z_{j,h}} ),
    #'   `"ISP"` (Ingoing Shared Partner, \eqn{z_{h,i} \, z_{j,h}}),
    #'   `"OSP"` (Outgoing Shared Partner, \eqn{z_{i,h} \, z_{j,h}}),
    #'   `"ITP"` (Incoming Two-Path, \eqn{z_{h,i} \, z_{j,h}}),
    #'   `"ALL"` (Any one of the above).
    #'   Default is `"ALL"`.
    #' @param mode (character) Either `"global"` (default) to evaluate across all dyads,
    #'   or `"local"` to evaluate only dyads with overlapping neighborhoods.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A sparse matrix (`dgCMatrix`) of shared partner counts.
    dyadwise_shared_partner = function(type = "ALL", mode = "global",
                                       x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL) {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      if (!type %in% c("OTP", "ITP", "ISP", "OSP", "ALL", "symm")) {
        stop("type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")
      }
      if (!private$.directed && type %in% c("OTP", "ITP", "ISP", "OSP")) {
        stop(sprintf("Type '%s' is only for directed networks. For undirected networks, use type = 'ALL'.", type))
      }
      if (private$.directed && type == "symm") {
        stop("Type 'symm' is only for undirected networks.")
      }
      if (is.null(private$.descriptives$dyadwise_shared_partner)) {
        private$.descriptives$dyadwise_shared_partner <- list()
      }

      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)
      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }
      has_constraints <- has_i_constr || has_j_constr
      key <- if (mode == "local") paste0(type, "_local") else type

      if (!has_constraints && !is.null(private$.descriptives$dyadwise_shared_partner[[key]])) {
        return(private$.descriptives$dyadwise_shared_partner[[key]])
      }

      adj_mat <- sparseMatrix(
        i = private$.z_network[, 1],
        j = private$.z_network[, 2],
        symmetric = !private$.directed,
        dims = c(private$.n_units, private$.n_units)
      )

      if (!private$.directed) {
        res <- Matrix::t(adj_mat) %*% t(adj_mat)
        diag(res) <- NA
        res[lower.tri(res)] <- NA
      } else {
        if (type == "OTP") {
          res <- adj_mat %*% Matrix::t(adj_mat)
        } else if (type == "ISP") {
          res <- adj_mat %*% adj_mat
        } else if (type == "OSP") {
          res <- Matrix::t(adj_mat) %*% Matrix::t(adj_mat)
        } else if (type == "ITP") {
          res <- Matrix::t(adj_mat) %*% adj_mat
        } else if (type == "ALL") {
          adj_mat_symm <- pmax(adj_mat, t(adj_mat))
          res <- Matrix::t(adj_mat_symm) %*% adj_mat_symm
        }
        diag(res) <- NA
      }
      # Only look at the local connections if asked to do so.
      if (mode == "local") {
        if (is.null(private$.overlap) || nrow(private$.overlap) == 0) {
          res <- Matrix::sparseMatrix(
            i = integer(0), j = integer(0), x = numeric(0),
            dims = c(private$.n_units, private$.n_units)
          )
        } else {
          overlap_idx <- if (!private$.directed) {
            private$.overlap[private$.overlap[, 1] < private$.overlap[, 2], , drop = FALSE]
          } else {
            private$.overlap[private$.overlap[, 1] != private$.overlap[, 2], , drop = FALSE]
          }
          if (nrow(overlap_idx) > 0) {
            overlap_sp <- Matrix::sparseMatrix(
              i = overlap_idx[, 1],
              j = overlap_idx[, 2],
              x = 1,
              dims = c(private$.n_units, private$.n_units)
            )
            res <- res * overlap_sp
          } else {
            res <- Matrix::sparseMatrix(
              i = integer(0), j = integer(0), x = numeric(0),
              dims = c(private$.n_units, private$.n_units)
            )
          }
        }
      }

      if (has_constraints) {
        cand <- get_candidate_dyads(
          directed = private$.directed, n_units = private$.n_units,
          overlap = private$.overlap, mode = mode,
          x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j,
          x_attribute = private$.x_attribute, y_attribute = private$.y_attribute,
          family_x = private$.family_x, family_y = private$.family_y
        )
        mask <- matrix(FALSE, private$.n_units, private$.n_units)
        if (nrow(cand) > 0) {
          mask[cand] <- TRUE
        }
        res[!mask] <- NA
      } else {
        private$.descriptives$dyadwise_shared_partner[[key]] <- res
      }
      return(res)
    },
    #' @description
    #' Calculates the geodesic distance distribution of the symmetrized
    #' `z_network`.
    #'
    #' @param value_range (numeric vector) A vector `c(min, max)` specifying
    #'   the range of distances to tabulate. If `NULL` (default), the range
    #'   is inferred from the data.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param prob (logical) If `TRUE` (default), returns a probability
    #'   distribution (proportions). If `FALSE`, returns raw counts.
    #' @param mode (character) Either `"global"` (default) to evaluate across all node pairs,
    #'   or `"local"` to evaluate only pairs with overlapping neighborhoods (from `overlap`).
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A named vector (a `table` object) with the distribution of
    #'   geodesic distances. Includes `Inf` for unreachable pairs.
    geodesic_distances_distribution = function(value_range = NULL,
                                               prob = TRUE,
                                               plot = TRUE,
                                               x_i = NULL,
                                               x_j = NULL,
                                               y_i = NULL,
                                               y_j = NULL,
                                               mode = "global") {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      if (!is.null(value_range) && length(value_range) != 2) {
        stop("'value_range' must be a numeric vector of length 2.")
      }
      if (sum(value_range < 0) > 0) {
        stop("'value_range' values must be non-negative.")
      }
      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)
      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }
      has_constraints <- has_i_constr || has_j_constr
      key <- if (mode == "local") "local" else "global"

      if (!has_constraints && is.null(private$.descriptives$geodesic_distances_distribution)) {
        private$.descriptives$geodesic_distances_distribution <- list()
      }

      cand <- get_candidate_dyads(
        directed = private$.directed, n_units = private$.n_units,
        overlap = private$.overlap, mode = mode,
        x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j,
        x_attribute = private$.x_attribute, y_attribute = private$.y_attribute,
        family_x = private$.family_x, family_y = private$.family_y
      )

      if (is.null(private$.descriptives$geodesic_distances[["global"]])) {
        self$geodesic_distances(mode = "global")
      }
      D_glob <- private$.descriptives$geodesic_distances[["global"]]

      D_vec <- if (nrow(cand) > 0) as.numeric(D_glob[cand]) else numeric(0)
      D_vec <- D_vec[!is.na(D_vec)]

      if (is.null(value_range)) {
        if (length(D_vec[is.finite(D_vec) & D_vec > 0]) > 0) {
          value_range <- range(D_vec[is.finite(D_vec) & D_vec > 0])
        } else {
          value_range <- c(1, 1)
        }
      }
      info_factor <- factor(D_vec,
        levels = c(seq(from = value_range[1], to = value_range[2]), Inf)
      )
      info <- table(info_factor)

      if (sum(info) > 0) {
        info <- info / (sum(info) * prob + (!prob))
      }
      if (!has_constraints) {
        private$.descriptives$geodesic_distances_distribution[[key]] <- info
      }
      if (plot) {
        prefix <- paste0("Geodesic Distance", if (mode == "local") " (Local)" else "")
        barplot(info,
          ylim = c(0, max(info) * 1.2),
          xlab = build_constrained_xlab(prefix, x_i, x_j, y_i, y_j, family_x = private$.family_x, family_y = private$.family_y),
          ylab = ifelse(prob, "Proportion", "Count"),
          las = 1
        )
      }
      invisible(info)
    },
    #' @description
    #' Calculates the all-pairs geodesic distance matrix for the
    #' symmetrized `z_network` using a matrix-based BFS algorithm.
    #' @param mode (character) Either `"global"` (default) to evaluate across all pairs,
    #'   or `"local"` to evaluate only pairs with overlapping neighborhoods.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A sparse matrix (`dgCMatrix`) where `D[i, j]` is the
    #'   shortest path distance from i to j. `Inf` indicates no path.
    #' @importFrom Matrix sparseMatrix t Matrix diag nnzero Diagonal
    geodesic_distances = function(mode = "global",
                                  x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL) {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)
      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }
      has_constraints <- has_i_constr || has_j_constr
      key <- if (mode == "local") "local" else "global"

      if (!has_constraints && !is.null(private$.descriptives$geodesic_distances[[key]])) {
        return(private$.descriptives$geodesic_distances[[key]])
      }

      if (is.null(private$.descriptives$geodesic_distances[["global"]])) {
        adj_mat <- Matrix::sparseMatrix(
          i = private$.z_network[, 1],
          j = private$.z_network[, 2],
          dims = c(private$.n_units, private$.n_units)
        )
        adj_mat <- pmax(adj_mat, Matrix::t(adj_mat))
        D_glob <- Matrix::Matrix(Inf, nrow(adj_mat), nrow(adj_mat), dimnames = dimnames(adj_mat))
        Matrix::diag(D_glob) <- 0
        F_tmp <- Matrix::Diagonal(nrow(adj_mat))
        k <- 0
        while (Matrix::nnzero(F_tmp) > 0) {
          k <- k + 1
          F_next <- (adj_mat %*% F_tmp) > 0
          F_new <- F_next & (D_glob == Inf)
          D_glob[F_new] <- k
          F_tmp <- F_new
        }
        if (is.null(private$.descriptives$geodesic_distances)) {
          private$.descriptives$geodesic_distances <- list()
        }
        private$.descriptives$geodesic_distances[["global"]] <- D_glob
      }
      D <- private$.descriptives$geodesic_distances[["global"]]

      if (mode == "local") {
        D_local <- Matrix::Matrix(NA_real_, nrow(D), ncol(D))
        if (!is.null(private$.overlap) && nrow(private$.overlap) > 0) {
          D_local[private$.overlap] <- D[private$.overlap]
        }
        D <- D_local
      }

      if (has_constraints) {
        cand <- get_candidate_dyads(
          directed = private$.directed, n_units = private$.n_units,
          overlap = private$.overlap, mode = mode,
          x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j,
          x_attribute = private$.x_attribute, y_attribute = private$.y_attribute,
          family_x = private$.family_x, family_y = private$.family_y
        )
        mask <- matrix(FALSE, private$.n_units, private$.n_units)
        if (nrow(cand) > 0) {
          mask[cand] <- TRUE
        }
        D[!mask] <- NA_real_
      } else {
        private$.descriptives$geodesic_distances[[key]] <- D
      }
      return(D)
    },
    #' @description
    #' Short alias for `edgewise_shared_partner`.
    #' @param type (character) The type of two-path to calculate. Default is `"ALL"`.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A numeric vector of shared partner counts for edges.
    esp = function(type = "ALL", mode = "global", x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL) {
      self$edgewise_shared_partner(type = type, mode = mode, x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j)
    },
    #' @description
    #' Short alias for `edgewise_shared_partner_distribution`.
    #' @param type (character) The type of two-path to calculate. Default is `"ALL"`.
    #' @param value_range (numeric vector) Range of counts to tabulate.
    #' @param prob (logical) If `TRUE` (default), returns proportions.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @return A named vector with the distribution.
    esp_dist = function(type = "ALL", value_range = NULL, prob = TRUE, plot = TRUE, x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL, mode = "global") {
      self$edgewise_shared_partner_distribution(type = type, value_range = value_range, prob = prob, plot = plot, x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j, mode = mode)
    },
    #' @description
    #' Short alias for `dyadwise_shared_partner`.
    #' @param type (character) The type of two-path to calculate. Default is `"ALL"`.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A sparse matrix (`dgCMatrix`) of shared partner counts.
    dsp = function(type = "ALL", mode = "global", x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL) {
      self$dyadwise_shared_partner(type = type, mode = mode, x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j)
    },
    #' @description
    #' Short alias for `dyadwise_shared_partner_distribution`.
    #' @param type (character) The type of two-path to calculate. Default is `"ALL"`.
    #' @param value_range (numeric vector) Range of counts to tabulate.
    #' @param prob (logical) If `TRUE` (default), returns proportions.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @return A named vector with the distribution.
    dsp_dist = function(type = "ALL", value_range = NULL, prob = TRUE, plot = TRUE, x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL, mode = "global") {
      self$dyadwise_shared_partner_distribution(type = type, value_range = value_range, prob = prob, plot = plot, x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j, mode = mode)
    },
    #' @description
    #' Short alias for `geodesic_distances`.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A sparse matrix (`dgCMatrix`) of geodesic distances.
    geo = function(mode = "global", x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL) {
      self$geodesic_distances(mode = mode, x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j)
    },
    #' @description
    #' Short alias for `geodesic_distances`.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A sparse matrix (`dgCMatrix`) of geodesic distances.
    geodist = function(mode = "global", x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL) {
      self$geodesic_distances(mode = mode, x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j)
    },
    #' @description
    #' Short alias for `geodesic_distances_distribution`.
    #' @param value_range (numeric vector) Range of distances to tabulate.
    #' @param prob (logical) If `TRUE` (default), returns proportions.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @return A named vector with the distribution.
    geo_dist = function(value_range = NULL, prob = TRUE, plot = TRUE, x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL, mode = "global") {
      self$geodesic_distances_distribution(value_range = value_range, prob = prob, plot = plot, x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j, mode = mode)
    },
    #' @description
    #' Calculates the distribution of edgewise shared partners.
    #'
    #' @param type (character) The type of shared partner matrix to use.
    #'   See `edgewise_shared_partner` for details. Default is `"ALL"`.
    #' @param mode (character) Either `"global"` (default) to evaluate across all edges,
    #'   or `"local"` to evaluate only edges with overlapping neighborhoods.
    #' @param value_range (numeric vector) A vector `c(min, max)` specifying
    #'   the range of counts to tabulate. If `NULL` (default), the range
    #'   is inferred from the data.
    #' @param prob (logical) If `TRUE` (default), returns a probability
    #'   distribution (proportions). If `FALSE`, returns raw counts.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A named vector (a `table` object) with the distribution of
    #'   shared partner counts.
    edgewise_shared_partner_distribution = function(type = "ALL",
                                                    value_range = NULL,
                                                    prob = TRUE,
                                                    plot = TRUE,
                                                    x_i = NULL,
                                                    x_j = NULL,
                                                    y_i = NULL,
                                                    y_j = NULL,
                                                    mode = "global") {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      if (!type %in% c("OTP", "ITP", "ISP", "OSP", "ALL", "symm")) {
        stop("type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")
      }
      if (!private$.directed && type %in% c("OTP", "ITP", "ISP", "OSP")) {
        stop(sprintf("Type '%s' is only for directed networks. For undirected networks, use type = 'ALL'.", type))
      }
      if (private$.directed && type == "symm") {
        stop("Type 'symm' is only for undirected networks.")
      }
      if (!is.null(value_range) && length(value_range) != 2) {
        stop("'value_range' must be a numeric vector of length 2.")
      }
      if (sum(value_range < 0) > 0) {
        stop("'value_range' values must be non-negative.")
      }

      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)
      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }
      has_constraints <- has_i_constr || has_j_constr
      key <- if (mode == "local") paste0(type, "_local") else type

      vals <- self$edgewise_shared_partner(
        type = type, mode = mode,
        x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j
      )

      if (is.null(value_range)) {
        if (length(vals) > 0) {
          value_range <- range(vals)
        } else {
          value_range <- c(0, 0)
        }
      }
      info_factor <- factor(vals,
        levels = seq(from = value_range[1], to = value_range[2])
      )
      info_table <- table(info_factor)
      if (sum(info_table) > 0) {
        info_table <- info_table / (sum(info_table) * prob + (!prob))
      }

      if (!has_constraints) {
        if (is.null(private$.descriptives$edgewise_shared_partner_distribution)) {
          private$.descriptives$edgewise_shared_partner_distribution <- list()
        }
        private$.descriptives$edgewise_shared_partner_distribution[[key]] <- info_table
      }

      if (plot) {
        base_lab <- paste0("Number of ", type, if (mode == "local") " (Local)" else "", "- Edgewise Shared Partners")
        barplot(info_table,
          ylim = c(0, max(info_table) * 1.2),
          xlab = build_constrained_xlab(base_lab, x_i, x_j, y_i, y_j, family_x = private$.family_x, family_y = private$.family_y),
          ylab = ifelse(prob, "Proportion", "Count"),
          las = 1
        )
      }
      invisible(info_table)
    },
    #' @description
    #' Calculates the distribution of dyadwise shared partners.
    #'
    #' @param type (character) The type of shared partner matrix to use.
    #'   See `dyadwise_shared_partner` for details. Default is `"ALL"`.
    #' @param mode (character) Either `"global"` (default) to evaluate across all dyads,
    #'   or `"local"` to evaluate only dyads with overlapping neighborhoods.
    #' @param value_range (numeric vector) A vector `c(min, max)` specifying
    #'   the range of counts to tabulate. If `NULL` (default), the range
    #'   is inferred from the data.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param prob (logical) If `TRUE` (default), returns a probability
    #'   distribution (proportions). If `FALSE`, returns raw counts.
    #' @param x_i (optional) Sender attribute constraint.
    #' @param x_j (optional) Receiver attribute constraint.
    #' @param y_i (optional) Sender attribute constraint.
    #' @param y_j (optional) Receiver attribute constraint.
    #' @return A named vector (a `table` object) with the distribution of
    #'   shared partner counts.
    dyadwise_shared_partner_distribution = function(type = "ALL",
                                                    value_range = NULL,
                                                    prob = TRUE,
                                                    plot = TRUE,
                                                    x_i = NULL,
                                                    x_j = NULL,
                                                    y_i = NULL,
                                                    y_j = NULL,
                                                    mode = "global") {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      if (!type %in% c("OTP", "ITP", "ISP", "OSP", "ALL", "symm")) {
        stop("type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")
      }
      if (!private$.directed && type %in% c("OTP", "ITP", "ISP", "OSP")) {
        stop(sprintf("Type '%s' is only for directed networks. For undirected networks, use type = 'ALL'.", type))
      }
      if (private$.directed && type == "symm") {
        stop("Type 'symm' is only for undirected networks.")
      }
      if (!is.null(value_range) && length(value_range) != 2) {
        stop("'value_range' must be a numeric vector of length 2.")
      }
      if (sum(value_range < 0) > 0) {
        stop("'value_range' values must be non-negative.")
      }

      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)
      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }
      has_constraints <- has_i_constr || has_j_constr
      key <- if (mode == "local") paste0(type, "_local") else type

      cand <- get_candidate_dyads(
        directed = private$.directed, n_units = private$.n_units,
        overlap = private$.overlap, mode = mode,
        x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j,
        x_attribute = private$.x_attribute, y_attribute = private$.y_attribute,
        family_x = private$.family_x, family_y = private$.family_y
      )

      if (is.null(private$.descriptives$dyadwise_shared_partner[[type]])) {
        self$dyadwise_shared_partner(type = type, mode = "global")
      }
      info_global <- private$.descriptives$dyadwise_shared_partner[[type]]

      vals <- if (nrow(cand) > 0) as.numeric(info_global[cand]) else numeric(0)
      vals <- vals[!is.na(vals)]

      if (is.null(value_range)) {
        if (length(vals) > 0) {
          value_range <- range(vals)
        } else {
          value_range <- c(0, 0)
        }
      }
      info_factor <- factor(vals,
        levels = seq(from = value_range[1], to = value_range[2])
      )
      info_table <- table(info_factor)
      if (sum(info_table) > 0) {
        info_table <- info_table / (sum(info_table) * prob + (!prob))
      }

      if (!has_constraints) {
        if (is.null(private$.descriptives$dyadwise_shared_partner_distribution)) {
          private$.descriptives$dyadwise_shared_partner_distribution <- list()
        }
        private$.descriptives$dyadwise_shared_partner_distribution[[key]] <- info_table
      }

      if (plot) {
        base_lab <- paste0("Number of ", type, if (mode == "local") " (Local)" else "", "- Dyadwise Shared Partners")
        barplot(info_table,
          xlab = build_constrained_xlab(base_lab, x_i, x_j, y_i, y_j, family_x = private$.family_x, family_y = private$.family_y),
          ylab = ifelse(prob, "Proportion", "Count"),
          las = 1, ylim = c(0, max(info_table) * 1.2)
        )
      }
      invisible(info_table)
    },
    #' @description
    #' Calculates the degree distribution of the `z_network`.
    #'
    #' A flexible, general function for evaluating network connectivity across
    #' global networks, local neighborhoods, attribute-defined subgroups, and
    #' cross-group spillover pathways.
    #'
    #' \subsection{Topological Scope (\code{mode})}{
    #' \itemize{
    #'   \item \code{"global"} (default): Evaluates degree distributions across all dyads in the network.
    #'   \item \code{"local"}: Evaluates local degree distributions restricted strictly to unit pairs
    #'     that share an overlapping neighborhood (\code{overlap}).
    #' }
    #' }
    #'
    #' \subsection{Directionality and Bipartite Subgroups}{
    #' \itemize{
    #'   \item \strong{Directed networks}: Always returns both \code{out_degree} (ties sent) and
    #'     \code{in_degree} (ties received).
    #'   \item \strong{Undirected networks}: Returns a single overall \code{degree} distribution when
    #'     unconstrained or single-side constrained. When bilateral constraints are supplied
    #'     (e.g., sender \eqn{i} and receiver \eqn{j}), ties are evaluated directionally
    #'     (\eqn{i \to j}), returning both \code{out_degree} and \code{in_degree}.
    #' }
    #' }
    #'
    #' \subsection{Spillover and Subgroup Conditioning}{
    #' Any combination of sender attributes (\code{x_i}, \code{y_i}) and receiver attributes
    #' (\code{x_j}, \code{y_j}) can be specified to measure spillover dynamics:
    #' \itemize{
    #'   \item \code{out_degree}: Distribution of ties sent from matching senders \eqn{i} to matching receivers \eqn{j} (spillover sending capacity).
    #'   \item \code{in_degree}: Distribution of ties received by matching receivers \eqn{j} from matching senders \eqn{i} (spillover exposure).
    #' }
    #' }
    #'
    #' \subsection{Supported Constraint Formats & Internal Handling}{
    #' Attribute constraints (\code{x_i}, \code{x_j}, \code{y_i}, \code{y_j}) accept:
    #' \itemize{
    #'   \item \strong{Exact scalar values}: For binary attributes, matches units with that exact value (e.g., \code{x_i = 1}).
    #'   \item \strong{Continuous / Count shortcuts}: When an attribute is continuous (\code{"normal"}) or count (\code{"poisson"}),
    #'     setting \code{1} internally selects above-mean units (\eqn{x_i > \bar{x}}), and \code{0} selects
    #'     below-or-equal-to-mean units (\eqn{x_i \le \bar{x}}). Any other numeric value \eqn{v} matches units with exact value \eqn{v}.
    #'   \item \strong{Discrete value sets}: Vectors such as \code{x_i = c(1, 2)} match units with any value in that set.
    #'   \item \strong{Filtering functions}: Custom functions (vectorized or scalar), e.g., \code{x_i = function(x) x > 0.5}
    #'     or \code{y_j = \(y) if (y > 2) TRUE else FALSE}.
    #' }
    #' }
    #'
    #' \subsection{Plotting and Axis Labels}{
    #' When \code{plot = TRUE}, mathematical expressions are formatted automatically for the x-axis:
    #' \itemize{
    #'   \item Exact values and sets display as \eqn{x_i == 1} or \eqn{x_i == \text{c(1, 2)}}.
    #'   \item Continuous shortcuts display with sample mean bars as \eqn{x_i > \bar{x}} or \eqn{x_i \le \bar{x}}.
    #'   \item Filtering functions display as \eqn{x_i == \text{"fn"}}.
    #' }
    #' }
    #'
    #' @param value_range (numeric vector or list) A vector \code{c(min, max)} specifying
    #'   the range of degrees to tabulate, or a list with \code{in_degree} and \code{out_degree}.
    #'   If \code{NULL} (default), ranges are inferred from the data.
    #' @param x_i (optional) Exact value, vector, or filtering function for attribute \code{x} of sender unit \eqn{i}.
    #' @param x_j (optional) Exact value, vector, or filtering function for attribute \code{x} of receiver unit \eqn{j}.
    #' @param y_i (optional) Exact value, vector, or filtering function for attribute \code{y} of sender unit \eqn{i}.
    #' @param y_j (optional) Exact value, vector, or filtering function for attribute \code{y} of receiver unit \eqn{j}.
    #' @param prob (logical) If \code{TRUE} (default), returns a probability
    #'   distribution (proportions). If \code{FALSE}, returns raw counts.
    #' @param plot (logical) If \code{TRUE}, plots the degree distribution barplot(s).
    #' @param mode (character) Either \code{"global"} (default) to evaluate across all dyads,
    #'   or \code{"local"} to evaluate only ties within overlapping neighborhoods (\code{overlap}).
    #' @return If the network is directed or if bilateral constraints are provided,
    #'   a list containing two \code{table} objects: \code{out_degree} and \code{in_degree}.
    #'   If undirected without bilateral constraints, a single \code{table} object with
    #'   the \code{degree} distribution.
    #' @examples
    #' data(copenhagen)
    #'
    #' # Overall degree distribution
    #' copenhagen$degree_distribution(prob = FALSE, plot = FALSE)
    #'
    #' # Spillover degree distribution (female to male call connections)
    #' copenhagen$deg_dist(
    #'   x_i = 1, # Sender: female
    #'   x_j = 0, # Receiver: male
    #'   mode = "local",
    #'   plot = FALSE
    #' )
    degree_distribution = function(value_range = NULL,
                                   prob = TRUE,
                                   plot = TRUE,
                                   x_i = NULL,
                                   x_j = NULL,
                                   y_i = NULL,
                                   y_j = NULL,
                                   mode = "global") {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)

      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }

      has_constraints <- has_i_constr || has_j_constr
      identical_constraints <- !private$.directed && has_i_constr && has_j_constr &&
        identical(x_i, x_j) && identical(y_i, y_j)
      is_directed_or_bipartite <- private$.directed || (has_i_constr && has_j_constr && !identical_constraints)

      deg_data <- if (has_constraints || mode == "local") {
        self$degree(x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j, mode = mode)
      } else {
        if (is.null(private$.descriptives$degree)) {
          self$degree()
        }
        private$.descriptives$degree
      }

      if (is_directed_or_bipartite) {
        range_in <- if (is.list(value_range)) {
          value_range$in_degree
        } else {
          (if (!is.null(value_range)) {
            value_range
          } else {
            if (length(deg_data$in_degree_seq) > 0) range(c(deg_data$in_degree_seq, 0)) else c(0, 0)
          })
        }
        range_out <- if (is.list(value_range)) {
          value_range$out_degree
        } else {
          (if (!is.null(value_range)) {
            value_range
          } else {
            if (length(deg_data$out_degree_seq) > 0) range(c(deg_data$out_degree_seq, 0)) else c(0, 0)
          })
        }
        info_in <- factor(deg_data$in_degree_seq,
          levels = seq(from = range_in[1], to = range_in[2])
        )

        info_out <- factor(deg_data$out_degree_seq,
          levels = seq(from = range_out[1], to = range_out[2])
        )
        info_in <- table(info_in)
        info_out <- table(info_out)

        if (sum(info_in) > 0) {
          info_in <- info_in / (sum(info_in) * prob + (!prob))
        }
        if (sum(info_out) > 0) {
          info_out <- info_out / (sum(info_out) * prob + (!prob))
        }

        if (!has_constraints && mode == "global") {
          private$.descriptives$in_degree_distribution <- info_in
          private$.descriptives$out_degree_distribution <- info_out
        }

        if (plot) {
          op <- par(no.readonly = TRUE)
          on.exit(par(op))
          par(mfrow = c(1, 2))
          barplot(info_out,
            xlab = build_constrained_xlab("Out-Degree", x_i, x_j, y_i, y_j, family_x = private$.family_x, family_y = private$.family_y),
            ylab = ifelse(prob, "Proportion", "Count"),
            las = 1, ylim = c(0, max(info_out) * 1.2)
          )
          barplot(info_in,
            xlab = build_constrained_xlab("In-Degree", x_i, x_j, y_i, y_j, family_x = private$.family_x, family_y = private$.family_y),
            ylab = ifelse(prob, "Proportion", "Count"),
            las = 1, ylim = c(0, max(info_in) * 1.2)
          )
        }
        info <- list(in_degree = info_in, out_degree = info_out)
      } else {
        deg_seq <- deg_data$degree_seq
        val_range <- if (is.list(value_range)) {
          value_range$degree
        } else {
          (if (!is.null(value_range)) {
            value_range
          } else {
            if (length(deg_seq) > 0) range(c(deg_seq, 0)) else c(0, 0)
          })
        }
        info <- factor(deg_seq,
          levels = seq(from = val_range[1], to = val_range[2])
        )
        info <- table(info)
        if (sum(info) > 0) {
          info <- info / (sum(info) * prob + (!prob))
        }
        if (!has_constraints && mode == "global") {
          private$.descriptives$degree_distribution <- info
        }
        if (plot) {
          prefix <- if (mode == "local") "Local " else ""
          barplot(info,
            xlab = build_constrained_xlab(paste0(prefix, "Degree"), x_i, x_j, y_i, y_j, family_x = private$.family_x, family_y = private$.family_y),
            ylab = ifelse(prob, "Proportion", "Count"),
            las = 1, ylim = c(0, max(info) * 1.2)
          )
        }
      }
      invisible(info)
    },
    #' @description
    #' Calculates the degree sequence(s) of the `z_network`.
    #'
    #' General function for calculating unit-level degree sequences across global
    #' topologies, local neighborhoods, attribute-defined subsets, or directional
    #' spillover pathways.
    #'
    #' @param x_i (optional) Exact value, vector, or filtering function for attribute \code{x} of sender unit \eqn{i}.
    #' @param x_j (optional) Exact value, vector, or filtering function for attribute \code{x} of receiver unit \eqn{j}.
    #' @param y_i (optional) Exact value, vector, or filtering function for attribute \code{y} of sender unit \eqn{i}.
    #' @param y_j (optional) Exact value, vector, or filtering function for attribute \code{y} of receiver unit \eqn{j}.
    #' @param mode (character) \code{"global"} (default) or \code{"local"}.
    #' @return If the network is directed or if asymmetric/bipartite bilateral constraints are given,
    #'   a list containing two numeric vectors: \code{out_degree_seq} and \code{in_degree_seq}.
    #'   If undirected without constraints or with identical bilateral constraints,
    #'   a list containing the vector \code{degree_seq}.
    #' @examples
    #' data(copenhagen)
    #'
    #' # Global degree sequence
    #' copenhagen$degree()
    #'
    #' # Local spillover degree sequence with filtering functions
    #' copenhagen$deg(
    #'   x_i = function(x) x > 2,
    #'   y_j = function(y) y > 2,
    #'   mode = "local"
    #' )
    degree = function(x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL, mode = "global") {
      if (!mode %in% c("global", "local")) {
        stop("'mode' must be either 'global' or 'local'.")
      }
      has_constraints <- !is.null(x_i) || !is.null(x_j) || !is.null(y_i) || !is.null(y_j)
      res <- list()

      if (!has_constraints && mode == "global") {
        if (private$.directed) {
          if (ncol(private$.z_network) == 2) {
            in_degree_seq_res <- table(private$.z_network[, 2])
            out_degree_seq_res <- table(private$.z_network[, 1])
            in_degree_seq <- numeric(private$.n_units)
            in_degree_seq[as.numeric(names(in_degree_seq_res))] <- in_degree_seq_res

            out_degree_seq <- numeric(private$.n_units)
            out_degree_seq[as.numeric(names(out_degree_seq_res))] <- out_degree_seq_res
          } else {
            in_degree_seq <- colSums(private$.z_network)
            out_degree_seq <- rowSums(private$.z_network)
          }
          res$out_degree_seq <- out_degree_seq
          res$in_degree_seq <- in_degree_seq
        } else {
          if (ncol(private$.z_network) == 2) {
            tmp <- table(private$.z_network)
            degree_seq <- numeric(private$.n_units)
            degree_seq[as.numeric(names(tmp))] <- tmp
          } else {
            degree_seq <- colSums(private$.z_network)
          }
          res$degree_seq <- degree_seq
        }
        private$.descriptives$degree <- res
        return(res)
      }

      # Constrained and/or local calculation
      filter_nodes <- function(attr_vec, spec, type = "binomial") {
        if (is.null(spec)) {
          return(rep(TRUE, length(attr_vec)))
        }
        if (is.function(spec)) {
          res <- tryCatch(
            as.logical(spec(attr_vec)),
            error = function(e) NULL,
            warning = function(w) NULL
          )
          if (is.null(res) || length(res) != length(attr_vec)) {
            res <- as.logical(vapply(attr_vec, spec, logical(1)))
          }
          return(res)
        }
        if (type == "binomial") {
          attr_vec %in% spec
        } else {
          m <- mean(attr_vec)
          res <- rep(FALSE, length(attr_vec))
          if (1 %in% spec) res <- res | (attr_vec > m)
          if (0 %in% spec) res <- res | (attr_vec <= m)
          other_spec <- spec[!spec %in% c(0, 1)]
          if (length(other_spec) > 0) res <- res | (attr_vec %in% other_spec)
          res
        }
      }

      has_i_constr <- !is.null(x_i) || !is.null(y_i)
      has_j_constr <- !is.null(x_j) || !is.null(y_j)

      if (!private$.directed) {
        if (!has_i_constr && has_j_constr) {
          x_i <- x_j
          y_i <- y_j
          x_j <- NULL
          y_j <- NULL
          has_i_constr <- TRUE
          has_j_constr <- FALSE
        }
      }

      identical_constraints <- !private$.directed && has_i_constr && has_j_constr &&
        identical(x_i, x_j) && identical(y_i, y_j)
      is_directed_or_bipartite <- private$.directed || (has_i_constr && has_j_constr && !identical_constraints)

      cond_sender <- filter_nodes(private$.x_attribute, x_i, private$.family_x) & filter_nodes(private$.y_attribute, y_i, private$.family_y)
      cond_receiver <- filter_nodes(private$.x_attribute, x_j, private$.family_x) & filter_nodes(private$.y_attribute, y_j, private$.family_y)
      units_sender <- which(cond_sender)
      units_receiver <- which(cond_receiver)

      if (mode == "local") {
        if (length(units_sender) == 0 || length(units_receiver) == 0) {
          if (is_directed_or_bipartite) {
            res$out_degree_seq <- if (length(units_sender) > 0) rep(0, length(units_sender)) else numeric(0)
            res$in_degree_seq <- if (length(units_receiver) > 0) rep(0, length(units_receiver)) else numeric(0)
          } else {
            res$degree_seq <- if (length(units_sender) > 0) rep(0, length(units_sender)) else numeric(0)
          }
          return(res)
        }

        z_net_all <- if (!private$.directed && ncol(private$.z_network) == 2 && nrow(private$.z_network) > 0) {
          rbind(private$.z_network, private$.z_network[, c(2, 1), drop = FALSE])
        } else {
          private$.z_network
        }

        edges_x_y <- matrix(
          z_net_all[(z_net_all[, 1] %in% units_sender) & (z_net_all[, 2] %in% units_receiver), ],
          ncol = 2
        )

        has_overlap_info <- !is.null(private$.overlap) && nrow(private$.overlap) > 0

        if (has_overlap_info) {
          adj_mat_x_y <- matrix(
            data = NA, nrow = length(units_sender),
            ncol = length(units_receiver),
            dimnames = list(units_sender, units_receiver)
          )

          overlap_tmp <- matrix(
            private$.overlap[(private$.overlap[, 1] %in% units_sender) & (private$.overlap[, 2] %in% units_receiver), ],
            ncol = 2
          )
          if (nrow(overlap_tmp) > 0) {
            row_idx <- match(overlap_tmp[, 1], units_sender)
            col_idx <- match(overlap_tmp[, 2], units_receiver)
            valid <- !is.na(row_idx) & !is.na(col_idx)
            if (any(valid)) {
              adj_mat_x_y[cbind(row_idx[valid], col_idx[valid])] <- 0
            }
          }
          # Exclude units that do not have any overlaps with the candidate set
          has_valid_overlap_row <- rowSums(!is.na(adj_mat_x_y)) > 0
          has_valid_overlap_col <- colSums(!is.na(adj_mat_x_y)) > 0
          adj_mat_x_y <- adj_mat_x_y[has_valid_overlap_row, has_valid_overlap_col, drop = FALSE]

          if (nrow(edges_x_y) > 0 && nrow(adj_mat_x_y) > 0 && ncol(adj_mat_x_y) > 0) {
            which_overlap <- check_overlap(edges_x_y, private$.overlap)
            edges_x_y_overlap <- matrix(edges_x_y[which_overlap, ], ncol = 2)
            if (nrow(edges_x_y_overlap) > 0) {
              row_idx <- match(edges_x_y_overlap[, 1], rownames(adj_mat_x_y))
              col_idx <- match(edges_x_y_overlap[, 2], colnames(adj_mat_x_y))
              valid <- !is.na(row_idx) & !is.na(col_idx)
              if (any(valid)) {
                adj_mat_x_y[cbind(row_idx[valid], col_idx[valid])] <- 1
              }
            }
          }
        } else {
          adj_mat_x_y <- matrix(
            0,
            nrow = length(units_sender),
            ncol = length(units_receiver),
            dimnames = list(units_sender, units_receiver)
          )
          if (nrow(edges_x_y) > 0) {
            row_idx <- match(edges_x_y[, 1], rownames(adj_mat_x_y))
            col_idx <- match(edges_x_y[, 2], colnames(adj_mat_x_y))
            valid <- !is.na(row_idx) & !is.na(col_idx)
            if (any(valid)) {
              adj_mat_x_y[cbind(row_idx[valid], col_idx[valid])] <- 1
            }
          }
        }

        if (is_directed_or_bipartite) {
          res$out_degree_seq <- rowSums(adj_mat_x_y, na.rm = TRUE)
          res$in_degree_seq <- colSums(adj_mat_x_y, na.rm = TRUE)
        } else {
          res$degree_seq <- rowSums(adj_mat_x_y, na.rm = TRUE)
        }
        return(res)
      }

      # Global mode with constraints
      z_mat <- if (ncol(private$.z_network) == 2) {
        mat <- matrix(0, nrow = private$.n_units, ncol = private$.n_units)
        if (nrow(private$.z_network) > 0) {
          mat[private$.z_network] <- 1
        }
        mat
      } else {
        private$.z_network
      }

      if (!private$.directed) {
        z_mat <- pmax(z_mat, t(z_mat))
      }

      if (is_directed_or_bipartite) {
        out_deg <- numeric(private$.n_units)
        in_deg <- numeric(private$.n_units)
        if (length(units_sender) > 0 && length(units_receiver) > 0) {
          out_deg[units_sender] <- rowSums(matrix(z_mat[units_sender, units_receiver, drop = FALSE], nrow = length(units_sender)))
          in_deg[units_receiver] <- colSums(matrix(z_mat[units_sender, units_receiver, drop = FALSE], ncol = length(units_receiver)))
        }
        res$out_degree_seq <- out_deg[units_sender]
        res$in_degree_seq <- in_deg[units_receiver]
      } else {
        deg_seq <- numeric(private$.n_units)
        if (length(units_sender) > 0 && length(units_receiver) > 0) {
          sub_mat <- matrix(z_mat[units_sender, units_receiver, drop = FALSE], nrow = length(units_sender))
          deg_seq[units_sender] <- rowSums(sub_mat)
        }
        res$degree_seq <- deg_seq[units_sender]
      }

      return(res)
    },
    #' @description
    #' Short alias for `degree`.
    #' @param x_i Optional sender attribute constraint.
    #' @param x_j Optional receiver attribute constraint.
    #' @param y_i Optional sender attribute constraint.
    #' @param y_j Optional receiver attribute constraint.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @return Degree sequence(s).
    deg = function(x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL, mode = "global") {
      self$degree(x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j, mode = mode)
    },
    #' @description
    #' Short alias for `degree_distribution`.
    #' Supports standard, local, and attribute-constrained (spillover) degree distributions.
    #' @param value_range Optional range of degrees to tabulate.
    #' @param prob (logical) If `TRUE`, returns proportions.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param x_i Optional sender attribute constraint.
    #' @param x_j Optional receiver attribute constraint.
    #' @param y_i Optional sender attribute constraint.
    #' @param y_j Optional receiver attribute constraint.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @return Degree distribution table(s).
    deg_dist = function(value_range = NULL, prob = TRUE, plot = TRUE,
                        x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL,
                        mode = "global") {
      self$degree_distribution(
        value_range = value_range, prob = prob, plot = plot,
        x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j,
        mode = mode
      )
    },
    #' @description
    #' Alias for `deg_dist` / `degree_distribution`.
    #' @param value_range Optional range of degrees to tabulate.
    #' @param prob (logical) If `TRUE`, returns proportions.
    #' @param plot (logical) If `TRUE`, plots the distribution.
    #' @param x_i Optional sender attribute constraint.
    #' @param x_j Optional receiver attribute constraint.
    #' @param y_i Optional sender attribute constraint.
    #' @param y_j Optional receiver attribute constraint.
    #' @param mode (character) `"global"` (default) or `"local"`.
    #' @return Degree distribution table(s).
    degree_dist = function(value_range = NULL, prob = TRUE, plot = TRUE,
                           x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL,
                           mode = "global") {
      self$degree_distribution(
        value_range = value_range, prob = prob, plot = plot,
        x_i = x_i, x_j = x_j, y_i = y_i, y_j = y_j,
        mode = mode
      )
    },
    #' @description
    #' Plot the network using `igraph`.
    #'
    #' Visualizes the `z_network` using the `igraph` package.
    #' Nodes can be colored by `x_attribute` and sized by `y_attribute`.
    #' `neighborhood` edges can be plotted as a background layer.
    #'
    #' @param node_color (character) Attribute to map to node color.
    #'   One of `"x"` (default), `"y"`, or `"none"`.
    #' @param node_size (character) Attribute to map to node size.
    #'   One of `"y"` (default), `"x"`, or `"constant"`.
    #' @param show_overlap (logical) If `TRUE` (default), plot the
    #'   `neighborhood` edges as a background layer.
    #' @param layout An `igraph` layout function (e.g., `igraph::layout_with_fr`).
    #' @param network_edges_col (character) Color for the `z_network` edges.
    #' @param neighborhood_edges_col (character) Color for the `neighborhood` edges.
    #' @param main (character) The main title for the plot.
    #' @param legend_col_n_levels (integer) Number of levels for the color legend.
    #' @param legend_size_n_levels (integer) Number of levels for the size legend.
    #' @param legend_pos (character) Position of the legend (e.g., `"right"`).
    #' @param alpha_neighborhood (numeric) Alpha transparency for neighborhood edges.
    #' @param edge.width (numeric) Width of the network edges.
    #' @param vertex.frame.width (numeric) Width of the vertex frame.
    #' @param edge.arrow.size (numeric) Size of the arrowheads for directed edges.
    #' @param coords (matrix) Optional matrix of x-y coordinates for node layout.
    #' @param legend_size (numeric) Scaling factor for the size legend.
    #' @param ... Additional arguments passed to `plot.igraph`.
    #' @return A list containing the `igraph` object (`graph`) and the
    #'   layout coordinates (`coords`), invisibly.
    plot = function(node_color = "x",
                    node_size = "y",
                    show_overlap = TRUE,
                    layout = igraph::layout_with_fr,
                    network_edges_col = "grey60",
                    neighborhood_edges_col = "orange",
                    main = "",
                    legend_col_n_levels = NULL,
                    legend_size_n_levels = NULL,
                    legend_pos = "right",
                    alpha_neighborhood = 0.2,
                    edge.width = 1,
                    edge.arrow.size = 1,
                    vertex.frame.width = 0.5,
                    coords = NULL, legend_size = 0.5,
                    ...) {
      # browser()
      if (is.null(legend_col_n_levels)) {
        if (node_color == "x") {
          if (private$.family_x == "binomial") {
            legend_col_n_levels <- 2
          } else {
            legend_col_n_levels <- 4
          }
        } else {
          if (private$.family_y == "binomial") {
            legend_col_n_levels <- 2
          } else {
            legend_col_n_levels <- 4
          }
        }
      }

      if (is.null(legend_size_n_levels)) {
        if (node_size == "x") {
          if (private$.family_x == "binomial") {
            legend_size_n_levels <- 2
          } else {
            legend_size_n_levels <- 3
          }
        } else {
          if (private$.family_y == "binomial") {
            legend_size_n_levels <- 2
          } else {
            legend_size_n_levels <- 3
          }
        }
      }

      # --- build igraph object from edge list

      if (!private$.directed) {
        g <- igraph::graph_from_edgelist(as.matrix(private$.z_network[private$.z_network[, 1] < private$.z_network[, 2], ]),
          directed = private$.directed
        )
      } else {
        g <- igraph::graph_from_edgelist(as.matrix(private$.z_network), directed = private$.directed)
      }
      n <- private$.n_units
      if (igraph::vcount(g) < n) {
        g <- igraph::add_vertices(g, n - igraph::vcount(g))
      }
      # set canonical vertex names = 1:n so mapping is stable
      igraph::V(g)$name <- as.character(seq_len(igraph::vcount(g)))

      # --- map attributes to nodes
      xa <- private$.x_attribute
      ya <- private$.y_attribute

      # normalize attributes for visual mapping
      # norm <- function(v) (v - min(v, na.rm = TRUE)) / (max(v, na.rm = TRUE) - min(v, na.rm = TRUE) + 1e-9)
      xa_n <- find_ranks(xa)
      xa_n <- xa_n / max(xa_n)
      ya_n <- find_ranks(ya)
      ya_n <- ya_n / max(ya_n)


      # node color
      if (node_color == "x") {
        pal <- grDevices::colorRampPalette(c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"))(100)
        node_cols <- pal[as.numeric(cut(xa_n, 100))]
        color_attr <- xa
        color_lab <- "x"
      } else if (node_color == "y") {
        pal <- grDevices::colorRampPalette(c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"))(100)
        node_cols <- pal[as.numeric(cut(ya_n, 100))]
        color_attr <- ya
        color_lab <- "y"
      } else {
        node_cols <- rep("grey70", n)
        color_attr <- NULL
        color_lab <- NULL
      }

      if (node_size == "x") {
        node_sizes <- xa_n
        size_attr <- xa

        size_lab <- "x"
      } else if (node_size == "y") {
        node_sizes <- ya_n
        size_attr <- ya

        size_lab <- "y"
      } else {
        node_sizes <- rep(6, n)
        size_attr <- NULL
        size_lab <- NULL
      }

      if (is.null(coords)) {
        coords <- layout(g)
      } else {
        # sanity: make sure dimensions match
        if (!is.matrix(coords) || nrow(coords) != igraph::vcount(g) || ncol(coords) < 2) {
          stop("`coords` must be a matrix with nrow = vcount(g) and at least 2 columns.")
        }
      }
      if (show_overlap && !is.null(private$.neighborhood) && nrow(private$.neighborhood) > 0) {
        overlap_edges <- as.matrix(private$.neighborhood)
        overlap_edges <- overlap_edges[overlap_edges[, 1] != overlap_edges[, 2], , drop = FALSE]
        g2 <- igraph::graph_from_data_frame(d = overlap_edges, vertices = igraph::V(g)$name, directed = FALSE)
        igraph::V(g2)$name <- as.character(seq_len(igraph::vcount(g2)))
        igraph::plot.igraph(
          g2,
          edge.color = grDevices::adjustcolor(neighborhood_edges_col, alpha.f = alpha_neighborhood),
          vertex.color = "white",
          edge.width = 1,
          vertex.size = 0,
          vertex.label = NA,
          layout = coords
        )
        add_mode <- TRUE
      } else {
        add_mode <- FALSE
      }
      # browser()
      plot(
        g,
        vertex.frame.width = vertex.frame.width,
        vertex.color = node_cols,
        vertex.size = log(node_sizes + 1) * 10 + 2,
        vertex.label = NA,
        edge.color = network_edges_col,
        edge.width = edge.width,
        edge.arrow.size = edge.arrow.size,
        add = add_mode,
        layout = coords,
      )

      if (!is.null(color_attr) || !is.null(size_attr)) {
        if (!is.null(color_attr)) {
          # browser()
          rng <- quantile(color_attr, na.rm = TRUE, probs = seq(from = 0, to = 1, length.out = legend_col_n_levels))
          color_vals <- round(rng, 2)
          color_cols <- c(pal[1], pal[round(seq(from = 0, to = 1, length.out = legend_col_n_levels) * 100)])
        } else {
          color_cols <- NULL
          color_vals <- NULL
        }
        if (!is.null(size_attr)) {
          vals <- sort(unique(size_attr))[findInterval(
            vec = sort(unique(node_sizes)),
            x = seq(from = 0, to = 0.99, length.out = legend_size_n_levels)
          ) + 1]
          sizes <- sort(unique(node_sizes))[findInterval(
            vec = sort(unique(node_sizes)),
            x = seq(from = 0, to = 0.99, length.out = legend_size_n_levels)
          ) + 1]
          sizes <- log(sizes + 1) * 10 + 2
        } else {
          sizes <- NULL
          vals <- NULL
        }
        length_a <- length(color_vals)
        length_b <- length(vals)
        if (length_a > 0) {
          labels_a <- paste(color_lab, "=", color_vals)
        } else {
          labels_a <- c()
        }
        if (length_b > 0) {
          labels_b <- paste(size_lab, "=", round(vals, 2))

          color_cols_b <- rep("grey70", times = length_b)
        } else {
          labels_b <- c()
          color_cols_b <- c()
        }

        labels <- c(labels_a, labels_b)
        colors_tmp <- c(color_cols, color_cols_b)
        sizes <- c(rep(1, times = length_a), sizes / 8)
        legend(
          title = "Legend",
          legend_pos,
          legend = labels,
          pt.bg = colors_tmp,
          border = "black",
          pch = 21,
          pt.cex = sizes,
          cex = legend_size
        )
      }

      invisible(list(graph = g, coords = coords))
    },
    #' @description
    #' Print a summary of the `iglm.data` object to the console.
    #'
    #' @param digits (integer) Number of digits to round numeric output to.
    #' @param ... Additional arguments (not used).
    #' @return The object's private environment, invisibly.
    print = function(digits = 3, ...) {
      n <- as.integer(private$.n_units)
      dir_flag <- isTRUE(private$.directed)

      m_z <- nrow(private$.z_network)
      m_nb <- nrow(private$.neighborhood)
      numfmt <- function(v) format(v, digits = digits, trim = TRUE)

      summarize_attr <- function(v, family, scale) {
        v <- as.vector(v)
        if (family == "binomial") {
          p1 <- mean(v == 1, na.rm = TRUE)
          paste0("binomial mean = ", numfmt(p1))
        } else if (family == "poisson") {
          paste0("poisson mean = ", numfmt(mean(v, na.rm = TRUE)))
        } else if (family == "normal") {
          paste0(
            "normal mean = ", numfmt(mean(v, na.rm = TRUE)),
            ", sd = ", numfmt(stats::sd(v, na.rm = TRUE))
          )
        } else {
          paste0("unknown family; length = ", length(v))
        }
      }

      x_sum <- summarize_attr(
        private$.x_attribute,
        private$.family_x,
        private$.scale_x
      )
      y_sum <- summarize_attr(
        private$.y_attribute,
        private$.family_y,
        private$.scale_y
      )

      w <- 28

      cat("iglm.data object\n")
      cat(sprintf("  %-*s: %s\n", w, "units", n))
      cat(sprintf("  %-*s: %s\n", w, "directed", if (dir_flag) "TRUE" else "FALSE"))
      edge_label <- if (private$.label_z != "z") {
        sprintf("connections [%s] (fixed = %s)", private$.label_z, private$.fix_z)
      } else {
        sprintf("connections (fixed = %s)", private$.fix_z)
      }
      cat(sprintf("  %-*s: %s\n", w, edge_label, m_z))

      cat(sprintf("  %-*s: %s\n", w, "neighborhood connections", m_nb))
      cat("\nAttribute summaries\n")

      x_label <- if (private$.label_x != "x") {
        sprintf("x_attribute [%s] (fixed = %s)", private$.label_x, private$.fix_x)
      } else {
        sprintf("x_attribute (fixed = %s)", private$.fix_x)
      }
      y_label <- if (private$.label_y != "y") {
        sprintf("y_attribute [%s]", private$.label_y)
      } else {
        "y_attribute"
      }
      cat(sprintf("  %-*s: %s\n", w, x_label, x_sum))
      cat(sprintf("  %-*s: %s\n", w, y_label, y_sum))
      invisible(private)
    }
  ),

  # --- Active Bindings ---
  active = list(
    #' @field label_x (`character`) Label/name for `x_attribute`.
    label_x = function(value) {
      if (missing(value)) private$.label_x else self$set_label_x(value)
    },
    #' @field label_y (`character`) Label/name for `y_attribute`.
    label_y = function(value) {
      if (missing(value)) private$.label_y else self$set_label_y(value)
    },
    #' @field label_z (`character`) Label/name for `z_network`.
    label_z = function(value) {
      if (missing(value)) private$.label_z else self$set_label_z(value)
    },
    #' @field x_attribute (`numeric`) The vector for the first unit-level attribute.
    x_attribute = function(value) {
      if (missing(value)) {
        private$.x_attribute
      } else {
        self$set_x_attribute(value)
      }
    },

    #' @field y_attribute (`numeric`) The vector for the second unit-level attribute.
    y_attribute = function(value) {
      if (missing(value)) private$.y_attribute else self$set_y_attribute(value)
    },

    #' @field z_network (`matrix`) The primary network structure as a 2-column integer edgelist.
    z_network = function(value) {
      if (missing(value)) private$.z_network else self$set_z_network(value)
    },

    #' @field neighborhood (`matrix`) Read-only. The secondary/neighborhood structure as a 2-column integer edgelist. An empty matrix if not provided.
    neighborhood = function(value) {
      if (missing(value)) {
        if (is.null(private$.neighborhood)) matrix(0, nrow = 0, ncol = 2) else private$.neighborhood
      } else {
        stop("`neighborhood` is read-only.", call. = FALSE)
      }
    },

    #' @field overlap (`matrix`) Read-only. The calculated overlap relation (dyads with shared neighbors in `neighborhood`) as a 2-column integer edgelist. An empty matrix if overlap hasn't been computed or is not available.
    overlap = function(value) {
      if (missing(value)) {
        if (is.null(private$.overlap)) matrix(0, nrow = 0, ncol = 2) else private$.overlap
      } else {
        stop("`overlap` is read-only.", call. = FALSE)
      }
    },

    #' @field directed (`logical`) Indicates if the `z_network` is treated as directed.
    directed = function(value) {
      if (missing(value)) private$.directed else stop("`directed` is read-only.", call. = FALSE)
    },

    #' @field n_units (`integer`) The total number of units (nodes) in the network.
    n_units = function(value) {
      if (missing(value)) private$.n_units else stop("`n_units` is read-only.", call. = FALSE)
    },
    #' @field n_actor (`integer`) Deprecated alias for `n_units`.
    n_actor = function(value) {
      if (missing(value)) {
        warning("`n_actor` is deprecated, please use `n_units`.", call. = FALSE)
        private$.n_units
      } else {
        stop("`n_actor` is read-only.", call. = FALSE)
      }
    },
    #' @field family_x (`character`) The specified distribution family for the `x_attribute`.
    family_x = function(value) {
      if (missing(value)) private$.family_x else self$set_family_x(value)
    },
    #' @field family_y (`character`) The specified distribution family for the `y_attribute`.
    family_y = function(value) {
      if (missing(value)) private$.family_y else self$set_family_y(value)
    },
    #' @field scale_x (`numeric`) The scale parameter associated with the `x_attribute`.
    scale_x = function(value) {
      if (missing(value)) private$.scale_x else self$set_scale_x(value)
    },
    #' @field scale_y (`numeric`) The scale parameter associated with the `y_attribute`.
    scale_y = function(value) {
      if (missing(value)) private$.scale_y else self$set_scale_y(value)
    },
    #' @field fix_x (`logical`) Indicates if the `x_attribute` is fixed during estimation/simulation.
    fix_x = function(value) {
      if (missing(value)) private$.fix_x else self$set_fix_x(value)
    },
    #' @field fix_z (`logical`) RIndicates if the `z_network` is fixed during estimation/simulation.
    fix_z = function(value) {
      if (missing(value)) private$.fix_z else self$set_fix_z(value)
    },
    #' @field descriptives (`list`)A list storing computed descriptive statistics for the network and attributes.
    descriptives = function(value) {
      if (missing(value)) private$.descriptives else stop("`descriptives` is read-only.", call. = FALSE)
    },
    #' @field fix_z_alocal (`logical`) Flag indicating whether nonoverlap edges are treated as random.
    fix_z_alocal = function(value) {
      if (missing(value)) private$.fix_z_alocal else self$set_fix_z_alocal(value)
    }
  )
)

#' Constructor for the iglm.data R6 object
#'
#' @description
#' Creates a `iglm.data` object, which stores network and attribute data.
#' This function acts as a user-friendly interface to the `iglm.data` R6 class generator.
#' It handles data input, infers parameters like the number of units (`n_units`)
#' and network directedness (`directed`) if not explicitly provided, processes
#' network data into a consistent edgelist format, calculates the overlap
#' relation based on an optional neighborhood definition, and performs
#' extensive validation of all inputs.
#'
#' @param x_attribute A numeric vector for the first unit-level attribute.
#' @param y_attribute A numeric vector for the second unit-level attribute.
#' @param z_network A matrix representing the network. Can be a 2-column
#'   edgelist or a square adjacency matrix.
#' @param neighborhood An optional matrix for the neighborhood representing local dependence.
#'   Can be a 2-column edgelist or a square adjacency matrix.
#'   A tie in `neighborhood` between unit i and j indicates that j is in the neighborhood of i,
#'   implying dependence between the respective units.
#' @param directed A logical value indicating if `z_network` is directed.
#'   If `NA` (default), directedness is inferred from the symmetry of
#'   `z_network`.
#' @param n_units An integer for the number of units in the system.
#'   If `NA` (default), `n_units` is inferred from the attributes or
#'   network matrices.
#' @param family_x Character string for the family of `x_attribute`.
#'   Must be one of `"binomial"`, `"poisson"`, or `"normal"`.
#'   Default is `"binomial"`.
#' @param family_y Character string for the family of `y_attribute`.
#'   Must be one of `"binomial"`, `"poisson"`, or `"normal"`.
#'   Default is `"binomial"`.
#' @param scale_x A positive numeric value for scaling (e.g., variance
#'   for "normal" type). Default is 1.
#' @param scale_y A positive numeric value for scaling (e.g., variance
#'   for "normal" type). Default is 1.
#' @param fix_x (logical) If `TRUE`, the 'x' predictor is held fixed
#'   during estimation/simulation (fixed design in regression). Default is `FALSE`.
#' @param fix_z (logical) If `TRUE`, the 'z' network is held fixed
#'   during estimation/simulation (fixed network design). Default is `FALSE`.
#'   Setting this to TRUE, allows practicioners to estimate autologistic unit attribute models,
#'   which were introduced in binary settings in Daraganova, G., & Robins, G. (2013).
#' @param fix_z_alocal (logical) If `TRUE`, edges outside the overlap region
#'   are fixed, else they are random (default).
#' @param return_neighborhood Logical. If `TRUE` (default) and
#'   `neighborhood` is `NULL`, a full neighborhood (all dyads) is
#'   generated implying global dependence. If `FALSE`, no neighborhood is set.
#' @param file (character) Optional file path to load a saved `iglm.data` object state.
#' @param label_x Character string for the label/name of `x_attribute`. Default is `"x"`.
#' @param label_y Character string for the label/name of `y_attribute`. Default is `"y"`.
#' @param label_z Character string for the label/name of `z_network`. Default is `"z"`.
#' @param n_actor (Deprecated) An integer for the number of units. Please use \code{n_units} instead.
#' @return An object of class `iglm.data` (and `R6`).
#' @references
#' Fritz, C., Schweinberger, M. , Bhadra S., and D. R. Hunter (2025). A Regression Framework for Studying Relationships among Attributes under Network Interference. Journal of the American Statistical Association, to appear.
#'
#' Daraganova, G., and Robins, G. (2013). Exponential random graph models for social networks: Theory, methods and applications, 102-114. Cambridge University Press.
#' @examples
#' \donttest{
#' data("state_twitter")
#' state_twitter$iglm.data$degree_distribution(prob = FALSE, plot = TRUE)
#' state_twitter$iglm.data$geodesic_distances_distribution(prob = FALSE, plot = TRUE)
#' state_twitter$iglm.data$mean_x()
#' state_twitter$iglm.data$mean_y()
#' }
#'
#' # Generate a small iglm data object either via adjacency matrix or edgelist
#' tmp_adjacency <- iglm.data(
#'   z_network = matrix(c(
#'     0, 1, 1, 0,
#'     1, 0, 0, 1,
#'     1, 0, 0, 1,
#'     0, 1, 1, 0
#'   ), nrow = 4, byrow = TRUE),
#'   directed = FALSE,
#'   n_units = 4,
#'   family_x = "binomial",
#'   family_y = "binomial"
#' )
#'
#'
#' tmp_edgelist <- iglm.data(
#'   z_network = tmp_adjacency$z_network,
#'   directed = FALSE,
#'   n_units = 4,
#'   family_x = "binomial",
#'   family_y = "binomial"
#' )
#'
#' tmp_edgelist$mean_z()
#' tmp_adjacency$mean_z()
#' @export
iglm.data <- function(x_attribute = NULL, y_attribute = NULL, z_network = NULL,
                      neighborhood = NULL, directed = TRUE, n_units = NA,
                      family_x = "binomial", family_y = "binomial",
                      scale_x = 1, scale_y = 1,
                      fix_x = FALSE,
                      fix_z = FALSE,
                      fix_z_alocal = FALSE,
                      return_neighborhood = TRUE, file = NULL,
                      label_x = "x", label_y = "y", label_z = "z",
                      n_actor = NULL) {
  if (is.na(n_units) && !is.null(n_actor)) {
    warning("'n_actor' is deprecated; please use 'n_units' instead.", call. = FALSE)
    n_units <- n_actor
  }
  if (!is.null(z_network)) {
    z_network <- as.matrix(z_network)
  }
  if (!is.null(neighborhood)) {
    neighborhood <- as.matrix(neighborhood)
  }

  iglm.data_generator$new(
    x_attribute = as.numeric(x_attribute),
    y_attribute = as.numeric(y_attribute),
    z_network = z_network,
    neighborhood = neighborhood,
    directed = as.logical(directed),
    n_units = n_units,
    family_x = as.character(family_x),
    family_y = as.character(family_y),
    scale_x = as.numeric(scale_x),
    scale_y = as.numeric(scale_y),
    fix_x = as.logical(fix_x),
    fix_z = as.logical(fix_z),
    fix_z_alocal = fix_z_alocal,
    return_neighborhood = as.logical(return_neighborhood),
    file = file,
    label_x = label_x,
    label_y = label_y,
    label_z = label_z
  )
}
