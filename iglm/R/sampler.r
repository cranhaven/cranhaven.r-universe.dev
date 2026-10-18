#' @docType class
#' @title Single Component Sampler Settings (R6 Class)
#' @description
#' The `sampler_net_attr` class is a simple R6 container used within the
#' `sampler.iglm` class. It holds the MCMC sampling parameters
#' for a single component of the `iglm` model, such as one attribute
#' (e.g., `x_attribute`) or a part of the network (e.g., `z_network` within
#' the overlap). It stores the number of proposals and the TNT flag.
#' The random seed is managed centrally by the parent `sampler.iglm` object.
#' @importFrom R6 R6Class
#' @importFrom stats runif
#' @export
sampler.net.attr.generator <- R6::R6Class("sampler.net.attr",
  private = list(
    .n_proposals = NULL,
    .tnt = NULL
  ),
  public = list(
    #' @description
    #' Create a new `sampler_net_attr` object.
    #' @param n_proposals (integer) The number of MCMC proposals (iterations)
    #'   to perform for this specific component during each sampling step.
    #'   Default is 10000. Must be a non-negative integer.
    #' @param file (character or `NULL`) If provided, loads the sampler state from
    #'  the specified .rds file instead of initializing from parameters.
    #' @param tnt (logical) If `TRUE` (default), use Tie-No-Tie sampling (only if used for networks).
    #' @return A new `sampler_net_attr` object.
    initialize = function(n_proposals = 10000, file = NULL, tnt = TRUE) {
      if (!is.null(file)) {
        if (!file.exists(file)) {
          stop(paste("File not found:", file), call. = FALSE)
        }
        message(paste("Loading object state from:", file))
        data <- readRDS(file)
        if (!is.list(data) || !"n_proposals" %in% names(data)) {
          stop("File does not contain a valid sampler_net_attr state.", call. = FALSE)
        }
        private$.n_proposals <- as.integer(data$n_proposals)
        private$.tnt <- if ("tnt" %in% names(data)) as.logical(data$tnt) else TRUE
      } else {
        private$.n_proposals <- as.integer(n_proposals)
        private$.tnt <- as.logical(tnt)
      }
      invisible(self)
    },
    #' @description
    #' Print a summary of the sampler settings for this component.
    #' @param indent (character) Indentation string. Default is "  ".
    #' @return The object itself, invisibly.
    print = function(indent = "  ") {
      cat(paste0(indent, "Number of proposals : ", format(private$.n_proposals), "\n"))
      cat(paste0(indent, "TNT sampling        : ", if (isTRUE(private$.tnt)) "TRUE" else "FALSE", "\n"))
      invisible(self)
    },
    #' @description Gathers all data into a list.
    #' @return A list with `n_proposals` and `tnt`.
    gather = function() {
      list(n_proposals = private$.n_proposals, tnt = private$.tnt)
    },
    #' @description Sets the number of MCMC proposals.
    #' @param n_proposals (integer) Number of proposals.
    set_n_proposals = function(n_proposals) {
      private$.n_proposals <- as.integer(n_proposals)
    },
    #' @description Sets whether to use TNT sampling.
    #' @param tnt (logical) `TRUE` to use TNT sampling.
    set_tnt = function(tnt) {
      private$.tnt <- as.logical(tnt)
    },
    #' @description Save state to an .rds file.
    #' @param file (character) File path.
    #' @return The object itself, invisibly.
    save = function(file) {
      if (missing(file) || !is.character(file) || length(file) != 1) {
        stop("A valid 'file' (character string) must be provided.", call. = FALSE)
      }
      saveRDS(self$gather(), file = file)
      message(paste("Object state saved to:", file))
      invisible(self)
    }
  ),
  active = list(
    #' @field n_proposals (`integer`) Read-only. Number of MCMC proposals per step.
    n_proposals = function(value) {
      if (missing(value)) private$.n_proposals else stop("`n_proposals` is read-only.", call. = FALSE)
    },
    #' @field tnt (`logical`) Read-only. Whether TNT sampling is used.
    tnt = function(value) {
      if (missing(value)) private$.tnt else stop("`tnt` is read-only.", call. = FALSE)
    }
  )
)
#' @title Constructor for Single Component Sampler Settings
#'
#' @description
#' Creates an object of class `sampler_net_attr` (and `R6`). Specifies MCMC
#' sampling parameters for one component (attribute or network) within the
#' `iglm` simulation framework. Used as input to `sampler.iglm()`.
#'
#' @param n_proposals (integer) Number of MCMC proposals per sampling update.
#'   Default: 10000.
#' @param file (character or `NULL`) If provided, loads state from an .rds file.
#' @param tnt (logical) If `TRUE` (default), use Tie-No-Tie sampling.
#' @return An object of class `sampler_net_attr` (and `R6`).
#' @export
#' @seealso `sampler.iglm`
#' @examples
#' sampler_comp_default <- sampler.net.attr()
#' sampler_comp_default
#'
#' sampler_comp_custom <- sampler.net.attr(n_proposals = 50000, tnt = FALSE)
#' sampler_comp_custom
sampler.net.attr <- function(n_proposals = 10000, file = NULL, tnt = TRUE) {
  sampler.net.attr.generator$new(n_proposals = n_proposals, file = file, tnt = tnt)
}


#' @docType class
#' @title iglm Sampler Settings (R6 Class)
#' @description
#' The `sampler.iglm` class is an R6 container for specifying and storing
#' the parameters that control the MCMC (Markov Chain Monte Carlo) sampling
#' process used in \code{\link{iglm}} simulations and potentially during estimation.
#' It includes settings for the number of simulations, burn-in period,
#' initialization, and
#' parallelization options. It also holds references to component samplers
#' (\code{\link{sampler.net.attr}} objects) responsible for sampling individual parts
#' (attributes x, y, network z).
#' @importFrom R6 R6Class
sampler.iglm.generator <- R6::R6Class("sampler.iglm",
  private = list(
    .sampler_x = NULL,
    .sampler_y = NULL,
    .sampler_z = NULL,
    .n_simulation = NULL,
    .n_burn_in = NULL,
    .init_empty = NULL,
    .seed = NULL,
    .cluster = NULL,
    .validate = function() {
      # Check if cluster is valid
      if (!is.null(private$.cluster)) {
        if (!inherits(private$.cluster, "cluster")) {
          stop("`cluster` must be a valid cluster object from the 'parallel' package.", call. = FALSE)
        }
      }
      if (private$.n_simulation < 0) {
        stop("`n_simulation` must be a non-negative integer.", call. = FALSE)
      }
      if (private$.n_burn_in < 0) {
        stop("`n_burn_in` must be a non-negative integer.", call. = FALSE)
      }
      if (!is.logical(private$.init_empty)) {
        stop("`init_empty` must be a logical value (TRUE or FALSE).", call. = FALSE)
      }
      if (!inherits(private$.sampler_x, "sampler.net.attr")) {
        stop("`sampler_x` must be created with `sampler.net.attr()`.", call. = FALSE)
      }
      if (!inherits(private$.sampler_y, "sampler.net.attr")) {
        stop("`sampler_y` must be created with `sampler.net.attr()`.", call. = FALSE)
      }
      if (!inherits(private$.sampler_z, "sampler.net.attr")) {
        stop("`sampler_z` must be created with `sampler.net.attr()`.", call. = FALSE)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `sampler.iglm` object. Initializes all sampler settings,
    #' using defaults for component samplers (`sampler.net.attr`) if not provided,
    #' and validates inputs.
    #' @param sampler_x An object of class `sampler.net.attr` controlling
    #'   sampling for the x attribute. If `NULL`, defaults from `sampler.net.attr()` are used.
    #' @param sampler_y An object of class `sampler.net.attr` controlling
    #'   sampling for the y attribute. If `NULL`, defaults from `sampler.net.attr()` are used.
    #' @param sampler_z An object of class `sampler.net.attr` controlling
    #'   sampling for the z network (within the defined neighborhood/overlap).
    #'   If `NULL`, defaults from `sampler.net.attr()` are used.
    #' @param n_simulation (integer) The number of network/attribute configurations
    #' @param init_empty (logical) If `FALSE` (default), the MCMC chain is
    #'   initialized from observed data. If `TRUE`, the MCMC chain is
    #'   initialized from an empty state (e.g., empty network, attributes at mean).
    #' @param seed (integer or `NA`) A single integer seed for the random number
    #'   generator. If `NA` (default), a fixed default seed (`123456789`) is used
    #'   for reproducibility.
    #' @param cluster A parallel cluster object (e.g., from the `parallel` package)
    #'   to use for running simulations in parallel. If `NULL` (default), simulations
    #'   are run sequentially.
    #' @param file (character or `NULL`) If provided, loads the sampler state from
    #'  the specified .rds file instead of initializing from parameters.
    #' @return A new `sampler.iglm` object.
    initialize = function(sampler_x = NULL, sampler_y = NULL, sampler_z = NULL,
                          n_simulation = 100, n_burn_in = 10, init_empty = FALSE,
                          seed = NA, cluster = NULL, file = NULL) {
      if (is.null(file)) {
        # Use default component samplers if not provided
        private$.sampler_x <- if (is.null(sampler_x)) sampler.net.attr() else sampler_x
        private$.sampler_y <- if (is.null(sampler_y)) sampler.net.attr() else sampler_y
        private$.sampler_z <- if (is.null(sampler_z)) sampler.net.attr() else sampler_z

        # Store core parameters
        private$.n_simulation <- as.integer(n_simulation)
        private$.n_burn_in <- as.integer(n_burn_in)
        private$.init_empty <- as.logical(init_empty)
        private$.cluster <- cluster
        if (is.na(seed)) {
          private$.seed <- 123456789  # Default seed if NA
        } else {
          private$.seed <- as.integer(seed)
        }

        # Validate sub-samplers
        sub_samplers <- list(private$.sampler_x, private$.sampler_y, private$.sampler_z)
        if (!all(sapply(sub_samplers, inherits, "sampler.net.attr"))) {
          stop("Component samplers (sampler_x, _y, _z) must be created with `sampler.net.attr()`.")
        }
      } else {
        if (!file.exists(file)) {
          stop(paste("File not found:", file), call. = FALSE)
        }
        message(paste("Loading object state from:", file))
        data <- readRDS(file)
        required_fields <- c(
          "sampler_x", "sampler_y", "sampler_z",
          "init_empty", "n_simulation", "n_burn_in", "seed"
        )
        if (!is.list(data) || !all(required_fields %in% names(data))) {
          stop("File does not contain a valid sampler.iglm state.", call. = FALSE)
        }
        private$.n_simulation <- data$n_simulation
        private$.n_burn_in <- data$n_burn_in
        private$.init_empty <- data$init_empty
        private$.seed <- data$seed
        private$.sampler_x <- sampler.net.attr.generator$new(
          n_proposals = data$sampler_x$n_proposals,
          tnt = data$sampler_x$tnt
        )
        private$.sampler_y <- sampler.net.attr.generator$new(
          n_proposals = data$sampler_y$n_proposals,
          tnt = data$sampler_y$tnt
        )
        private$.sampler_z <- sampler.net.attr.generator$new(
          n_proposals = data$sampler_z$n_proposals,
          tnt = data$sampler_z$tnt
        )
      }
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Sets the parallel cluster object to be used for simulations.
    #' @param cluster A parallel cluster object from the `parallel` package.
    set_cluster = function(cluster) {
      private$.cluster <- cluster
      private$.validate()
    },
    #' @description
    #' Deactivates parallel processing for this sampler instance by setting
    #' the internal cluster object reference to `NULL`.
    #' @return The `sampler.iglm` object itself (`self`), invisibly.
    deactive_cluster = function() {
      private$.cluster <- NULL
      private$.validate()
    },
    #' @description
    #' Sets the number of simulations to generate after burn-in.
    #' @param n_simulation (integer) The number of simulations to set.
    #' @return None.
    set_n_simulation = function(n_simulation) {
      private$.n_simulation <- n_simulation
      private$.validate()
    },
    #' @description
    #' Sets the number of burn-in iterations.
    #' @param n_burn_in (integer) The number of burn-in iterations to set.
    #' @return None.
    set_n_burn_in = function(n_burn_in) {
      private$.n_burn_in <- n_burn_in
      private$.validate()
    },
    #' @description
    #' Sets whether to initialize simulations from an empty state.
    #' @param init_empty (logical) `TRUE` to initialize from empty, `FALSE` otherwise.
    #' @return None.
    set_init_empty = function(init_empty) {
      if (!is.logical(init_empty)) {
        stop("`init_empty` must be a logical value (TRUE or FALSE).", call. = FALSE)
      }
      private$.init_empty <- init_empty
      private$.validate()
    },
    #' @description
    #' Sets the sampler configuration for the x attribute.
    #' @param sampler_x An object of class `sampler_net_attr`.
    #' @return None.
    set_x_sampler = function(sampler_x) {
      if (!inherits(sampler_x, "sampler_net_attr")) {
        stop("`sampler_x` must be created with `sampler.net.attr()`.", call. = FALSE)
      }
      private$.sampler_x <- sampler_x
      private$.validate()
    },
    #' @description
    #' Sets the sampler configuration for the y attribute.
    #' @param sampler_y An object of class `sampler_net_attr`.
    #' @return None.
    set_y_sampler = function(sampler_y) {
      if (!inherits(sampler_y, "sampler_net_attr")) {
        stop("`sampler_y` must be created with `sampler.net.attr()`.", call. = FALSE)
      }
      private$.sampler_y <- sampler_y
      private$.validate()
    },
    #' @description
    #' Sets the sampler configuration for the z attribute.
    #' @param sampler_z An object of class `sampler_net_attr`.
    #' @return None.
    set_z_sampler = function(sampler_z) {
      if (!inherits(sampler_z, "sampler_net_attr")) {
        stop("`sampler_z` must be created with `sampler.net.attr()`.", call. = FALSE)
      }
      private$.sampler_z <- sampler_z
      private$.validate()
    },
    #' @description
    #' Sets the random seed for this sampler.
    #' @param seed (integer) The random seed to set.
    #' @return None.
    set_seed = function(seed) {
      private$.seed <- as.integer(seed)
    },
    #' @description
    #' Prints a formatted summary of the sampler configuration to the console.
    #' @param digits (integer) Number of digits for formatting numeric values. Default: 3.
    #' @param ... Additional arguments (currently ignored).
    #' @return The `sampler.iglm` object itself (`self`), invisibly.
    print = function(digits = 3, ...) {
      cat("Sampler settings\n")
      cat(strrep("-", 60), "\n", sep = "")
      cat("Core parameters\n")
      cat("  n_simulation :", private$.n_simulation, "\n", sep = "")
      cat("  n_burn_in    :", private$.n_burn_in, "\n", sep = "")
      cat("  init_empty   :", if (isTRUE(private$.init_empty)) "TRUE" else "FALSE", "\n", sep = "")
      cat("  seed         :", private$.seed, "\n", sep = "")
      cat("\n")
      cat("Sub-samplers\n")
      cat("  sampler_x:\n")
      private$.sampler_x$print(indent = "    ")
      cat("  sampler_y:\n")
      private$.sampler_y$print(indent = "    ")
      cat("  sampler_z:\n")
      private$.sampler_z$print(indent = "    ")
      invisible(self)
    },
    #' @description
    #' Gathers all data from private fields into a list.
    #' @return A list containing all information of the sampler.
    gather = function() {
      list(
        sampler_x = private$.sampler_x$gather(),
        sampler_y = private$.sampler_y$gather(),
        sampler_z = private$.sampler_z$gather(),
        n_simulation = private$.n_simulation,
        n_burn_in = private$.n_burn_in,
        init_empty = private$.init_empty,
        seed = private$.seed
      )
    },
    #' @description
    #' Save the object's complete state to a directory.
    #' This will save the main sampler's settings to a file
    #' named 'sampler.iglm_state.rds' within the specified
    #' directory, and will also call the `save()` method for each
    #' nested sampler (.x, .y, .z), saving them into the same
    #' directory.
    #' @param file (character) The file to a directory where the
    #'   state files will be saved. The directory will be created
    #'   if it does not exist.
    #' @return The object itself, invisibly.
    save = function(file) {
      if (missing(file) || !is.character(file) || length(file) != 1) {
        stop("A valid 'file' (character string) must be provided.", call. = FALSE)
      }
      data_to_save <- self$gather()
      saveRDS(data_to_save, file = file)
      message(paste("Object state saved to:", file))
      invisible(self)
    }
  ),
  active = list(
    #' @field sampler_x (`sampler_net_attr`) The sampler configuration object for the x attribute.
    sampler_x = function(value) {
      if (missing(value)) {
        private$.sampler_x
      } else {
        self$set_x_sampler(value)
      }
    },
    #' @field sampler_y (`sampler_net_attr`) The sampler configuration object for the y attribute.
    sampler_y = function(value) {
      if (missing(value)) private$.sampler_y else self$set_y_sampler(value)
    },
    #' @field sampler_z (`sampler_net_attr`) The sampler configuration object for the z network (overlap region).
    sampler_z = function(value) {
      if (missing(value)) private$.sampler_z else self$set_z_sampler(value)
    },
    #' @field n_simulation (`integer`) The number of configurations to simulate.
    n_simulation = function(value) {
      if (missing(value)) private$.n_simulation else if (is.numeric(value)) self$set_n_simulation(value) else stop("`n_simulation` must be numeric.", call. = FALSE)
    },
    #' @field n_burn_in (`integer`) The number of initial MCMC iterations to discard.
    n_burn_in = function(value) {
      if (missing(value)) private$.n_burn_in else if (is.numeric(value)) self$set_n_burn_in(value) else stop("`n_burn_in` must be numeric.", call. = FALSE)
    },
    #' @field init_empty (`logical`) Whether to initialize simulations from an empty state.
    init_empty = function(value) {
      if (missing(value)) private$.init_empty else if (is.logical(value)) self$set_init_empty(value) else stop("`init_empty` must be logical", call. = FALSE)
    },
    #' @field seed (`integer`) Read-only. The random seed used for sampling.
    seed = function(value) {
      if (missing(value)) private$.seed else stop("`seed` is read-only. Use `set_seed()` to change it.", call. = FALSE)
    },
    #' @field cluster (`cluster` object or `NULL`) The parallel cluster object being used, or `NULL`.
    cluster = function(value) {
      if (missing(value)) private$.cluster else self$set_cluster(value)
    }
  )
)


#' Constructor for a iglm Sampler
#'
#' @description
#' Creates an object of class `sampler.iglm` (and `R6`) which holds all
#' parameters controlling the MCMC sampling process for `iglm` models.
#' This includes global settings like the number of simulations and burn-in,
#' as well as references to specific samplers for the network (`z`) and
#' attribute (`x`, `y`) components.
#'
#' This function provides a convenient way to specify these settings before
#' passing them to the `iglm` constructor or simulation functions.
#'
#' @param sampler_x An object of class `sampler.net.attr` (created by
#'   `sampler.net.attr()`) specifying how to sample the `x_attribute`.
#'   If `NULL` (default), default `sampler.net.attr()` settings are used.
#' @param sampler_y An object of class `sampler.net.attr` specifying how to
#'   sample the `y_attribute`. If `NULL` (default), default settings are used.
#' @param sampler_z An object of class `sampler.net.attr` specifying how to
#'   sample the `z_network` ties *within* the defined neighborhood/overlap region.
#'   If `NULL` (default), default settings are used.
#' @param n_simulation (integer) The number of independent samples to generate
#'   after the burn-in period. Default: 100. Must be non-negative.
#' @param n_burn_in (integer) The number of MCMC iterations to discard at the
#'   start for burn-in. Default: 10. Must be non-negative.
#' @param init_empty (logical) If `FALSE` (default), the MCMC chain is initialized
#'   from observed data. If `TRUE`, initialize the MCMC chain from an empty state.
#' @param seed (integer or `NA`) A single integer seed set once before sampling
#'   begins to ensure reproducibility. If `NA` (default), a fixed default seed
#'   (`123456789`) is used.
#' @param cluster A parallel cluster object (e.g., from `parallel::makeCluster()`)
#'   for parallel simulations. If `NULL` (default), simulations run sequentially.
#' @param file (character or `NULL`) If provided, loads the sampler state from
#'   the specified .rds file instead of initializing from parameters.
#'
#' @return An object of class `sampler.iglm` (and `R6`).
#' @export
#' @seealso `sampler.net.attr`, `iglm`, `control.iglm`
#' @examples
#' n_units <- 50
#' sampler_new <- sampler.iglm(
#'   n_burn_in = 100, n_simulation = 10,
#'   seed = 42,
#'   sampler_x = sampler.net.attr(n_proposals = n_units * 10),
#'   sampler_y = sampler.net.attr(n_proposals = n_units * 10),
#'   sampler_z = sampler.net.attr(n_proposals = n_units^2, tnt = TRUE),
#'   init_empty = FALSE
#' )
#' sampler_new
#' sampler_new$seed
#' sampler_new$set_n_simulation(100)
#' sampler_new$n_simulation
sampler.iglm <- function(sampler_x = NULL, sampler_y = NULL, sampler_z = NULL,
                         n_simulation = 100, n_burn_in = 10, init_empty = FALSE,
                         seed = NA, cluster = NULL, file = NULL) {
  sampler.iglm.generator$new(
    sampler_x = sampler_x,
    sampler_y = sampler_y,
    sampler_z = sampler_z,
    n_simulation = n_simulation,
    n_burn_in = n_burn_in,
    init_empty = init_empty,
    seed = seed,
    file = file,
    cluster = cluster
  )
}
