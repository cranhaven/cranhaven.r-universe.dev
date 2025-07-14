#' Fit a Spatial Model for Age Parameters using TMB
#'
#' @description
#' Fits a spatial model for age parameters using Template Model Builder (TMB)
#' and C++. The model incorporates spatial correlation through distance matrices
#' and handles both scale and shape parameters simultaneously. Progress is
#' tracked with clear status updates. Can optionally load from cache.
#'
#' @param country_code Optional country code to save/load cached model. Default
#'   NULL runs model without caching.
#' @param data A data frame containing the response variables, covariates, and
#'   spatial coordinates (web_x, web_y)
#' @param scale_outcome Character string specifying the column name for the
#'   scale parameter response variable
#' @param shape_outcome Character string specifying the column name for the
#'   shape parameter response variable
#' @param covariates Character vector of covariate names to include in both
#'   scale and shape models
#' @param cpp_script_name Character string specifying the name of the C++ file
#'   (without extension) containing the TMB model definition
#' @param verbose Logical indicating whether to show progress updates. Default
#'    TRUE
#' @param control_params List of control parameters passed to nlminb optimizer.
#'   Default: list(trace = 2)
#' @param manual_params Optional list of manual parameter values. If NULL
#'   (default), initial parameters are estimated from linear regression. The
#'   list should contain:
#'   \itemize{
#'     \item beta1: Vector of coefficients for scale model
#'     \item beta2: Vector of coefficients for shape model
#'     \item gamma: Scalar value (default 1.0)
#'     \item log_sigma2: Log of sigma squared (default log(1.0))
#'     \item log_phi: Log of phi (estimated from variogram)
#'     \item log_tau2_1: Log of tau squared (default log(1.0))
#'   }
#' @param output_dir Directory to save cached models. Only used if country_code
#'   is provided.
#' @param ignore_cache Whether to ignore existing cache. Default FALSE.
#'
#' @return An object of class 'nlminb' containing:
#'   \itemize{
#'     \item par - Optimized parameter values
#'     \item objective - Final value of objective function
#'     \item convergence - Convergence code
#'     \item message - Convergence message
#'     \item iterations - Number of iterations
#'     \item evaluations - Number of function/gradient evaluations
#'     \item scale_formula - Formula used for scale model
#'     \item shape_formula - Formula used for shape model
#'     \item variogram - Fitted variogram model from automap containing:
#'       \itemize{
#'         \item range - Spatial correlation range parameter
#'         \item psill - Partial sill (structured variance)
#'         \item nugget - Nugget effect (unstructured variance)
#'         \item kappa - Smoothness parameter for Matern models
#'       }
#'   }
#'
#' @details
#' The function performs the following steps with progress tracking:
#' 1. Fits initial linear models for scale and shape parameters
#' 2. Calculates spatial distance matrix from web coordinates
#' 3. Estimates optimal phi parameter using variogram:
#'    - Computes empirical variogram using automap
#'    - Automatically selects best theoretical variogram model
#'    - Range parameter is used to initialize spatial correlation
#'    - Default range of 100 used if estimation fails
#' 4. Compiles and loads the TMB C++ template
#' 5. Optimizes the joint likelihood using nlminb
#'
#' The spatial correlation is modeled using an exponential variogram with
#' parameters estimated from the data. The distance matrix is computed from
#' the web coordinates (web_x, web_y) and used in the spatial covariance
#' structure.
#'
#' The C++ template should implement the joint spatial model for both
#' parameters.
#'
#' @note
#' Requires TMB package and a working C++ compiler. The C++ template must be
#' properly structured for TMB. The automap package is required for variogram
#' fitting.
#'
#' @examples
#'
#' \donttest{
#' set.seed(123)
#' # Set parameters for simulation
#' total_population <- 266
#' urban_proportion <- 0.602
#' total_coords <- 266
#' lon_range <- c(-16.802, -13.849)
#' lat_range <- c(13.149, 13.801)
#' mean_web_x <- -1764351
#' mean_web_y <- 1510868
#'
#' # Simulate processed survey dataset for Gambia
#' df_gambia <- NULL
#' df_gambia$age_param_data <- dplyr::tibble(
#'   country = "Gambia",
#'   country_code_iso3 = "GMB",
#'   country_code_dhs = "GM",
#'   year_of_survey = 2024,
#'   id_coords = rep(1:total_coords, length.out = total_population),
#'   lon = runif(total_population, lon_range[1], lon_range[2]),
#'   lat = runif(total_population, lat_range[1], lat_range[2]),
#'   web_x = rnorm(total_population, mean_web_x, 50000),
#'   web_y = rnorm(total_population, mean_web_y, 50000),
#'   log_scale = rnorm(total_population, 2.82, 0.2),
#'   log_shape = rnorm(total_population, 0.331, 0.1),
#'   urban = rep(c(1, 0), c(
#'     round(total_population * urban_proportion),
#'     total_population - round(total_population * urban_proportion)
#'  )),
#'  b1 = rnorm(total_population, 0.0142, 0.002),
#'  c = rnorm(total_population, -0.00997, 0.001),
#'   b2 = rnorm(total_population, 0.00997, 0.002),
#'   nsampled = sample(180:220, total_population, replace = TRUE)
#' )
#'
#'
#' tf <- file.path(tempdir(), "test_env")
#' dir.create(tf, recursive = TRUE, showWarnings = FALSE)
#'
#' #initialise files and key scripts
#' init(
#'   r_script_name = "full_pipeline.R",
#'   cpp_script_name = "model.cpp",
#'   path = tf,
#'   open_r_script = FALSE
#' )
#'
#' mod <- fit_spatial_model(
#'   df_gambia$age_param_data,
#'   scale_outcome = "log_scale",
#'   shape_outcome = "log_shape",
#'   covariates = "urban",
#'   cpp_script_name = file.path(tf, "02_scripts/model"),
#'   country_code = "GMB",
#'   output_dir = file.path(tf, "03_outputs/3a_model_outputs")
#' )
#'
#' }
#' @export
fit_spatial_model <- function(data,
                              country_code = NULL,
                              scale_outcome,
                              shape_outcome, covariates,
                              cpp_script_name,
                              verbose = TRUE,
                              control_params = list(trace = 2),
                              manual_params = NULL,
                              output_dir = NULL,
                              ignore_cache = FALSE) {
  # Check for caching
  if (!is.null(country_code)) {
    country_code <- tolower(country_code)
    model_param_path <- file.path(
      output_dir,
      glue::glue("{country_code}_age_param_spatial.rds")
    )

    if (!ignore_cache && file.exists(model_param_path)) {
      cli::cli_process_start(
        msg = "Importing cached model results...",
        msg_done = "Successfully imported cached model results."
      )
      spat_model_param <- readRDS(model_param_path)
      cli::cli_process_done()
      return(spat_model_param)
    }
  }

  # Section 1: Initial Linear Models -------------------------------------------

  cli::cli_process_start(
    msg = "Fitting initial linear models...",
    msg_done = "Fitted initial linear models."
  )

  # Create model formulas
  scale_formula <- reformulate(covariates,
    response = scale_outcome,
    intercept = FALSE
  )

  shape_formula <- reformulate(covariates,
    response = shape_outcome,
    intercept = FALSE
  )

  # Fit
  lm_scale <- tryCatch(
    {
      lm(scale_formula, data = data, x = TRUE)
    },
    error = function(e) {
      stop("Error fitting scale model: ", e$message)
    }
  )

  lm_shape <- tryCatch(
    {
      lm(shape_formula, data = data, x = TRUE)
    },
    error = function(e) {
      stop("Error fitting shape model: ", e$message)
    }
  )

  # Extract design matrices
  design_scale <- lm_scale$x
  design_shape <- lm_shape$x

  cli::cli_process_done()

  # Section 2: Distance Matrix and Variogram ----------------------------------

  # Initialize range estimate
  range_est <- NA
  if (is.null(manual_params) || is.null(manual_params$log_phi)) {
    cli::cli_process_start(
      msg = "Calculating empirical variogram...",
      msg_done = "Empirical variogram fitted."
    )

    if (!requireNamespace("automap", quietly = TRUE)) {
      stop("Package 'automap' is required for variogram fitting.")
    }

    # Create spatial coordinates
    coords <- data |>
      dplyr::select(web_x, web_y) |>
      as.matrix()

    # Calculate distance matrix
    dist_matrix <- coords |>
      dist() |>
      as.matrix()

    # Set up spatial data
    vgm_data <- data
    sp::coordinates(vgm_data) <- ~ web_x + web_y

    # Fit variogram using automap
    fit_vario <- automap::autofitVariogram(
      stats::formula(paste0(scale_outcome, "~1")),
      vgm_data,
      cutoff = max(dist_matrix) / 2
    )$var_model

    # Get range estimate from the auto-fitted model
    range_est <- fit_vario$range[2]

    # Set default if estimate is invalid
    if (range_est <= 0) range_est <- 100

    cli::cli_process_done()
  }

  optimal_phi <- range_est

  # Section 3: Parameter Setup ------------------------------------------------

  cli::cli_process_start(
    msg = "Initializing data and parameters for optimisation...",
    msg_done = "Initialization complete."
  )

  # Use manual parameters if provided,
  # otherwise use linear regression estimates and variogram phi
  parameters <- if (!is.null(manual_params)) {
    # Validate manual parameters structure
    required_params <- c(
      "beta1", "beta2", "gamma", "log_sigma2",
      "log_phi", "log_tau2_1"
    )
    if (!all(required_params %in% names(manual_params))) {
      stop(
        "manual_params must contain all required parameters: ",
        paste(required_params, collapse = ", ")
      )
    }
    manual_params
  } else {
    list(
      beta1 = as.vector(coef(lm_scale)),
      beta2 = as.vector(coef(lm_shape)),
      gamma = 1.0,
      log_sigma2 = log(1.0),
      log_phi = log(optimal_phi),
      log_tau2_1 = log(1.0)
    )
  }

  tmb_data <- list(
    design_scale = design_scale,
    design_shape = design_shape,
    y = c(data[[scale_outcome]], data[[shape_outcome]]),
    dist_matrix = dist_matrix,
    b1 = data$b1,
    c = data$c,
    b2 = data$b2
  )

  cli::cli_process_done()

  # Section 4: TMB Compilation ------------------------------------------------

  cli::cli_process_start(
    msg = "Compiling TMB model",
    msg_done = "Compiled TMB model"
  )
  # In fit_spatial_model function
  if (!verbose) {
    suppressWarnings(
      suppressMessages({
        cpp_path <- normalizePath(paste0(cpp_script_name, ".cpp"),
          winslash = "/",
          mustWork = TRUE
        )
        system2("R",
          args = c(
            "CMD", "SHLIB",
            shQuote(cpp_path), "-O2"
          ),
          stdout = FALSE, stderr = FALSE
        )
        dyn.load(TMB::dynlib(cpp_script_name))
      })
    )
  } else {
    cpp_path <- normalizePath(paste0(cpp_script_name, ".cpp"),
      winslash = "/",
      mustWork = TRUE
    )
    TMB::compile(cpp_path)
    dyn.load(TMB::dynlib(cpp_script_name))
  }

  cli::cli_process_done()

  # Section 5: Optimization ----------------------------------------------------

  cli::cli_process_start(
    msg = "Optimizing model",
    msg_done = "Optimized model"
  )

  obj <- TMB::MakeADFun(
    data = tmb_data,
    parameters = parameters,
    DLL = stringr::str_extract(
      cpp_script_name, "[^/]+$"
    ),
    silent = TRUE
  )

  if (!verbose) {
    control_params$trace <- 0
  }

  opt <- nlminb(obj$par, obj$fn, obj$gr,
    control = control_params
  )

  names(opt$par) <- c(
    names(coef(lm_scale)),
    names(coef(lm_shape)),
    "gamma", "log_sigma2", "log_phi", "log_tau1"
  )

  cli::cli_process_done()

  # include formula to output
  # Convert formulas to character strings before storing
  opt$scale_formula <- deparse(scale_formula)
  opt$shape_formula <- deparse(shape_formula)
  opt$variogram <- fit_vario

  # Save if country code provided
  if (!is.null(country_code)) {
    saveRDS(opt, file = model_param_path)
    cli::cli_alert_success("Model fitted and saved at {model_param_path}")
  }

  return(opt)
}

#' Compute Covariance Matrix for Spatial Model
#'
#' This function computes a block covariance matrix for a bivariate spatial
#' model with age-structured parameters.
#'
#' @param gamma Correlation parameter between the two spatial processes
#' @param sigma2 Variance parameter for the spatial processes
#' @param phi Range parameter for the spatial correlation
#' @param u_dist Distance matrix between locations
#' @param n_x Number of spatial locations
#' @param age_param_data List containing age-structured parameters:
#'   \itemize{
#'     \item b1: Vector of age parameters for first process
#'     \item b2: Vector of age parameters for second process
#'     \item c: Vector of cross-process age parameters
#'   }
#' @param tau2_1 Variance parameter for first process (default = 1)
#' @param tau2_2 Variance parameter for second process (default = 1)
#'
#' @return A sparse symmetric matrix of dimension 2n_x × 2n_x
#'
#' @export
compute_cov <- function(gamma, sigma2, phi, u_dist,
                        n_x, tau2_1 = 1, tau2_2 = 1,
                        age_param_data) {
  sigma_s <- sigma2 * exp(-u_dist / phi)
  m <- matrix(NA, 2 * n_x, 2 * n_x)

  m[1:n_x, 1:n_x] <- sigma_s + diag(age_param_data$b1) * tau2_1

  m[(n_x + 1):(2 * n_x), 1:n_x] <-
    m[1:n_x, (n_x + 1):(2 * n_x)] <-
    gamma * sigma_s + diag(age_param_data$c) * sqrt(tau2_1 * tau2_2)

  m[(n_x + 1):(2 * n_x), (n_x + 1):(2 * n_x)] <-
    (gamma^2) * sigma_s + diag(age_param_data$b2) * tau2_2

  m
}

#' Log-Likelihood Function for Spatial Model
#'
#' Computes the log-likelihood for a spatial statistical model with a covariance
#' structure determined by parameters including spatial decay and variance.
#'
#' @param par A numeric vector of parameters to estimate. The vector contains:
#'   \itemize{
#'     \item \code{par[1:p1]}: Coefficients for fixed effects in dataset 1
#'     (\eqn{\beta_1}).
#'     \item \code{par[(p1 + 1):(p1 + p2)]}: Coefficients for fixed effects in
#'     dataset 2 (\eqn{\beta_2}).
#'     \item \code{par[p1 + p2 + 1]}: Spatial decay parameter (\eqn{\gamma}).
#'     \item \code{par[p1 + p2 + 2]}: Log of the variance parameter
#'     (\eqn{\sigma^2}).
#'     \item \code{par[p1 + p2 + 3]}: Log of the range parameter (\eqn{\phi}).
#'   }
#' @param p1 An integer. The number of fixed-effect parameters in dataset 1.
#' @param p2 An integer. The number of fixed-effect parameters in dataset 2.
#' @param d1 A numeric matrix. Design matrix for dataset 1 used to model the
#'    mean structure.
#' @param d2 A numeric matrix. Design matrix for dataset 2 used to model the
#'    mean structure.
#' @param y A numeric vector. Observed response variable, including both
#'    datasets.
#' @param u_dist A numeric matrix. Distance matrix for spatial locations.
#' @param n_x An integer. The number of unique spatial locations.
#' @param age_param_data A numeric matrix or vector. Additional parameters
#'    specific to age-based modeling.
#' @param tau2_1 Variance parameter for first process (default = 1)
#' @param tau2_2 Variance parameter for second process (default = 1)
#'
#' @return A numeric scalar. The computed log-likelihood value.
#'
#' @details
#' The log-likelihood is computed as:
#' \deqn{
#' -0.5 \left[ \log(\det(M)) + (y - \mu)^T M^{-1} (y - \mu) \right]
#' }
#' where:
#' \itemize{
#'   \item \eqn{M} is the covariance matrix, computed using \code{compute_cov}.
#'   \item \eqn{\mu} is the mean structure, determined by the design matrices
#'    \code{d1}, \code{d2} and coefficients \eqn{\beta_1, \beta_2}.
#' }
#'
#' The covariance matrix \eqn{M} is computed using spatial parameters
#'  (\eqn{\gamma, \sigma^2, \phi}) and the distance matrix \code{u_dist}.
#'
#' @note
#' This function requires a helper function, \code{compute_cov}, to compute
#'  the covariance matrix based on spatial parameters.
#'
#' @export
log_lik <- function(par, p1, p2, d1, d2, y, u_dist, n_x, tau2_1 = 1, tau2_2 = 1,
                    age_param_data) {
  beta1 <- par[1:p1]
  beta2 <- par[(p1 + 1):(p1 + p2)]
  gamma <- par[p1 + p2 + 1]
  sigma2 <- exp(par[p1 + p2 + 2])
  phi <- exp(par[p1 + p2 + 3])

  mu1 <- as.numeric(d1 %*% beta1)
  mu2 <- as.numeric(d2 %*% beta2)
  mu <- c(mu1, mu2)

  m <- compute_cov(
    gamma, sigma2, phi, u_dist, n_x,
    tau2_1, tau2_2, age_param_data
  )
  m_inv <- chol2inv(chol(m))

  -0.5 * as.numeric(determinant(m)$modulus +
    t(y - mu) %*% m_inv %*% (y - mu))
}

#' Extract Beta Parameters from Model Output
#'
#' This function extracts beta coefficients from a model parameter object,
#' separating them into beta1 and beta2 components.
#'
#' @param params_result A model parameter object containing parameter estimates
#' @param params A character vector specifying parameter names, defaults to
#'   c("gamma", "log_sigma2", "log_phi", "log_tau1")
#' @return A list with two components:
#'   \itemize{
#'     \item beta1: First set of beta coefficients
#'     \item beta2: Second set of beta coefficients
#'   }
#'
#' @details
#' The function assumes the parameter vector contains beta coefficients
#' followed by other model parameters. It splits the betas into two equal
#' groups after removing the last 4 parameters.
#' @export
extract_betas <- function(params_result,
                          params = c(
                            "gamma", "log_sigma2",
                            "log_phi", "log_tau1"
                          )) {
  params_length <- length(params)
  par_start <- as.numeric(params_result$par)
  par_length <- length(par_start)
  beta_length <- par_length - 4
  beta1_locend <- (beta_length / 2)
  beta2_locstart <- beta1_locend + 1
  beta2_locend <- beta_length

  list(
    beta1 = par_start[1:beta1_locend],
    beta2 = par_start[beta2_locstart:beta2_locend]
  )
}

#' Generate or Load Cached Predictors Data
#'
#' This function creates predictors data based on spatial inputs or loads cached
#' predictors data if the file already exists. It saves the generated data to a
#' specified directory for reuse and provides progress updates.
#'
#' @param country_code A string representing the country code (e.g., "KEN").
#' @param country_shape An `sf` object representing the country's administrative
#'   boundaries.
#' @param pop_raster A `terra` raster object representing the population raster.
#' @param ur_raster A `terra` raster object representing the urban extent
#'  raster.
#' @param adm2_shape An `sf` object representing the administrative level 2
#'  boundaries.
#' @param cell_size An integer specifying the cell size for the prediction grid
#'   in meters (default is 5000).
#' @param ignore_cache A boolean input which is set to determine whether
#'  to ignore the existing cache and write over it. Default is set to FALSE.
#' @param output_dir A string specifying the directory where the predictors data
#'   file should be saved (default is "03_outputs/3a_model_outputs").
#'
#' @return A data object (`predictor_data`) containing the generated predictors.
#'
#' @examples
#' \donttest{
#' tf <- file.path(tempdir(), "test_env")
#'
#' # Initialize with normalized path
#' dir.create(tf, recursive = TRUE, showWarnings = FALSE)
#'
#' init(
#'   r_script_name = "full_pipeline.R",
#'   cpp_script_name = "model.cpp",
#'   path = tf,
#'   open_r_script = FALSE
#' )
#'
#' # Download shapefiles
#' download_shapefile(
#'   country_codes =  "COM",
#'   dest_file = file.path(
#'     tf, "01_data", "1c_shapefiles",
#'     "district_shape.gpkg"
#'   )
#' )
#'
#' # Download population rasters from worldpop
#' download_pop_rasters(
#'   country_codes = "COM",
#'   dest_dir = file.path(tf, "01_data", "1b_rasters", "pop_raster")
#' )
#'
#' # Extract urban extent raster
#' extract_afurextent(
#'   dest_dir = file.path(tf, "01_data", "1b_rasters", "urban_extent")
#' )
#'
#'
#' urban_raster <-  terra::rast(
#'   file.path(tf, "01_data", "1b_rasters",
#'             "urban_extent", "afurextent.asc"))
#'
#' pop_raster <-  terra::rast(
#'   file.path(tf, "01_data", "1b_rasters", "pop_raster",
#'             "com_ppp_2020_constrained.tif")
#'
#' )
#'
#' adm2_sf <- sf::read_sf(
#'  file.path(tf, "01_data", "1c_shapefiles",
#'             "district_shape.gpkg"))
#'
#' country_sf <- sf::st_union(adm2_sf)
#'
#' predictors <- create_prediction_data(
#'   country_code =  "COM",
#'   country_shape = country_sf,
#'   pop_raster = pop_raster,
#'   ur_raster = urban_raster,
#'   adm2_shape = adm2_sf,
#'   cell_size = 5000,
#'   output_dir = file.path(
#'     tf, "03_outputs/3a_model_outputs"
#'   )
#' )
#' }
#'
#' @export
create_prediction_data <- function(country_code, country_shape, pop_raster,
                                   ur_raster, adm2_shape, cell_size = 5000,
                                   ignore_cache = FALSE,
                                   output_dir = here::here(
                                     "03_outputs", "3a_model_outputs"
                                   )) {
  # Check cache and import if it exists ------------------------------------------

  # Create lowercase country code
  country_code <- tolower(country_code)

  # Construct the predictors data path
  predictor_data_path <- file.path(
    output_dir,
    glue::glue("{country_code}_predictor_data.rds")
  )

  # Check if predictors data file exists and return if cached
  if (!ignore_cache && file.exists(predictor_data_path)) {
    # Notify user about cached predictors
    cli::cli_process_start(
      msg = "Importing cached predictors data...",
      msg_done = "Successfully imported cached predictors data."
    )
    # Load cached predictors data
    predictor_data <- readRDS(predictor_data_path)
    cli::cli_process_done()
    return(predictor_data)
  }

  # Process shapefiles and grids -----------------------------------------------

  cli::cli_process_start(
    msg = "Processing shapefiles and grids...",
    msg_done = "Processed shapefiles and grids."
  )

  grid_polygons <- sf::st_make_grid(
    sf::st_transform(
      country_shape,
      crs = 3857
    ),
    cellsize = cell_size,
    square = TRUE
  ) |>
    sf::st_intersection(
      sf::st_transform(country_shape, crs = 3857)
    ) |>
    #  correct small overlaps by applying a small buffer
    sf::st_buffer(dist = 1e-9)

  # Generate country_grid points from grid_polygons
  country_grid <- sf::st_centroid(grid_polygons) |>
    sf::st_coordinates() |>
    as.data.frame() |>
    setNames(c("web_x", "web_y"))

  cli::cli_process_done()

  # Process pop raster data ----------------------------------------------------

  cli::cli_process_start(
    msg = "Processing population raster...",
    msg_done = "Processed population raster."
  )

  pop_raster_dens <- pop_raster |>
    terra::project("EPSG:3857")

  cli::cli_process_done()

  # Process urban/rural classification --------------------------------------

  cli::cli_process_start(
    msg = "Processing urban-rural raster...",
    msg_done = "Processed urban-rural raster."
  )

  ur_raster_web <- ur_raster |>
    terra::project("EPSG:3857")

  cli::cli_process_done()

  # Extract and process grid data ----------------------------------------------

  cli::cli_process_start(
    msg = "Extracting data from rasters onto grid...",
    msg_done = "Extracted data from rasters onto grid."
  )

  # Use exact_extract to calculate total population per grid cell
  pop_values <- exactextractr::exact_extract(
    pop_raster_dens, grid_polygons,
    fun = "sum",
    max_cells_in_memory = 3e+08, progress = FALSE
  )

  urb_values <- terra::extract(
    ur_raster_web, country_grid |>
      terra::vect(
        geom = c("web_x", "web_y"),
        crs = "EPSG:3857"
      )
  )[[2]] |>
    tidyr::replace_na(0)

  cli::cli_process_done()

  # Create predictors dataframe ------------------------------------------------

  cli::cli_process_start(
    msg = "Creating grided data with predictors...",
    msg_done = "Created grided data with predictors."
  )

  predictors <- data.frame(
    web_x = country_grid[, 1],
    web_y = country_grid[, 2],
    pop = pop_values,
    urban = urb_values
  ) |>
    dplyr::mutate(
      lat = web_y,
      lon = web_x
    ) |>
    sf::st_as_sf(
      coords = c(x = "lon", y = "lat"),
      crs = 3857
    ) |>
    sf::st_transform(crs = 3857) |>
    sf::st_join(sf::st_transform(adm2_shape, crs = 3857)) |>
    dplyr::select(
      country, region, district,
      web_x, web_y, urban, pop
    )

  # Assign unmatched points to the nearest boundary
  predictor_data <- predictors |>
    dplyr::filter(is.na(country)) |>
    dplyr::select(web_x, web_y, pop, urban) |>
    sf::st_join(
      sf::st_transform(dplyr::select(adm2_shape, -country_code),
        crs = 3857
      ),
      join = sf::st_nearest_feature
    ) |>
    dplyr::bind_rows(
      predictors |>
        dplyr::filter(!is.na(country))
    ) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      country_code = toupper(country_code)
    )

  cli::cli_process_done()

  # Save predictors data
  saveRDS(predictor_data, file = predictor_data_path)

  cli::cli_alert_success(
    "Predictors data created and saved at {predictor_data_path}"
  )

  return(predictor_data)
}

#' Predict Gamma Distribution Parameters for Spatial Grid
#'
#' This function predicts the scale and shape parameters of a Gamma distribution
#' across a spatial grid using a bivariate spatial model. It can either generate
#' new predictions or load cached results if available.
#'
#' @param country_code A string representing the country code (e.g., "KEN").
#' @param age_param_data A data frame containing:
#'   \itemize{
#'     \item web_x, web_y: Spatial coordinates
#'     \item urban: Urban/rural indicator
#'     \item log_scale: Log of scale parameter at observed locations
#'     \item log_shape: Log of shape parameter at observed locations
#'   }
#' @param model_params A list containing model parameters:
#'   \itemize{
#'     \item par: Named vector with gamma, log_sigma2, log_phi, log_tau1
#'     \item Additional parameters for extracting beta coefficients
#'   }
#' @param predictor_data A data object containing the predictors data.
#' @param shapefile An sf object defining the boundary for predictions
#' @param cell_size Numeric. Grid cell size in meters (default: 5000)
#' @param n_sim Integer. Number of simulations for prediction (default: 5000)
#' @param ignore_cache A boolean input which is set to determine whether
#'  to ignore the existing cache and write over it. Default is set to FALSE.
#' @param save_file A boolean to determine whether to save prediction or not.
#'   Default is FALSE as this will require lots of space.
#' @param output_dir A string specifying the directory where the predictions
#'   file should be saved (default is "03_outputs/3a_model_outputs").
#' @return A list containing:
#'   \itemize{
#'     \item scale_pred: Matrix of simulated scale parameters
#'     \item shape_pred: Matrix of simulated shape parameters
#'   }
#' @importFrom stats as.formula
#' @export
generate_gamma_predictions <- function(country_code,
                                       age_param_data,
                                       model_params,
                                       predictor_data,
                                       shapefile,
                                       cell_size = 5000,
                                       n_sim = 5000,
                                       ignore_cache = FALSE,
                                       save_file = FALSE,
                                       output_dir = here::here(
                                         "03_outputs", "3a_model_outputs"
                                       )) {
  # Create lowercase country code and prediction path
  country_code <- tolower(country_code)
  gamma_prediction_path <- file.path(
    output_dir,
    glue::glue("{country_code}_gamma_prediction.rds")
  )

  # Check if prediction file exists and should be used
  if (!ignore_cache && file.exists(gamma_prediction_path)) {
    cli::cli_process_start(
      msg = "Importing cached prediction results...",
      msg_done = "Successfully imported cached prediction results."
    )
    return(readRDS(gamma_prediction_path))
  }

  # Make grid ----------------------------------------------------------------
  cli::cli_process_start(
    msg = "Making a prediction grid...",
    msg_done = "Prediction grid successfully made."
  )

  country_grid <- predictor_data |> dplyr::select(web_x, web_y)
  cli::cli_process_done()

  # Set parameters from models ------------------------------------------------
  predictor_data <- predictor_data |>
    dplyr::mutate(log_scale = 1, log_shape = 1)

  cli::cli_process_start(
    msg = "Setting model parameters...",
    msg_done = "Model parameters set successfully."
  )

  gamma <- model_params$par["gamma"]
  sigma2 <- exp(model_params$par["log_sigma2"])
  phi <- exp(model_params$par["log_phi"])
  tau <- exp(model_params$par["log_tau1"])
  beta1 <- extract_betas(model_params)$beta1
  beta2 <- extract_betas(model_params)$beta2
  y <- c(age_param_data$log_scale, age_param_data$log_shape)
  d1 <- d2 <- model.matrix(as.formula(model_params$scale_formula),
    data = age_param_data
  )
  mu1 <- as.numeric(d1 %*% beta1)
  mu2 <- as.numeric(d2 %*% beta2)
  mu <- c(mu1, mu2)

  # Predict the gamma parameters
  d_pred <- model.matrix(as.formula(model_params$scale_formula),
    data = predictor_data
  )

  mu1_pred <- as.numeric(d_pred %*% beta1)
  mu2_pred <- as.numeric(d_pred %*% beta2)

  u_dist <- as.matrix(dist(age_param_data[, c("web_x", "web_y")]))
  n_x <- nrow(u_dist)

  cli::cli_process_done()

  # Predict the random effects ----------------------------------------------
  cli::cli_process_start(
    msg = "Calculating pairwise distances for prediction grid...",
    msg_done = "Computed pairwise distances for prediction grid."
  )

  u_pred <- pdist::pdist(
    country_grid,
    age_param_data[, c("web_x", "web_y")]
  ) |>
    as.matrix()

  n_pred <- nrow(country_grid)
  cli::cli_process_done()

  cli::cli_process_start(
    msg = "Computing and inverting covariance matrix...",
    msg_done = "Computed and inverted covariance matrix."
  )

  c_s_star <- cbind(
    sigma2 * exp(-u_pred / phi),
    gamma * sigma2 * exp(-u_pred / phi)
  )

  matrix_inv <- compute_cov(
    gamma = gamma,
    phi = phi,
    sigma2 = sigma2,
    u_dist = u_dist,
    n_x = n_x,
    tau2_1 = tau,
    age_param_data = age_param_data
  ) |>
    chol() |>
    chol2inv()

  cli::cli_process_done()

  cli::cli_process_start(
    msg = "Computing mean and standard deviation of predictions...",
    msg_done = "Computed mean and standard deviation of predictions."
  )

  a <- c_s_star %*% matrix_inv
  mean_s_pred <- a %*% (y - mu)
  sd_s_pred <- sqrt(sigma2 - apply(a * c_s_star, 1, sum))

  cli::cli_process_done()

  # Simulate random effects ----------------------------------------------------
  cli::cli_process_start(
    msg = "Simulating random effects...",
    msg_done = "Simulated random effects."
  )

  s_samples <- sapply(
    1:n_sim,
    function(i) rnorm(n_pred, mean_s_pred, sd_s_pred)
  )

  cli::cli_process_done()

  # Predict the gamma parameters -----------------------------------------------
  cli::cli_process_start(
    msg = "Predicting gamma, scale, and shape parameters...",
    msg_done = "Predicted gamma, scale, and shape parameters."
  )

  scale_pred <- exp(sapply(
    1:n_sim,
    function(i) mu1_pred + s_samples[, i]
  ))
  shape_pred <- exp(sapply(
    1:n_sim,
    function(i) mu2_pred + gamma * s_samples[, i]
  ))

  cli::cli_process_done()

  # Create results list
  gamma_prediction <- list(
    scale_pred = scale_pred,
    shape_pred = shape_pred
  )

  # Save predictions if requested
  if (save_file) {
    saveRDS(gamma_prediction, file = gamma_prediction_path)
    cli::cli_alert_success(
      "Gamma predictions generated and saved at {gamma_prediction_path}"
    )
  }

  return(gamma_prediction)
}


#' Process Gamma Prediction Results
#'
#' This function processes gamma prediction results to calculate the mean age
#' predictions, scale, and shape parameters efficiently.
#'
#' @param gamma_prediction A list containing `scale_pred` and `shape_pred`
#'   matrices from the gamma prediction model.
#'
#' @return A list containing the following elements:
#'   - `mean_age_pred`: A vector of mean age predictions.
#'   - `scale_hat`: A vector of mean scale parameters.
#'   - `shape_hat`: A vector of mean shape parameters.
#'
#' @export
process_gamma_predictions <- function(gamma_prediction) {
  # Extract scale and shape predictions
  scale_pred <- gamma_prediction$scale_pred
  shape_pred <- gamma_prediction$shape_pred

  # Compute mean age predictions
  mean_age_pred <- rowMeans(scale_pred * shape_pred)

  # Compute mean scale and shape parameters
  scale_hat <- rowMeans(scale_pred)
  shape_hat <- rowMeans(shape_pred)

  # Return results as a list
  list(
    mean_age_pred = mean_age_pred,
    scale_hat = scale_hat,
    shape_hat = shape_hat
  )
}

#' Run Country-Specific Spatial Modeling Workflow with Logging
#'
#' @description
#' This function runs the entire spatial modeling workflow for a given country
#' code and logs the results. It processes Survey data, fits a spatial model,
#' generates predictions, creates population tables, and produces raster
#' outputs. The function is modular and can be reused for different countries
#' with minimal adjustments.
#'
#' @param country_code Character. The ISO3 country code (e.g., "TZA").
#' @param survey_data_path Character. Path to Survey data.
#'   Default: "01_data/1a_survey_data/processed".
#' @param survey_data_suffix Character. Suffix for Survey data files.
#'   Default: "dhs_pr_records_combined.rds".
#' @param shape_path Character. Path to shapefile data.
#'   Default: "01_data/1c_shapefiles".
#' @param shape_suffix Character. Suffix for shapefile data.
#'   Default: "district_shape.gpkg".
#' @param pop_raster_path Character. Path to population raster data.
#'   Default: "01_data/1b_rasters/pop_raster".
#' @param pop_raster_suffix Character. Suffix for population raster files.
#'   Default: "_ppp_2020_constrained.tif".
#' @param ur_raster_path Character. Path to urban-rural extent data.
#'   Default: "01_data/1b_rasters/urban_extent".
#' @param ur_raster_suffix Character. Suffix for urban-rural raster.
#'   Default: "afurextent.asc".
#' @param n_cores Integer number of cores for parallel processing for age
#'   population table, default detectCores()-2
#' @param pred_save_file Logical. Whether to save prediction files.
#'   Default: FALSE
#' @param raster_width Integer. Width of raster plots in pixels.
#'   Default: 2500
#' @param raster_height Integer. Height of raster plots in pixels.
#'   Default: 2000
#' @param raster_resolution Integer. Resolution of PNG outputs.
#'   Default: 300
#' @param save_raster Logical. Whether to save raster outputs to disk.
#'   Default: TRUE
#' @param generate_pop_raster Logical. Whether to generate population raster.
#'   Default: FALSE
#' @param pyramid_line_color Character. Hex color code for the age pyramid's
#'   outline. Default: "#67000d"
#' @param pyramid_fill_high Character. Hex color code for the age pyramid's
#'   higher values fill. Default: "#fee0d2"
#' @param pyramid_fill_low Character. Hex color code for the age pyramid's
#'   lower values fill. Default: "#a50f15"
#' @param pyramid_caption Character. Caption text for the age pyramid plot.
#'   Default:
#' "Note: Total population includes ages 99+, pyramid shows ages 0-99"
#' @param output_paths List of output paths:
#'   \itemize{
#'     \item model: Path for model outputs.
#'          Default: "03_outputs/3a_model_outputs"
#'     \item plot: Path for plots.
#'          Default: "03_outputs/3b_visualizations"
#'     \item raster: Path for rasters.
#'          Default: "03_outputs/3c_raster_outputs"
#'     \item table: Path for tables.
#'          Default: "03_outputs/3c_table_outputs"
#'     \item compiled: Path for compiled results.
#'          Default: "03_outputs/3d_compiled_results"
#'     \item excel: Path for Excel outputs.
#'          Default:
#'           "03_outputs/3d_compiled_results/age_pop_denom_compiled.xlsx"
#'     \item log: Path for logs.
#'          Default: "03_outputs/3a_model_outputs/modelling_log.rds"
#'   }
#' @param model_params List of model parameters:
#'   \itemize{
#'     \item cell_size: Cell size in meters. Default: 5000
#'     \item n_sim: Number of simulations. Default: 5000
#'     \item ignore_cache: Whether to ignore cache. Default: FALSE
#'     \item age_range: Age range vector. Default: c(0, 99)
#'     \item age_interval: Age interval. Default: 1
#'     \item return_prop: Return proportions. Default: TRUE
#'     \item scale_outcome: Scale outcome variable. Default: "log_scale"
#'     \item shape_outcome: Shape outcome variable. Default: "log_shape"
#'     \item covariates: Model covariates. Default: "urban"
#'     \item cpp_script: C++ script path. Default: "02_scripts/model"
#'     \item control_params: Control parameters. Default: list(trace = 2)
#'     \item manual_params: Manual parameters. Default: NULL
#'     \item verbose: Verbose output. Default: TRUE
#'     \item age_range_raster: Age range for raster output. Default: c(0, 10)
#'     \item age_interval_raster: Age interval for raster output. Default: 1
#'   }
#' @param return_results Logical. Whether to return results. Default: FALSE.
#' @param ... Additional arguments passed to subfunctions.
#'
#' @return If return_results is TRUE, a list containing:
#'   \itemize{
#'     \item spat_model_param: Fitted spatial model parameters
#'     \item predictor_data: Predictor dataset
#'     \item gamma_prediction: Generated gamma predictions
#'     \item pred_list: Processed gamma prediction results
#'     \item final_age_pop_table: Age-population table data
#'     \item final_pop: Compiled population data
#'     \item all_mod_params: Compiled model parameters
#'   }
#'   If return_results is FALSE, the function saves all outputs to disk and
#'   returns NULL invisibly.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{fit_spatial_model}}: Fits the spatial model for age
#'     parameters
#'   \item \code{\link{create_prediction_data}}: Creates predictor dataset from
#'     spatial inputs
#'   \item \code{\link{generate_gamma_predictions}}: Generates predictions using
#'     fitted model
#'   \item \code{\link{process_gamma_predictions}}: Processes gamma prediction
#'     results
#'   \item \code{\link{generate_gamma_raster_plot}}: Creates prediction raster
#'     plots
#'   \item \code{\link{generate_age_pop_table}}: Generates age-population tables
#'   \item \code{\link{generate_age_pyramid_plot}}: Creates age pyramid plots
#'   \item \code{\link{extract_age_param}}: Extracts and compiles model
#'     parameters
#'   \item \code{\link{process_final_population_data}}: Processes final
#'     population data
#'   \item \code{\link{generate_variogram_plot}}: Creates variogram plots
#'     showing spatial dependence structure
#' }
#'
#'@examples
#'
#' \donttest{
#' # set country code
#' country_codeiso <- "GMB"
#'
#' set.seed(123)
#' # Set parameters for simulation
#' total_population <- 266
#' urban_proportion <- 0.602
#' total_coords <- 266
#' lon_range <- c(-16.802, -13.849)
#' lat_range <- c(13.149, 13.801)
#' mean_web_x <- -1764351
#' mean_web_y <- 1510868
#'
#' # Simulate processed survey dataset for Gambia
#' df_gambia <- NULL
#' df_gambia$age_param_data <- dplyr::tibble(
#'   country = "Gambia",
#'   country_code_iso3 = "GMB",
#'   country_code_dhs = "GM",
#'   year_of_survey = 2024,
#'   id_coords = rep(1:total_coords, length.out = total_population),
#'   lon = runif(total_population, lon_range[1], lon_range[2]),
#'   lat = runif(total_population, lat_range[1], lat_range[2]),
#'   web_x = rnorm(total_population, mean_web_x, 50000),
#'   web_y = rnorm(total_population, mean_web_y, 50000),
#'   log_scale = rnorm(total_population, 2.82, 0.2),
#'   log_shape = rnorm(total_population, 0.331, 0.1),
#'   urban = rep(c(1, 0), c(
#'     round(total_population * urban_proportion),
#'     total_population - round(total_population * urban_proportion)
#'   )),
#'   b1 = rnorm(total_population, 0.0142, 0.002),
#'   c = rnorm(total_population, -0.00997, 0.001),
#'   b2 = rnorm(total_population, 0.00997, 0.002),
#'  nsampled = sample(180:220, total_population, replace = TRUE)
#' )
#'
#'
#' # Create temp directory with normalized path
#' tf <- file.path(tempdir(), "test_env")
# Initialize with normalized path
#' dir.create(tf, recursive = TRUE, showWarnings = FALSE)
#' tf <- normalizePath(tf, winslash = "/", mustWork = FALSE)
#'
#' AgePopDenom::init(
#'   r_script_name = "full_pipeline.R",
#'   cpp_script_name = "model.cpp",
#'   path = tf,
#'   open_r_script = FALSE
#' )
#'
#' # save as processed dhs data
#' saveRDS(
#'   df_gambia,
#'   file = file.path(
#'     tf, "01_data", "1a_survey_data", "processed",
#'     "dhs_pr_records_combined.rds"
#'   ) |>
#'     normalizePath(winslash = "/", mustWork = FALSE)
#' )
#'
#' # Download shapefiles
#' download_shapefile(
#'   country_codes = country_codeiso,
#'   dest_file = file.path(
#'     tf, "01_data", "1c_shapefiles",
#'     "district_shape.gpkg"
#'   ) |>
#'     normalizePath(winslash = "/", mustWork = FALSE)
#' )
#'
#' # Download population rasters from worldpop
#' download_pop_rasters(
#'  country_codes = country_codeiso,
#'   dest_dir = file.path(tf, "01_data", "1b_rasters", "pop_raster") |>
#'     normalizePath(winslash = "/", mustWork = FALSE)
#' )

#' # Extract urban extent raster
#' extract_afurextent(
#'   dest_dir = file.path(tf, "01_data", "1b_rasters", "urban_extent") |>
#'     normalizePath(winslash = "/", mustWork = FALSE)
#' )
#'
#' # Modelling --------------------------------------------------------------
#'
#' run_full_workflow(
#'   country_code = country_codeiso,
#'   survey_data_path = file.path(
#'     tf, "01_data", "1a_survey_data", "processed"
#'   ) |>
#'     normalizePath(winslash = "/", mustWork = FALSE),
#'   survey_data_suffix = "dhs_pr_records_combined.rds",
#'   shape_path = file.path(
#'     tf, "01_data", "1c_shapefiles"
#'   ) |>
#'     normalizePath(winslash = "/", mustWork = FALSE),
#'   shape_suffix = "district_shape.gpkg",
#'   pop_raster_path = file.path(
#'     tf, "01_data", "1b_rasters", "pop_raster"
#'   ) |>
#'     normalizePath(winslash = "/", mustWork = FALSE),
#'   pop_raster_suffix = "_ppp_2020_constrained.tif",
#'   ur_raster_path = file.path(
#'     tf, "01_data", "1b_rasters", "urban_extent"
#'   ) |>
#'     normalizePath(winslash = "/", mustWork = FALSE),
#'   ur_raster_suffix = "afurextent.asc",
#'   pred_save_file = FALSE,
#'   raster_width = 2500,
#'   raster_height = 2000,
#'   raster_resolution = 300,
#'   save_raster = TRUE,
#'   pyramid_line_color = "#67000d",
#'   pyramid_fill_high = "#fee0d2",
#'   pyramid_fill_low = "#a50f15",
#'   pyramid_caption = paste0(
#'     "Note: Total population includes ",
#'     "ages 99+, pyramid shows ages 0-99"
#'   ),
#'   generate_pop_raster = TRUE,
#'   output_paths = list(
#'     model = file.path(tf, "03_outputs", "3a_model_outputs"),
#'     plot = file.path(tf, "03_outputs", "3b_visualizations"),
#'     raster = file.path(tf, "03_outputs", "3c_raster_outputs"),
#'     table = file.path(tf, "03_outputs", "3c_table_outputs"),
#'     compiled = file.path(tf, "03_outputs", "3d_compiled_results"),
#'     excel = file.path(
#'       tf, "03_outputs", "3d_compiled_results",
#'       "age_pop_denom_compiled.xlsx"
#'     ),
#'     log = file.path(
#'       tf, "03_outputs", "3a_model_outputs", "modelling_log.rds"
#'     )
#'   ) |> lapply(\(x) normalizePath(x, winslash = "/", mustWork = FALSE)),
#'   model_params = list(
#'     cell_size = 5000,
#'     n_sim = 10,
#'     ignore_cache = FALSE,
#'     age_range = c(0, 1),
#'     age_interval = 1,
#'     return_prop = TRUE,
#'     scale_outcome = "log_scale",
#'     shape_outcome = "log_shape",
#'     covariates = "urban",
#'     cpp_script = file.path(tf, "02_scripts", "model") |>
#'       normalizePath(winslash = "/", mustWork = FALSE),
#'     control_params = list(trace = 2),
#'     manual_params = NULL,
#'     verbose = TRUE
#'   ),
#'   return_results = FALSE,
#'   n_cores = 1
#' )
#' }
#'
#' @export
run_full_workflow <- function(
    country_code,
    survey_data_path = here::here("01_data", "1a_survey_data", "processed"),
    survey_data_suffix = "dhs_pr_records_combined.rds",
    shape_path = here::here("01_data", "1c_shapefiles"),
    shape_suffix = "district_shape.gpkg",
    pop_raster_path = here::here("01_data", "1b_rasters", "pop_raster"),
    pop_raster_suffix = "_ppp_2020_constrained.tif",
    ur_raster_path = here::here("01_data", "1b_rasters", "urban_extent"),
    ur_raster_suffix = "afurextent.asc",
    pred_save_file = FALSE,
    raster_width = 2500,
    raster_height = 2000,
    raster_resolution = 300,
    save_raster = TRUE,
    generate_pop_raster = FALSE,
    pyramid_line_color = "#67000d",
    pyramid_fill_high = "#fee0d2",
    pyramid_fill_low = "#a50f15",
    pyramid_caption = paste0(
      "Note: Total population includes ",
      "ages 99+, pyramid shows ages 0-99"
    ),
    output_paths = list(),
    model_params = list(),
    return_results = FALSE,
    n_cores = parallel::detectCores() - 2, ...) {
  # Define default output paths
  default_output_paths <- list(
    model = here::here("03_outputs", "3a_model_outputs"),
    plot = here::here("03_outputs", "3b_visualizations"),
    table = here::here("03_outputs", "3c_table_outputs"),
    compiled = here::here("03_outputs", "3d_compiled_results"),
    excel = here::here(
      "03_outputs", "3d_compiled_results",
      "age_pop_denom_compiled.xlsx"
    ),
    log = here::here("03_outputs", "3a_model_outputs", "modelling_log.rds")
  )

  # Define default model parameters
  default_model_params <- list(
    cell_size = 5000,
    n_sim = 5000,
    ignore_cache = FALSE,
    age_range = c(0, 99),
    age_interval = 1,
    age_range_raster = c(0, 10),
    age_interval_raster = 1,
    return_prop = TRUE,
    scale_outcome = "log_scale",
    shape_outcome = "log_shape",
    covariates = "urban",
    cpp_script = here::here("02_scripts", "model"),
    control_params = list(trace = 2),
    manual_params = NULL,
    verbose = TRUE
  )

  # Merge user-provided parameters with defaults
  output_paths <- utils::modifyList(default_output_paths, output_paths)
  model_params <- utils::modifyList(default_model_params, model_params)

  # Logging --------------------------------------------------------------------

  # Initialize logging
  start_time <- Sys.time()
  log_data <- list()

  # Import historical log if exists
  hist_log <- if (file.exists(output_paths$log)) {
    readRDS(output_paths$log)
  } else {
    list()
  }

  # Set up datasets ------------------------------------------------------------

  # Process each country code
  for (cc in country_code) {
    # Initialize log entry for this country code
    log_data[[cc]] <- list()

    tryCatch(
      {
        country_code_lw <- tolower(cc)

        # Load survey data
        survey_file <- file.path(survey_data_path, survey_data_suffix)
        if (!file.exists(survey_file)) {
          stop(glue::glue("Survey data file not found at {survey_file}"))
        }

        age_param_data <- readRDS(survey_file)$age_param_data |>
          dplyr::filter(country_code_iso3 == cc)

        # Load shapefiles
        shape_file <- file.path(shape_path, shape_suffix)
        adm2_shape <- sf::read_sf(shape_file) |>
          dplyr::filter(country_code %in% cc) |>
          sf::st_transform(crs = 3857)

        country_shape <- sf::st_union(adm2_shape)
        country_name <- adm2_shape$country[1]
        country_name_clr <- stringr::str_to_title(country_name) |>
          crayon::blue()

        # Load rasters
        pop_raster_file <- file.path(
          pop_raster_path,
          glue::glue("{country_code_lw}{pop_raster_suffix}")
        )

        if (!file.exists(pop_raster_file)) {
          stop(glue::glue(
            "Population raster not found for ",
            "{country_name_clr} at {pop_raster_file}"
          ))
        }

        pop_raster <- terra::rast(pop_raster_file)

        ur_raster_file <- file.path(ur_raster_path, ur_raster_suffix)

        if (!file.exists(ur_raster_file)) {
          stop(glue::glue(
            "Urban extent raster not found at {ur_raster_file}"
          ))
        }

        ur_raster <- terra::rast(ur_raster_file) |>
          terra::crop(terra::ext(pop_raster))

        # Run modeling workflow -----------------------------------------------

        cli::cli_h1(glue::glue(
          "Fitting Spatial Model for {country_name_clr}"
        ))

        spat_model_param <- fit_spatial_model(
          country_code = cc,
          data = age_param_data,
          scale_outcome = model_params$scale_outcome,
          shape_outcome = model_params$shape_outcome,
          covariates = model_params$covariates,
          cpp_script_name = model_params$cpp_script,
          verbose = model_params$verbose,
          control_params = model_params$control_params,
          manual_params = model_params$manual_params,
          output_dir = output_paths$model,
          ignore_cache = model_params$ignore_cache
        )

        cli::cli_h2(
          "Generating variogram plot for {country_name_clr}"
        )

        generate_variogram_plot(
          age_param_data = age_param_data,
          fit_vario = spat_model_param$variogram,
          country_code = cc,
          output_dir = output_paths$plot,
          scale_outcome = model_params$scale_outcome
        )

        cli::cli_h1(glue::glue(
          "Creating Predictor Data for {country_name_clr}"
        ))

        predictor_data <- create_prediction_data(
          country_code = cc,
          country_shape = country_shape,
          pop_raster = pop_raster,
          ur_raster = ur_raster,
          adm2_shape = adm2_shape,
          cell_size = model_params$cell_size,
          ignore_cache = model_params$ignore_cache,
          output_dir = output_paths$model
        )

        cli::cli_h1(glue::glue(
          "Running Prediction for {country_name_clr}"
        ))

        gamma_prediction <- generate_gamma_predictions(
          country_code = cc,
          age_param_data = age_param_data,
          model_params = spat_model_param,
          predictor_data = predictor_data,
          shapefile = adm2_shape,
          cell_size = model_params$cell_size,
          n_sim = model_params$n_sim,
          ignore_cache = model_params$ignore_cache,
          save_file = pred_save_file,
          output_dir = output_paths$model
        )

        # Create outputs -------------------------------------------------------

        cli::cli_h1(glue::glue(
          "Producing Prediction Rasters for {country_name_clr}"
        ))

        pred_list <- process_gamma_predictions(gamma_prediction)

        generate_gamma_raster_plot(
          predictor_data = predictor_data,
          pred_list = pred_list,
          country_code = country_code_lw,
          output_dir = output_paths$plot,
          save_raster = save_raster,
          file_name_suffix = "gamma_prediction_rasters",
          width = raster_width,
          height = raster_height,
          png_resolution = raster_resolution
        )

        cli::cli_h1(glue::glue(
          "Producing District-level ",
          "Age-Population Tables for {country_name_clr}"
        ))

        scale_pred <- gamma_prediction$scale_pred
        shape_pred <- gamma_prediction$shape_pred
        rm(gamma_prediction)

        final_age_pop_table <- generate_age_pop_table(
          predictor_data = predictor_data,
          scale_pred = scale_pred,
          shape_pred = shape_pred,
          country_code = country_code_lw,
          age_range = model_params$age_range,
          age_interval = model_params$age_interval,
          ignore_cache = model_params$ignore_cache,
          output_dir = output_paths$table,
          n_cores = n_cores
        )

        if (generate_pop_raster) {
          cli::cli_h1(glue::glue(
            "Producing Age-Population Raster for {country_name_clr}"
          ))

          age_pop_raster <- generate_age_pop_raster(
            predictor_data = predictor_data,
            scale_pred = scale_pred,
            shape_pred = shape_pred,
            age_range = model_params$age_range_raster,
            age_interval = model_params$age_interval_raster,
            country_code = country_code_lw,
            ignore_cache = model_params$ignore_cache,
            output_dir = output_paths$plot,
            n_cores = n_cores
          )
        }

        cli::cli_h1(glue::glue(
          "Producing Regional-level Age-pyramid for {country_name_clr}"
        ))

        # programmatically set up model breaks in y axis
        axis_by <- (model_params$age_range[2] / model_params$age_interval) / 10

        generate_age_pyramid_plot(
          dataset = final_age_pop_table,
          country_code = country_code_lw,
          output_dir = output_paths$plot,
          break_axis_by = axis_by,
          line_color = pyramid_line_color,
          fill_high = pyramid_fill_high,
          fill_low = pyramid_fill_low,
          caption = pyramid_caption
        )

        cli::cli_h1(
          glue::glue("Compiling model parameter data for all countries")
        )

        all_mod_params <- extract_age_param(
          dir_path = output_paths$model,
          output_file = output_paths$compiled
        )

        cli::cli_h1(glue::glue(
          "Compiling age-structured population data for all countries"
        ))

        final_pop <- process_final_population_data(
          input_dir = output_paths$table,
          excel_output_file = output_paths$excel
        )

        # Logging workflow ----------------------------------------------------

        # Log success
        log_data[[cc]] <- list(
          status = "Success",
          start_time = start_time,
          end_time = Sys.time(),
          duration = difftime(Sys.time(), start_time, units = "mins"),
          error_message = NA
        )

        if (return_results) {
          return(list(
            spat_model_param = spat_model_param,
            predictor_data = predictor_data,
            gamma_prediction = gamma_prediction,
            pred_list = pred_list,
            final_age_pop_table = final_age_pop_table,
            final_pop = final_pop,
            all_mod_params = all_mod_params,
            age_pop_raster = age_pop_raster
          ))
        }
      },
      error = function(e) {
        log_data[[cc]] <- list(
          status = "Error",
          start_time = start_time,
          end_time = Sys.time(),
          duration = difftime(Sys.time(), start_time, units = "mins"),
          error_message = e$message
        )

        cli::cli_alert_danger(glue::glue(
          "Error running model for {cc}: {e$message}"
        ))
      }
    )
  }

  # Update logs
  log_data_df <- dplyr::bind_rows(lapply(names(log_data), function(country) {
    if (length(log_data[[country]]) > 0) {
      data.frame(
        Country = country,
        Status = log_data[[country]]$status,
        Start_Time = as.character(log_data[[country]]$start_time),
        End_Time = as.character(log_data[[country]]$end_time),
        Duration_Minutes = as.numeric(log_data[[country]]$duration),
        Error_Message = as.character(log_data[[country]]$error_message),
        stringsAsFactors = FALSE
      )
    }
  }))

  logs <- dplyr::bind_rows(hist_log, log_data_df) |>
    dplyr::mutate(Error_Message = dplyr::coalesce(Error_Message, "None"))
  saveRDS(logs, output_paths$log)
}
