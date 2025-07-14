#' Package Initialization and Dependency Check
#'
#' @description
#' This function checks for suggested packages and prompts the user to install
#' any missing ones that are needed for full functionality.
#'
#' @param libname The library name where the package is installed (not used)
#' @param pkgname The name of the package being loaded (not used)
#'
#' @details
#' The function maintains a predefined list of suggested packages and checks if
#' they are installed. For missing packages, it prompts the user for
#' installation in interactive sessions.
#'
#' The function uses 'cli' for user communication and handles errors gracefully
#' during installation attempts. In non-interactive sessions, it skips
#' installation and returns with a warning.
#'
#' @return
#' Returns NULL invisibly. The function's main effects are:
#' \itemize{
#'   \item Checking for missing suggested packages
#'   \item Displaying missing packages to user
#'   \item Installing packages if user agrees
#'   \item Providing feedback on installation success/failure
#' }
#'
#' @note
#' - Function requires an interactive session for package installation
#' - Some functionality may be limited if suggested packages are not installed
#' - Installation errors are caught and reported but don't stop execution
#'
#' @keywords internal
#' @export
install_suggested_packages <- function(libname = NULL, pkgname = NULL) {
  suggested_pkgs <- c(
    "cli", "countrycode", "crayon", "scales", "glue", "gstat",
    "haven", "here", "matrixStats", "rstudioapi", "geodata",
    "pbmcapply", "remotes", "future", "future.apply",
    "testthat", "rdhs", "openxlsx2", "purrr", "rlang", "pak",
    "sp", "automap", "knitr", "rmarkdown", "mockery"
  )

  missing_pkgs <- suggested_pkgs[!sapply(
    suggested_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    cli::cli_h2("Package Installation Required")
    cli::cli_text("The following packages are missing:")
    cli::cli_ol(paste0("{.pkg ", missing_pkgs, "}"))

    # Handle non-interactive sessions
    if (!interactive()) {
      cli::cli_alert_warning(
        "Non-interactive session detected. Skipping package installation."
      )
      return(invisible(NULL))
    }

    # Prompt user for installation
    user_choice <- readline(
      prompt = cli::col_blue(
        "Do you want to install all missing packages? (y/n): "
      )
    )

    if (tolower(user_choice) == "y") {
      cli::cli_alert_success("Installing missing packages...")

      # Install CRAN packages
      for (pkg in missing_pkgs) {
        tryCatch({
          utils::install.packages(pkg, quiet = TRUE)
        }, error = function(e) {
          cli::cli_alert_danger(paste0(
            "Failed to install package: ", pkg,
            ". Error: ", e$message
          ))
        })
      }

      cli::cli_alert_success("Installation of all packages complete.")
    } else {
      cli::cli_alert_warning(paste0(
        "Skipping installation of packages. ",
        "Some functionality might be limited."
      ))
    }
  } else {
    cli::cli_alert_success("All suggested packages are already installed.")
  }

  invisible(NULL)
}

#' Create a Standardized Project Folder Structure
#'
#' This function creates a standardized folder structure for organizing
#' data, scripts, and outputs within a project directory. It ensures
#' consistency and reproducibility for data-related workflows.
#'
#' @param base_path A character string specifying the root directory where
#'   the folder structure will be created. Defaults to `here::here()`
#'   to use the current project directory.
#'
#' @return Creates directories under the specified `base_path`. Returns
#'   invisible `NULL` and prints messages about folder creation status.
#'
#' @details The function generates the following folder structure:
#' \preformatted{
#' # 01_data/
#' # +-- 1a_survey_data/
#' # |    +-- processed/
#' # |    \-- raw/
#' # +-- 1b_rasters/
#' # |    +-- urban_extent/
#' # |    \-- pop_raster/
#' # +-- 1c_shapefiles/
#' # 02_scripts/
#' # 03_outputs/
#' # +-- 3a_model_outputs/
#' # +-- 3b_visualizations/
#' # +-- 3c_table_outputs/
#' # \-- 3d_compiled_results/
#' }
#'
#' @examples
#'\donttest{
#' # Create temp directory with normalized path
#' tf <- file.path(tempdir(), "test_env")
#' dir.create(tf, recursive = TRUE, showWarnings = FALSE)
#'
#' #  Initialize with normalized path
#' cpp_path <- file.path(tf, "02_scripts", "model")
#' dir.create(cpp_path, recursive = TRUE, showWarnings = FALSE)
#' cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = FALSE)
#'
#' create_project_structure(base_path = tf)
#' }
#' @export
create_project_structure <- function(base_path = here::here()) {

  # Define relative directories
  relative_dirs <- c(
    "01_data/1a_survey_data/processed",
    "01_data/1a_survey_data/raw",
    "01_data/1b_rasters/urban_extent",
    "01_data/1b_rasters/pop_raster",
    "01_data/1c_shapefiles",
    "02_scripts",
    "03_outputs/3a_model_outputs",
    "03_outputs/3b_visualizations",
    "03_outputs/3c_table_outputs",
    "03_outputs/3d_compiled_results"
  )

  # Construct full paths and create directories
  for (relative_dir in relative_dirs) {
    dir_path <- normalizePath(file.path(base_path, relative_dir),
                              winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cli::cli_alert_info("Created: {dir_path}")
    } else {
      cli::cli_alert_warning("Exists: {dir_path}")
    }
  }

  cli::cli_alert_success("Folder structure created successfully.")
}

#' Initialize Full Pipeline Script and Model Script
#'
#' @description
#' Creates a full pipeline R script and a model C++ script, saving them to the
#' appropriate folders within the project directory structure. The folder
#' structure is created using `AgePopDenom::create_project_structure()`.
#' The scripts contain example code for downloading and processing DHS data,
#' shapefiles, and running models.
#'
#' @param r_script_name Character. The name of the R script file to be created.
#' Defaults to `"full_pipeline.R"`.
#' @param cpp_script_name Character. The name of the C++ script file to be
#' created. Defaults to `"model.cpp"`.
#' @param path Character. The directory in which to create the scripts.
#'   Defaults to `here::here()`.
#' @param open_r_script Logical. Whether to open the R script automatically in
#'   RStudio (if available). Defaults to `TRUE`.
#' @param setup_rscript Logical. Whether to setup the R script with example
#'  code. Defaults to `TRUE`.
#'
#' @return Invisibly returns a list containing:
#' \itemize{
#'   \item r_script_path: Path to the created R script
#'   \item cpp_script_path: Path to the created C++ script
#' }
#' The function also prints success messages upon script creation.
#'
#' @examples
#' \donttest{
#' # Create temp directory with normalized path
#' tf <- file.path(tempdir(), "test_env")
#' dir.create(tf, recursive = TRUE, showWarnings = FALSE)
#'
#' #  Initialize with normalized path
#' cpp_path <- file.path(tf, "02_scripts", "model")
#' dir.create(cpp_path, recursive = TRUE, showWarnings = FALSE)
#' cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = FALSE)
#'
#' init(
#' r_script_name = "full_pipeline.R",
#' cpp_script_name = "model.cpp",
#' path = tf,
#' open_r_script = FALSE
#' )
#' }
#' @export
init <- function(r_script_name = "full_pipeline.R",
                 cpp_script_name = "model.cpp",
                 path = here::here(),
                 open_r_script = TRUE,
                 setup_rscript = TRUE) {
  # ensure sugegsted packages are installed
  AgePopDenom::install_suggested_packages()

  # Ensure the project structure is created
  AgePopDenom::create_project_structure(base_path = path)

  # Define the script directories
  r_script_dir <- file.path(path, "02_scripts")
  cpp_script_dir <- r_script_dir

  # Create the C++ folder if not already created by the project structure
  if (!dir.exists(cpp_script_dir)) {
    dir.create(cpp_script_dir, recursive = TRUE)
  }

  # Define the R script content
  r_script_content <- if (setup_rscript) {
    '
# set up country of interest
cntry_codes = c("GMB", "COM")

# Gather and process datasets --------------------------------------------------

# Download DHS datasets
AgePopDenom::download_dhs_datasets(
  country_codes = cntry_codes,
  email = "my_email@exmaple.com",
  project = "My populaiton denom project")

# Process DHS datasets
AgePopDenom::process_dhs_data()

# Download shapefiles
AgePopDenom::download_shapefile(cntry_codes)

# Download population rasters from worldpop
AgePopDenom::download_pop_rasters(cntry_codes)

# Extract urban extent raster
AgePopDenom::extract_afurextent()

# Run models and get outputs ---------------------------------------------------

# Run the model
AgePopDenom::run_full_workflow(cntry_codes)
'
  } else {
    ""
  }

  # Define the C++ script content
  cpp_script_content <- "
#include <TMB.hpp>

// Covariance computation function
template<class Type>
matrix<Type> compute_cov_init(Type gamma, Type sigma2, Type phi, Type tau2_1,
                              matrix<Type> dist_matrix,
                              vector<Type> b1, vector<Type> c, vector<Type> b2){
  int n_x = dist_matrix.rows();

  // Compute the covariance scaling
  matrix<Type> sigma_s = sigma2 * (-dist_matrix / phi).array().exp().matrix();

  Type tau2_2 = tau2_1;
  Type sqrt_tau = sqrt(tau2_1 * tau2_2);

  // Initialize the full covariance matrix
  matrix<Type> M(2 * n_x, 2 * n_x);
  M.setZero();

  // Block [1,1]: sigma_s + diag(b1) * tau2_1
  M.block(0, 0, n_x, n_x) = sigma_s;
  for (int i = 0; i < n_x; i++) {
    M(i, i) += tau2_1 * b1(i);
  }

  // Block [1,2] and [2,1]: gamma * sigma_s + diag(c) * sqrt_tau
  matrix<Type> cross_sigma_s = gamma * sigma_s;
  for (int i = 0; i < n_x; i++) {
    cross_sigma_s(i, i) += c(i) * sqrt_tau;
  }
  M.block(0, n_x, n_x, n_x) = cross_sigma_s;
  M.block(n_x, 0, n_x, n_x) = cross_sigma_s.transpose();

  // Block [2,2]: (gamma^2) * sigma_s + diag(b2) * tau2_2
  M.block(n_x, n_x, n_x, n_x) = pow(gamma, 2) * sigma_s;
  for (int i = 0; i < n_x; i++) {
    M(i + n_x, i + n_x) += tau2_2 * b2(i);
  }

  // Add small jitter for numerical stability
  M.diagonal().array() += 1e-6;

  return M;
}

// Objective function
template<class Type>
Type objective_function<Type>::operator() () {
  // INPUT DATA ----------------------------------------------------------------
  DATA_MATRIX(design_scale);   // Design matrix for scale
  DATA_MATRIX(design_shape);   // Design matrix for shape
  DATA_VECTOR(y);              // Response vector
  DATA_MATRIX(dist_matrix);    // Distance matrix
  DATA_VECTOR(b1);             // Diagonal components for scale
  DATA_VECTOR(c);              // Shared diagonal components
  DATA_VECTOR(b2);             // Diagonal components for shape

  // PARAMETERS ----------------------------------------------------------------
  PARAMETER_VECTOR(beta1);     // Regression coefficients for scale
  PARAMETER_VECTOR(beta2);     // Regression coefficients for shape
  PARAMETER(gamma);            // Cross-covariance scaling factor
  PARAMETER(log_sigma2);       // Log-transformed variance parameter
  PARAMETER(log_phi);          // Log-transformed spatial decay
  PARAMETER(log_tau2_1);       // Log-transformed variance for the process

  // TRANSFORM PARAMETERS -----------------------------------------------------
  Type sigma2 = exp(log_sigma2);
  Type tau2_1 = exp(log_tau2_1);
  Type phi = exp(log_phi);

  // LINEAR PREDICTOR ---------------------------------------------------------
  vector<Type> mu(design_scale.rows() + design_shape.rows());
  mu.head(design_scale.rows()) = design_scale * beta1;
  mu.tail(design_shape.rows()) = design_shape * beta2;

  // COVARIANCE MATRIX --------------------------------------------------------
  matrix<Type> M = compute_cov_init(gamma, sigma2, phi, tau2_1,
                                    dist_matrix, b1, c, b2);

  // NEGATIVE LOG-LIKELIHOOD --------------------------------------------------
  using namespace density;
  MVNORM_t<Type> neg_log_dmvnorm(M);
  parallel_accumulator<Type> nll(this);  // Parallelize likelihood computation
  nll += neg_log_dmvnorm(y - mu);

  // Subtract the constant term to match the R code
  Type n = y.size();
  Type log2pi = log(2.0 * M_PI);
  Type const_term = 0.5 * n * log2pi;
  nll -= const_term;

  return nll;  // Return the negative log-likelihood
}
"

# Write the R script if setup_rscript is TRUE
if (setup_rscript) {
  r_script_path <- file.path(r_script_dir, r_script_name)
  writeLines(r_script_content, r_script_path)

  # Open the R script in RStudio if available and requested
  if (open_r_script && requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::navigateToFile(r_script_path)
    cli::cli_alert_success(
      "R script '{r_script_path}' successfully created and opened."
    )
  } else {
    cli::cli_alert_info(
      glue::glue(
        "R script created but could not open automatically:",
        " RStudio not available."
      )
    )
  }
}

# Write the C++ script
cpp_script_path <- file.path(cpp_script_dir, cpp_script_name)
writeLines(cpp_script_content, cpp_script_path)
cli::cli_alert_success(
  "C++ script '{cpp_script_path}' successfully created."
)
}
