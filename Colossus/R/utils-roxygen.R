#' @param a0  linear slope
#' @param a1_goal  exponential maximum desired
#' @param a_n  list of initial parameter values, used to determine number of parameters. May be either a list of vectors or a single vector.
#' @param age_unit  age unit
#' @param alternative_model  the new model of interest in list form, output from a poisson regression
#' @param b  optimum parameter values used
#' @param ce  columns to check for truncation, (t0, t1, event)
#' @param cens_weight  column containing the row weights
#' @param ch  cumulative hazards of baseline
#' @param col_list  an array of column names that should have factor terms defined
#' @param col_name  vector of new column names
#' @param cols  columns to check
#' @param control  list of parameters controlling the convergence, see Def_Control() for options or vignette("Control_Options")
#' @param cons_mat Matrix containing coefficients for system of linear constraints, formatted as matrix
#' @param cons_vec Vector containing constants for system of linear constraints, formatted as vector
#' @param dep_cols  columns that are not needed in the new dataframe
#' @param der_iden  number for the subterm to test derivative at, only used for testing runs with a single varying parameter, should be smaller than total number of parameters. indexed starting at 0
#' @param df  a data.table containing the columns of interest
#' @param dnames  list of covariate columns to plot by
#' @param dt  spacing in time for new rows
#' @param e  output from a baseline calculation
#' @param er  standard deviation for the parameters
#' @param event0  column used for event status
#' @param factor_check  a boolean used to skip comparing columns of the form ?_? with the same initial string, which is used for factored columns
#' @param fir  term number for the initial term, used for models of the form T0*f(Ti) in which the order matters
#' @param fname  filename used for new dataframe
#' @param func_form  vector of functions to apply to each time-dependent covariate. Of the form func(df, time) returning a vector of the new column value
#' @param guesses_control  list of parameters to control how the guessing works, see Def_Control_Guess() for options or vignette("Control_Options")
#' @param h  hazards of baseline
#' @param interactions  array of strings, each one is of form term1?*?term2" for term1 interaction of type * or + with term2, "?" dlimits
#' @param iscox  boolean if rows not at event times should not be kept, rows are removed if true. a Cox proportional hazards model does not use rows with intervals not containing event times
#' @param keep_constant  binary values to denote which parameters to change
#' @param model_control  controls which alternative model options are used, see Def_model_control() for options and vignette("Control_Options") for further details
#' @param modelform  string specifying the model type: M, ME, A, PA, PAE, GMIX, GMIX-R, GMIX-E
#' @param msv  value to replace na with, same used for every column used
#' @param name_list  vector of string column names to check
#' @param names  columns for elements of the model, used to identify data columns
#' @param new_names  list of new names to use instead of default, default used if entry is ''
#' @param nthreads  number of threads to use, do not use more threads than available on your machine
#' @param null_model  a model to compare against, in list form
#' @param paras  list of formula parameters
#' @param plot_name  plot identifier, used in filename for saved plots
#' @param plot_options  list of parameters controlling the plot options, see RunCoxPlots() for different options
#' @param plot_type  list of parameters controlling the plot options: surv, risk, schoenfeld
#' @param pyr0  column used for person-years per row
#' @param realization_columns  used for multi-realization regressions. Matrix of column names with rows for each column with realizations, columns for each realization
#' @param realization_index  used for multi-realization regressions. Vector of column names, one for each column with realizations. each name should be used in the "names" variable in the equation definition
#' @param strat_col  column to stratify by if needed
#' @param studyID  id to group by, NaN for no grouping
#' @param surv  survival fraction of baseline
#' @param t  event times
#' @param term_n  term numbers for each element of the model
#' @param tform  list of string function identifiers, used for linear/step
#' @param tforms  list of formula types
#' @param time1  column used for time period starts
#' @param time2  column used for time period end
#' @param time_lims  limits for x axis of plot
#' @param tref  reference time in date format
#' @param tu  unique event times
#' @param units  time unit to use
#' @param verbose  integer valued 0-4 controlling what information is printed to the terminal. Each level includes the lower levels. 0: silent, 1: errors printed, 2: warnings printed, 3: notes printed, 4: debug information printed. Errors are situations that stop the regression, warnings are situations that assume default values that the user might not have intended, notes provide information on regression progress, and debug prints out C++ progress and intermediate results. The default level is 2 and True/False is converted to 3/0.
#' @param y  point formula switch
#'
#' @name R_template
NULL

#' @param CR_bool  boolean for competing risks
#' @param Control  control list
#' @param Dose  term matrix
#' @param KeepConstant  binary vector to identify constant parameters
#' @param Ll  Log-likelihood vector
#' @param Lld  Log-likelihood first derivative matrix
#' @param Lldd  Log-likelihood second derivative matrix
#' @param Lldd_inv  inverse of second derivative of Log-Likelihood matrix
#' @param Lls1  Second Risk sum storage
#' @param Lls2  Second Risk sum derivative storage
#' @param Lls3  Second Risk sum second derivative storage
#' @param PyrC  matrix of person-years and event counts
#' @param R  risk vector
#' @param Rd  Optimal values
#' @param Rd  Risk first derivative matrix
#' @param RdR  Risk to first derivative ratio matrix
#' @param Rdd  Risk second derivative matrix
#' @param RddR  Risk to second derivative ratio matrix
#' @param RiskFail  matrix of indexes for event rows in each risk group
#' @param RiskGroup  list of string indices for every risk group
#' @param RiskGroup_Strata  matrix of strings with rows at risk for each event time and strata
#' @param Risk_Sub_bool  boolean for competing risks
#' @param Risk_bool  boolean for competing risks
#' @param Rls1  First Risk sum storage
#' @param Rls2  First Risk sum derivative storage
#' @param Rls3  First Risk sum second derivative storage
#' @param STRATA_vals  vector of strata identifier values
#' @param Schoenfeld_bool  boolean for competing risks
#' @param Surv_bool  boolean for competing risks
#' @param T0  Term value for each subterm
#' @param TTerm  Total term matrix
#' @param Td0  Term by subterm derivative matrix
#' @param Tdd0  Term by subterm second derivative matrix
#' @param Te  temporary term storage matrix
#' @param term_n  Term numbers
#' @param a_er  Optimal value standard error
#' @param a_n  starting values
#' @param a_ns  matrix of starting values
#' @param abs_max  Maximum allowed parameter change
#' @param basic_bool  boolean for multiplicative log-linear model
#' @param beta_0  parameter estimates
#' @param cens_cutoff  double threshold for adding competing risk to risk group, not implemented
#' @param cens_thres  threshold to add competing event to risk group
#' @param cens_vec  censoring weight list
#' @param cens_weight  vector of censoring weights
#' @param change_all  boolean if every parameter is being updated
#' @param colToRemove  column index to remove
#' @param cols  list of column identifiers, single continuous list
#' @param constraint_bool  boolean for system of linear equality constraints used
#' @param cons_mat Matrix containing coefficients for system of linear constraints
#' @param cons_vec Vector containing constants for system of linear constraints
#' @param dbeta  parameter change vector
#' @param dbeta_cap  learning rate for newton step toward 0 log-likelihood
#' @param debugging  additional boolean for verbosity in testing
#' @param der_iden  subterm number for derivative tests
#' @param deriv_epsilon  threshold for near-zero derivative
#' @param df0  matrix of covariate values
#' @param df0_Times  Matrix with (starting time, ending time)
#' @param df0_const  matrix with values that are held constant
#' @param df0_dep  matrix with pairs of (covariate at start, covariate at end) for each time-dependent covariate
#' @param df0_event  matrix with event status, zero up to the last entry for each original row
#' @param df_groups  matrix with time and event information
#' @param df_m  event/time matrix
#' @param dfc  vector matching subterm number to matrix column
#' @param dfe  Matrix with person-year/event count information
#' @param dfs  Matrix with stratification columns, assumed to be binary and mutually exclusive
#' @param dint  value used for threshold derivative finite step
#' @param dose_abs_max  Maximum allowed threshold parameter change
#' @param double_step  controls the step calculation, 0 for independent changes, 1 for solving b=Ax with complete matrices
#' @param dslp  value used for slope derivative finite step
#' @param dt  spacing in time
#' @param epsilon  minimum acceptable maximum parameter change
#' @param filename  file to save the data to
#' @param fir  st term number
#' @param gmix_term list of 0/1 to identify which terms to set as Relative Risk (0) or Excess Risk (1)
#' @param gmix_theta theta value for geometric-mixture model
#' @param guesses  number of initial
#' @param halfmax  maximum number of half steps
#' @param iscox  boolean of cox formatting is used
#' @param iter_stop  binary value used to tell the function not to continue iteration
#' @param Lin_Res Vector containing constants for system of linear constraints
#' @param Lin_Sys Matrix containing coefficients for system of linear constraints
#' @param lr  learning rate for newton step toward 0 derivative
#' @param matrix_modify matrix to remove rows or columns from 
#' @param maxiter   integer of maximum number of iterations
#' @param maxiters  list of maximum number of iterations
#' @param model_control  controls which alternative model options are used
#' @param modelform  string model identifier
#' @param nonDose  term matrix
#' @param nonDose_LIN  Linear term matrix
#' @param nonDose_LOGLIN  Loglinear term matrix
#' @param nonDose_PLIN  Product linear term matrix
#' @param nthreads  number of threads available
#' @param ntime  number of risk groups
#' @param null_bool  boolean for null model
#' @param reqrdnum  total number of free parameters
#' @param rowToRemove  row index to remove
#' @param s_weights  vector of weights for every row
#' @param single_bool  boolean for single calculation without derivatives
#' @param start  starting time for regression
#' @param strata_bool  boolean for stratification
#' @param term_tot  total number of terms
#' @param tform  subterm types
#' @param tform_tdep  vector with types of time dependent variables
#' @param ties_method  Ties method
#' @param totalnum  total number of parameters
#' @param tu  Event time vector
#' @param uniq_v  number of unique covariate values
#' @param vals  list of values for each column, single continuous list
#' @param verbose  boolean for additional printing
#' @param vm_usage  double to store peak usage at
#' @param x  std::vector to take norm of, assumed doubles
#' @param x_all  covariate matrix
#'
#' @name CPP_template
NULL

#' @importFrom Rcpp evalCpp
#' @importFrom data.table data.table fread setkeyv copy setorderv setnames as.data.table set := .SD
#' @importFrom parallel detectCores
#' @importFrom stats runif
#' @importFrom utils combn head
#' @importFrom grDevices colorRampPalette dev.off jpeg
#' @importFrom graphics legend lines smoothScatter
#' @importFrom stats approxfun time qchisq
#' @importFrom rlang .data
#' @importFrom processx run
#' @importFrom stringr str_match
#' @importFrom callr rcmd
#' @useDynLib Colossus, .registration = TRUE
NULL



