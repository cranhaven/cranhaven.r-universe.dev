#' Similar to a named list but with custom error checking for the SPARSEMODr model.
#'
#' @author Seth Borkovec 2020
#'
#' @description Provides control structures used for model-specific parameters
#'              and includes data validation.


# Control parameters specifically used for the COVID19 model
covid19_control <- function(
    input_N_pops=NULL,
    input_S_pops=NULL,
    input_E_pops=NULL,
    input_I_asym_pops=NULL,
    input_I_presym_pops=NULL,
    input_I_sym_pops=NULL,
    input_I_home_pops=NULL,
    input_I_hosp_pops=NULL,
    input_I_icu1_pops=NULL,
    input_I_icu2_pops=NULL,
    input_R_pops=NULL,
    input_D_pops=NULL,
    frac_beta_asym=0.55,
    frac_beta_hosp=0.05,
    delta=1/3.0,
    recov_a=1/6.0,
    recov_p=1/2.0,
    recov_s=1/6.0,
    recov_home=1/3.0,
    recov_icu1=1/8.0,
    recov_icu2=1/4.0,
    asym_rate=0.40,
    sym_to_icu_rate=0.015)
{
    # At least one of these should be supplied
    if ((is.null(input_E_pops)) &&
        (is.null(input_I_asym_pops)) &&
        (is.null(input_I_presym_pops)) &&
        (is.null(input_I_sym_pops)) &&
        (is.null(input_I_home_pops)) &&
        (is.null(input_I_hosp_pops)) &&
        (is.null(input_I_icu1_pops)) &&
        (is.null(input_I_icu2_pops)) &&
        (is.null(input_R_pops)) &&
        (is.null(input_D_pops)))
    {
        stop("Error: At least one potentially infectous class of population must be supplied.")
    }

    pop_length <- 0
    nonzero_pops_exists <- FALSE
    # Function to validate or populate the infectious pops
    populateAndValidatePops <- function(pops, pops_name, pop_length, nonzero_pops_exists)
    {
        if (is.null(pops))
        {
            message(paste("Parameter ", pops_name, " was not specified; assuming to be zeroes.", sep=""))
            pops <- rep(0, pop_length)
        }
        else
        {
            if (length(pops) != pop_length) stop(paste("Error: The length of ", pops_name, " does not match the other populations.", sep=""))
            if (!all(pops >= 0)) stop(paste("Error: The parameter ", pops_name, " has a negative value.", sep=""))
            # Check for at least one nonzero value
            if (sum(pops) > 0) nonzero_pops_exists <- TRUE
        }
        return(list(pops, nonzero_pops_exists))
    }

    # Either N pops or S pops must be supplied
    if (is.null(input_S_pops))
    {
        if (is.null(input_N_pops))
        {
            stop("Error: At least one of input_N_pops or input_S_pops must be supplied.")
        }
        else
        {
            if (!all(input_N_pops >= 0)) stop("Error: The parameter input_N_pops has a negative value.")
            pop_length <- length(input_N_pops)
        }
    }
    else
    {
        if (!all(input_S_pops >= 0)) stop("Error: The parameter input_S_pops has a negative value.")
        pop_length <- length(input_S_pops)
    }

    # Populate unsupplied populations with zeroes
    result <- populateAndValidatePops(input_E_pops, "input_E_pops", pop_length, nonzero_pops_exists)
    input_E_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_asym_pops, "input_I_asym_pops", pop_length, nonzero_pops_exists)
    input_I_asym_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_presym_pops, "input_I_presym_pops", pop_length, nonzero_pops_exists)
    input_I_presym_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_sym_pops, "input_I_sym_pops", pop_length, nonzero_pops_exists)
    input_I_sym_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_home_pops, "input_I_home_pops", pop_length, nonzero_pops_exists)
    input_I_home_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_hosp_pops, "input_I_hosp_pops", pop_length, nonzero_pops_exists)
    input_I_hosp_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_icu1_pops, "input_I_icu1_pops", pop_length, nonzero_pops_exists)
    input_I_icu1_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_icu2_pops, "input_I_icu2_pops", pop_length, nonzero_pops_exists)
    input_I_icu2_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_R_pops, "input_R_pops", pop_length, nonzero_pops_exists)
    input_R_pops <- result[[1]]
    result <- populateAndValidatePops(input_D_pops, "input_D_pops", pop_length, nonzero_pops_exists)
    input_D_pops <- result[[1]]

    if (!nonzero_pops_exists) stop("Error: At least one potentially infectous class of population must contain a nonzero value.")

    # Sum the pops for calculating S pops or N pops
    combined_pops <- input_E_pops
    combined_pops <- combined_pops + input_I_asym_pops
    combined_pops <- combined_pops + input_I_presym_pops
    combined_pops <- combined_pops + input_I_sym_pops
    combined_pops <- combined_pops + input_I_home_pops
    combined_pops <- combined_pops + input_I_hosp_pops
    combined_pops <- combined_pops + input_I_icu1_pops
    combined_pops <- combined_pops + input_I_icu2_pops
    combined_pops <- combined_pops + input_R_pops
    combined_pops <- combined_pops + input_D_pops

    # Either N pops or S pops must be supplied. Calculate the other one.
    if (!is.null(input_S_pops))
    {
        combined_pops <- combined_pops + input_S_pops

        # Calculate N pops
        if (is.null(input_N_pops)) {
            input_N_pops <- combined_pops
        }
        # Validate N pops is total of S, E, I, and R pops
        else
        {
            if (length(input_N_pops) != pop_length) stop("Error: The lengths of input_N_pops ind input_S_pops do not match.")
            if (!all(input_N_pops == combined_pops)) stop("Error: The parameter input_N_pops must equal the sums of the other populations.")
        }
    }
    # S pops was not supplied
    else
    {
        # calculate S pops
        input_S_pops <- input_N_pops - combined_pops
    }

    # Make sure N pops does not have a zero population
    if (!all(input_N_pops > 0)) stop("Error: Parameter input_N_pops cannot contain a zero.")

    # Function to verify that parameter is within (0,1]
    checkIfInZeroToOne <- function(parameter, name)
    {
        if ((parameter <= 0) || (parameter > 1)) stop(paste("Error: The value of ", name, " must be within the range (0, 1]. The value of ", name, " was: ", parameter, ".", sep=""))
    }

    # Function to verify the parameter is not negative. Warning for greater than one
    checkIfGreaterThanZero <- function(parameter, name)
    {
        if (parameter < 0) stop(paste("Error: The value of ", name, " must be greater than or equal to zero. The value of ", name, " was: ", parameter, ".", sep=""))
        if (parameter > 1) warning(paste("Warning: The value of ", name, " is ", parameter, ", which is greater than one and unusual. Are you sure you want this?", sep=""))
    }

    # Validate parameters
    checkIfInZeroToOne(frac_beta_asym, "frac_beta_asym")
    checkIfInZeroToOne(frac_beta_hosp, "frac_beta_hosp")
    checkIfInZeroToOne(asym_rate, "asym_rate")
    checkIfInZeroToOne(sym_to_icu_rate, "sym_to_icu_rate")
    checkIfGreaterThanZero(delta, "delta")
    checkIfGreaterThanZero(recov_a, "recov_a")
    checkIfGreaterThanZero(recov_p, "recov_p")
    checkIfGreaterThanZero(recov_s, "recov_s")
    checkIfGreaterThanZero(recov_home, "recov_home")
    checkIfGreaterThanZero(recov_icu1, "recov_icu1")
    checkIfGreaterThanZero(recov_icu2, "recov_icu2")

    # Assign the values to the class fields
    value <- list(
        model="covid19",
        input_N_pops=input_N_pops,
        input_S_pops=input_S_pops,
        input_E_pops=input_E_pops,
        input_I_asym_pops=input_I_asym_pops,
        input_I_presym_pops=input_I_presym_pops,
        input_I_sym_pops=input_I_sym_pops,
        input_I_home_pops=input_I_home_pops,
        input_I_hosp_pops=input_I_hosp_pops,
        input_I_icu1_pops=input_I_icu1_pops,
        input_I_icu2_pops=input_I_icu2_pops,
        input_R_pops=input_R_pops,
        input_D_pops=input_D_pops,
        frac_beta_asym=frac_beta_asym,
        frac_beta_hosp=frac_beta_hosp,
        delta=delta,
        recov_a=recov_a,
        recov_p=recov_p,
        recov_s=recov_s,
        recov_home=recov_home,
        recov_icu1=recov_icu1,
        recov_icu2=recov_icu2,
        asym_rate=asym_rate,
        sym_to_icu_rate=sym_to_icu_rate)

    # Create the S3 class
    attr(value, "class") <- "control"
    value
}



# Control parameters specifically used for the SEIR model
seir_control <- function(
    input_N_pops=NULL,
    input_S_pops=NULL,
    input_E_pops=NULL,
    input_I_pops=NULL,
    input_R_pops=NULL,
    birth=1/(75*365),
    incubate=1/8.0,
    recov=1/3.0)
{
    # Required pops
    if (is.null(input_I_pops) && is.null(input_E_pops)) stop("Error: At least one of input_I_pops and input_E_pops must be supplied.")

    pop_length <- 0
    nonzero_pops_exists <- FALSE
    # Function to validate or populate the infectious pops
    populateAndValidatePops <- function(pops, pops_name, pop_length, nonzero_pops_exists)
    {
        if (is.null(pops))
        {
            message(paste("Parameter ", pops_name, " was not specified; assuming to be zeroes.", sep=""))
            pops <- rep(0, pop_length)
        }
        else
        {
            if (length(pops) != pop_length) stop(paste("Error: The length of ", pops_name, " does not match the other populations.", sep=""))
            if (!all(pops >= 0)) stop(paste("Error: The parameter ", pops_name, " has a negative value.", sep=""))
            # Check for at least one nonzero value
            if (sum(pops) > 0) nonzero_pops_exists <- TRUE
        }
        return(list(pops, nonzero_pops_exists))
    }

    # Either N pops or S pops must be supplied
    if (is.null(input_S_pops))
    {
        if (is.null(input_N_pops))
        {
            stop("Error: At least one of input_N_pops or input_S_pops must be supplied.")
        }
        else
        {
            if (!all(input_N_pops >= 0)) stop("Error: The parameter input_N_pops has a negative value.")
            pop_length <- length(input_N_pops)
        }
    }
    else
    {
        if (!all(input_S_pops >= 0)) stop("Error: The parameter input_S_pops has a negative value.")
        pop_length <- length(input_S_pops)
    }

    # Populate unsupplied populations with zeroes
    result <- populateAndValidatePops(input_E_pops, "input_E_pops", pop_length, nonzero_pops_exists)
    input_E_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_I_pops, "input_I_pops", pop_length, nonzero_pops_exists)
    input_I_pops <- result[[1]]
    nonzero_pops_exists <- result[[2]]
    result <- populateAndValidatePops(input_R_pops, "input_R_pops", pop_length, nonzero_pops_exists)
    input_R_pops <- result[[1]]

    if (!nonzero_pops_exists) stop("Error: At least one of input_E_pops and input_I_pops must contain a nonzero value.")

    # Sum the pops for calculating S pops or N pops
    combined_pops <- input_E_pops + input_I_pops

    # Either N pops or S pops must be supplied. Calculate the other one.
    if (!is.null(input_S_pops))
    {
        combined_pops <- combined_pops + input_S_pops

        # Calculate N pops
        if (is.null(input_N_pops)) {
            input_N_pops <- combined_pops
        }
        # Validate N pops is total of S, E, I, and R pops
        else
        {
            if (length(input_N_pops) != pop_length) stop("Error: The lengths of input_N_pops ind input_S_pops do not match.")
            if (!all(input_N_pops == combined_pops)) stop("Error: The parameter input_N_pops must equal the sums of the other populations.")
        }
    }
    # S pops was not supplied
    else
    {
        # calculate S pops
        input_S_pops <- input_N_pops - combined_pops
    }

    # Make sure N pops does not have a zero population
    if (!all(input_N_pops > 0)) stop("Error: Parameter input_N_pops cannot contain a zero.")

    # Function to verify that parameter is within (0,1]
    checkIfInZeroToOne <- function(parameter, name)
    {
        if ((parameter <= 0) || (parameter > 1)) stop(paste("Error: The value of ", name, " must be within the range (0, 1]. The value of ", name, " was: ", parameter, ".", sep=""))
    }

    # Function to verify the parameter is not negative. Warning for greater than one
    checkIfGreaterThanZero <- function(parameter, name)
    {
        if (parameter < 0) stop(paste("Error: The value of ", name, " must be greater than or equal to zero. The value of ", name, " was: ", parameter, ".", sep=""))
        if (parameter > 1) warning(paste("Warning: The value of ", name, " is ", parameter, ", which is greater than one and unusual. Are you sure you want this?", sep=""))
    }

    # Validate parameters
    checkIfGreaterThanZero(birth, "birth")
    checkIfGreaterThanZero(incubate, "incubate")
    checkIfGreaterThanZero(recov, "recov")

    # Assign the values to the class fields
    value <- list(
        model="seir",
        input_N_pops=input_N_pops,
        input_S_pops=input_S_pops,
        input_E_pops=input_E_pops,
        input_I_pops=input_I_pops,
        input_R_pops=input_R_pops,
        birth=birth,
        incubate=incubate,
        recov=recov)

    # Create the S3 class
    attr(value, "class") <- "control"
    value
}
