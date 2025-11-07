model_interface <- function(control, arg.list)
{
    # Replicate beta vectors for all populations if only one provided
    tw <- arg.list[["input_tw"]]
    if (length(tw$beta) == 1) {
        beta_list <- tw$beta[[1]]
        n_pop <- length(control[["input_N_pops"]])
        for (this_pop in 2:n_pop) {
            tw$beta[[this_pop]] <- beta_list
        }
        arg.list[["input_tw"]] <- tw
    }

    if (control["model"] == "covid19")
    {
        arg.list[["input_N_pops"]] <- control[["input_N_pops"]]
        arg.list[["input_S_pops"]] <- control[["input_S_pops"]]
        arg.list[["input_E_pops"]] <- control[["input_E_pops"]]
        arg.list[["input_I_asym_pops"]] <- control[["input_I_asym_pops"]]
        arg.list[["input_I_presym_pops"]] <- control[["input_I_presym_pops"]]
        arg.list[["input_I_sym_pops"]] <- control[["input_I_sym_pops"]]
        arg.list[["input_I_home_pops"]] <- control[["input_I_home_pops"]]
        arg.list[["input_I_hosp_pops"]] <- control[["input_I_hosp_pops"]]
        arg.list[["input_I_icu1_pops"]] <- control[["input_I_icu1_pops"]]
        arg.list[["input_I_icu2_pops"]] <- control[["input_I_icu2_pops"]]
        arg.list[["input_R_pops"]] <- control[["input_R_pops"]]
        arg.list[["input_D_pops"]] <- control[["input_D_pops"]]
        arg.list[["frac_beta_asym"]] <- control[["frac_beta_asym"]]
        arg.list[["frac_beta_hosp"]] <- control[["frac_beta_hosp"]]
        arg.list[["delta"]] <- control[["delta"]]
        arg.list[["recov_a"]] <- control[["recov_a"]]
        arg.list[["recov_p"]] <- control[["recov_p"]]
        arg.list[["recov_s"]] <- control[["recov_s"]]
        arg.list[["recov_home"]] <- control[["recov_home"]]
        arg.list[["recov_icu1"]] <- control[["recov_icu1"]]
        arg.list[["recov_icu2"]] <- control[["recov_icu2"]]
        arg.list[["asym_rate"]] <- control[["asym_rate"]]
        arg.list[["sym_to_icu_rate"]] <- control[["sym_to_icu_rate"]]

        one.seed.list <- do.call(covid19_model_interface, arg.list)
    }

    if (control["model"] == "seir")
    {
        arg.list[["input_N_pops"]] <- control[["input_N_pops"]]
        arg.list[["input_S_pops"]] <- control[["input_S_pops"]]
        arg.list[["input_E_pops"]] <- control[["input_E_pops"]]
        arg.list[["input_I_pops"]] <- control[["input_I_pops"]]
        arg.list[["input_R_pops"]] <- control[["input_R_pops"]]
        arg.list[["birth"]] <- control[["birth"]]
        arg.list[["incubate"]] <- control[["incubate"]]
        arg.list[["recov"]] <- control[["recov"]]

        one.seed.list <- do.call(seir_model_interface, arg.list)
    }

    return(one.seed.list)
}
