#define ERROR_POP_FILE_NOT_FOUND 1
#include <gsl/gsl_rng.h>
#include "general_model.h"

// Use these codes for readabililty
enum
{
    TRANS_FROM_ASYM,            // 0
    TRANS_FROM_PRESYM,          // 1
    TRANS_FROM_SYM,             // 2
    TRANS_FROM_HOME,            // 3
    TRANS_FROM_HOSP,            // 4
    TRANS_FROM_ICU1,            // 5
    TRANS_FROM_REHAB,           // 6
    EXPOSED,                    // 7
    RECOVERY_ASYM,              // 8
    PRESYM_TO_SYM,              // 9
    SYM_TO_HOME_HOSP_OR_ICU,    // 10
    RECOV_HOME,                 // 11
    RECOV_HOSP_OR_MOVE_TO_ICU,  // 12
    ICU1_TO_REHAB_OR_DEATH,     // 13
    RECOV_ICU2,                 // 14
    MIGRATE_S,                  // 15
    MIGRATE_I_ASYM,             // 16
    MIGRATE_I_PRESYM            // 17
};


//Parameter structure type:
struct COVID19ParamStruct : GeneralParamStruct
{
    int *input_N_pops;
    int *input_S_pops;
    int *input_E_pops;
    int *input_I_asym_pops;
    int *input_I_presym_pops;
    int *input_I_sym_pops;
    int *input_I_home_pops;
    int *input_I_hosp_pops;
    int *input_I_icu1_pops;
    int *input_I_icu2_pops;
    int *input_R_pops;
    int *input_D_pops;

    double *input_hosp_rate;     // vector from time windows
    double *input_icu_rate;     // vector from time windows
    double *input_death_rate;     // vector from time windows
    double *input_recov_hosp;     // vector from time windows

    double delta; //incub period
    double recov_a;
    double recov_p;
    double recov_s;
    double recov_home;
    double recov_hosp;
    double recov_icu1;
    double recov_icu2;
    double asym_rate;
    double sym_to_icu_rate;
    double hosp_rate;
    double icu_rate;
    double death_rate;
    double frac_beta_asym;
    double frac_beta_hosp;
};

typedef struct
{
  // Populations to track
  int **S_pop, **E_pop, **I_asym_pop;
  int **I_presym_pop, **I_sym_pop, **I_home_pop;
  int **I_hosp_pop, **I_icu1_pop, **I_icu2_pop;
  int **R_pop, **D_pop;
} COVID19PopStruct;


typedef struct
{
  // Populations to move
  int **S_move, **I_move;
} COVID19MoveMatStruct;


// struct covid_beta_calc_struct
// {
//     float r0;
//     COVID19ParamStruct *Params;
// };


int covid19_model(
    COVID19ParamStruct *params,
    int *out_pops_seed,
    int *out_pops_pop,
    int *out_pops_time,
    int *out_pops_S_pop,
    int *out_pops_E_pop,
    int *out_pops_I_asym_pop,
    int *out_pops_I_presym_pop,
    int *out_pops_I_sym_pop,
    int *out_pops_I_home_pop,
    int *out_pops_I_hosp_pop,
    int *out_pops_I_icu1_pop,
    int *out_pops_I_icu2_pop,
    int *out_pops_R_pop,
    int *out_pops_D_pop,
    int *out_events_pos,
    int *out_events_sym,
    int *out_events_total_hosp,
    int *out_events_total_icu,
    int *out_events_n_death
);
void trans_type_beta(
    double& beta_scaled,
    int this_pop,
    double infect_sum,
    COVID19ParamStruct *Params,
    COVID19PopStruct AllPops,
    gsl_rng *rand1
);
void tau_leap_1step(
    int *n_occur,
    int this_pop,
    COVID19ParamStruct *Params,
    COVID19PopStruct AllPops,
    gsl_rng *rand1,
    int n_events
);
void update_pops(
    int *update_vec,
    int* n_occur,
    int this_pop,
    int this_time,
    int this_seed,
    COVID19ParamStruct *Params,
    COVID19PopStruct AllPops,
    gsl_rng *rand1,
    int *out_events_pos,
    int *out_events_sym,
    int *out_events_total_hosp,
    int *out_events_total_icu,
    int *out_events_n_death
);
void move_pops(
    int *update_vec_move,
    int n_occur,
    int this_pop,
    COVID19ParamStruct *Params,
    gsl_rng *rand1
);
void update_pop_migrants(
    int *update_vec_migrants,
    int this_pop,
    COVID19ParamStruct *Params,
    COVID19PopStruct AllPops,
    COVID19MoveMatStruct MovePops,
    gsl_rng *rand1
);
// double covid19_beta_calc (double beta, void *params);
// double calculateBeta(float r0, COVID19ParamStruct *Params);
