#define ERROR_POP_FILE_NOT_FOUND 1
#include <gsl/gsl_rng.h>
#include "general_model.h"

// Use these codes for readabililty
enum
{
    BIRTH,        // 0
    TRANS_FROM_I, // 1
    LATENCY,      // 2
    RECOVERY,     // 3
    DEATH_S,      // 4
    DEATH_E,      // 5
    DEATH_I,      // 6
    DEATH_R,      // 7
    S_MIGRATE_S,  // 8
    MIGRATE_I     // 9
};


//Parameter structure type:
struct SEIRParamStruct : GeneralParamStruct
{
    int *input_N_pops;
    int *input_S_pops;
    int *input_E_pops;
    int *input_I_pops;
    int *input_R_pops;

    double birth; // birth = death
    double incubate; //incubation
    double recov;
};

typedef struct
{
  // Populations to track
  int **S_pop, **E_pop, **I_pop, **R_pop;

} PopStruct;


typedef struct
{
  // Populations to move
  int **S_move, **I_move;
} MoveMatStruct;


// struct beta_calc_struct
// {
//     float r0;
//     SEIRParamStruct *Params;
// };


int seir_model(
    SEIRParamStruct *params,
    int *out_pops_seed,
    int *out_pops_pop,
    int *out_pops_time,
    int *out_pops_S_pop,
    int *out_pops_E_pop,
    int *out_pops_I_pop,
    int *out_pops_R_pop,
    int *out_events_birth,
    int *out_events_exposed,
    int *out_events_infectious,
    int *out_events_recov,
    int *out_events_death
);
void trans_type_beta(
    double& beta_scaled, int this_pop, double infect_sum,
    SEIRParamStruct *Params, PopStruct AllPops, gsl_rng *rand1
);
void tau_leap_1step(
    int *n_occur, int this_pop, SEIRParamStruct *Params,
    PopStruct AllPops, gsl_rng *rand1, int n_events
);
void update_pops(
    int *update_vec, int* n_occur, int this_pop,
    int this_time, int this_seed,
    SEIRParamStruct *Params, PopStruct AllPops, gsl_rng *rand1,
    int *out_events_birth,
    int *out_events_death,
    int *out_events_exposed,
    int *out_events_infectious,
    int *out_events_recov
);
void move_pops(
    int *update_vec_move, int n_occur, int this_pop,
    SEIRParamStruct *Params, gsl_rng *rand1
);
void update_pop_migrants(
    int *update_vec_migrants, int this_pop,
    SEIRParamStruct *Params, PopStruct AllPops,
    MoveMatStruct MovePops, gsl_rng *rand1
);
// double beta_calc (double beta, void *params);
// double calculateBeta(float r0, SEIRParamStruct *Params);
