#include <time.h>
#include "stdio.h"
#include "string.h"
#include <stdlib.h>
#include <unistd.h>
#include <unordered_map>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_errno.h>	// GSL_SUCCEnn ...

#include <gsl/gsl_types.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_min.h>

#include <gsl/gsl_roots.h>

#include "math.h"

//Custom headers and functions:
#include "Nrutil.h"
#include "seir_model.h"
#include "time_window_utils.h"



int seir_model (
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
            int *out_events_death)
{
    // Allocate random numbers structures:
    gsl_rng_env_setup();
    const gsl_rng_type *T1 = gsl_rng_default;
    gsl_rng *rand1 = gsl_rng_alloc(T1);
    //*****************************************

    double beta_temp = 0, beta_slope = 0, beta_intercept = 0;
    double m_slope = 0, m_intercept = 0;
    double dist_phi = 0, dist_phi_low = 0, dist_phi_slope = 0;
    double dist_phi_intercept = 0;
    double imm_frac_slope = 0, imm_frac_intercept = 0;
    bool dist_phi_changed = false;
    bool daily_mode_on = false;

    // Create a map for the beta values to avoid repeated calcuations. <unordered_map> is used because it is faster than the regular map.
    std::unordered_map<double, double> beta_map;

    // Import the time window data into a linked list
    TimeWindow *head_node, *current_node = NULL;
    head_node = importTimeWindowData(params->n_pop,
                                     params->total_windows,
                                     params->input_beta,
                                     params->input_dist_phi,
                                     params->input_m,
                                     params->input_imm_frac,
                                     NULL,  // input_hosp_rate
                                     NULL,  // input_icu_rate
                                     NULL,  // input_death_rate
                                     NULL,  // input_recov_hosp
                                     params->input_window_length);

    current_node = head_node;

    int n_pop = params->n_pop;
    int n_times;
    n_times = params->t_max / params->tau;  // t_max = 250, tau = 1

    double *beta;

    // Events and Updates:
    int n_events = 10;
    int *n_occur = new int[n_events];
    int *update_vec = new int[params->n_equations];
    int *update_vec_migrants = new int[params->n_equations];
    int *update_vec_move = new int[n_pop];
    //*****************************************

    /* *************************************************************** */
    /* Read in the Pop-specific data and Distance Matrix   :           */
    /* *************************************************************** */

    float *pop_N, *prob_ColSum;
    float *census_area;
    float **dist_mat, **prob_move;

    //TODO: replace these matrix/vector allocations from Nrutil.h with
    //pre-allocated pointer arguments, so we don't have to worry about
    //memory management in here.
    dist_mat = nrutil_matrix(1, n_pop, 1, n_pop);
    prob_move = nrutil_matrix(1, n_pop, 1, n_pop);
    prob_ColSum = nrutil_vector(1, n_pop);
    pop_N = nrutil_vector(1, n_pop);
    census_area = nrutil_vector(1, n_pop);
    beta = nrutil_dvector(1, n_pop);

    // compute prob_ColSum and prob_move based on dist_mat and
    // dist_phi_low.
    for(int i = 1; i <= n_pop; i++){
        pop_N[i] = params->input_N_pops[i-1];//TODO remove pop_N/census_area, just use input_*
        census_area[i] = params->input_census_area[i-1];
        prob_ColSum[i] = 0.0;

        for(int j = 1; j <= n_pop; j++){
            dist_mat[i][j] = params->input_dist_vec[(i-1)+(j-1)*n_pop];
            prob_move[i][j] = 0;

            if(i != j){
                prob_move[i][j] = 1 / exp(dist_mat[i][j] / dist_phi_low);
            }

            prob_ColSum[j] = prob_ColSum[j] + prob_move[i][j];
            // printf("prob_move[%d][%d]: %f, ", i, j, prob_move[i][j]);
        }
         // printf("\n");
    }

    params->pop_N = pop_N;
    params->census_area = census_area;
    params->dist_mat = dist_mat;
    params->prob_move = prob_move;

    /* ***************************************************  */
    /* Allocate and store populaitons   :                   */
    /* ***************************************************  */
    // TRACKED POPULATIONS
    // # ROWS = POPS
    // # Cols = 1: Current, 2: Future
    int **S_pop, **E_pop, **I_pop, **R_pop;
    S_pop = nrutil_imatrix(1, n_pop, 1, 2);
    E_pop = nrutil_imatrix(1, n_pop, 1, 2);
    I_pop = nrutil_imatrix(1, n_pop, 1, 2);
    R_pop = nrutil_imatrix(1, n_pop, 1, 2);

    // MIGRANTS (from = cols, to = rows)

    int **S_move, **I_move;

    S_move = nrutil_imatrix(1, n_pop, 1, n_pop);
    I_move = nrutil_imatrix(1, n_pop, 1, n_pop);

    for(int i=1;i<=n_pop;i++){
        for(int j=1;j<=2;j++){
            S_pop[i][j] = 0.0; E_pop[i][j] = 0.0;
            I_pop[i][j] = 0.0; R_pop[i][j] = 0.0;
        }

        for(int j=1;j<=n_pop;j++){
            S_move[i][j] = 0;
            I_move[i][j] = 0;
        }
    }


    //*****************************************

    /* ***************************************************  */
    /* INITIAL CONDITIONS  :                                */
    /* ***************************************************  */

    //********************

    PopStruct AllPops;

    AllPops.S_pop = S_pop;
    AllPops.E_pop = E_pop;
    AllPops.I_pop = I_pop;
    AllPops.R_pop = R_pop;

    MoveMatStruct MovePops;

    MovePops.S_move = S_move;
    MovePops.I_move = I_move;

    //*****************************************
    //*****************************************
    //*****************************************

    // RUN THE SIMULATION:

    int out_pops_line=0;
    int out_events_line=0;

    for(int seed_i = 0; seed_i < params->n_realz; seed_i++){ // n_realz how many times running the model
        int this_seed = params->input_realz_seeds[seed_i];
        //printf("seed_i=%d this_seed=%d\n", seed_i, this_seed);
        gsl_rng_set(rand1, this_seed);
        /* ***************************************************  */
        /* INITIAL CONDITIONS  :                                */
        /* ***************************************************  */

        for(int k=1;k<=n_pop;k++){
            for(int j=1;j<=2;j++){

                // SET AS INPUT_*_POPS
                S_pop[k][j] = params->input_S_pops[(k-1)];
                E_pop[k][j] = params->input_E_pops[(k-1)];
                I_pop[k][j] = params->input_I_pops[(k-1)];
                R_pop[k][j] = params->input_R_pops[(k-1)];
            }

            for(int j=1;j<=n_pop;j++){
                S_move[k][j] = 0;
                I_move[k][j] = 0;
            }
        }

        AllPops.S_pop = S_pop;
        AllPops.E_pop = E_pop;
        AllPops.I_pop = I_pop;
        AllPops.R_pop = R_pop;

        // TEST:
        // exit(1);

        /* ***************************************************  */
        /* BEGIN  :                                             */
        /* ***************************************************  */

        for(int t = 1; t <= n_times; t++){ //n_times

            // Check for moving to the next time window (1st window has length 0)
            if (current_node->window_length < 1)
            {
                if (current_node->next != NULL)
                {
                    // Get the next time window
                    current_node = current_node->next;
                    // Check if running in daily mode or time window mode
                    if (current_node->window_length == 1)
                    {
                        daily_mode_on = true;
                    }
                    else
                    {
                        daily_mode_on = false;
                    }
                }
                else
                {
                    /*
                     * No more time windows but loop is still going.
                     * Use initial values for remainder of the loop.
                     */
                    current_node = head_node;
                    current_node->window_length = n_times - t;
                }

                if (dist_phi != current_node->dist_phi)
                {
                    dist_phi_changed = true;

                    // dist_phi
                    dist_phi_low = current_node->getMinDistParam();
                    dist_phi_slope = current_node->getDistParamSlope();
                    dist_phi_intercept = current_node->getDistParamIntercept(t - 1);
                }
                else
                {
                    dist_phi_changed = false;
                }

                // m
                m_slope = current_node->getMSlope();
                m_intercept = current_node->getMIntercept(t - 1);

                // imm_frac
                imm_frac_slope = current_node->getImmFracSlope();
                imm_frac_intercept = current_node->getImmFracIntercept(t - 1);
            }

            //*****************************************
            // Effects of time window data

            if (daily_mode_on)
            {
                params->m = current_node->m;
                params->imm_frac = current_node->imm_frac;
                dist_phi = current_node->dist_phi;
            }
            else
            {
                params->m = m_slope * t + m_intercept;
                params->imm_frac = imm_frac_slope * t + imm_frac_intercept;
                dist_phi = dist_phi_slope * t + dist_phi_intercept;
            }

            // Calculate beta values for each population
            for (int this_pop = 1; this_pop <= n_pop; this_pop++) {
                if (daily_mode_on) {
                    beta_temp = current_node->beta[this_pop-1];
// 
//                     // Avoid calculateBeta if beta was already calculated for this R0
//                     std::unordered_map<double, double>::const_iterator beta_iter = beta_map.find(r0);
//                     if (beta_iter != beta_map.end()) {
//                         beta[this_pop] = beta_iter->second;
//                     }
//                     else {
                        beta[this_pop] = beta_temp; //calculateBeta(r0, params);
                        // beta_map.insert(std::pair<double, double>(r0, beta[this_pop]));
                    // }
                }
                else {
                    // Min/max for R0
                    beta_slope = current_node->getbetaSlope(this_pop-1);
                    beta_intercept = current_node->getbetaIntercept(this_pop-1, t - 1);
                    beta_temp = beta_slope * t + beta_intercept;

                    beta[this_pop] = beta_temp; //calculateBeta(r0, params);
                }
            }
            params->beta = beta;

            // Only deal with prob_move if dist_phi changes
            if (dist_phi_changed)
            {
                for(int k = 1; k <= n_pop; k++){
                    // prob_ColSum[k] = 0.0;

                    for(int j = 1; j <= n_pop; j++){
                        prob_move[k][j] = 0.0;

                        if(k != j){
                            prob_move[k][j] = 1 / exp(dist_mat[k][j] / dist_phi);
                        }
                        // prob_ColSum[j] = prob_ColSum[j] + prob_move[k][j];
                    }
                }

                params->prob_move = prob_move;
            }


            //*****************************************
            // 1 tau leap (internal)
            // printf("up to tau_leap_1step.\n");

            for(int this_pop = 1; this_pop <= n_pop; this_pop++){
                // printf("up to tau_leap_1step.\n");
                tau_leap_1step( n_occur,
                                this_pop,
                                params,
                                AllPops,
                                rand1,
                                n_events);
                update_pops(update_vec,
                            n_occur,
                            this_pop,
                            t,
                            this_seed,
                            params,
                            AllPops,
                            rand1,
                            out_events_birth       +out_events_line,
                            out_events_death       +out_events_line,
                            out_events_exposed     +out_events_line,
                            out_events_infectious  +out_events_line,
                            out_events_recov       +out_events_line);

                out_events_line++;
                AllPops.S_pop[this_pop][2] = update_vec[0];
                AllPops.E_pop[this_pop][2] = update_vec[1];
                AllPops.I_pop[this_pop][2] = update_vec[2];
                AllPops.R_pop[this_pop][2] = update_vec[3];

                // printf("S_pop[%d]: %d\n", this_pop, AllPops.S_pop[this_pop][2]);

                //// MIGRATION:

                // Calculate where to move folks:
                // printf("up to move_pop()\n");
                // Move S
                if(n_occur[8] > 0){
                    move_pops(update_vec_move, n_occur[8], this_pop, params, rand1);

                    for(int k=0;k<n_pop;k++){
                        // printf("update_move[%d]: %d\n", (k+1), update_move[k]);
                        MovePops.S_move[k+1][this_pop] = update_vec_move[k];
                    }
                }

                // Move I
                if(n_occur[9] > 0){
                    move_pops(update_vec_move, n_occur[9], this_pop, params, rand1);

                    for(int k=0;k<n_pop;k++){
                        // printf("update_I_move[%d][%d]: %d\n", this_pop, (k+1), update_move[k]);
                        MovePops.I_move[k+1][this_pop] = update_vec_move[k];
                    }
                }

                // for(k = 0; k < n_pop; k++){
                //   printf("S_move[%d][%d]: %d\n", this_pop, k+1, MovePops.S_move[k+1][this_pop]);
                //   printf("I_move[%d][%d]: %d\n", this_pop, k+1, MovePops.I_move[k+1][this_pop]);
                // }
            }//end j loop (pop)

            //*****************************************
            //*****************************************
            // Migrants can cause exposure (I_asym) or migrants can become exposed (S)

            // printf("up to migrants.\n");
            for(int this_pop = 1; this_pop <= n_pop; this_pop++){
                update_pop_migrants(update_vec_migrants,
                                    this_pop,
                                    params,
                                    AllPops,
                                    MovePops,
                                    rand1);

                AllPops.S_pop[this_pop][2] = update_vec_migrants[0];
                AllPops.E_pop[this_pop][2] = update_vec_migrants[1];
                AllPops.I_pop[this_pop][2] = update_vec_migrants[2];
                AllPops.R_pop[this_pop][2] = update_vec_migrants[3];

                // printf("S_pop[%d]: %d\n", this_pop, AllPops.S_pop[this_pop][2]);
                // printf("E_pop[%d]: %d\n", this_pop, AllPops.E_pop[this_pop][2]);

            }// end j loop (this_pop)


            //*****************************************
            //*****************************************
            // REDUCING LOOPING BY COMBINING STEPS:

            for(int this_pop=1;this_pop<=n_pop;this_pop++){
                // Check for negatives:
                if(AllPops.S_pop[this_pop][2] < 0) AllPops.S_pop[this_pop][2] = 0;
                if(AllPops.E_pop[this_pop][2] < 0) AllPops.E_pop[this_pop][2] = 0;
                if(AllPops.I_pop[this_pop][2] < 0) AllPops.I_pop[this_pop][2] = 0;
                if(AllPops.R_pop[this_pop][2] < 0) AllPops.R_pop[this_pop][2] = 0;

                //Recalculate pop size after all movement:
                pop_N[this_pop] =
                AllPops.S_pop[this_pop][2] + AllPops.E_pop[this_pop][2] +
                AllPops.I_pop[this_pop][2] + AllPops.R_pop[this_pop][2];

                // write one output line.
                int one_or_two;
                if(t==1){
                    one_or_two=1;
                }
                else{
                    one_or_two=2;
                }

            	out_pops_seed[out_pops_line] = this_seed;
            	out_pops_pop[out_pops_line] = this_pop;
            	out_pops_time[out_pops_line] = t;
            	out_pops_S_pop[out_pops_line] = AllPops.S_pop[this_pop][one_or_two];
            	out_pops_E_pop[out_pops_line] = AllPops.E_pop[this_pop][one_or_two];
            	out_pops_I_pop[out_pops_line] = AllPops.I_pop[this_pop][one_or_two];
            	out_pops_R_pop[out_pops_line] = AllPops.R_pop[this_pop][one_or_two];
            	out_pops_line++;

                // Reset movement:
                for(int k=1;k<=n_pop;k++){
                    MovePops.S_move[this_pop][k] = 0;
                    MovePops.I_move[this_pop][k] = 0;
                }

                // Update Pops 1 step;
                AllPops.S_pop[this_pop][1] = AllPops.S_pop[this_pop][2];
                AllPops.E_pop[this_pop][1] = AllPops.E_pop[this_pop][2];
                AllPops.I_pop[this_pop][1] = AllPops.I_pop[this_pop][2];
                AllPops.R_pop[this_pop][1] = AllPops.R_pop[this_pop][2];

                AllPops.S_pop[this_pop][2] = 0;
                AllPops.E_pop[this_pop][2] = 0;
                AllPops.I_pop[this_pop][2] = 0;
                AllPops.R_pop[this_pop][2] = 0;

                // printf("S_pop1[%d]: %d\n", this_pop, AllPops.S_pop[this_pop][1]);
                // printf("E_pop1[%d]: %d\n", this_pop, AllPops.E_pop[this_pop][1]);

            }
            // fclose(fp2);

            // Remove a day from the time window
            current_node->window_length--;

        }//end t loop (time)

    } // end i loop (realz)

    //*****************************************

    /* ***************************************************  */
    /* FREE MEMORY ALLOCATIONS   :                          */
    /* ***************************************************  */

    // Clean up memory used by time windows
    clearTimeWindows(head_node);

    gsl_rng_free(rand1);

    delete[] n_occur;
    delete[] update_vec;
    delete[] update_vec_migrants;
    delete[] update_vec_move;

    free_matrix(dist_mat, 1, n_pop, 1, n_pop);
    free_matrix(prob_move, 1, n_pop, 1, n_pop);
    free_vector(prob_ColSum, 1, n_pop);
    free_vector(pop_N, 1, n_pop);
    free_vector(census_area, 1, n_pop);
    free_dvector(beta, 1, n_pop);

    free_imatrix(S_pop, 1, n_pop, 1, 2);
    free_imatrix(E_pop, 1, n_pop, 1, 2);
    free_imatrix(I_pop, 1, n_pop, 1, 2);
    free_imatrix(R_pop, 1, n_pop, 1, 2);

    free_imatrix(S_move, 1, n_pop, 1, n_pop);
    free_imatrix(I_move, 1, n_pop, 1, n_pop);
    //free_imatrix(output_pops, 1, dim_out2, 1, dim_out1);

    return 0;
}

//********************** Funcs.h **************************

void trans_type_beta(double& beta_scaled, int this_pop, double infect_sum,
                     SEIRParamStruct *Params, PopStruct AllPops, gsl_rng *rand1){

    double noise_temp = 0.0;
    double pop_dens = 0.0;
    double beta_dens = 0.0;

    noise_temp = gsl_ran_gaussian(rand1, Params->stoch_sd);

    switch(Params->trans_type){

    case 1:
        // FREQUENCY-DEPENDENT TRANSMISSION

        beta_scaled = fabs(Params->beta[this_pop] / Params->pop_N[this_pop] *
          (1 + (noise_temp) / pow(infect_sum, 0.5)));

        break;

    case 2:
        // DENSITY-DEPENDENT TRANSMISSION
        //// MONOD EQUATION (to determine rel'n btw beta and raw pop_dens)
        pop_dens = (Params->pop_N[this_pop] / Params->census_area[this_pop]);
        beta_dens = Params->beta[this_pop] * pop_dens / (Params->dd_trans_monod_k + pop_dens);

        beta_scaled = fabs(beta_dens / Params->pop_N[this_pop] *
          (1 + (noise_temp) /  pow(infect_sum, 0.5)));

        break;

    default:
        break;

    }// end switch

}

void tau_leap_1step(int *n_occur, int this_pop, SEIRParamStruct *Params,
                    PopStruct AllPops, gsl_rng *rand1, int n_events){

    double infect_sum  = 0.0;
    double beta_scaled;

    double *event_prob = new double[n_events]; // size = number of events

    int i;

    // sum of infectious:
    infect_sum = AllPops.I_pop[this_pop][1];

    if(infect_sum > 0.0){ // As long as there are some infectious hosts...

        trans_type_beta(beta_scaled,
                        this_pop,
                        infect_sum,
                        Params,
                        AllPops,
                        rand1);

    }else{
        beta_scaled = 0.0;
    }

    // printf("beta_scaled = %f\n", beta_scaled);


    //*****************************************
    // Calculate Event Probabilities:

    // Birth (all classes):
    event_prob[BIRTH] =
        Params->birth * Params->pop_N[this_pop];
    // Transmission from I:
    event_prob[TRANS_FROM_I] =
        beta_scaled * AllPops.S_pop[this_pop][1] * AllPops.I_pop[this_pop][1];
    // Latency:
    event_prob[LATENCY] =
        Params->incubate * AllPops.E_pop[this_pop][1];
    // Recovery:
    event_prob[RECOVERY] =
        Params->recov * AllPops.I_pop[this_pop][1];

    //// DEATH: (death rate = birth rate)
    // Death: S
    event_prob[DEATH_S] = Params->birth * AllPops.S_pop[this_pop][1];
    // Death: E
    event_prob[DEATH_E] = Params->birth * AllPops.E_pop[this_pop][1];
    // Death: I
    event_prob[DEATH_I] = Params->birth * AllPops.I_pop[this_pop][1];
    // Death: R
    event_prob[DEATH_R] = Params->birth * AllPops.R_pop[this_pop][1];

    //// MIGRATION:
    // Migrate: S
    event_prob[S_MIGRATE_S] = Params->m * AllPops.S_pop[this_pop][1];
    // Migrate: I
    event_prob[MIGRATE_I] = Params->m * AllPops.I_pop[this_pop][1];

    // How many events of each type will occur over time period tau?

    for(i=0;i<n_events;i++){
        // Draw Poisson random variate:
        n_occur[i] = gsl_ran_poisson(rand1, event_prob[i]*Params->tau);
    }

    delete[] event_prob;
}

/* ***************************************************  */
/* ***************************************************  */
/* ***************************************************  */

void update_pops
    (int *update_vec, int* n_occur, int this_pop,
     int this_time, int this_seed,
     SEIRParamStruct *Params, PopStruct AllPops, gsl_rng *rand1,
     int *out_events_birth,
     int *out_events_death,
     int *out_events_exposed,
     int *out_events_infectious,
     int *out_events_recov
    ){

    int total_death;

    // # 1 Suscept
    update_vec[0] =
        AllPops.S_pop[this_pop][1] +
        n_occur[BIRTH] -
        n_occur[TRANS_FROM_I] -
        n_occur[DEATH_S];

    if(update_vec[0] < 0){
        // FIRST, ASSUME FEWER DEATHS
        n_occur[DEATH_S] = n_occur[DEATH_S] - update_vec[0]; // adds a negative
        if(n_occur[DEATH_S] < 0){
            n_occur[TRANS_FROM_I] = n_occur[TRANS_FROM_I] + n_occur[DEATH_S];
            n_occur[DEATH_S] = 0;
        }
        update_vec[0] = 0;
    }

    // # 2 Exposed
    update_vec[1] =
        AllPops.E_pop[this_pop][1] +
        n_occur[TRANS_FROM_I] -
        n_occur[LATENCY] -
        n_occur[DEATH_E];

    if(update_vec[1] < 0){
        // FIRST, ASSUME FEWER DEATHS
        n_occur[DEATH_E] = n_occur[DEATH_E] + update_vec[1];
        if(n_occur[DEATH_E] < 0){
            n_occur[LATENCY] = n_occur[LATENCY] + n_occur[DEATH_E];
            n_occur[DEATH_E] = 0;
        }
        update_vec[1] = 0;
    }

    // # 3 Infectious,
    update_vec[2] =
        AllPops.I_pop[this_pop][1] +
        n_occur[LATENCY] -
        n_occur[RECOVERY] -
        n_occur[DEATH_I];

    if(update_vec[2] < 0){
        // FIRST, ASSUME FEWER DEATHS
        n_occur[DEATH_I] = n_occur[DEATH_I] + update_vec[2];
        if(n_occur[DEATH_I] < 0){
            n_occur[RECOVERY] = n_occur[RECOVERY] + n_occur[DEATH_I];
            n_occur[DEATH_I] = 0;
        }
        update_vec[2] = 0;
    }

    // # 4 Recovered,
    update_vec[3] =
        AllPops.R_pop[this_pop][1] +
        n_occur[RECOVERY] -
        n_occur[DEATH_R];

    if(update_vec[3] < 0){
        n_occur[DEATH_R] = n_occur[DEATH_R] + update_vec[3];
        update_vec[3] = 0;
    }


    total_death =
        n_occur[DEATH_S] + n_occur[DEATH_E] +
        n_occur[DEATH_I] + n_occur[DEATH_R];

    // SAVE NEW EVENTS
    *out_events_birth = n_occur[BIRTH];
    *out_events_death = total_death;
    *out_events_exposed = n_occur[TRANS_FROM_I];
    *out_events_infectious = n_occur[LATENCY];
    *out_events_recov = n_occur[RECOVERY];
}

/* ***************************************************  */
/* ***************************************************  */
/* ***************************************************  */

void move_pops(int *update_vec_move, int n_occur, int this_pop,
               SEIRParamStruct *Params, gsl_rng *rand1){

    int n_pop = Params->n_pop;
    int i;
    int this_total;
    unsigned int *temp_vec = new unsigned int[n_pop];

    //temp variables
    double *temp_prob = new double[(n_pop)];

    for(i=0;i<n_pop;i++){
        temp_prob[i] = Params->prob_move[(i+1)][this_pop];
        // temp_prob[i] = Params->dist_mat[(i+1)][this_pop];

        //printf("temp_prob[%d][%d]: %f\n", i+1, this_pop, temp_prob[i]);
    }

    // How many will travel to each population:
    //*****************************************

    //:
    this_total = n_occur;
    // printf("this_total: %d\n", this_total);
    // Choose which pops...

    // printf("Up to multinom\n");
    gsl_ran_multinomial(rand1, n_pop, this_total, temp_prob, temp_vec);

    //*****************************************
    for(i=0;i<n_pop;i++){
        update_vec_move[i] = temp_vec[i];
    }

    delete[] temp_vec;
    delete[] temp_prob;
}

/* ***************************************************  */
/* ***************************************************  */
/* ***************************************************  */

void update_pop_migrants(int *update_vec_migrants, int this_pop,
                         SEIRParamStruct *Params, PopStruct AllPops,
                         MoveMatStruct MovePops, gsl_rng *rand1){

    int i, other_pop;
    int n_pop = Params->n_pop;
    int S_move_sum = 0;
    int S_remain = 0;
    int n_infect_visitors;
    double n_visitors, infect_frac;

    double event_prob = 0.0;
    int n_event = 0;

    double infect_sum  = 0.0;
    double beta_scaled;

    update_vec_migrants[0] = AllPops.S_pop[this_pop][2];
    update_vec_migrants[1] = AllPops.E_pop[this_pop][2];
    update_vec_migrants[2] = AllPops.I_pop[this_pop][2];
    update_vec_migrants[3] = AllPops.R_pop[this_pop][2]; // No Change

    // How many moved out of the pop?
    // These should not be able to get exposed by I_asym imports
    S_move_sum = 0;
    for(i=1;i<=n_pop;i++){
        other_pop = i;
        S_move_sum = S_move_sum + MovePops.S_move[other_pop][this_pop];
    }
    S_remain = AllPops.S_pop[this_pop][2] - S_move_sum;

    for(i=1;i<=n_pop;i++){

        other_pop = i;

        if(other_pop != this_pop){

            /* ******************** */
            // Suscept move TO other pop and get exposed...
            /* ******************** */

            //// sum of infectious:
            //// not visiting hospital...
            infect_sum = AllPops.I_pop[other_pop][2];

            if(infect_sum > 0.0){

                trans_type_beta(beta_scaled,
                                other_pop, // NEEDS TO BE *OTHER* POP
                                infect_sum,
                                Params,
                                AllPops,
                                rand1);

            }else{
                beta_scaled = 0.0;
            }

            // The sucept that moved get exposed
            event_prob =
                beta_scaled * MovePops.S_move[other_pop][this_pop] * (AllPops.I_pop[other_pop][2]);

            n_event = gsl_ran_poisson(rand1, event_prob*Params->tau);

            // update the S and E:
            update_vec_migrants[0] = update_vec_migrants[0] - n_event;
            update_vec_migrants[1] = update_vec_migrants[1] + n_event;


            /* ******************** */
            // I move FROM other pop and CAUSE exposure in focal pop...
            /* ******************** */
            event_prob = 0;
            n_event = 0;

            //// sum of infectious from OTHER pop:
            infect_sum = MovePops.I_move[this_pop][other_pop];

            if(infect_sum > 0.0){

                trans_type_beta(beta_scaled,
                                this_pop, // NEEDS TO BE *THIS* POP
                                infect_sum,
                                Params,
                                AllPops,
                                rand1);

            }else{
                beta_scaled = 0.0;
            }

            // The remaining sucept that moved get exposed
            event_prob = beta_scaled * S_remain * infect_sum;

            n_event = gsl_ran_poisson(rand1, event_prob*Params->tau);

            // update the S and E:
            update_vec_migrants[0] = update_vec_migrants[0] - n_event;
            update_vec_migrants[1] = update_vec_migrants[1] + n_event;

        }// end other_pop != this_pop

    }//end i n_pop


    /* ******************** */
    // DO "TOURISTS" CAUSE INFECTION?
    /* ******************** */

    n_visitors = Params->imm_frac * Params->pop_N[this_pop];

    infect_frac =
        AllPops.I_pop[this_pop][2] / Params->pop_N[this_pop];

    n_infect_visitors = gsl_ran_poisson(rand1, infect_frac * n_visitors);

    if(n_infect_visitors > 0){

        infect_sum = n_infect_visitors;

        trans_type_beta(beta_scaled,
                        this_pop,
                        infect_sum,
                        Params,
                        AllPops,
                        rand1);

        event_prob = beta_scaled * S_remain * infect_sum;
        n_event = gsl_ran_poisson(rand1, event_prob*Params->tau);

        // update the S and E:
        update_vec_migrants[0] = update_vec_migrants[0] - n_event;
        update_vec_migrants[1] = update_vec_migrants[1] + n_event;
    }


}


/*
 * Function: beta_calc
 *
 * Used by GSL root solver in calculateBeta.
 */
// double beta_calc (double beta, void *params)
// {
//     double result;
//     struct beta_calc_struct *p = (struct beta_calc_struct *)params;
// 
//     result = beta / (p->Params->birth + p->Params->recov) - p->r0;
// 
//     return result;
// }


/*
 * Function: calculateBeta
 *
 * Calculates beta based on R0 value.
 */
// double calculateBeta(float r0, SEIRParamStruct *Params)
// {
//     int status;
//     int iteration = 0;
//     int max_iter = MAX_BRENT_ITERATIONS;
// 
//     double root = 0;
//     double root_low = BETA_LOWER_LIMIT;
//     double root_high = BETA_UPPER_LIMIT;
// 
//     const gsl_root_fsolver_type *root_fsolver_type;
//     root_fsolver_type = gsl_root_fsolver_brent;
// 
//     gsl_root_fsolver *s;
//     s = gsl_root_fsolver_alloc(root_fsolver_type);
// 
//     struct beta_calc_struct params = {r0, Params};
// 
//     gsl_function F;
//     F.function = &beta_calc;
//     F.params = &params;
// 
//     gsl_root_fsolver_set (s, &F, root_low, root_high);
// 
//     // For debugging output in console, set the constant to 1 at the top of this file.
//     if (OUTPUT_DEBUG_ON == 1)
//     {
//         printf ("using %s method\n", gsl_root_fsolver_name (s));
// 
//         printf ("%5s [%9s, %9s] %9s %9s\n",
//                 "iter", "lower", "upper", "root",
//                 "err(est)");
//     }
// 
//     /*
//      * Use GSL root solver to find the root. Number of iterations to try and
//      * the upper and lower bounds for beta are set as constants at the top
//      * of this file.
//      */
//     do
//     {
//         iteration++;
//         status = gsl_root_fsolver_iterate (s);
//         root = gsl_root_fsolver_root(s);
//         root_low = gsl_root_fsolver_x_lower(s);
//         root_high = gsl_root_fsolver_x_upper(s);
//         status = gsl_root_test_interval(root_low, root_high, 0, 0.001);
// 
//         // For debugging output in console, set the constant to 1 at the top of this file.
//         if (OUTPUT_DEBUG_ON == 1)
//         {
//             if (status == GSL_SUCCESS) printf ("Converged:\n");
// 
//             printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
//                     iteration, root_low, root_high,
//                     root ,
//                     root_high - root_low);
//         }
//     }
//     while (status == GSL_CONTINUE && iteration < max_iter);
// 
//     // For debugging output in console, set the constant to 1 at the top of this file.
//     if (OUTPUT_DEBUG_ON == 1)
//     {
//         printf("\tDEBUG: beta = %.4f for R0 = %.1f\n\n", root, r0);
//     }
// 
//     gsl_root_fsolver_free(s);
// 
//     return root;
// }
