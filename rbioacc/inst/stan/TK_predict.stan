functions {

#include /include/linear_interpolation.stan

}
data {
  // Time points
  int<lower=0> lentp;
  vector[lentp] tp ;
  
  int<lower=0> len_vt;
  vector[len_vt] vt ;
  // Exposure profiles
  int<lower=0> n_exp ;
  matrix[len_vt, n_exp] Cexp ;

  // TK accumulation / depuration
  int<lower=0> rankacc ;
  real<lower=0> tacc ;
  real<lower=0> C0 ;

  real elim_rate ;

  // PARAMETERS
  int<lower=0> n_out ;
  int<lower=0> n_met ;
  int N_samples;
  matrix[N_samples, n_exp] log10ku ; // uptake
  matrix[N_samples, n_out] log10ke ;
  matrix[N_samples, n_met] log10km ;
  matrix[N_samples, n_met] log10kem ;

  array[N_samples] real M ;
  array[N_samples] real E ;

  array[N_samples, n_out] real<lower=0> sigmaCGpred ;
  matrix<lower=0>[N_samples, n_met] sigmaCmetpred ;

  array[N_samples, n_out - 1] real<lower=0> gmax ;
  array[N_samples, n_out -1] real<lower=0> G0;

}
parameters {

}
model {

}
generated quantities {

  array[lentp,n_out] real CGobs_out;
  array[lentp,n_met] real Cmet_out;

  matrix[lentp, n_exp] Cexp_interpol;

  // PRIORS
  vector<lower=0>[n_exp] ku ;
  vector<lower=0>[n_out] ke ;
  vector<lower=0>[n_met] km ;
  vector<lower=0>[n_met] kem ;

  vector[lentp] U ;
  vector[lentp] R ;
  vector[n_met] D ;
  // little hack merging Cpred and Gpred
  matrix[lentp,n_out] CGpred ;
  matrix[lentp,n_met] Cmetpred ;

  // EXPOSURE INTERPOLATION
  for(i in 1:n_exp){
    for(t in 1:lentp){
      Cexp_interpol[t,i] = interpolate(tp[t], vt, Cexp[1:len_vt, i]) ;
    }
  }

  // SAMPLING DATA
  for(s in 1:N_samples){
    for(i in 1:n_exp){
      ku[i] = 10 ^ log10ku[s,i] ;
    }
    for(i in 1:n_out){
      if(!is_inf(elim_rate)){
         ke[i] = 0 ;
      } else{
        ke[i] = 10 ^ log10ke[s,i] ;
      }
    }
    for(i in 1:n_met){
      km[i] = 10 ^ log10km[s,i] ;
      kem[i] = 10 ^ log10kem[s,i] ;
    }
    for(t in 1:lentp){
      // real operator*(row_vector x, vector y)
      U[t] =  Cexp_interpol[t,1:n_exp] * ku ;
      R[t] =  U[t] / (E[s] + M[s]) ;
    }
    for(i in 1:n_met){
        D[i] =  kem[i] - (E[s] + M[s]) ;
    }
    // ACCUMULATION PHASE (0 =< t =< tacc)
    for(t in 1:rankacc){
      // Parent compound
      CGpred[t, 1] = (C0 - R[t]) * exp(-(E[s] + M[s]) * tp[t]) + R[t] ;
      // Metabolites
      if(n_met > 0){
        for(i in 1:n_met){
           Cmetpred[t,i] = km[i] * (
             (C0-R[t])/ D[i] * (exp(-(E[s]+ M[s])*tp[t])-exp(-kem[i] * tp[t])) + R[t] / kem[i] * (1 - exp(-(kem[i] * tp[t])))
           ) ;
        }
      }
    }
    //DEPURATION PHASE (t > tacc)
    for(t in (rankacc+1):lentp){
      // Parent compound
      CGpred[t, 1] = (C0 - R[t] * (1 - exp((E[s] + M[s])*tacc))) * exp(-(E[s] + M[s]) * tp[t]) ;
      // Metabolites
      if(n_met > 0){
        for(i in 1:n_met){
        Cmetpred[t,i] = km[i] * (
          (C0-R[t]) / D[i] * (exp(-(E[s] + M[s]) * tp[t]) - exp(-kem[i] * tp[t])) +
          R[t] / kem[i] * (exp(-kem[i] * (tp[t]-tacc)) - exp(-kem[i] * tp[t])) +
          R[t] / D[i] * (exp(-(E[s]+M[s])*(tp[t]-tacc)) - exp(-kem[i] * (tp[t] - tacc)))
          ) ;
        }
      }
    }
    // GROWTH
    if(n_out == 2){
      for(t in 1:lentp){
        CGpred[t, 2] = (G0[s,1] - gmax[s,1]) * exp(-ke[2] * tp[t]) + gmax[s,1] ;
      }
    }
    ////////////////////////////////////////////////////////////////////////////
    // PREDICTION
    for(t in 1:lentp){
      // Parent compound
      CGobs_out[t,1] = normal_rng(CGpred[t,1], sigmaCGpred[s,1]) ;
      // Metabolites
      for(i in 1:n_met){
        Cmet_out[t,i] = normal_rng(Cmetpred[t,i], sigmaCmetpred[s,i]) ;
      }
    }
    if(n_out == 2){
      for(t in 1:lentp){
        CGobs_out[t,2] = normal_rng(CGpred[t,2], sigmaCGpred[s,2]) ;
      }
    }
  }
}

