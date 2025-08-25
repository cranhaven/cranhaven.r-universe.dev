functions {

#include /include/linear_interpolation.stan

}
data {
  // Number of replicate
  int<lower=0> n_rep ;
  
  // Time points
  int<lower=0> lentp;
  vector[lentp] tp ;
  
  // Exposure profiles
  int<lower=0> n_exp ;
  matrix[lentp, n_exp] Cexp ;
  
  // Internal concentraion
  // Growth
  int<lower=0> n_out ;
  array[lentp, n_out, n_rep] real CGobs;

  // Metabolites
  int<lower=0> n_met ;
  array[lentp, n_met, n_rep] real Cmet ;

  real<lower=0> gmaxsup ;
  
  // TK accumulation / depuration
  int<lower=0> rankacc ;
  real<lower=0> tacc ;
  real<lower=0> C0 ;
  
  real elim_rate ;
  
  real unifMax ;
  int<lower=0> len_vt;
  vector[len_vt] vt ;
}
parameters {
  vector[n_exp] log10ku ; // uptake
  vector[n_out] log10ke ;
  vector[n_met] log10km ;
  vector[n_met] log10kem ;
  
  array[n_out] real<lower=0> sigmaCGpred ; 
  vector<lower=0>[n_met] sigmaCmetpred ;
  
  array[n_out - 1] real<lower=0> gmax ;
  array[n_out -1] real<lower=0> G0;
  
}
transformed parameters{
  // PRIORS
  vector<lower=0>[n_exp] ku ;
  vector<lower=0>[n_out] ke ;
  vector<lower=0>[n_met] km ;
  vector<lower=0>[n_met] kem ;
  vector[lentp] U ;
  real M ;
  real E ;
  vector[lentp] R ;
  vector[n_met] D ;
  // little hack merging Cpred and Gpred
  matrix[lentp,n_out] CGpred ; 
  matrix[lentp,n_met] Cmetpred ;

  for(i in 1:n_exp){
    ku[i] = 10 ^ log10ku[i] ;
  }
  for(i in 1:n_out){
    if(!is_inf(elim_rate)){
       ke[i] = 0 ;
    } else{
      ke[i] = 10 ^ log10ke[i] ;
    }
  }
  for(i in 1:n_met){
    km[i] = 10 ^ log10km[i] ;
    kem[i] = 10 ^ log10kem[i] ;
  }
  if(n_met == 0){
    M = 0 ;
  } else{
     M = sum(km) ;
  }
  if(!is_inf(elim_rate)){
    E = elim_rate ;
  } else{
    E = sum(ke) ;
  }
  for(t in 1:lentp){
    // real operator*(row_vector x, vector y)
    U[t] =  Cexp[t,1:n_exp] * ku ;
    R[t] =  U[t] / (E + M) ;
  }
  for(i in 1:n_met){
      D[i] =  kem[i] - (E + M) ;
  }
  // ACCUMULATION PHASE (0 =< t =< tacc)
  for(t in 1:rankacc){
    // Parent compound
    CGpred[t, 1] = (C0 - R[t]) * exp(-(E + M) * tp[t]) + R[t] ;
    // Metabolites
    if(n_met > 0){
      for(i in 1:n_met){
         Cmetpred[t,i] = km[i] * (
           (C0-R[t])/ D[i] * (exp(-(E+ M)*tp[t])-exp(-kem[i] * tp[t])) + R[t] / kem[i] * (1 - exp(-(kem[i] * tp[t]))) 
         ) ;
      }
    }
  }
  //DEPURATION PHASE (t > tacc)
  for(t in (rankacc+1):lentp){
    // Parent compound
    CGpred[t, 1] = (C0 - R[t] * (1 - exp((E + M)*tacc))) * exp(-(E + M) * tp[t]) ;
    // Metabolites
    if(n_met > 0){
      for(i in 1:n_met){
      Cmetpred[t,i] = km[i] * (
        (C0-R[t]) / D[i] * (exp(-(E + M) * tp[t]) - exp(-kem[i] * tp[t])) + 
        R[t] / kem[i] * (exp(-kem[i] * (tp[t]-tacc)) - exp(-kem[i] * tp[t])) +
        R[t] / D[i] * (exp(-(E+M)*(tp[t]-tacc)) - exp(-kem[i] * (tp[t] - tacc)))
        ) ;
      }
    }
  }
  // GROWTH
  if(n_out == 2){
    for(t in 1:lentp){
      CGpred[t, 2] = (G0[1] - gmax[1]) * exp(-ke[2] * tp[t]) + gmax[1] ;
    }
  }
}
model {
  // PRIORS
  target += uniform_lpdf(log10ku | -5, 5) ;
  target += uniform_lpdf(log10ke | -5, 5) ;
  target += uniform_lpdf(log10km | -5, 5) ;
  target += uniform_lpdf(log10kem | -5, 5) ;
  target += uniform_lpdf(sigmaCGpred[1] | 0, unifMax) ;
  target += uniform_lpdf(sigmaCmetpred | 0, unifMax) ;
  if(n_out == 2){
     target +=  uniform_lpdf(sigmaCGpred[2] | 0, unifMax) ;
     target +=  uniform_lpdf(gmax[1] | gmaxsup/6, gmaxsup) ;
     target +=  uniform_lpdf(G0[1] | 0, gmaxsup) ;
  }
  
  for(rep in 1:n_rep){
    // ACCUMULATION PHASE (0 =< t =< tacc) #
    for(t in 1:rankacc){
      // Parent compound
      if(!is_inf(CGobs[t,1,rep])){
        target += normal_lpdf(CGobs[t,1,rep] | CGpred[t, 1], sigmaCGpred[1]) ;
      }
      // Metabolites
      for(i in 1:n_met){
        if(!is_inf(Cmet[t,i,rep])){
          target += normal_lpdf(Cmet[t,i, rep] | Cmetpred[t,i], sigmaCmetpred[i]) ;
        }
      }
    }
    //DEPURATION PHASE (t > tacc)
    for(t in (rankacc+1):lentp){
      // Parent compound
      if(!is_inf(CGobs[t,1,rep])){
        target += normal_lpdf(CGobs[t,1,rep] | CGpred[t, 1], sigmaCGpred[1]) ;
      }
      // Metabolites
      for(i in 1:n_met){
        if(!is_inf(Cmet[t,i,rep])){
          target += normal_lpdf(Cmet[t,i,rep] | Cmetpred[t,i], sigmaCmetpred[i]) ;
        }
      }
    }
    // GROWTH
    if(n_out == 2){
      for(t in 1:lentp){
        if(!is_inf(CGobs[t,2,rep])){
          target += normal_lpdf(CGobs[t,2,rep] | CGpred[t,2], sigmaCGpred[2]) ;
        }
      }
    }
  }
}

generated quantities {
  
  array[lentp,n_out] real CGobs_out; 
  array[lentp,n_met] real Cmet_out;

  array[len_vt, n_exp] real Cexp_interpol;
  //vector[lentp] tp_y ;
  
  for(t in 1:lentp){
    // Parent compound
    CGobs_out[t,1] = normal_rng(CGpred[t,1], sigmaCGpred[1]) ;
    // Metabolites
    for(i in 1:n_met){
      Cmet_out[t,i] = normal_rng(Cmetpred[t,i], sigmaCmetpred[i]) ;
    }
  }
  if(n_out == 2){
    for(t in 1:lentp){
      CGobs_out[t,2] = normal_rng(CGpred[t,2], sigmaCGpred[2]) ;
    }
  }
  for(i in 1:n_exp){
    //tp_y = Cexp[1:lentp, i] ;
    for(t in 1:len_vt){
      Cexp_interpol[t,i] = interpolate(vt[t], tp, Cexp[1:lentp, i]) ;
    }
  }
}

