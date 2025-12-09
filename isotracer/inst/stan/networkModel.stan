/// * Description

/*
This file contains code that can run a network model using one of several
approaches to solve the system of ODEs governing the system:

- using matrix exponentials
- using a forward Euler scheme with constant step size

Historically, each approach was implemented separately first, but there was a
lot of duplicated code so merging them into a single Stan model with a switch
controlling which approach to use made sense.
*/

/// * Functions

functions {

  /// * Matrix exponential approach
  
  /// ** buildTransferMatrix()

  /// *** Doc
  
  /*
  Parameters:
  
  nComps Integer, number of compartments in the network. Used to define the
      size of the transfer matrix, which is a square matrix nComps x nComps.

  nSteady Integer, number of steady-state compartments.

  steadyIndices Array of integers, indices of the steady-state compartments.

  nUpsilons Integer, number of upsilon parameters to add to the transfer
      matrix. This is the number of non-zero uptake rate parameters. An uptake
      rate parameters is non-zero if the two corresponding compartments are
      connected.

  mappingU Array of integers, with 3 columns and nUpsilons rows. The k^th row
      defines the mapping between the location of the k^th upsilon parameter in
      the transfer matrix at [mappingU[k, 2], mappingU[k, 1]] and the
      parameter values in params[mappingU[k, 3]]. It is used to fill in the
      transfer matrix with the upsilon rates values.

  nLambdas Integer, number of lambda parameters to add to the transfer
      matrix.

  mappingL Array of integers, with 2 columns and nLambdas rows. The k^th row
      defines the mapping between the location of the k^th lambda parameter in
      the loss rate vector at [mappingL[k, 1]] and the parameter values in
      params[mappingL[k, 2]].

  params Array of real numbers, of length the number of model parameters. It
      contains the values of the parameters currently sampled by the MCMC. They
      are actually transformed from the raw parameters defined in the
      parameters{} block, so they are already on the "real-world" scale and
      ready to use for network calculations.

  Returned value:

  A nComps by nComps transfer matrix A that can be used to project a network
  state from (t) to (t+dt). It is part of the equation: dx/dt = A x(t).
  */

  /// *** Code
  
  matrix buildTransferMatrix(int nComps, int nSteady, array[] int steadyIndices,
                             int nUpsilons, array[,] int mappingU,
                             int nLambdas, array[,] int mappingL, array[] real params) {
    matrix[nComps, nComps] transfer = rep_matrix(0, nComps, nComps);
    vector[nComps] lossRates = rep_vector(0, nComps);
    for (k in 1:nUpsilons) {
      transfer[mappingU[k, 2], mappingU[k, 1]] = params[mappingU[k, 3]];
      lossRates[mappingU[k, 1]] += params[mappingU[k, 3]];
    }
    for (k in 1:nLambdas) {
      lossRates[mappingL[k, 1]] += params[mappingL[k, 2]];
    }
    for (k in 1:nComps) {
      transfer[k,k] -= lossRates[k];
    }
    // Apply steady states
    if (nSteady > 0) {
      for (k in 1:nSteady) {
        transfer[steadyIndices[k],steadyIndices[k]] = 0;
      }
    }
    return(transfer);
  }

  /// ** buildTransferMatrixDecay()

  /*
  Parameters:

  nComps Integer, number of compartments in the network. Used to define the
      size of the transfer matrix, which is a square matrix nComps x nComps.

  transferRef Matrix of size nComps x nComps. This is the transfer matrix
      to which a decay will be applied on the diagonal.

  lambda_decay Real, the decay constant to be applied on the diagonal of the
      transfer matrix.

  Returned value:

  A nComps by nComps transfer matrix similar to transferRef but to which a
  decay has been applied on the diagonal.
  */

  matrix buildTransferMatrixDecay(int nComps, matrix transferRef,
                                  real lambda_decay) {
    matrix[nComps, nComps] transfer = transferRef;
    for (k in 1:nComps) {
      transfer[k,k] -= lambda_decay;
    }
    return(transfer);
  }

  /// * Forward Euler approach

  /// ** buildTransitionMatrix()

  /// *** Code
  
  /*
  Parameters:
  
  nComps Integer, number of compartments in the network. Used to define the
      size of the transition matrix, which is a square matrix nComps x nComps.

  nUpsilons Integer, number of upsilon parameters to add to the transition
      matrix. This is the number of non-zero uptake rate parameters. An uptake
      rate parameters is non-zero if the two corresponding compartments are
      connected.

  mappingU Array of integers, with 3 columns and nUpsilons rows. The k^th row
      defines the mapping between the location of the k^th upsilon parameter in
      the transition matrix at [mappingU[k, 2], mappingU[k, 1]] and the
      parameter values in params[mappingU[k, 3]]. It is used to fill in the
      transition matrix with the upsilon rates values.

  nLambdas Integer, number of lambda parameters to add to the transition
      matrix.

  mappingL Array of integers, with 2 columns and nLambdas rows. The k^th row
      defines the mapping between the location of the k^th lambda parameter in
      the loss rate vector at [mappingL[k, 1]] and the parameter values in
      params[mappingL[k, 2]].

  dt Real, the time step to use to build the transition matrix.
      
  params Array of real numbers, of length the number of model parameters. It
      contains the values of the parameters currently sampled by the MCMC. They
      are actually transformed from the raw parameters defined in the
      parameters{} block, so they are already on the "real-world" scale and
      ready to use for network calculations.

  Returned value:

  A nComps x nComps transition matrix that can be used to project a network
  state from (t) to (t+dt).
  */

  /// *** Code
  
  matrix buildTransitionMatrix(int nComps,
                               int nUpsilons, array[,] int mappingU,
                               int nLambdas, array[,] int mappingL, 
                               real dt, array[] real params) {
    matrix[nComps, nComps] transition = rep_matrix(0, nComps, nComps);
    vector[nComps] lossRates = rep_vector(0, nComps);
    for (k in 1:nUpsilons) {
      transition[mappingU[k, 2], mappingU[k, 1]] = params[mappingU[k, 3]];
      lossRates[mappingU[k, 1]] += params[mappingU[k, 3]];
    }
    for (k in 1:nLambdas) {
      lossRates[mappingL[k, 1]] += params[mappingL[k, 2]];
    }
    transition *= dt;
    lossRates *= dt;
    for (k in 1:nComps) {
      transition[k,k] += 1 - lossRates[k];
    }
    return(transition);
  }

  /// ** buildTransitionMatrixDecay()

  /*
  Parameters:

  nComps Integer, number of compartments in the network. Used to define the
      size of the transition matrix, which is a square matrix nComps x nComps.

  transitionRef Matrix of size nComps x nComps. This is the transition matrix
      to which a decay will be applied on the diagonal.

  lambda_decay Real, the decay constant to be applied on the diagonal of the
      transition matrix.

  dt Real, the time step applied to lambda_decay before applying it to the
      diagonal of the transition matrix.

  Returned value:

  A nComps x nComps transition matrix similar to transitionRef but to which a
  decay has been applied on the diagonal.
  */

  matrix buildTransitionMatrixDecay(int nComps,
                                    matrix transitionRef,
                                    real lambda_decay,
                                    real dt) {
    matrix[nComps, nComps] transition = transitionRef;
    for (k in 1:nComps) {
      transition[k,k] -= lambda_decay * dt;
    }
    return(transition);
  }

  /// * Common to both solvers (matrix exp. and forward Euler)

  /// ** buildSizePredictions()

  /*
  Parameters:

  nObs Integer, the number of observations for the current group.

  currentGroup Integer, the indice corresponding to the current group.

  maxNobs Integer, the maximum number of observations across all groups in the
      network model.

  unmarked Array of vectors, of length maxNtimesteps+1. Each vector is of
      length nComps and contains the quantities of unmarked material for the
      network compartment.

  marked Same as unmarked, but for marked material.

  indices Array of integer, defined by
      sizesObsIndices[maxNsizesObs,3,nGroups][,,g]. It has three columns and
      maxNsizesObs rows. For each observation (k^th row), indices[k,1] is the
      compartment index and indices[k,2] is the timestep index.
  
  splitPresent Boolean, is there any split compartment?

  splitComps Array of booleans, of length nComps. Indicates which compartments
      are split compartments.

  initRefr Array of reals, with two columns and nComps rows. For each
      compartment, it contains the initial quantity of unmarked (column 1) and
      marked (column 2) material in the refractory sub-compartment.
      
  Returned value:

  An array of reals of length maxNobs, containing the predicted sizes for all
  compartments and all timesteps.
  
  */
  
  array[] real buildSizePredictions(int nObs, int currentGroup, int maxNobs,
                              array[] vector unmarked, array[] vector marked,
                              array[,] int indices, int splitPresent,
                              array[] int splitComps, array[,] real initRefr) {
    array[maxNobs] real pred = rep_array(0.0, maxNobs); 
    real unmarkedQ;
    real markedQ;
    if (splitPresent > 0) {
      // Split compartments
      for (k in 1:nObs) {
        unmarkedQ = unmarked[indices[k,2]][indices[k,1]];
        markedQ = marked[indices[k,2]][indices[k,1]];
        if (splitComps[indices[k,1]] > 0) {
          unmarkedQ = unmarkedQ + initRefr[indices[k,1], 1];
          markedQ = markedQ + initRefr[indices[k,1], 2];
        }
        pred[k] = unmarkedQ + markedQ;
      }
      return(pred);
    } else {
      // No split compartments
      for (k in 1:nObs) {
        unmarkedQ = unmarked[indices[k,2]][indices[k,1]];
        markedQ = marked[indices[k,2]][indices[k,1]];
        pred[k] = unmarkedQ + markedQ;
      }
      return(pred);
    }
  }

  /// ** buildPropPredictions()

  /*
  Parameters:

  nObs Integer, the number of observations for the current group.

  currentGroup Integer, the indice corresponding to the current group.

  maxNobs Integer, the maximum number of observations across all groups in the
      network model.

  unmarked Array of vectors, of length maxNtimesteps+1. Each vector is of
      length nComps and contains the quantities of unmarked material for the
      network compartment.

  marked Same as unmarked, but for marked material.

  indices Array of integer, defined by
      sizesObsIndices[maxNsizesObs,3,nGroups][,,g]. It has three columns and
      maxNsizesObs rows. For each observation (k^th row), indices[k,1] is the
      compartment index and indices[k,2] is the timestep index.
  
  splitPresent Boolean, is there any split compartment?

  splitComps Array of booleans, of length nComps. Indicates which compartments
      are split compartments.

  initRefr Array of reals, with two columns and nComps rows. For each
      compartment, it contains the initial quantity of unmarked (column 1) and
      marked (column 2) material in the refractory sub-compartment.
      
  Returned value:

  An array of reals of length maxNobs, containing the predicted proportions for
  all compartments and all timesteps.
  
  */
  
  array[] real buildPropPredictions(int nObs, int currentGroup, int maxNobs,
                              array[] vector unmarked, array[] vector marked,
                              array[,] int indices, int splitPresent,
                              array[] int splitComps, array[,] real initRefr) {
    array[maxNobs] real pred = rep_array(0.0, maxNobs); 
    real unmarkedQ;
    real markedQ;
    if (splitPresent > 0) {
      // Split compartments
      for (k in 1:nObs) {
        unmarkedQ = unmarked[indices[k,2]][indices[k,1]];
        markedQ = marked[indices[k,2]][indices[k,1]];
        if (splitComps[indices[k,1]] > 0) {
          unmarkedQ = unmarkedQ + initRefr[indices[k,1], 1];
          markedQ = markedQ + initRefr[indices[k,1], 2];
        }
        pred[k] = markedQ / (unmarkedQ + markedQ);
      }
    return(pred);
    } else {
      // No split compartments
      for (k in 1:nObs) {
        unmarkedQ = unmarked[indices[k,2]][indices[k,1]];
        markedQ = marked[indices[k,2]][indices[k,1]];
        pred[k] = markedQ / (unmarkedQ + markedQ);
      }
      return(pred);
    }
  }
  
}

/// * Data (TO CHECK)

data {

  /// * Common to both approaches (matrix exp. and Euler scheme)

  /// ** Method switch
  int<lower=1,upper=2> ode_method; // 1 = matrix exp, 2 = Euler
  
  /// ** Counts
  int<lower=1> nComps; // Number of compartments in a topology
  int<lower=0> nGroups; // Number of replication groups (i.e. rows in the original networkModel object)
  int<lower=0> nParams; // Total number of parameters to be estimated
  // Priors
  int<lower=1> nNonConstantPriors; // Number of non-constant parameters
  int<lower=0> nPriorUniform_code1; // Number of uniform priors
  int<lower=0> nPriorHcauchy_code2; // Number of half-Cauchy priors
  int<lower=0> nPriorBeta_code3; // Number of beta priors
  int<lower=0> nPriorTrNormal_code4; // Number of truncated normal priors
  int<lower=0> nPriorExponential_code5; // Number of exponential priors
  int<lower=0> nPriorGamma_code6; // Number of gamma priors
  
  /// ** Parameter priors

  // Constant
  array[nParams] real constantParams; // Fixed values (for constant parameters)
  
  // Uniform
  array[nParams] real lowerParams; // Lower boundaries (for uniform)
  array[nParams] real upperParams; // Upper boundaries (for uniform)

  // Half-Cauchy
  array[nParams] real hcauchyScaleParams; // Scale parameter (for half-Cauchy)

  // Scaled Beta
  array[nParams] real<lower=0> rawBetaAlpha; // Alpha parameter (for scaled beta)
  array[nParams] real<lower=0> rawBetaBeta; // Beta parameter (for scaled beta)
  array[nParams] real<lower=0> betaScaleParams; // Scale parameter (for scaled beta)

  // Truncated normal
  array[nParams] real trNormMeanParams; // Mean of the untruncated normal
  array[nParams] real trNormSdParams; // Sd of the untruncated normal

  // Exponential
  array[nParams] real exponentialRateParams; // Rate of the exponential

  // Gamma
  array[nParams] real gammaAlphaParams; // Shape of the gamma
  array[nParams] real gammaBetaParams; // Rate of the gamma
  
  /// ** Mapping between unscaled and scaled parameters
  array[nParams] int<lower=0> mappingParamPriorType; // Prior type for each parameter (numeric code)
  array[nParams] int<lower=0> mappingParamPriorID; // Param ID within each prior typeID (1:nParams)

  /// ** Distribution family for observed proportions
  int<lower=1,upper=4> propFamily;
  // Code is:
  // 1 for gamma parameterized on mean and cv (eta = cv)
  // 2 for normal parameterized on mean and cv (eta = cv)
  // 3 for normal parameterized on mean and sd (eta = sd)
  // 4 for beta parameterized on mean and precision phi (eta = precision)
  
  /// ** Distribution family for observed sizes
  int<lower=1,upper=2> sizeFamily;
  // Code is:
  // 1 for normal parameterized on mean and cv (zeta = cv)
  // 2 for normal parameterized on mean and sd (zeta = sd)
  
  /// ** Initial conditions
  array[nComps,2,nGroups] real<lower=0> initialQuantities; // Columns are (unmarked, marked)

  /// ** Steady-state compartments
  int<lower=0> maxNsteady; // Maximum number of steady state comps across groups
  array[nGroups+1] int<lower=0> nSteady; // Padded
  array[maxNsteady,nGroups] int<lower=0> steadyIndices;

  /// ** Split compartments
  int<lower=0,upper=1> splitPresent; // Boolean
  array[nComps,nGroups] int<lower=0,upper=1> splitComps; // Booleans

  /// ** Parameter mapping for pis (portions of active compartments)
  array[nComps, nGroups] int<lower=0> piMapping; // Mapping to params[x]

  /// ** Lambda due to decay
  real<lower=0> lambda_decay; // 0 is equivalent to stable isotopes (no decay)

  /// ** Mapping for upsilons
  int<lower=0> maxNupsilons;
  array[nGroups+1] int<lower=0> nUpsilons; // Number of upsilon parameters per group, padded
  array[maxNupsilons,3,nGroups] int<lower=0> upsilonMapping;
  
  /// ** Mapping for lambdas
  int<lower=0> maxNlambdas;
  array[nGroups+1] int<lower=0> nLambdas; // Number of lambda parameters per group, padded
  array[maxNlambdas,2,nGroups] int<lower=0> lambdaMapping;


  /// * common to both approaches, but with different values

  /// ** Pulse events

  int<lower=0> maxNpulseEvents; // Maximum number of pulse events across groups
  array[nGroups+1] int<lower=0> nPulseEvents; // Padded
  array[maxNpulseEvents,2,nGroups] int<lower=0> pulseEventsIndices; // Indices mapping events to the interval at the beginning of which they happen
  array[maxNpulseEvents,2,nGroups] real pulseEventsQuantities;
  
  // ** Individual observations
  int<lower=0> maxNsizesObs;
  int<lower=0> maxNpropsObs;
  array[nGroups+1] int<lower=0> nSizesObs; // Padded
  array[nGroups+1] int<lower=0> nPropsObs; // Padded
  array[maxNsizesObs,3,nGroups] int<lower=0> sizesObsIndices; // Columns are compartment, timepoint, zeta param index
  array[maxNpropsObs,3,nGroups] int<lower=0> propsObsIndices; // Columns are compartment, timepoint, eta param index
  array[maxNsizesObs, nGroups] real<lower=0> sizesObs;
  array[maxNpropsObs, nGroups] real<lower=0> propsObs;
  
  /// * Specific to matrix exponential approach
  
  /// ** Time intervals
  int<lower=0> maxNtimeIntervals; // Maximum number of intervals across groups
  array[nGroups+1] int<lower=0> nTimeIntervals; // Number of intervals separated by events, per group (padded)
  array[maxNtimeIntervals,nGroups] real<lower=0> intervalsLengths; // Duration of time intervals
  
  /// ** Unique observation times per group
  
  int<lower=0> maxNobsTimes; // Maximum number of unique obs times per group, across groups
  array[nGroups+1] int<lower=0> nObsTimes; // Number of unique obs times per group, padded
  array[nGroups,maxNobsTimes] real<lower=0> elapsedTimeSinceEvent; // Elapsed duration between observation times and the previous event (or t0)
  array[nGroups,maxNobsTimes] int<lower=0> obsIntervalsIndices; // Indices of the intervals corresponding to each observation time
  
  /// * Specific to forward Euler approach

  /// ** Timepoints for Euler approach
  int<lower=2> maxNtimesteps; // Maximum number of timesteps across groups
  array[nGroups+1] int<lower=0> nTimesteps; // Number of timesteps per group (padded)
  int<lower=1> maxNuniqueDts; // Maximum number of unique values for dt per group
  array[nGroups+1] int<lower=0> nUniqueDts; // Number of unique values for dt per group (padded)
  array[maxNuniqueDts,nGroups] real<lower=0> unique_dts; // Unique dt values (padded)
  array[maxNtimesteps,nGroups] int<lower=0> timesteps; // Indices for the Sequence of delta t between successive timesteps, to be used with unique_dts[]

}

/// * Transformed data

transformed data {

  /// * Initialization
  int<lower=0> nTotal; // Total number of observations
  int<lower=0> n_quantity_records; // Size of 'unmarked' and 'marked' arrays
  nTotal = 0;
  
  /// * Count observations
  for (g in 1:nGroups) {
    nTotal += nSizesObs[g] + nPropsObs[g];
  }

  /// * Size of 'unmarked' and 'marked' arrays in transformed parameters block
  if (ode_method == 1) {
    // Matrix exp
    n_quantity_records = maxNobsTimes+1; // nObsTimes + t0
  }
  if (ode_method == 2) {
    // Euler
    n_quantity_records = maxNtimesteps+1;
  }

}

/// * Parameters

parameters {
  vector<lower=0,upper=1>[nPriorUniform_code1] rawUniformParams;
  vector<lower=0>[nPriorHcauchy_code2] rawHcauchyParams;
  vector<lower=0,upper=1>[nPriorBeta_code3] rawBetaParams;
  vector<lower=0>[nPriorTrNormal_code4] rawTrNormParams;
  vector<lower=0>[nPriorExponential_code5] rawExponentialParams;
  vector<lower=0>[nPriorGamma_code6] rawGammaParams;
}

/// * Transformed parameters

transformed parameters {

  /// * Initialization (MERGED)

  /// ** Common to matrix exp and Euler

  // Parameters on usable scale (converted from raw parameters)
  array[nParams] real<lower=0> params;

  // Initialize the array to store initial quantities for refractory portions
  array[nComps, 2, nGroups] real<lower=0> initRefr = rep_array(0.0, nComps, 2, nGroups); // Columns are unmarked, marked
  
  // Initialize the [nGroups x nSteps] array for unmarked tracer
  array[nGroups, n_quantity_records] vector[nComps] unmarked;
  // Initialize the [nGroups x nSteps] array for marked tracer
  array[nGroups, n_quantity_records] vector[nComps] marked;

  // Predicted values (to compare with observations)
  array[maxNsizesObs, nGroups] real<lower=0> sizesPred;
  array[maxNpropsObs, nGroups] real<lower=0> propsPred;

  // Variables for likelihood calculations
  array[maxNsizesObs, nGroups] real<lower=0> sizesPred_zeta = rep_array(0.0, maxNsizesObs, nGroups);
  array[maxNsizesObs, nGroups] real<lower=0> sizesPred_alpha = rep_array(0.0, maxNsizesObs, nGroups);
  array[maxNsizesObs, nGroups] real<lower=0> sizesPred_beta = rep_array(0.0, maxNsizesObs, nGroups);
  array[maxNpropsObs, nGroups] real<lower=0> propsPred_eta = rep_array(0.0, maxNpropsObs, nGroups);
  array[maxNpropsObs, nGroups] real<lower=0> propsPred_alpha = rep_array(0.0, maxNpropsObs, nGroups);
  array[maxNpropsObs, nGroups] real<lower=0> propsPred_beta = rep_array(0.0, maxNpropsObs, nGroups);

  /// ** Specific to Euler
  
  // Initialize the array containing the transition matrices associated with
  // each unique dt value (updated from group to group)
  array[maxNuniqueDts] matrix[nComps,nComps] transitions;
  array[maxNuniqueDts] matrix[nComps,nComps] transitionsDecay;

  /// ** Specific to matrix exp
  
  // Initialize the transfer matrices (updated/reused from group to group)
  matrix[nComps,nComps] transfer;
  matrix[nComps,nComps] transferDecay;
  matrix[nComps,nComps] transition_tmp; // Temporary variable used to store exp(t*A)
  matrix[nComps,nComps] transitionDecay_tmp; // Temporary variable used to store exp(t*A)

  // Initialize the arrays containing the initial states of each timeline
  // interval (updated/reused from group to group)
  array[maxNtimeIntervals] vector[nComps] intervals_init_states_marked;
  array[maxNtimeIntervals] vector[nComps] intervals_init_states_unmarked;
  
  // Start block to use int counter
  // Cf. https://discourse.mc-stan.org/t/integer-loop-index-in-transformed-parameters-block/9264/3
  {

    int pulseIndex;

  /// * Convert raw parameters to usable parameters (MERGED)
  for (i in 1:nParams) {
    if (mappingParamPriorType[i] == 0) {
      params[i] = constantParams[i];
    }
    if (mappingParamPriorType[i] == 1) {
      params[i] = lowerParams[i] + (upperParams[i] - lowerParams[i]) * rawUniformParams[mappingParamPriorID[i]];
    }
    if (mappingParamPriorType[i] == 2) {
      params[i] = hcauchyScaleParams[i] * rawHcauchyParams[mappingParamPriorID[i]];
    }
    if (mappingParamPriorType[i] == 3) {
      params[i] = betaScaleParams[i] * rawBetaParams[mappingParamPriorID[i]];
    }
    if (mappingParamPriorType[i] == 4) {
      params[i] = rawTrNormParams[mappingParamPriorID[i]];
    }
    if (mappingParamPriorType[i] == 5) {
      params[i] = rawExponentialParams[mappingParamPriorID[i]];
    }
    if (mappingParamPriorType[i] == 6) {
      params[i] = rawGammaParams[mappingParamPriorID[i]];
    }
  }

  /// * Trajectory calculation per group (MERGED)
  for (g in 1:nGroups) {

    /// ** Build the transfer or transition matrices (MERGED)
    
    if (ode_method == 1) {
      // Build the transfer matrices A for group g (matrix exp)
      //
      // We need both the transfer matrix and the transfer matrix with decay in
      // the case of a radioactive tracer, because only the marked quantities
      // will use "transferDecay" while the unmarked quantities will use
      // "transfer" (no decay of the stable material).
      transfer = buildTransferMatrix(nComps, nSteady[g], steadyIndices[,g],
                                     nUpsilons[g], upsilonMapping[,,g],
                                     nLambdas[g], lambdaMapping[,,g], params);
      transferDecay = buildTransferMatrixDecay(nComps, transfer, lambda_decay);
    }
    if (ode_method == 2) {
      // Build the transition matrices, using dt (Euler)
      for (i in 1:nUniqueDts[g]) {
        transitions[i] = buildTransitionMatrix(nComps,
                                               nUpsilons[g], upsilonMapping[,,g],
                                               nLambdas[g], lambdaMapping[,,g],
                                               unique_dts[i,g], params);
        transitionsDecay[i] = buildTransitionMatrixDecay(nComps,
                                                         transitions[i],
                                                         lambda_decay,
                                                         unique_dts[i,g]);
      }
    }
    
    /// ** Initialize event index (MERGED)
    pulseIndex = 1;
    
    /// ** Initialize first rows of unmarked and marked tracer quantities (MERGED)
    // Those are the quantities at t=0
    unmarked[g, 1] = to_vector(initialQuantities[,1,g]);
    marked[g, 1] = to_vector(initialQuantities[,2,g]);

    /// ** Adjust split compartments (MERGED)
    if (splitPresent > 0) {
      for (j in 1:nComps) {
        if (splitComps[j,g] > 0) {
          // Store the quantities for the refractory part
          initRefr[j,1,g] = unmarked[g, 1][j] * (1 - params[piMapping[j,g]]);
          initRefr[j,2,g] = marked[g, 1][j] * (1 - params[piMapping[j,g]]);
          // Update the initial conditions for the active part
          unmarked[g, 1][j] = unmarked[g, 1][j] * params[piMapping[j,g]];
          marked[g, 1][j] = marked[g, 1][j] * params[piMapping[j,g]];
        }
      }
    }

    /// ** Apply pulses, if any at t=0 (MERGED)
    if (nPulseEvents[g] > 0) {
      if (pulseIndex <= nPulseEvents[g]) {
        while(pulseEventsIndices[pulseIndex,1,g] == 1) {
          unmarked[g, 1][pulseEventsIndices[pulseIndex,2,g]] += pulseEventsQuantities[pulseIndex,1,g];
          marked[g, 1][pulseEventsIndices[pulseIndex,2,g]] += pulseEventsQuantities[pulseIndex,2,g];
          pulseIndex += 1;
          if (pulseIndex > nPulseEvents[g]) {
            break;
          }
        }
      }
    }

    /// ** ODEs solving using matrix exponential
    if (ode_method == 1) {
      
      /// *** Store the initial state of the first timeline interval (MATRIX EXP)
      intervals_init_states_unmarked[1] = unmarked[g, 1];
      intervals_init_states_marked[1] = marked[g, 1];

      // unmarked[g,1] and marked[g,1] will be overwritten by the first
      // observation time in the 1:nObsTimes[g] loop below.
    
      /// *** Calculate initial state for each timeline interval after the first one (MATRIX EXP)
      if (nTimeIntervals[g] > 1) {
        for (t in 2:nTimeIntervals[g]) {

          // Calculate the end-point of the previous interval
          transition_tmp = matrix_exp(intervalsLengths[t-1,g] * transfer);
          if (lambda_decay == 0) {
            transitionDecay_tmp = transition_tmp;
          } else {
            transitionDecay_tmp = matrix_exp(intervalsLengths[t-1,g] * transferDecay);
          }
          intervals_init_states_unmarked[t] = transition_tmp * intervals_init_states_unmarked[t-1];
          intervals_init_states_marked[t] = transitionDecay_tmp * intervals_init_states_marked[t-1];
          
          // Apply pulses, if any
          if (nPulseEvents[g] > 0) {
            if (pulseIndex <= nPulseEvents[g]) {
              while(pulseEventsIndices[pulseIndex,1,g] == t) {
                intervals_init_states_unmarked[t][pulseEventsIndices[pulseIndex,2,g]] += pulseEventsQuantities[pulseIndex,1,g];
                intervals_init_states_marked[t][pulseEventsIndices[pulseIndex,2,g]] += pulseEventsQuantities[pulseIndex,2,g];
                pulseIndex += 1;
                if (pulseIndex > nPulseEvents[g]) {
                  break;
                }
              }
            }
          } // End of pulse block
        } // End of loop over time intervals
      } // End of calculation of intervals initial states
      
      /// *** Calculate projections for each observation time (MATRIX_EXP)
      for (k in 1:nObsTimes[g]) {
        transition_tmp = matrix_exp(elapsedTimeSinceEvent[g,k]*transfer);
        if (lambda_decay == 0) {
          transitionDecay_tmp = transition_tmp;
        } else {
          transitionDecay_tmp = matrix_exp(elapsedTimeSinceEvent[g,k]*transferDecay);
        }
        unmarked[g,k] = transition_tmp*intervals_init_states_unmarked[obsIntervalsIndices[g,k]];
        marked[g,k] = transitionDecay_tmp*intervals_init_states_marked[obsIntervalsIndices[g,k]];
      }

    } // End of solving ODEs with matrix exponential

    /// ** ODEs solving using Euler scheme
    if (ode_method == 2) {

      // For each timestep
      for (t in 1:nTimesteps[g]) {
        
        //Apply the transfer matrix
        unmarked[g,t+1] = transitions[timesteps[t,g]] * unmarked[g, t];
        marked[g,t+1] = transitionsDecay[timesteps[t,g]] * marked[g, t];
        
        // Reset steady-state compartments
        if (nSteady[g] > 0) {
          for (k in 1:nSteady[g]) {
            unmarked[g,t+1][steadyIndices[k,g]] = unmarked[g,t][steadyIndices[k,g]];
            marked[g,t+1][steadyIndices[k,g]] = marked[g,t][steadyIndices[k,g]];
          }
        }
        
        // Apply pulses
        if (nPulseEvents[g] > 0) {
          if (pulseIndex <= nPulseEvents[g]) {
            while(pulseEventsIndices[pulseIndex,1,g] == t+1) {
              unmarked[g, t+1][pulseEventsIndices[pulseIndex,2,g]] += pulseEventsQuantities[pulseIndex,1,g];
              marked[g, t+1][pulseEventsIndices[pulseIndex,2,g]] += pulseEventsQuantities[pulseIndex,2,g];
              pulseIndex += 1;
              if (pulseIndex > nPulseEvents[g]) {
                break;
              }
            }
          }
        }
        
      } // End of timesteps loop

    } // End of solving ODEs with Euler scheme
    
    /// ** Store predicted values (MERGED)
    
    sizesPred[,g] = buildSizePredictions(nSizesObs[g], g, maxNsizesObs,
                                         unmarked[g,], marked[g,],
                                         sizesObsIndices[,,g],
                                         splitPresent, splitComps[,g],
                                         initRefr[,,g]);
    propsPred[,g] = buildPropPredictions(nPropsObs[g], g, maxNpropsObs,
                                         unmarked[g,], marked[g,],
                                         propsObsIndices[,,g],
                                         splitPresent, splitComps[,g],
                                         initRefr[,,g]);

    /// ** Prepare variables for likelihood calculations (MERGED)
    for (k in 1:nSizesObs[g]) {
      sizesPred_zeta[k,g] = params[sizesObsIndices[k,3,g]];
      if (sizeFamily == 1) {
        // Normal(mean, zeta = cv)
        sizesPred_alpha[k,g] = sizesPred[k,g]; // mean
        sizesPred_beta[k,g] = sizesPred_zeta[k,g] * sizesPred[k,g]; // sd
      }
      if (sizeFamily == 2) {
        // Normal(mean, zeta = sd)
        sizesPred_alpha[k,g] = sizesPred[k,g]; // mean
        sizesPred_beta[k,g] = sizesPred_zeta[k,g]; // sd
      }
    }
    for (k in 1:nPropsObs[g]) {
      propsPred_eta[k,g] = params[propsObsIndices[k,3,g]];
      if (propFamily == 1) {
        // Gamma(mean, eta = cv)
        propsPred_alpha[k,g] = pow(propsPred_eta[k,g], -2);
        propsPred_beta[k,g] = propsPred_alpha[k,g] / propsPred[k,g];
      }
      if (propFamily == 2) {
        // Normal(mean, eta = cv)
        propsPred_alpha[k,g] = propsPred[k,g]; // Mean
        propsPred_beta[k,g] = propsPred_eta[k,g] * propsPred_alpha[k,g]; // Sd
      }
      if (propFamily == 3) {
        // Normal(mean, eta = sd)
        propsPred_alpha[k,g] = propsPred[k,g]; // Mean
        propsPred_beta[k,g] = propsPred_eta[k,g]; // Sd
      }
      if (propFamily == 4) {
        // Beta(mean, eta = precision phi)
        propsPred_alpha[k,g] = propsPred[k,g] * propsPred_eta[k,g]; // alpha = mean * phi
        propsPred_beta[k,g] = propsPred_eta[k,g] * (1 - propsPred[k,g]); // beta = phi * (1 - mu)
      }
    }
    
  } // End of groups loop

  } // End of block for pulseIndex counter
  
} // End of transformed parameters block

/// * Model

model {

  /// * Priors
  for (i in 1:nParams) {
    if (mappingParamPriorType[i] == 1) {
      rawUniformParams[mappingParamPriorID[i]] ~ uniform(0, 1);
    }
    if (mappingParamPriorType[i] == 2) {
      rawHcauchyParams[mappingParamPriorID[i]] ~ cauchy(0, 1);
    }
    if (mappingParamPriorType[i] == 3) {
      rawBetaParams[mappingParamPriorID[i]] ~ beta(rawBetaAlpha[i], rawBetaBeta[i]);
    }
    if (mappingParamPriorType[i] == 4) {
      rawTrNormParams[mappingParamPriorID[i]] ~ normal(trNormMeanParams[i], trNormSdParams[i]);
    }
    if (mappingParamPriorType[i] == 5) {
      rawExponentialParams[mappingParamPriorID[i]] ~ exponential(exponentialRateParams[i]);
    }
    if (mappingParamPriorType[i] == 6) {
      rawGammaParams[mappingParamPriorID[i]] ~ gamma(gammaAlphaParams[i],
                                                     gammaBetaParams[i]);
    }
  }

  /// * Likelihood
  for (g in 1:nGroups) {

    /// ** Sizes
    if (sizeFamily == 1) {
      // Normal(mean, zeta = cv)
      sizesObs[1:nSizesObs[g], g] ~ normal(sizesPred_alpha[1:nSizesObs[g],g],
                                           sizesPred_beta[1:nSizesObs[g],g]);
    }
    if (sizeFamily == 2) {
      // Normal(mean, zeta = sd)
      sizesObs[1:nSizesObs[g], g] ~ normal(sizesPred_alpha[1:nSizesObs[g],g],
                                           sizesPred_beta[1:nSizesObs[g],g]);
    }

    /// ** Proportions
    if (propFamily == 1) {
      // Gamma(mean, eta = cv)
      propsObs[1:nPropsObs[g], g] ~ gamma(propsPred_alpha[1:nPropsObs[g],g],
                                          propsPred_beta[1:nPropsObs[g],g]);
    }
    if (propFamily == 2) {
      // Normal(mean, eta = cv)
      propsObs[1:nPropsObs[g], g] ~ normal(propsPred_alpha[1:nPropsObs[g],g],
                                           propsPred_beta[1:nPropsObs[g],g]);
    }
    if (propFamily == 3) {
      // Normal(mean, eta = sd)
      propsObs[1:nPropsObs[g], g] ~ normal(propsPred_alpha[1:nPropsObs[g],g],
                                           propsPred_beta[1:nPropsObs[g],g]);
    }
    if (propFamily == 4) {
      // Beta(mean, eta = precision phi)
      propsObs[1:nPropsObs[g], g] ~ beta(propsPred_alpha[1:nPropsObs[g],g],
                                         propsPred_beta[1:nPropsObs[g],g]);
    }
    
  } // End of groups loop
  
}

/// * Generated quantities

generated quantities {

  // Initialization
  vector[nNonConstantPriors] nonConstantParams;
  int paramIndex;
  vector[nTotal] log_lik;
  int llIndexShift;
  llIndexShift = 0;

  // Non-constant parameters
  paramIndex = 1;
  for (i in 1:nParams) {
    if (mappingParamPriorType[i] != 0) {
      // This is a non-constant parameter, save it
      nonConstantParams[paramIndex] = params[i];
      paramIndex += 1;
    }
  }
  
  // Sizes
  for (g in 1:nGroups) {
    for (o in 1:nSizesObs[g]) {
      if (sizeFamily == 1) {
        log_lik[o+llIndexShift] = normal_lpdf(sizesObs[o,g] | sizesPred_alpha[o,g], sizesPred_beta[o,g]);
      }
      if (sizeFamily == 2) {
        log_lik[o+llIndexShift] = normal_lpdf(sizesObs[o,g] | sizesPred_alpha[o,g], sizesPred_beta[o,g]);
      }
    }
    llIndexShift += nSizesObs[g];
  }

  // Proportions
  for (g in 1:nGroups) {
    for (o in 1:nPropsObs[g]) {
      if (propFamily == 1) {
        log_lik[o+llIndexShift] = gamma_lpdf(propsObs[o,g] | propsPred_alpha[o,g], propsPred_beta[o,g]);
      }
      if (propFamily == 2) {
        log_lik[o+llIndexShift] = normal_lpdf(propsObs[o,g] | propsPred_alpha[o,g], propsPred_beta[o,g]);
      }
      if (propFamily == 3) {
        log_lik[o+llIndexShift] = normal_lpdf(propsObs[o,g] | propsPred_alpha[o,g], propsPred_beta[o,g]);
      }
      if (propFamily == 4) {
        log_lik[o+llIndexShift] = beta_lpdf(propsObs[o,g] | propsPred_alpha[o,g], propsPred_beta[o,g]);
      }
    }
    llIndexShift += nPropsObs[g];
  }
  
}
