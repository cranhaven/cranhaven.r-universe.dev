// [[Rcpp::depends(RcppArmadillo)]]

#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#include <RcppArmadillo.h>

using namespace Rcpp;

//' Get collapsed data based on model
//'
//' Get the (collapsed) model data specified by a particular partition (model)
//'
//' @param yMat     matrix of \code{(y, n-y)} data
//' @param model  vector of indices giving how to partition the data
//' @return matrix giving partitioned data according to model
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::mat collapseData(
    arma::mat const& yMat,
    arma::vec const& model
) {
  int D0 = max(model) + 1;                 // number of distinct parameters;
  arma::mat y(D0, 2, arma::fill::zeros);   // create container for compressed data;
  
  // put data into compressed buckets;
  for ( unsigned int k = 0; k < yMat.n_cols; k++ )
  {
    int d   = model[k];
    y(d,0) += yMat(0,k);
    y(d,1) += yMat(1,k);				
  }
  return y;
}





//' Simulate basket data
//'
//' Simulate data from basket trial based on specified parameters
//'
//' @param K0 integer giving number of baskets
//' @param I0 integer giving number of interim analyses
//' @param targSSPer  target sample size increment for each basket
//' @param rRates     \code{vector} of true response rates for each basket
//' @param eScales    \code{vector} giving reciprocal of poisson process rate for each basket
//' @param aParms     vector of length 2 giving normal mean and variance for time to outcome ascertainment
//' 
//' @return \code{matrix} giving simulated data from basket based on parameters
//' @details Each column of the returning matrix is as follows:
//' * y
//' * b
//' * et
//' * at
//' * ft
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::mat simData(
    int        const& K0,
    int        const& I0,
    arma::ivec const& targSSPer,
    arma::vec  const& rRates,
    arma::vec  const& eScales,
    arma::vec  const& aParms
)
{
  // calculate maximum possible number of data points (enough so that the trial could enroll the total N from one basket);
  int basket_nMax = 0;
  for ( int i = 0; i < I0; i++ )
  {
    basket_nMax += targSSPer[i] * K0;
  }
  int nMax = basket_nMax * K0;
  
  arma::rowvec d(5, arma::fill::zeros); // (y, b, et, at, ft)
  arma::mat bData(nMax, 5, arma::fill::zeros);
  
  // simulate all possible data
  int idx = 0;
  for (int k = 0; k < K0; k++) 
  {
    d(1) = k;
    double cumTime = 0;
    
    for ( int n=0; n<basket_nMax; n++ )
    {
      cumTime  += R::rexp(eScales[k]);
      d(2)      = cumTime;                             // simulate enrollment time;
      d(3)      = R::rnorm( aParms[0], aParms[1] );    // simulate response ascertainment time;
      d(4)      = arma::accu(d(arma::span(2,3)));      // calculate total follow-up time;
      d(0)      = R::rbinom( 1, rRates[k] );           // simulate response;
      
      bData.row(idx) = d;
      idx++;
    }
  }		
  // put the data in chronological order based on outcome ascertainment;
  arma::uvec sort_idx = arma::sort_index(bData.col(3));    // obtain indices of sorted column
  for ( unsigned int i = 0; i < bData.n_cols; i++ ){          // loop through all columns of mtx
    arma::vec replace = bData.col(i);                         // replace = the column to replace
    bData.col(i)      = replace.elem(sort_idx);               // replace ith column with sorted elements
  }
  return bData;
}



//' Simulate a BMA design
//'
//' Simulates a BMA design given hyperparameters
//'
//' @param nSims number of simulation studies to be performed
//' @param eRates \code{vector} of Poisson process rates for each basket
//' @param rRates \code{vector} of true response rates for each basket
//' @param aParms \code{vector} giving time to outcome ascertainment distribution parameters (common)
//' @param ppEffCrit \code{vector} giving basket-specific posterior probability threshold for activity (i.e., efficacy)
//' @param ppFutCrit \code{vector} giving basket-specific posterior probability threshold for futility
//' @param futOnly \code{logical} giving whether design allows only for futility stopping (\code{TRUE} = futility only, \code{FALSE} = both futility and efficacy)
//' @param rRatesNull \code{vector} of basket-specific null hypothesis values (for efficacy determination)
//' @param rRatesAlt \code{vector} of basket-specific hypothesized alternative values (for futility determination)
//' @param minSSFut minimum number of subjects in basket to assess futility
//' @param minSSEff minimum number of subjects in basket to assess activity
//' @param minSSEnr matrix giving minimum number of new subjects per basket before next analysis (each row is an interim analysis)
//' @param targSSPer vector giving target sample size increment for each basket
//' @param I0 maximum number of analyses
//' @param mu0 prior mean for the response probabilities
//' @param phi0 prior dispersion response probabilities
//' @param partitionMat matrix giving partitions
//' @param logModelPriors vector giving prior model probabilities
//' 
//' @return a list giving aspects of the simulation
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
Rcpp::List bma_design_cpp (
    int        const& nSims,
    arma::vec  const& eRates, 
    arma::vec  const& rRates, 
    arma::vec  const& aParms, 
    arma::vec  const& ppEffCrit,
    arma::vec  const& ppFutCrit, 
    int        const& futOnly, 
    arma::vec  const& rRatesNull,
    arma::vec  const& rRatesAlt, 
    int        const& minSSFut, 
    int        const& minSSEff, 
    arma::imat const& minSSEnr,
    arma::imat const& maxSSEnr, 
    arma::ivec const& targSSPer, 
    int        const& I0,
    double     const& mu0, 
    double     const& phi0,
    arma::mat  const& models,
    arma::rowvec  const& logPriorModelProbs
)
{
  // initialize random number generator;
  RNGScope scope;    
  
  // calculate the number of baskets;
  int K0 = eRates.size();
  
  // calculate number of models and number of distinct parms
  int M0 = models.n_cols;              // number of models
  
  // calculate enrollment distrution scale parameters;
  arma::vec eScales = 1.0 / eRates;
  
  // calculate maximum possible number of data points (enough so that the trial could enroll the total N from one basket);
  int basket_nMax = 0;
  for ( int i = 0; i < I0; i++ )
  {
    basket_nMax += targSSPer[i] * K0;
  }
  int nMax = basket_nMax * K0;
  
  double a0 = mu0 * phi0;
  double b0 = (1 - mu0) * phi0;
  
  
  // precalculate all poster normalizing constants to avoid using lgamma function repeatedly;
  double log_prior_nc = lgamma(a0+b0) - lgamma(a0) - lgamma(b0);
  
  // precalculate posterior probabilities for treatment efficacy using each baskets threshold;		
  arma::mat log_post_nc( nMax + 1, nMax + 1, arma::fill::zeros );
  
  std::vector<arma::mat> post_prob_cdf(K0);
  std::vector<arma::mat> post_prob_cdf_mid(K0);
  
  // get posterior normalizing constants
  for (int k = 0; k < K0; k++)
  {
    arma::mat pp( nMax + 1, nMax + 1, arma::fill::zeros);	
    arma::mat pp_mid(nMax + 1, nMax + 1, arma::fill::zeros);				
    for ( int y0 = 0; y0 <= nMax; y0++ )
    {
      for ( int y1 = 0; y1 <= nMax; y1++ )
      {
        if ( (y0 + y1) <= nMax )
        {
          double a1 = a0+y1;
          double b1 = b0+y0;
          
          log_post_nc(y0, y1) = lgamma(a1+b1) - lgamma(a1) - lgamma(b1);
          pp(y0,y1)           = R::pbeta( rRatesNull[k], a1, b1, 0, 0 );
          pp_mid(y0,y1)       = R::pbeta( rRatesNull[k] * 0.5 + rRatesAlt[k] * 0.5, a1, b1, 0, 0);
        }
      }		
    }
    post_prob_cdf[k]     = pp;
    post_prob_cdf_mid[k] = pp_mid;
  }
  
  arma::mat all_PostProbs(nSims, K0);
  arma::mat all_PostMeans(nSims, K0);
  arma::mat  all_Efficacy(nSims, K0, arma::fill::zeros);
  arma::mat  all_Futility(nSims, K0, arma::fill::zeros);
  arma::mat         all_n(nSims, K0, arma::fill::zeros);
  arma::mat     all_piHat(nSims, K0, arma::fill::zeros);
  arma::mat  all_pInterim(nSims, I0, arma::fill::zeros);
  
  arma::mat all_bias(nSims, K0, arma::fill::zeros);
  arma::mat  all_mse(nSims, K0, arma::fill::zeros);
  arma::mat all_cont(nSims, I0, arma::fill::zeros);
  
  arma::vec all_durations(nSims, arma::fill::zeros);
  arma::vec       all_fwr(nSims, arma::fill::zeros);
  arma::vec    all_numerr(nSims, arma::fill::zeros);

  
  // Loop through number of simulations
  for ( int s = 0; s < nSims; s++ )
  {
    // simulate all possible data
    arma::mat bData = simData(K0, I0, targSSPer, rRates, eScales, aParms);
    
    // container for whether enrollment is open in a basket (1=Yes;0=No)
    arma::irowvec active(K0, arma::fill::ones);
    
    // hypothesis testing result for basket (1=Efficacy;-1=Futility;0=Indeterminate)
    arma::irowvec decision(K0, arma::fill::zeros);
    
    int final_interim = 0;    // variable to store at which interim the study stopped
    double duration   = 0;    // varuabke to store the final study duration;
    int nStart        = 0;    // total sample size
    int numAdd        = 0;    // number of additional outcomes to observe before next analysis
    
    // containers for final study results;
    arma::rowvec basket_specific_pp(    K0, arma::fill::zeros);	    // container for basket specific pp for efficacy
    arma::rowvec basket_specific_pp_mid(K0, arma::fill::zeros);			// container for basket specific pp for futility
    arma::rowvec basket_specific_mn(    K0, arma::fill::zeros);			// container for basket specific posterior mean
    
    // containers for final study data;
    arma::mat yMat(2, K0, arma::fill::zeros);		
    arma::irowvec nVec(K0, arma::fill::zeros);				
    
    
    // loop over interim analyzes;
    for (int i = 0; i < I0; i++)
    {
      
      // determine number of additional outcomes needed (ideally);
      numAdd = sum(active) * targSSPer[i];
      
      // find the number of subjects needed to meet requirements on minimum enrollment
      // without exceeding requirements on maximum enrollment;
      arma::rowvec        nAcc( K0, arma::fill::zeros );
      arma::rowvec nMinCritMet( K0, arma::fill::zeros );
      arma::rowvec nMaxCritMet( K0, arma::fill::zeros );
      
      for (int k = 0; k < K0; k++)
      {
        if (minSSEnr(i, k) == 0) { nMinCritMet[k] = 1; }
      }
      
      int idx2=0;
      for ( int n = nStart; n < nMax; n++ )
      {
        int b = bData(n, 1);
        int y = bData(n, 0);
        
        if (active[b] == 1 and nMaxCritMet[b] == 0)
        {
          nAcc[b] +=1;
          if (nAcc[b] >= minSSEnr(i,b)) { nMinCritMet[b]=1; }
          if (nAcc[b] >= maxSSEnr(i,b)) { nMaxCritMet[b]=1; }
          
          yMat(y,b)   += 1;
          nVec[b]     += 1;	
          duration     = bData(n,4);   // = ft						
          
          if ( (sum(nAcc)>= numAdd) and (sum(nMinCritMet)==sum(active)) ) { n = nMax + 100; }
        }	
        idx2++;
      }		
      
      // update total number of subjects
      nStart += idx2;
      
      //-----------------------------
      // BMA model fitting section;
      //-----------------------------
      arma::rowvec ppModel = logPriorModelProbs;          // container for posterior model probability container; 
      
      arma::mat     pp(M0, K0, arma::fill::zeros);       // container for model-specific posterior probabilities for efficacy;
      arma::mat pp_mid(M0, K0, arma::fill::zeros);       // container for model-specific posterior probabilities for futility;				
      arma::mat     mn(M0, K0, arma::fill::zeros);       // container for model-specific posterior means;
      
      // loop over models
      for ( int m = 0; m < M0; m++ )            
      {
        // get collapsed data according to model
        arma::vec mID  = models.col(m);            // vector giving basket assignments;
        arma::mat y    = collapseData(yMat, mID);  // collapse yMat according to model mID
        int D0         = max(mID) + 1;             // number of distinct parameters in model;
        
        // compute unnormalized posterior model probabilities;
        for (int d = 0; d < D0; d++)
        {
          ppModel[m] += log_prior_nc - log_post_nc( y(d,0), y(d,1) );
        }
        
        
        // compute model-specific posterior quantities;
        for ( int k = 0; k < K0; k++ )
        {
          int d        = mID[k];                                          // assignment for kth basket in model mID
          pp(m, k)     = post_prob_cdf[k]( y(d,0), y(d,1) );              // CDF pp for efficacy  
          pp_mid(m, k) = post_prob_cdf_mid[k]( y(d,0), y(d,1) );					// CDF pp for futility
          mn(m, k)     = ( a0 + y(d,1) ) / ( a0 + b0 + y(d,1) + y(d,0) ); // posterior mean
        }		
      }
      
      // Compute posterior model probabilities
      double modMax  = max(ppModel);
      ppModel        = exp(ppModel - modMax);
      double sumProb = sum(ppModel);
      ppModel        = ppModel / sumProb;
      
      // model averaged posterior quantities
      basket_specific_pp     = ppModel * pp;
      basket_specific_pp_mid = ppModel * pp_mid;
      basket_specific_mn     = ppModel * mn;
      
      int prev_active = sum(active);

      for (int k = 0; k < K0; k++)
      {
        int eval_crit_met =    (nVec[k] >= minSSEff) 
                            or ( (prev_active == 1) and (active[k]==1) ) 
                            or (i == (I0 - 1) );
        
        if ( 
              (eval_crit_met == 1) and (futOnly == 0 or (i == (I0 - 1)) ) and (basket_specific_pp[k] >= ppEffCrit[k]) 
        ) { 
          active[k]   = 0; 
          decision[k] = 1; 
        }
        
        eval_crit_met = ( (nVec[k] >= minSSFut) or ((prev_active == 1) and (active[k] == 1)) ) and (i < (I0 - 1));
        if ((eval_crit_met==1) and (basket_specific_pp_mid[k]<=ppFutCrit[k])) { 
          active[k] = 0; 
          decision[k] = -1; 
        }
      }
      
      if ( futOnly == 1 and ( i < (I0 - 1) ) )
      {
        arma::irowvec active_Fut   = active;
        arma::irowvec decision_Fut = decision;
        
        prev_active   = sum(active);
        int poss_stop = 0;
        for (int k = 0; k < K0; k++)
        {
          if ( active[k] == 1 and ( nVec[k] >= minSSEff ) and ( basket_specific_pp[k] >= ppEffCrit[k] ) )
          {
            poss_stop       += 1;
            active_Fut[k]    = 0;
            decision_Fut[k]  = 1;
          }
        }
        
        if (poss_stop == prev_active)
        {
          active   = active_Fut;
          decision = decision_Fut;
        }
      }
      
      
      all_cont(s,i) = sum(active);
      
      if ( max(active) == 0 or i == (I0 - 1) ) 
      { 
        final_interim = i+1; 
        i=10000;
      }
    }
    
    all_PostProbs.row(s) = basket_specific_pp;
    all_PostMeans.row(s) = basket_specific_mn;
    
    for ( int k = 0; k < K0; k++ )
    {
      all_bias(s,k) = basket_specific_mn[k] - rRates[k];
      all_mse(s,k)  = all_bias(s,k)*all_bias(s,k);
    }
    
    for ( int k=0; k < K0; k++ )
    {
      if ( decision[k] == 1 ) {
        all_Efficacy(s,k) = 1.0;
      }
      else if (decision[k] == -1) {
        all_Futility(s,k) = 1.0;
      }
      
      if ((decision[k]==1) and (rRates[k]<=rRatesNull[k])) { 
        all_fwr[s]     = 1; 
        all_numerr[s] +=1;
      }
    }
    
    arma::rowvec nVecDouble = arma::conv_to<arma::rowvec>::from(nVec);
    arma::mat yMatDouble    = yMat;
    arma::rowvec yVecDouble = yMatDouble.row(1);
    
    
    all_n.row(s) = nVecDouble;
    all_piHat.row(s) = yVecDouble/nVecDouble;
    
    all_pInterim(s,final_interim-1) += 1;
    
    all_durations[s] = duration;
  }   // end loop throughsimulations
  
  double NERR = 0;
  if ( mean(all_fwr) > 0 ) { 
    NERR = mean(all_numerr) / mean(all_fwr);
  }
  
  Rcpp::List HT = Rcpp::List::create(  
    Rcpp::Named("rr")     = mean(all_Efficacy),
    Rcpp::Named("fw.fpr") = mean(all_fwr),
    Rcpp::Named("nerr")   = NERR,
    Rcpp::Named("fut")    = mean(all_Futility)								  
  );
  
  
  Rcpp::List PE = Rcpp::List::create(  
    Rcpp::Named("PM.ave") = mean(all_PostMeans),
    Rcpp::Named("SP.ave") = mean(all_piHat),
    Rcpp::Named("PP.ave") = mean(all_PostProbs),
    Rcpp::Named("bias")   = mean(all_bias),
    Rcpp::Named("mse")    = mean(all_mse)
  );
  
  
  Rcpp::List SS = Rcpp::List::create(  
    Rcpp::Named("basket.ave")  = mean(all_n),
    
    Rcpp::Named("basket.med")  = median(all_n),								  
    Rcpp::Named("basket.min")  = min(all_n),
    Rcpp::Named("basket.max")  = max(all_n),	
    
    Rcpp::Named("overall.ave")  = mean(sum(all_n,1),0)
    /*
     ,
     Rcpp::Named("overall.med")  = median(sum(all_n,1),0),								  
     Rcpp::Named("overall.min")  = min(sum(all_n,1),0),
     Rcpp::Named("overall.max" ) = max(sum(all_n,1),0)
     */
  );														
  
  Rcpp::List DU = Rcpp::List::create(  
    Rcpp::Named("average")      = mean(all_durations)
  );	
  
  Rcpp::List SP = Rcpp::List::create(  
    Rcpp::Named("interim.stop.prob")           = mean(all_pInterim),
    Rcpp::Named("baskets.continuing.ave")      = mean(all_cont)			
  );	
  
  
  
  return Rcpp::List::create(
    Rcpp::Named("hypothesis.testing")    = HT,	
    Rcpp::Named("sample.size")           = SS,										
    Rcpp::Named("point.estimation")      = PE,
    Rcpp::Named("trial.duration")        = DU,
    Rcpp::Named("early.stopping")        = SP								  			  
  );
} 
