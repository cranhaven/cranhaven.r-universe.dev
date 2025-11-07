#include "CoreSelection.h"

using namespace Rcpp;


double estimateRadius(Rcpp::NumericMatrix & dist, int n) {
  //must be doubles
  double* d = REAL(dist);
  //full number
  int N = dist.nrow();
  if(n>=N) {
    return 0;
  } else {
    //estimated number around selected
    int m = std::floor(N/n);
    //initiate
    double r = 0;
    int k=0;
    std::vector<double> c(N) ;
    //average top-m value of r over all rows
    for(int i=0; i<N; i++) {
      memcpy(&c[0], &d[k], N*sizeof(double));
      std::partial_sort(c.begin(), c.begin()+m, c.end());
      r += c[m-1];
      k+=N;
    }
    return r/N;
  }
}

struct IncGenerator {
  int current_;
  IncGenerator (int start) : current_(start) {}
  int operator() () { return current_++; }
};

Rcpp::IntegerVector randomSelection(Rcpp::NumericMatrix & dist, double r) {
  //full number
  int N = dist.nrow();
  //number of selected values
  int s = 0;
  //selected values
  Rcpp::IntegerVector v(N);
  //administration
  int an = N;
  std::vector<int> a(an) ;
  IncGenerator g (0);
  std::generate( a.begin(), a.end(), g);
  int i,j,k,nk;
  //start
  while(an>0) {
    //choose random
    //k = rand() % an;
    k = ((int) Rcpp::runif(1,0,an)[0]) % an;
    nk = N*k;
    //register (positive) id
    v[s] = a[k];
    s++;
    //update administration
    j = 0;
    for(i=0;i<an;i++) {
      if(i!=k && dist[i+nk]>=r) {
        a[j]=a[i];
        j++;
      }
    }
    an=j;
  }
  return head(v,s);
}

int coreNumber(Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & selected, Rcpp::IntegerVector & preselected) {
  //length of selected list
  int sl = selected.length();
  int n = sl;
  //check for preselected
  if(preselected.length()>0) {
    //full number
    int N = dist.nrow();
    //find not contained
    Rcpp::IntegerVector d = setdiff(preselected, selected);
    int l = d.length();
    std::vector<int> r(l) ;
    int nl,i;
    double newM, m = 0;
    //loop over all notContained
    while(l>0) {
      nl = N*(d[l-1]-1);
      i = sl;
      //find nearest selection (must replace this one)
      while(i>0) {
        newM = dist[nl+selected[i-1]];
        if(i==sl || m>=newM) {
          r[l-1]=selected[i-1];
          m=newM;
        }
        i--;
      }
      l--;
    }
    //increase number with #notContained minus #uniqueReplacements
    sort( r.begin(), r.end() );
    r.erase( unique( r.begin(), r.end() ), r.end() );
    n+=d.length()-r.size();
  }
  return n;
}

Rcpp::IntegerVector CoreSelection::createSelectionResult(Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & selected) {
  //full number
  int N = dist.nrow();
  int ns = selected.length();
  int nl = 0;
  Rcpp::IntegerVector result(N);
  double distance;
  int js;
  //loop over all items
  for(int i=0; i<N; i++) {
    //initialise
    js = selected[0];
    result[i] = selected[0];
    distance = dist[nl+js];
    //loop over selection
    for(int j=0; j<ns; j++) {
      js = selected[j];
      if(i==js) {
        result[i] = selected[j];
        break;
      } else if(dist[nl+js]<distance) {
        distance = dist[nl+js];
        result[i] = selected[j];
      }
    }
    nl+=N;
  }
  return result;
}

Rcpp::IntegerVector CoreSelection::computeRandomSelection(Rcpp::NumericMatrix & dist, int requiredN, Rcpp::IntegerVector & preselected) {
  //full number
  int N = dist.nrow();
  //get data
  int preselectedLength = preselected.length();
  int maxN = dist.nrow();
  int minN = std::max(1,preselectedLength);
  double maxR = *std::max_element(dist.begin(), dist.end());
  double minR = 0;
  //initialise
  int upperBase = maxN;
  int upperValue = maxN;
  double upperR = minR;
  int lowerBase = 1;
  int lowerValue = minN;
  double lowerR = maxR;
  //initial estimate using base
  int estimateBase = requiredN;
  double estimateR = estimateRadius(dist, estimateBase);
  //nothing selected
  Rcpp::IntegerVector selected = Rcpp::IntegerVector(0);
  int n = -1;
  int mainLoopCounterMaximum = 100;
  //minimum steps necessary to compute sd and mu for estimate
  int minSN = 269; // (1.46 * sigma / (0.1 sigma) )^2
  int i, sn, sx, sx2, newN, newEstimateBase;
  double mu, sigma, slope, newEstimateR;
  bool stepLoopFinished;
  //start loop
  bool mainLoopFinished = FALSE;
  Rcpp::IntegerVector newSelection;
  while(!mainLoopFinished) {
    mainLoopCounterMaximum--;
    //reset stats for computing estimate sd and mu
    sx = 0;
    sx2 = 0;
    //try
    stepLoopFinished = FALSE;
    sn = minSN;
    i = 0;
    while(!stepLoopFinished) {
      newSelection = randomSelection(dist, estimateR);
      newN = coreNumber(dist, newSelection, preselected);
      if(n<0 || abs(newN-requiredN)<abs(n-requiredN)) {
        n = newN;
        selected = newSelection;
      }
      //update stats
      sx = sx + newN;
      sx2 = sx2 + (newN * newN);
      i++;
      //compute mu and sd
      mu = (sx/i);
      sigma = sqrt((sx2/i) - (mu*mu));
      //checks
      if(n==requiredN) {
        mainLoopFinished = TRUE;
        stepLoopFinished = TRUE;
      } else if(i>=sn) {
        stepLoopFinished = TRUE;
      }
    }

    if(!mainLoopFinished) {
      //always return after maximum number of steps
      if(mainLoopCounterMaximum<=0) {
        mainLoopFinished = TRUE;
        //finish if mu within sigma distance of n if not using estimateBase
      } else if(estimateBase<0 && (requiredN>=(mu-0.5*sigma)) && (requiredN<=(mu+0.5*sigma))) {
        mainLoopFinished = TRUE;
      } else {
        //try to improve using estimateBase
        if(estimateBase>=0) {
          newEstimateBase = estimateBase;
          if(mu>requiredN) {
            slope = (mu-lowerValue)/(estimateBase-lowerBase);
            upperBase = estimateBase;
            upperValue = mu;
            upperR = estimateR;
            newEstimateBase = floor(estimateBase - ((mu-requiredN)/slope));
          } else {
            slope = (upperValue-mu)/(upperBase-estimateBase);
            lowerBase = estimateBase;
            lowerValue = mu;
            lowerR = estimateR;
            newEstimateBase = ceil(estimateBase - ((mu-requiredN)/slope));
          }
          //check for improvement/change, otherwise stop using estimateBase and switch to estimateR approach
          if(newEstimateBase==estimateBase || newEstimateBase>=upperBase || newEstimateBase<=lowerBase) {
            estimateBase = -1;
          } else {
            estimateBase = newEstimateBase;
            newEstimateR = estimateRadius(dist, estimateBase);
            if(newEstimateR<=0) {
              estimateBase = -1;
            } else {
              estimateR = newEstimateR;
            }
          }
        }
        //try to improve using estimateR
        if(estimateBase<0) {
          if(mu>requiredN) {
            //things shouldn't get worse
            if(upperValue<mu) {
              mainLoopFinished = TRUE;
            }
            upperBase = -1;
            upperValue = mu;
            upperR = estimateR;
            estimateR = (estimateR+lowerR)/2;
          } else {
            //things shouldn't get worse
            if(lowerValue>mu) {
              mainLoopFinished = TRUE;
            }
            lowerBase = -1;
            lowerValue = mu;
            lowerR = estimateR;
            estimateR = (estimateR+upperR)/2;
          }
        }
      }
    }
  }
  return createSelectionResult(dist, selected);
}

void CoreSelection::initialise(int seed) {
  //srand (seed); // seed the generator
}
