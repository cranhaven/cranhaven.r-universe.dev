// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

class MsceNumerical : public Worker
{
private:
  const RMatrix<double> t_;
  const RMatrix<double> Nnu0_;
  const std::vector<RMatrix<double> > alpha_;
  const std::vector<RMatrix<double> > gamma_;
  const std::vector<RMatrix<double> > nu_;
  
  //positive number. The higher, the more accurate calculation
  const int innerSteps_;
  
  unsigned int nRows_;
  unsigned int nIntervals_;
  unsigned int nStages_;
  
  RVector<double> hazard_;
  RVector<double> lnSurvival_;
  
public:
  MsceNumerical(const NumericMatrix t,
                const NumericMatrix Nnu0,
                const std::vector<RMatrix<double> > alpha,
                const std::vector<RMatrix<double> > gamma,
                const std::vector<RMatrix<double> > nu,
                const int innerSteps,
                NumericVector hazard,
                NumericVector lnSurvival) :
    t_(t),
    Nnu0_(Nnu0),alpha_(alpha),gamma_(gamma), nu_(nu),
    innerSteps_(innerSteps),
    hazard_(hazard),lnSurvival_(lnSurvival)
  {
    nRows_ = t_.nrow();
    nIntervals_ = t_.ncol();
    nStages_=nu_.size();
    
    //
    //Check matrices
    //
    //Check dimensions
    if (nIntervals_<1) {
      Rcerr<<"ERROR: First argument (t) needs to have at least one column."<<
        std::endl;
      throw std::invalid_argument("Wrong matrix dimension.");
    }
    if (Nnu0_.ncol() != nIntervals_) {
      Rcerr<<"ERROR: All matrices have to have same number of columns!"<<
        std::endl;
      throw std::invalid_argument("Wrong matrix dimension.");
    }
    if ( (Nnu0_.nrow() != nRows_ && Nnu0_.nrow() !=1) ) {
      Rcerr<<"ERROR: All matrices need to have same number of rows"<<
        " or just a single row!"<<std::endl;
      throw std::invalid_argument("Wrong matrix dimension.");
    }
    for (unsigned int s=0; s<nStages_;s++) {
      if (alpha_[s].ncol() != nIntervals_ ||
          gamma_[s].ncol() != nIntervals_ || 
          nu_[s].ncol()    != nIntervals_) {
        Rcerr<<"ERROR: All matrices have to have same number of columns!"<<
          std::endl;
        throw std::invalid_argument("Wrong matrix dimension.");
      }
      if ( (alpha_[s].nrow() != nRows_ && alpha_[s].nrow() !=1) ||
           (gamma_[s].nrow() != nRows_ && gamma_[s].nrow() !=1) ||
           (nu_[s].nrow()    != nRows_ && nu_[s].nrow()    !=1) ) {
        Rcerr<<"ERROR: All matrices need to have same number of rows"<<
          " or just a single row!"<<std::endl;
        throw std::invalid_argument("Wrong matrix dimension.");
      }
    }
    // Check that t is non-negative, monotonously increasing and
    // that last interval is large enough to allow for finite differentiation
    for (unsigned int r=0; r<nRows_; r++) {
      if (t_(r,0)<0) {
        Rcerr<<"ERROR: Each element of t must be non-negative!"<<std::endl;
        throw std::range_error("Inadmissible value");
      }
      for(unsigned int c=1; c< nIntervals_-1; c++)
        if (t_(r,c)<t_(r,c-1) ) {
          Rcerr<<"ERROR: Each row of t must be monotonously increasing!"<<
            std::endl;
          throw std::range_error("Inadmissible value");
        }
      if ( (nIntervals_==1 && t_(r,0)<=1e-4) ||
           (nIntervals_>1  && t_(r,nIntervals_-1)<=t_(r,nIntervals_-2)+1e-4) ) {
        Rcerr<<"ERROR: In the last column t must be larger than 1e-4!"<<
          std::endl;
        throw std::range_error("Inadmissible value");
      }
    }
    // Check range of other parameters
    for (unsigned int r=0; r<Nnu0_.nrow(); r++)
      for(unsigned int i=0; i< nIntervals_; i++)
        if (Nnu0(r,i) <0) {
          Rcerr<<"ERROR: All elements of Nnu0 have to be non-negative!"<<
            std::endl;
          throw std::range_error("Inadmissible value");
        }
    for (unsigned int s=0; s<nStages_;s++)
      for (unsigned int r=0; r<nRows_; r++) {
        unsigned int rAlpha=r;
        unsigned int rGamma=r;
        unsigned int rNu=r;
        if ( alpha_[s].nrow() ==1)
          rAlpha =0;
        if ( gamma_[s].nrow() ==1)
          rGamma =0;
        if ( nu_[s].nrow() ==1)
          rNu =0;
        for(int unsigned i=0; i< nIntervals_; i++) {
          if (alpha_[s](rAlpha,i) < gamma_[s](rGamma,i)) {
            Rcerr<<"ERROR: Elements of alpha"<<
              " must be at least as large as gamma!"<<std::endl;
            throw std::range_error("Inadmissible value");
          }
          if (nu_[s](rNu,i) <0) {
            Rcerr<<"ERROR: All elements of nu have to be non-negative!"<<
              std::endl;
            throw std::range_error("Inadmissible value");
          }
        }
      }
    
    //
    // Check that innerSteps is positive
    //
    if (innerSteps<1) {
      Rcerr<<"ERROR: innerSteps must be a positive number!"<<std::endl;
      throw std::range_error("Inadmissible value");
    }
  }
  
  void operator()(std::size_t begin, std::size_t end)
  {
    //loop over rows (different Poisson cells)
    for (unsigned int r = begin; r < end; r++)
    {
      //number of leading zero time intervals
      unsigned int no0=0;
      while(no0<nIntervals_ && t_(r,no0)<1e-20)
        no0++;

      //no nonzero time-interval in the row
      if (no0 == nIntervals_) {
        hazard_[r]=0;
        lnSurvival_[r]=1;
        return;
      }
      //Not all model parameters are different for different rows
      int rNnu0=r;
      std::vector<int> rAlpha(nStages_,r);
      std::vector<int> rGamma(nStages_,r);
      std::vector<int> rNu(nStages_,r);
      if (Nnu0_.nrow()==1)
        rNnu0=0;
      for (unsigned int s=0; s<nStages_; s++) {
        if (alpha_[s].nrow()==1)
          rAlpha[s]=0;
        if (gamma_[s].nrow()==1)
          rGamma[s]=0;
        if (nu_[s].nrow()==1)
          rNu[s]=0;
      }
      //
      //calculate lnSurival at age age=t_(r,nIntervals_-1)
      //and at the same time calculate difference
      //dlnS=lnSurvival(age)-lnSurvival(age-1e-4)
      //
      std::vector<long double> w(nStages_,0.);
      //use the d* variables to calculate dlnS
      std::vector<long double> dw(nStages_,0.);
      long double lnS=0.;
      long double dlnS=0.;

      //walk through time steps
      for (int i = (int) nIntervals_-1; i >= (int) no0; i--)
      {
        double dt;
        double ddt;
        if (i==(int) no0)
          dt=t_(r,i) - 0;
        else
          dt=t_(r,i) - t_(r,i-1);

        if (dt<1e-15) //this would very likely yield numerical nonsense
          continue;
        
        dt/=innerSteps_;

        ddt=dt;
        if (i==(int) nIntervals_-1)
          ddt-=1e-4/innerSteps_;

        //piecewise constant paras.
        double Nnu0i=Nnu0_(rNnu0,i);
        std::vector<double> gammai(nStages_);
        std::vector<double> alphai(nStages_);
        std::vector<double> nui(nStages_);
        for (unsigned int s=0; s<nStages_; s++) {
          gammai[s]=gamma_[s](rGamma[s],i);
          alphai[s]=alpha_[s](rAlpha[s],i);
          nui[s]=nu_[s](rNu[s],i);
        }
        
        //walk through sub-time intervals
        unsigned int nS=nStages_-1;//abbreviation
        for (int j=0;j<innerSteps_;j++)
        {
          // and here, finally, the characteristic equations
          // use transformed values w such that initial condition w=0
          // but do not transform with alpha, i.e. w has no unit
          w[nS] -= (-alphai[nS] * w[nS] * w[nS] 
                    -(gammai[nS] -nui[nS]) * w[nS]
                    +nui[nS]  ) * dt;
         dw[nS] -= (-alphai[nS] * dw[nS] * dw[nS] 
                    -(gammai[nS] -nui[nS]) * dw[nS]
                    +nui[nS]  ) * ddt; //the same for age-1e-4

          for (int s=(int) nS -1;s>=0;s--) {
            w[s] -= (-alphai[s] * w[s] * w[s] 
                     -(gammai[s] +nui[s]*w[s+1]) * w[s]
                     -nui[s]*w[s+1]  ) * dt;
            dw[s]-= (-alphai[s] * dw[s] * dw[s] 
                     -(gammai[s] +nui[s]*dw[s+1]) * dw[s]
                     -nui[s]*dw[s+1]  ) * ddt; //the same for age-1e-4
          }
          lnS+= Nnu0i*w[0]*dt;
          //the same for age-1e-4 but calculating the difference
          dlnS+=Nnu0i*(dw[0]*ddt-w[0]*dt); 
        }//end for walk through sub-time intervals
      } //end for walk through time steps

      //2) Calculate hazard from finite difference
      lnSurvival_[r]= lnS;
      hazard_[r]=dlnS*1e4;
    }//end loop over rows
  }//end method operator()
  
};//end of class

//' Numerical solution of the Multi-Stage Clonal Expansion Model
//' 
//' This function aims to solve the general multi-stage model 
//' with piecewise constant parameters approximatively
//' by integrating the characteristic equations with Euler's method.
//' For sufficiently small time intervalls, this approximation often 
//' yields reasonable results.
//' Small time intervals can either be provided explicitly with many columns
//' in argument \code{t}.
//' An alternative is the optional parameter \code{innerSteps}.
//' @param t Each element in a row of \code{t} defines the endpoint of a 
//'        time interval.
//'        The first time interval starts at time \code{0}.
//'        The last element is the time point for which hazard and survival
//'        function are evaluated.
//'        Elements in a row have to be in monotonously increasing order.
//'        In order to achieve a different number of time intervals for
//'        different rows, rows may start with an arbitrary number of zeros
//'        (i.e. time intervals of length zero).
//' @param parameterList List of Matrices.
//'        Each list member has to be named. Allowed names are \code{Nnu0},
//'        \code{alphaX}, \code{gammaX}, and \code{nuX} where \code{X}
//'        can be any positive integer value.
//'        The number of stages is deduced from \code{nuX}
//'        with the highest \code{X}.
//'        Matrices \code{Nnu0} and successive \code{nuX} must be provided.
//'        Missing other matrices are assumed to be zero.
//'        For each matrix it has to hold that the number of columns 
//'        must be equal to the ones in \code{t}.
//'        The number of rows can either equal to the number of rows
//'        in \code{t}, or only one row is provided, 
//'        which then is applied to all rows of \code{t}.
//'        
//'        Values in matrices \code{Nnu0}, \code{alphaX}, \code{gammaX},
//'        \code{nuX} correspond to parameters for each time interval,
//'        see the figure and explanations in the package vignette.
//'        Here, \code{Nnu0} is the product of \eqn{N}{N} and \eqn{\nu_0}{nu0}
//'        and \eqn{\gamma_X}{gammaX} is defined by
//'        \eqn{\alpha_X-\beta_X}{alphaX-betaX}.
//' @param innerSteps Positive integer.
//'        To improve accuracy, each time interval is internally
//'        divided into \code{innerSteps} time intervals of equal length.
//'        Defaults to 1000.
//'        Note, however, that even in the limit of infinite innerSteps,
//'        there will always be a finite discrepancy to the exact result.
//' @return The output list contains all used arguments of the
//'        \code{parameterList} and vectors of the model results for
//'        hazard and logarithm of the survival function
//'        for each row of the input.
//' @details
//' \figure{MSCE.jpg}{Schematic depiction of the MSCE model.
//'         See the package vignette for details.}
//' @examples
//' t <-matrix(data=c(10,20,65,10,20,70),nrow=2,byrow=TRUE)
//' Nnu0 <- matrix(c(0.3,0.7,1),nrow = 1)
//' nu1  <- matrix(1e-6,nrow=1,ncol=3)
//' alpha1<- matrix(1,nrow=1,ncol=3)
//' gamma1<- matrix(c(0.13,0.13,0.13, 0.15,0.15,0.15),nrow=2,byrow=TRUE)
//' pars = list(Nnu0=Nnu0, nu1=nu1,alpha1=alpha1,gamma1=gamma1) 
//' 
//' msce_numerical(t,pars)
//' @seealso \code{\link{tsce}}
//' @export
// [[Rcpp::export]]
List msce_numerical(NumericMatrix t,
                    List parameterList,
                    int innerSteps=1000)
{
  //
  //List -> vectors of matrices
  //
  List ret = List::create();
  NumericMatrix Nnu0 = parameterList["Nnu0"];
  ret.push_back(Nnu0,"Nnu0");

  std::vector<RMatrix<double> > nu;
  std::vector<RMatrix<double> > alpha;
  std::vector<RMatrix<double> > gamma;

  NumericMatrix empty(1,t.ncol());
  for (int s=1;; s++) {
    //nu
    std::string nuName="nu"+ std::to_string(s);
    if (!parameterList.containsElementNamed(nuName.c_str())) {
      if (nu.size()==0) {
        Rcerr<<"ERROR: At least Nnu0 and nu1 must be provided"<<
          " in parameter list."<<std::endl;
        throw std::invalid_argument("Argument missing.");
      }
      break;
    }
    NumericMatrix nus=parameterList[nuName];
    nu.push_back(RMatrix<double>(nus));
    ret.push_back(nus,nuName);
    //alpha
    std::string alphaName="alpha"+ std::to_string(s);
    if (parameterList.containsElementNamed(alphaName.c_str())) {
      NumericMatrix alphas=parameterList[alphaName];
      alpha.push_back(RMatrix<double>(alphas));
      ret.push_back(alphas,alphaName);
    }
    else {
      alpha.push_back(RMatrix<double>(empty));
      ret.push_back(empty,alphaName);
    }
    //gamma
    std::string gammaName="gamma"+ std::to_string(s);
    if (parameterList.containsElementNamed(gammaName.c_str())) {
      NumericMatrix gammas=parameterList[gammaName];
      gamma.push_back(RMatrix<double>(gammas));
      ret.push_back(gammas,gammaName);
    }
    else {
     gamma.push_back(RMatrix<double>(empty));
     ret.push_back(empty,gammaName);
    }
  }
  
  NumericVector hazard(t.nrow(),-1.0);
  NumericVector lnSurvival(t.nrow(),1.0);
  MsceNumerical msce(t,Nnu0,alpha,gamma,nu,innerSteps,hazard,lnSurvival);
  
  for (int i=0; i<t.rows(); i+=100)
  {
    Rcpp::checkUserInterrupt();
    parallelFor(i,std::min(t.rows(),i+100),msce);
  }
  ret.push_back(hazard,"hazard");
  ret.push_back(lnSurvival,"lnSurvival");
  return ret;
}
