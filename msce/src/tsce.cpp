// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

class TsceAnalytical : public Worker
{
private:
  const RMatrix<double> t_;
  const RMatrix<double> Nnu0_;
  const RMatrix<double> alpha_;
  const RMatrix<double> gamma_;
  const RMatrix<double> nu1_;
  
  unsigned int nRows_;
  unsigned int nIntervals_;
  
  RVector<double> hazard_;
  RVector<double> lnSurvival_;
  
public:
  TsceAnalytical(const NumericMatrix t,
                 const NumericMatrix Nnu0,
                 const NumericMatrix alpha,
                 const NumericMatrix gamma,
                 const NumericMatrix nu1,
                 NumericVector hazard,
                 NumericVector lnSurvival) :
    t_(t),Nnu0_(Nnu0),alpha_(alpha),gamma_(gamma),nu1_(nu1),
    hazard_(hazard),lnSurvival_(lnSurvival)
  {
    nRows_ = t_.nrow();
    nIntervals_ = t_.ncol();
    
    //Check matrices
    if (nIntervals_<1) {
      Rcerr<<"ERROR: First argument (t) needs to have at least one column."<<
        std::endl;
      throw std::invalid_argument("Wrong matrix dimension.");
    }
    if (Nnu0_.ncol() != nIntervals_ || alpha_.ncol() != nIntervals_
          || gamma_.ncol() != nIntervals_ || nu1_.ncol() != nIntervals_) {
      Rcerr<<"ERROR: All matrices have to have same number of rows!"<<
        std::endl;
      throw std::invalid_argument("Wrong matrix dimension.");
    }
    if ( (Nnu0_.nrow() != nRows_ && Nnu0_.nrow() !=1 ) ||
         (alpha_.nrow() != nRows_ && alpha_.nrow() !=1) ||
         (gamma_.nrow() != nRows_ && gamma_.nrow() !=1) ||
         (nu1_.nrow() != nRows_ && nu1_.nrow() !=1) ) {
      Rcerr<<"ERROR: All matrices need to have same number of rows"<<
        " or just a single row!"<<std::endl;
      throw std::invalid_argument("Wrong matrix dimension.");
    }
    for (unsigned int r=0; r<t_.nrow(); r++)
    {
      if (t_(r,0)<0) {
        Rcerr<<"ERROR: Each element of t must be non-negative!"<<std::endl;
        throw std::range_error("Inadmissible value");
      }
      for(unsigned int c=1; c< nIntervals_; c++)
        if (t_(r,c)<t_(r,c-1) ) {
          Rcerr<<"ERROR: Each row of t must be monotonously increasing!"<<
            std::endl;
          throw std::range_error("Inadmissible value");
        }
    }

    for (unsigned int r=0; r<Nnu0_.nrow(); r++)
      for(unsigned int i=0; i< nIntervals_; i++)
        if (Nnu0(r,i) <0) {
          Rcerr<<"ERROR: All elements of Nnu0 have to be non-negative!"<<
            std::endl;
          throw std::range_error("Inadmissible value");
        }
    for (unsigned int r=0; r<nRows_; r++) {
      unsigned int rAlpha=r;
      unsigned int rGamma=r;
      if ( alpha_.nrow() ==1)
        rAlpha =0;
      if ( gamma_.nrow() ==1)
        rGamma =0;
      for(int unsigned i=0; i< nIntervals_; i++)
        if (alpha_(rAlpha,i) <= gamma_(rGamma,i)) {
          Rcerr<<"ERROR: Elements of alpha"<<
            " must be at least as large as gamma!"<<std::endl;
          throw std::range_error("Inadmissible value");
        }
    }
    for (unsigned int r=0; r<nu1_.nrow(); r++)
      for(unsigned int i=0; i< nIntervals_; i++)
        if (nu1_(r,i) <=0) {
          Rcerr<<"ERROR: All elements of nu1 have to be positive!"<<std::endl;
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
      
      
      //next produce vectors for the non-zero time steps
      int nt=nIntervals_-no0;
      std::vector<double> ti(nt+1);
      std::vector<double> Xi(nt);
      std::vector<double> gi(nt);
      std::vector<double> qi(nt);
      std::vector<double> mi(nt);
      
      ti[0]=0;
      for (int i=0; i< nt; i++)
        ti[i+1]=t_(r,i+no0);
      
      int rNnu0=r;
      int rAlpha=r;
      int rGamma=r;
      int rNu1=r;
      if (Nnu0_.nrow()==1)
        rNnu0=0;
      if (alpha_.nrow()==1)
        rAlpha=0;
      if (gamma_.nrow()==1)
        rGamma=0;
      if (nu1_.nrow()==1)
        rNu1=0;
        
      for (int i=0; i< nt; i++)
      {
        Xi[i]=Nnu0_(rNnu0,i+no0)*nu1_(rNu1,i+no0);
        gi[i]=gamma_(rGamma,i+no0)-nu1_(rNu1,i+no0);
        qi[i] = 0.5*(std::sqrt(
          gi[i]*gi[i]+4.0*alpha_(rAlpha,i+no0)*nu1_(rNu1,i+no0))
                       -gi[i]);
        mi[i]=nu1_(rNu1,i+no0)/nu1_(rNu1,no0);
      }
      
      //
      //compute the lnSurvival, hazard
      //
      int i;
      double wi=0;
      double wipr=-qi[nt-1]*(gi[nt-1]+qi[nt-1]);
      long double lnS=0.;
      long double haz =0.;

      for(i=nt-1;i>=0;i--){
        double gqi=gi[i]+qi[i];
        double dt =ti[i]-ti[i+1];
        double exp1i=exp(-gqi*dt);
        double exp2i=exp(qi[i]*dt);
        
        double qwexp1i=(qi[i]-wi)*exp1i;
        double gqwexp2i=(wi+gqi)*exp2i;
        double fi=qwexp1i+gqwexp2i;
        double qgqi=qi[i]*gqi;
        lnS+=Xi[i]/qgqi*mi[i]*log((gqi+qi[i])/fi);
        haz+=Xi[i]/qi[i]/gqi*mi[i]/fi*wipr*(-exp1i+exp2i);
        if(i>0){ 
          double boundfactor=qi[i-1]*(gi[i-1]+qi[i-1])*mi[i]/(qgqi*mi[i-1]);
          wi=boundfactor/fi*
            (-gqi*qwexp1i+qi[i]*gqwexp2i);
          wipr*=boundfactor/(fi*fi)*
            (gqi+qi[i])*(gqi+qi[i])*exp1i*exp2i;
        }
      }

      lnSurvival_[r]= lnS;
      hazard_[r] = haz;
    }//end loop over rows
  }//end method operator()
  
};//end of class

//' Exact solution of the Two-Stage Clonal Expansion Model
//' 
//' For piecewise constant parameters \code{tsce(t,parameterList)}
//' returns the exact hazard and logarithm of the survival function
//' of the Two-Stage Clonal Expansion Model.
//' All arguments are matrices.
//' Evaluation is performed separately for each row.
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
//'        \code{alpha}, \code{gamma}, and \code{nu1}.
//'        Matrices \code{Nnu0} and \code{nu1} must be provided.
//'        If \code{alpha} or \code{gamma} are missing,
//'        they are assumed to be zero.
//'        For each matrix it has to hold that the number of columns 
//'        must be equal to the ones in \code{t}.
//'        The number of rows can either equal to the number of rows in
//'        \code{t}, or only one row is provided,
//'        which then is applied to all rows of \code{t}.
//'        
//'        Values in matrices \code{Nnu0}, \code{alpha}, \code{gamma},
//'        \code{nu1} correspond to parameters for each time interval,
//'        see the figure and explanations in the package vignette.
//'        Here, \code{Nnu0} is the product of \eqn{N}{N} and \eqn{\nu_0}{nu0}
//'        and \eqn{\gamma}{gamma} is defined by
//'        \eqn{\alpha-\beta}{alpha-beta}.
//' @details
//' \figure{TSCE.jpg}{Schematic depiction of the TSCE model.
//'         See the package vignette for details.}
//' @examples
//' t <-matrix(data=c(10,20,65,10,20,70),nrow=2,byrow=TRUE)
//' Nnu0 <- matrix(c(0.3,0.7,1),nrow = 1)
//' alpha<- matrix(1,nrow=1,ncol=3)
//' gamma<- matrix(c(0.13,0.13,0.13, 0.15,0.15,0.15),nrow=2,byrow=TRUE)
//' nu1  <- matrix(1e-6,nrow=1,ncol=3)
//' pars = list(Nnu0=Nnu0, alpha=alpha,gamma=gamma,nu1=nu1) 
//' 
//' tsce(t,pars)
//' @seealso \code{\link{msce_numerical}}
//' @export
// [[Rcpp::export]]
List tsce(NumericMatrix t,
          List parameterList)
{
  NumericMatrix Nnu0 = parameterList["Nnu0"];
  NumericMatrix alpha = parameterList["alpha"];
  NumericMatrix gamma = parameterList["gamma"];
  NumericMatrix nu1 = parameterList["nu1"];
  
  NumericVector hazard(t.nrow(),-1.0);
  NumericVector lnSurvival(t.nrow(),1.0);
  TsceAnalytical tsce(t,Nnu0,alpha,gamma,nu1,hazard,lnSurvival);
  
  for (int i=0; i<t.rows(); i+=100)
  {
    Rcpp::checkUserInterrupt();
    parallelFor(i,std::min(t.rows(),i+100),tsce);
  }
  
  return List::create( 
    Named("Nnu0") = Nnu0,
    _["alpha"] = alpha,
    _["gamma"] = gamma,
    _["nu1"] = nu1,
    _["hazard"] = hazard,
    _["lnSurvival"] = lnSurvival );
}
