#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::export]]
List eva_four_elements_omit(
    NumericMatrix EE_ctrl,NumericMatrix tt_ctrl,IntegerVector nvisits_ctrl,
    NumericMatrix EE_case,NumericMatrix tt_case,IntegerVector nvisits_case,
    NumericVector add_thres){
  const int nctrl=EE_ctrl.nrow();
  const int ncase=EE_case.nrow();
  int ii,jj;
  
  std::vector<double> thres_std=as<std::vector<double> >(EE_ctrl);
  std::vector<double> thres_std_add=as<std::vector<double> >(add_thres);
  std::vector<double> thres_std_case=as<std::vector<double> >(EE_case);
  thres_std.insert(thres_std.end(),thres_std_case.begin(),thres_std_case.end());
  thres_std.insert(thres_std.end(),thres_std_add.begin(),thres_std_add.end());
  std::sort(thres_std.begin(),thres_std.end());
  NumericVector thres(thres_std.begin(),thres_std.end());
  thres=sort_unique(thres);
  const int nthres=thres.length();
  
  //NumericVector Emax_ctrl(nctrl);
  //NumericVector Emax_case(ncase);
  
  int nFP/*sumsign_ctrl*/,nTP/*sumsign_case*/;
  double sumtime_ctrl,sumtime_case;
  NumericVector all_nFP(nthres),all_nTP(nthres),all_sumtime_ctrl(nthres),all_sumtime_case(nthres);
  int idx;double rho;bool signal;
  for(idx=0;idx<nthres;idx++){
    nFP=0;nTP=0;sumtime_ctrl=0;sumtime_case=0;
    rho=thres[idx];
    for(ii=0;ii<nctrl;ii++){
      signal=false;
      for(jj=0;jj<nvisits_ctrl(ii);jj++){
        if(EE_ctrl(ii,jj)>=rho){
          signal=true;
          nFP++;
          sumtime_ctrl+=tt_ctrl(ii,jj);
          break;
        }
      }
    }
    for(ii=0;ii<ncase;ii++){
      signal=false;
      for(jj=0;jj<nvisits_case(ii);jj++){
        if(EE_case(ii,jj)>=rho){
          signal=true;
          nTP++;
          sumtime_case+=tt_case(ii,jj);
          break;
        }
      }
    }
    all_nFP(idx)=nFP;
    all_nTP(idx)=nTP;
    all_sumtime_case(idx)=sumtime_case;
    all_sumtime_ctrl(idx)=sumtime_ctrl;
  }
  
  List result=Rcpp::List::create(Rcpp::Named("thres") = thres,
                                 Rcpp::Named("nFP") = all_nFP,
                                 Rcpp::Named("nTP") = all_nTP,
                                 Rcpp::Named("FPR") = all_nFP/double(nctrl),
                                 Rcpp::Named("TPR") = all_nTP/double(ncase),
                                 Rcpp::Named("sumtime_ctrl") = all_sumtime_ctrl,
                                 Rcpp::Named("sumtime_case") = all_sumtime_case);
  return(result);
}

//[[Rcpp::export]]
List eva_four_elements_impute(
    NumericMatrix EE_ctrl,NumericMatrix tt_ctrl,IntegerVector nvisits_ctrl,IntegerVector imputetime_ctrl,
    NumericMatrix EE_case,NumericMatrix tt_case,IntegerVector nvisits_case,IntegerVector imputetime_case,
    NumericVector add_thres){
  const int nctrl=EE_ctrl.nrow();
  const int ncase=EE_case.nrow();
  int ii,jj;
  
  std::vector<double> thres_std=as<std::vector<double> >(EE_ctrl);
  std::vector<double> thres_std_add=as<std::vector<double> >(add_thres);
  std::vector<double> thres_std_case=as<std::vector<double> >(EE_case);
  thres_std.insert(thres_std.end(),thres_std_case.begin(),thres_std_case.end());
  thres_std.insert(thres_std.end(),thres_std_add.begin(),thres_std_add.end());
  std::sort(thres_std.begin(),thres_std.end());
  NumericVector thres(thres_std.begin(),thres_std.end());
  thres=sort_unique(thres);
  const int nthres=thres.length();
  
  //NumericVector Emax_ctrl(nctrl);
  //NumericVector Emax_case(ncase);
  
  int nFP/*sumsign_ctrl*/,nTP/*sumsign_case*/;
  double sumtime_ctrl,sumtime_case;
  NumericVector all_nFP(nthres),all_nTP(nthres),all_sumtime_ctrl(nthres),all_sumtime_case(nthres);
  int idx;double rho;bool signal;
  for(idx=0;idx<nthres;idx++){
    nFP=0;nTP=0;sumtime_ctrl=0;sumtime_case=0;
    rho=thres[idx];
    for(ii=0;ii<nctrl;ii++){
      signal=false;
      for(jj=0;jj<nvisits_ctrl(ii);jj++){
        if(EE_ctrl(ii,jj)>=rho){
          signal=true;
          nFP++;
          sumtime_ctrl+=tt_ctrl(ii,jj);
          break;
        }
      }
      if(!signal)sumtime_ctrl+=imputetime_ctrl(ii);
    }
    for(ii=0;ii<ncase;ii++){
      signal=false;
      for(jj=0;jj<nvisits_case(ii);jj++){
        if(EE_case(ii,jj)>=rho){
          signal=true;
          nTP++;
          sumtime_case+=tt_case(ii,jj);
          break;
        }
      }
      if(!signal)sumtime_case+=imputetime_case(ii);
    }
    all_nFP(idx)=nFP;
    all_nTP(idx)=nTP;
    all_sumtime_case(idx)=sumtime_case;
    all_sumtime_ctrl(idx)=sumtime_ctrl;
  }
  
  List result=Rcpp::List::create(Rcpp::Named("thres") = thres,
                                 Rcpp::Named("nFP") = all_nFP,
                                 Rcpp::Named("nTP") = all_nTP,
                                 Rcpp::Named("FPR") = all_nFP/double(nctrl),
                                 Rcpp::Named("TPR") = all_nTP/double(ncase),
                                 Rcpp::Named("sumtime_ctrl") = all_sumtime_ctrl,
                                 Rcpp::Named("sumtime_case") = all_sumtime_case);
  return(result);
}
