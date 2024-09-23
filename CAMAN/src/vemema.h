#ifndef _DLL_H_
#define _DLL_H_
#include <errno.h>
#include <vector>
#include <ctype.h>
//#include <iomanip.h>

# define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

using namespace std;




  
///	C-Wrapper for using in R-code.


  
//sexp__cdecl CalculateVemema(sexp a, sexp b);

//SEXP add (const char* name);
//SEXP init (const char* name);
//extern "C" __declspec(dllexport) void __cdecl CalculateVemema(const char* initFile);
extern "C"{
class VEMEMA
{
 private:

    //int N; /* size of the sample*/

 //  // vector<vector<double> > sample;
//    vector<vector<double> > lambda;
//    vector<vector<double> > var;
//
//    vector<double> prob;
//    vector<double> corr;


    //double mean(vector<double>& v);
    
    double variance(vector<double>& v);
    double variance(vector<double>& v, double m);
    double correlation(vector<double>& vec1, vector<double>& vec2, double m1, double m2, double v1, double v2);

    void get_variance();

    void get_corr();

    double normal(double k, double l, double v);

    double normal_mult(double x1, double x2, double m1, double m2, double v1, double v2, double cor);

    double density(int i, int j);
    double density_meta(int i, int j);
 double density_uni(int i, int j);
    void get_dens(vector<double>& s, vector<double>& l, vector<vector<double> >& d);
    void get_dens(vector<vector<double> >& l, vector<vector<double> >& d);
    void get_dens1(vector<vector<double> >& l, vector<vector<double> >& d);
        void get_dens_meta(vector<vector<double> >& l, vector<vector<double> >& d);

    double mix_den(int i, vector<double>& p, vector<vector<double> >& dens);
    double mix_den(int i);
        double mix_den_meta(int i);
  double mix_den_uni(int i);
    double likelihood();
    double likelihood_meta();
  double likelihood_uni();
    void gradient(vector<vector<double> >& dens, vector<double>& p, vector<double>& g);

    //void sort(vector<double>& vec, vector<double>& sort_vec);

    void get_min(vector<double>& v, double& min, int& i_min);
    void get_max_min(vector<double>& v, vector<double>& p, int& i_max, int& i_min, double& v_max);
    void get_max_min(vector<double>& v, double& max, double& min);

    void get_start_values(int start_nr_cl, vector<double>& s, vector<double>& l, vector<double>& p);
    void get_start_values(int start_nr_cl, vector<vector<double> >& L, vector<double>& p);

    void get_ht(int i, int j, vector<vector<double> >& dens, vector<double>& p, vector<double>& ht);

    double stepsize(vector<double>& ht, vector<vector<double> >& dens, vector<double>& p);

    void grid(vector<vector<double> >& v_in, vector<vector<double> >& v_out);

    void vem(int col, int start_nr_cl, double tol, vector<double>& l_out, vector<double>& p_out);
    
    
 public:
//SEXP mean(SEXP ra);
 double mean(vector<double>& v);
   void initialize(const char* name);//

     vector<vector<double> > vem_bivariate(int start_nr_cl, double tol);
	  vector<vector<double> > vem_bivariate_meta(int start_nr_cl, double tol);
	  vector<vector<double> > vem_bivariate_grad(int start_nr_cl, double tol);
    //void vem_bivariate(int start_nr_cl, double tol, vector<vector<double> >& l_out, vector<double>& p_out); 
    vector<vector<double> > do_vem(int start_nr_cl, double tol);
vector<vector<double> > f1();
vector<vector<double> > f1_meta();
    vector<vector<double> > ema_uni(double tol);
    vector<vector<double> > ema_versh(double tol);
    vector<vector<double> > ema_versh_sh(double tol);
    vector<vector<double> > ema_univariat(double tol);
    vector<vector<double> > ema_versh_start(double tol);
    vector<vector<double> > ema_versh_meta(double tol);
    vector<vector<double> > ema_meta(double tol);
     vector<vector<double> > ema_meta_sh(double tol);
    vector<double> ema_ind(double tol);
    vector<double> ema_ind_sh(double tol);
     vector<double> ema_ind_uni(double tol);
     vector<double> ema_ind_meta(double tol);
     vector<double> ema_ind_meta_sh(double tol);
     vector<double> ema_ind_start(double tol);
      vector<double> ema_ind_meta_start(double tol);
    vector<vector<double> >ema_bi(double tol);



};

}

//double addiere (SEXP a, SEXP b);
#endif

