#include <cassert>
#include <cstdlib>
#include <cfloat>
#include <cmath>
#include <R.h>
#include <fstream>
#include "lasvdgp.hpp"
#include "exceptions.hpp"
#include "cpputil.hpp"
#ifdef _OPENMP
#include <omp.h>
#endif
extern "C"{
#include "matrix.h"
#include "linalg.h"
#include "maximinLHS.h"
}

#undef NAN
#define NAN R_NaN

using std::free;

static const double dab1 = 1.5;
static const double sqreps = 1.490116119384766E-8;
static double gab[2] = {0.0, 0.0};

static void genStarts(int numstarts, int dim, double logdmin, double logdmax,
		      double loggmin, double loggmax, double** dstarts, double* gstarts)
{
  int i, j, din;
  double **results;
  double tval, ldrange, lgrange;
  din = dim + 1;
  results = new_matrix(numstarts,din);
  genmaximinLHS(numstarts,din,results);
  ldrange = logdmax - logdmin;
  lgrange = loggmax - loggmin;
  for(i = 0; i < numstarts; ++i)
  {
    for(j = 0; j < dim; ++j)
    {
	tval = results[i][j];
	dstarts[i][j] = exp(logdmin+tval*ldrange);
    }
    tval = results[i][dim];
    gstarts[i] = exp(loggmin+tval*lgrange);
  }
  delete_matrix(results);
}
void jmlelasvdGPms(lasvdGP *lasvdgp, unsigned int numstarts,
		   unsigned int maxit, unsigned int verb)
{
  double dab[2], grange[2]={sqreps,lasvdgp->gstart};
  double dstart, ddmin, ddmax, dab2;
  double **dstarts, *gstarts;
  double *optd, optg, optllik, llik;
  double *dmin, *dmax;
  int dim, dits, gits, dconv;
  unsigned int i, j;
  getDs(lasvdgp->gpseps[0]->X,lasvdgp->n0,lasvdgp->m, &dstart, &ddmin, &ddmax,
	&dab2);
  dab[0] = dab1;
  dab[1] = dab2;
  /* get the start points */
  dim = lasvdgp-> m;
  dstarts = new_matrix(numstarts,dim);
  gstarts = new_vector(numstarts);
  genStarts(numstarts, dim, log(ddmin), log(ddmax),
	    log(grange[0]), log(grange[1]),
	    dstarts, gstarts);
  optd = new_vector(dim);
  dmin = new_const_vector(ddmin,lasvdgp->m);
  dmax = new_const_vector(ddmax,lasvdgp->m);
  optg = gstarts[0];
  for(i=0; i<lasvdgp->nbas; ++i)
  {
    optllik = -DBL_MAX;
    for(j = 0; j < numstarts; ++j)
    {
      try{
	newparamsGPsep(lasvdgp->gpseps[i], dstarts[j], gstarts[j]);
	myjmleGPsep(lasvdgp->gpseps[i], maxit, dmin, dmax,
		    grange, dab, gab, verb, &dits,
		    &gits, &dconv);
	llik = llikGPsep(lasvdgp->gpseps[i], dab, gab);
      }
      catch(cholException& e){
	llik = -DBL_MAX;
      }
      if(llik > optllik)
      {
	optllik = llik;
	dupv(optd,lasvdgp->gpseps[i]->d,dim);
	optg = lasvdgp->gpseps[i]->g;
      }
    }
    if(optllik <= -DBL_MAX){	// all starts throw exceptions, retrieve the exception
      dupv(optd,lasvdgp->gpseps[i]->d, dim);
      optg = lasvdgp->gpseps[i]->g;
    }
    try{
      newparamsGPsep(lasvdgp->gpseps[i], optd, optg);
    }
    catch(cholException& e){
      free(gstarts);
      free(optd);
      free(dmin);
      free(dmax);
      delete_matrix(dstarts);
      throw e;
    }
  }
  lasvdgp->hasfitted = 1;
  free(gstarts);
  free(optd);
  free(dmin);
  free(dmax);
  delete_matrix(dstarts);
}

void iterlasvdGPms(lasvdGP* lasvdgp, unsigned int resvdThres,
		   unsigned int every, unsigned int numstarts,
		   unsigned int maxit, unsigned int verb)
{
  int i, n0, nn, niter, nadd, nrem;
  nn = lasvdgp -> nn;
  n0 = lasvdgp -> n0;
  nadd = lasvdgp -> nadd;
  niter = ceil_divide(nn-n0,nadd);
  for(i = 1; i <= niter; ++i)
  {
    n0 = lasvdgp->n0;
    nrem = nn - n0;
    nadd = lasvdgp-> nadd;
    nadd = nadd<nrem ? nadd : nrem;
    lasvdgp -> nadd = nadd;
    selectNewPoints(lasvdgp);
    if(lasvdgp -> nappsvd >= resvdThres)
    {
      renewlasvdGP(lasvdgp);
      jmlelasvdGPms(lasvdgp, numstarts, maxit, verb);
      continue;
    }
    if(i % every == 0)
	jmlelasvdGP(lasvdgp, maxit, verb);
  }
  /* finishing off */
  if(lasvdgp->nappsvd>0)
  {
    renewlasvdGP(lasvdgp);
    jmlelasvdGPms(lasvdgp, numstarts, maxit, verb);
    return;
  }
  if(lasvdgp->hasfitted == 0)
    jmlelasvdGP(lasvdgp, maxit, verb);
}
void lasvdGPms_worker(double** X0, double **design, double **resp,
		      unsigned int M, unsigned int N, unsigned int m,
		      unsigned int tlen, unsigned int nn, unsigned int n0,
		      unsigned int nfea, unsigned int nsvd, unsigned int nadd,
		      double frac, double gstart, unsigned int resvdThres,
		      unsigned int every, unsigned int numstarts,
		      unsigned int maxit, unsigned int verb, char* errlog,
		      double **pmean, double **ps2, int *flags)
{
  unsigned int i;
  double *xpred;
  lasvdGP *lasvdgp = NULL;
  for(i = 0; i < M; ++i)
  {
      xpred = X0[i];
      try{
	lasvdgp = newlasvdGP(xpred, design, resp, N, m, tlen, nn, n0,
			     nfea, nsvd, nadd, frac, gstart);
	jmlelasvdGPms(lasvdgp, numstarts, maxit, verb);
	iterlasvdGPms(lasvdgp, resvdThres, every, numstarts, maxit, verb);
	predlasvdGP(lasvdgp, pmean[i], ps2[i]);
	flags[i] = 0;
      }
      catch(cholException& e){
	flags[i] = Chol;
	fill_vector(pmean[i], NAN, tlen);
	fill_vector(ps2[i], NAN, tlen);
	if(*errlog != '\0'){
	  std::ofstream ofs(errlog, std::ofstream::out | std::ofstream::app);
	  writeVector(ofs,"xpred",xpred,m);
	  ofs<<"\n"<<e;
	  ofs.close();
	}
      }
      catch(svdException& e){
	flags[i] = SVD;
	fill_vector(pmean[i], NAN, tlen);
	fill_vector(ps2[i], NAN, tlen);
	if(*errlog != '\0'){
	  std::ofstream ofs(errlog, std::ofstream::out | std::ofstream::app);
	  writeVector(ofs,"xpred",xpred,m);
	  ofs<<"\n"<<e;
	  ofs.close();
	}
      }
      catch(optException& e){
	flags[i] = Opt;
	fill_vector(pmean[i], NAN, tlen);
	fill_vector(ps2[i], NAN, tlen);
	if(*errlog != '\0'){
	  std::ofstream ofs(errlog, std::ofstream::out | std::ofstream::app);
	  writeVector(ofs,"xpred",xpred,m);
	  ofs<<"\n"<<e;
	  ofs.close();
	}
      }
      if(lasvdgp) deletelasvdGP(lasvdgp);
  }
}
void lasvdGPms_omp(double** X0, double **design, double **resp,
		   unsigned int M, unsigned int N, unsigned int m,
		   unsigned int tlen, unsigned int nn, unsigned int n0,
		   unsigned int nfea, unsigned int nsvd, unsigned int nadd,
		   double frac, double gstart, unsigned int resvdThres,
		   unsigned int every, unsigned int numstarts,unsigned int maxit,
		   unsigned int verb, char* errlog, unsigned int nthread,
		   double **pmean, double **ps2, int *flags)
{
  unsigned int mxth;
#ifdef _OPENMP
  mxth = omp_get_max_threads();
#else
  mxth = 1;
#endif
  if(nthread > mxth)
  {
    Rprintf("NOTE: omp.threads(%d) > max(%d), using %d\n",nthread, mxth, mxth);
    nthread = mxth;
  }
#ifdef _OPENMP
#pragma omp parallel num_threads(nthread)
  {
    unsigned int i, start, step;
    double *xpred;
    lasvdGP *lasvdgp = NULL;
    start = omp_get_thread_num();
    step  = nthread;
#else
    unsigned int i, start, step;
    double *xpred;
    lasvdGP *lasvdgp = NULL;
    start = 0; step = 1;
#endif
    for(i = start; i < M; i+=step)
    {
      xpred = X0[i];
      try{
	lasvdgp = newlasvdGP(xpred, design, resp, N, m, tlen, nn, n0,
			     nfea, nsvd, nadd, frac, gstart);
	jmlelasvdGPms(lasvdgp, numstarts, maxit, verb);
	iterlasvdGPms(lasvdgp, resvdThres, every, numstarts, maxit, verb);
	predlasvdGP(lasvdgp, pmean[i], ps2[i]);
	flags[i] = 0;
      }
      catch(cholException& e){
	flags[i] = Chol;
	fill_vector(pmean[i], NAN, tlen);
	fill_vector(ps2[i], NAN, tlen);
	if(*errlog != '\0'){
#ifdef _OPENMP
#pragma omp critical
	  {
#endif
	  std::ofstream ofs(errlog, std::ofstream::out | std::ofstream::app);
	  writeVector(ofs,"xpred",xpred,m);
	  ofs<<"\n"<<e;
	  ofs.close();
#ifdef _OPENMP
	  }
#endif
	}
      }
      catch(svdException& e){
	flags[i] = SVD;
	fill_vector(pmean[i], NAN, tlen);
	fill_vector(ps2[i], NAN, tlen);
	if(*errlog != '\0'){
#ifdef _OPENMP
#pragma omp critical
	  {
#endif
	  std::ofstream ofs(errlog, std::ofstream::out | std::ofstream::app);
	  writeVector(ofs,"xpred",xpred,m);
	  ofs<<"\n"<<e;
	  ofs.close();
#ifdef _OPENMP
	  }
#endif
	}
      }
      catch(optException& e){
	flags[i] = Opt;
	fill_vector(pmean[i], NAN, tlen);
	fill_vector(ps2[i], NAN, tlen);
	if(*errlog != '\0'){
#ifdef _OPENMP
#pragma omp critical
	  {
#endif
	  std::ofstream ofs(errlog, std::ofstream::out | std::ofstream::app);
	  writeVector(ofs,"xpred",xpred,m);
	  ofs<<"\n"<<e;
	  ofs.close();
#ifdef _OPENMP
	  }
#endif
	}
      }
      if(lasvdgp) deletelasvdGP(lasvdgp);
    }
#ifdef _OPENMP
  }
#endif
}
extern "C"{
  void lasvdGPms_R(double *X0_, double *design_, double *resp_, int* M_,
		   int *N_, int *m_, int *tlen_, int *nn_, int *n0_,
		   int *nfea_, int* nsvd_, int *nadd_, double *frac_,
		   double *gstart_, int *resvdThres_, int *every_,
		   int *numstarts_, int *maxit_, int *verb_, char** errlog_,
		   double *pmean_, double *ps2_, int* flags_)
  {
    double **X0, **design, **resp;
    double **pmean, **ps2;
    X0 = new_matrix_bones(X0_,*M_, *m_);
    design = new_matrix_bones(design_,*N_,*m_);
    resp = new_matrix_bones(resp_,*N_, *tlen_);
    pmean = new_matrix_bones(pmean_,*M_,*tlen_);
    ps2 = new_matrix_bones(ps2_,*M_,*tlen_);
    lasvdGPms_worker(X0,design,resp,*M_, *N_, *m_, *tlen_, *nn_, *n0_,
		     *nfea_, *nsvd_, *nadd_, *frac_, *gstart_, *resvdThres_,
		     *every_, *numstarts_, *maxit_, *verb_, *errlog_,
		     pmean, ps2, flags_);
    free(X0);
    free(design);
    free(resp);
    free(pmean);
    free(ps2);
  }
  void lasvdGPmsomp_R(double *X0_, double *design_, double *resp_, int* M_,
		      int *N_, int *m_, int *tlen_, int *nn_, int *n0_,
		      int *nfea_, int* nsvd_, int *nadd_, double *frac_,
		      double *gstart_, int *resvdThres_, int *every_,
		      int *numstarts_, int *maxit_, int *verb_, char **errlog_,
		      int *nthread_, double *pmean_, double *ps2_, int* flags_)
  {
    double **X0, **design, **resp;
    double **pmean, **ps2;
    X0 = new_matrix_bones(X0_,*M_, *m_);
    design = new_matrix_bones(design_,*N_,*m_);
    resp = new_matrix_bones(resp_,*N_, *tlen_);
    pmean = new_matrix_bones(pmean_,*M_,*tlen_);
    ps2 = new_matrix_bones(ps2_,*M_,*tlen_);
    lasvdGPms_omp(X0,design,resp,*M_, *N_, *m_, *tlen_, *nn_, *n0_,
		  *nfea_, *nsvd_, *nadd_, *frac_, *gstart_, *resvdThres_,
		  *every_, *numstarts_, *maxit_, *verb_, *errlog_,
		  *nthread_, pmean, ps2, flags_);
    free(X0);
    free(design);
    free(resp);
    free(pmean);
    free(ps2);
  }
}
