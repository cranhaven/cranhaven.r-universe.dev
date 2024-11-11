#include <cassert>
#include <cstdlib>
#include <R.h>
#include "gpseplm.hpp"
#include "exceptions.hpp"
using std::free;
extern "C"{
#include "matrix.h"
}

unsigned int NGPsepLm = 0;
GPsepLm **gplms = NULL;

unsigned int get_gpsepLm(void)
{
  unsigned int i;
  if(NGPsepLm == 0)
  {
    assert(gplms == NULL);
    gplms = (GPsepLm**) malloc(sizeof(GPsepLm*));
    gplms[0] = NULL;
    NGPsepLm = 1;
    return 0;
  }
  else
  {
    for(i = 0; i<NGPsepLm; ++i)
      if(gplms[i] == NULL) return i;
    gplms = (GPsepLm**) realloc(gplms, sizeof(GPsepLm*) * (2*NGPsepLm));
    for(i=NGPsepLm; i<2*NGPsepLm; ++i) gplms[i] = NULL;
    NGPsepLm *= 2;
    return NGPsepLm/2;
  }
}

void deleteGPsepLm_index(unsigned int i)
{
  if(!(gplms == NULL || i >= NGPsepLm || gplms[i] == NULL))
  {
    deleteGPsepLm(gplms[i]);
    gplms[i] = NULL;
  }
  else
    error("gplms %d is not allocated\n", i);

}

void deleteGPsepLms()
{
  unsigned int i;
  for(i=0; i<NGPsepLm; ++i)
  {
    if(gplms[i])
      deleteGPsepLm(gplms[i]);
  }
  if(gplms) free(gplms);
  gplms = NULL;
  NGPsepLm = 0;
}
extern "C"{
  void deleteGPsepLm_R(int *igplm)
  {
    deleteGPsepLm_index(*igplm);
  }

  void deleteGPsepLms_R()
  {
    if(gplms) deleteGPsepLms();
  }

  void newGPsepLm_R(int *m_in, int *n_in, double *X_in, double *Z_in,
		    double *d_in, double *g_in, int *dK_in, int *p_in,
		    double *H_in, int* gplm_idx)
  {
    double **X, **H;
    *gplm_idx = get_gpsepLm();

    X = new_matrix_bones(X_in, *n_in, *m_in);
    H = new_matrix_bones(H_in, *n_in, *p_in);
    try{
      gplms[*gplm_idx] = newGPsepLm(*m_in, *n_in, X, Z_in, d_in, *g_in, *dK_in,
				    *p_in, H);
    }
    catch(cholException& e){
      free(X); free(H);
      error("bad cholesky decomposition, info=%d", e.getInfo());
    }
    free(X); free(H);
  }
  void getmGPsepLm_R(int *gplmi_in, int *m_out)
  {
    GPsepLm *gplm;
    unsigned int gplmi = *gplmi_in;

    if(gplms == NULL || gplmi >= NGPsepLm || gplms[gplmi] == NULL)
      error("gplm %d is not allocated\n", gplmi);
    gplm = gplms[gplmi];

    *m_out = gplm->gpsep->m;
  }
  void jmleGPsepLm_R(int *gplmi_in, int *maxit_in, int *verb_in,
		     double *dmin_in, double* dmax_in, double *grange_in,
		     double *dab_in, double *gab_in,
		     double *d_out, double *g_out, int *dits_out,
		     int *gits_out, int *dconv_out)
  {
    GPsepLm *gplm;
    unsigned int gplmi, k;
    GPsep *gpsep;
    gplmi = *gplmi_in;
    if(gplms == NULL || gplmi >= NGPsepLm || gplms[gplmi] == NULL)
      error("gplm %d is not allocated\n", gplmi);
    gplm = gplms[gplmi];
    gpsep = gplm->gpsep;

    assert(grange_in[0] >= 0 && grange_in[0] < grange_in[1]);
    for(k=0; k<gpsep->m; ++k)
    {
      assert(dmin_in[k] >= 0 && dmin_in[k] < dmax_in[k]);
      if(gpsep->d[k] < dmin_in[k] || gpsep->d[k] > dmax_in[k])
	error("gpsep->d[%d]=%g outside drange[%d]=[%g,%g]",
	      k, gpsep->d[k], k, dmin_in[k], dmax_in[k]);
    }
    if(gpsep->g < grange_in[0] || gpsep->g > grange_in[1])
      error("gp->g=%g outside grange=[%g,%g]", gpsep->g, grange_in[0], grange_in[1]);

    if(! gpsep->dK)
      error("derivative info not in gpsep; use newGPsep with dK=TRUE");
    try{
      jmleGPsepLm(gplm, *maxit_in, dmin_in, dmax_in, grange_in, dab_in, gab_in, *verb_in,
		  dits_out, gits_out, dconv_out);
    }
    catch(cholException& e){
      error("bad cholesky decomposition, info=%d", e.getInfo());
    }
    catch(optException& e){
      error("unable to opimize the nugget term in fmin()");
    }
    dupv(d_out, gpsep->d, gpsep->m);
    *g_out = gpsep->g;
  }
  void llikGPsepLm_R(/* inputs */
    int *gplmi_in,
    double *dab_in,
    double *gab_in,
    /* outputs */
    double *llik_out)
  {
    GPsepLm *gplm;
    unsigned int gplmi;

    /* get the cloud */
    gplmi = *gplmi_in;
    if(gplms == NULL || gplmi >= NGPsepLm || gplms[gplmi] == NULL) 
      error("gpsep %d is not allocated\n", gplmi);
    gplm = gplms[gplmi];

    /* calculate log likelihood */
    *llik_out = llikGPsepLm(gplm, dab_in, gab_in);
  }

  void predGPsepLm_R(int *gplmi_in, int *m_in, int *nn_in, int *p_in,
		     double *XX_in, double *HH_in,
		     double *mean_out, double *sigma2_out,
		     double *df_out, double *llik_out)
  {
    GPsepLm *gplm;
    GPsep *gpsep;
    unsigned int gplmi;
    double **XX, **HH;

    gplmi = *gplmi_in;

    if(gplms == NULL || gplmi >= NGPsepLm || gplms[gplmi] == NULL)
      error("gplm %d is not allocated\n", gplmi);
    gplm = gplms[gplmi];
    gpsep = gplm -> gpsep;

    if((unsigned) *m_in != gpsep->m)
      error("ncol(XX)=%d does not match GPsep/C-side (%d)", *m_in, gpsep->m);

    if((unsigned) *p_in != gplm->p)
      error("ncol(HH)=%d does not match GPsep/C-side (%d)", *p_in, gplm->p);

    XX = new_matrix_bones(XX_in, *nn_in, *m_in);
    HH = new_matrix_bones(HH_in, *nn_in, *p_in);
    try{
      predGPsepLm_lite(gplm, *nn_in, XX, HH, mean_out, sigma2_out,
		       df_out, llik_out);
      free(XX);
      free(HH);
    }
    catch(cholException& e){
      error("bad cholesky decomposition, info=%d", e.getInfo());
    }
  }
}
