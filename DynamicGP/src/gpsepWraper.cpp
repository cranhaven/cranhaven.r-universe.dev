#include<cassert>
#include<cstdlib>
#include<R.h>
#include "gp_sep.hpp"
#include "exceptions.hpp"
extern"C"{
#include "matrix.h"
}
using std::free;

unsigned int NGPsep = 0;
GPsep **gpseps = NULL;

unsigned int get_gpsep(void)
{
  unsigned int i;
  if(NGPsep == 0) {
    assert(gpseps == NULL);
    gpseps = (GPsep**) malloc(sizeof(GPsep*));
    gpseps[0] = NULL;
    NGPsep = 1;
    return 0;
  } else {
    for(i=0; i<NGPsep; i++) {
      if(gpseps[i] == NULL) return i;
    }
    gpseps = (GPsep**) realloc(gpseps, sizeof(GPsep*) * (2*NGPsep));
    for(i=NGPsep; i<2*NGPsep; i++) gpseps[i] = NULL;
    NGPsep *= 2;
    return NGPsep/2;
  }
}

void deleteGPsep_index(unsigned int i)
{
  if(!(gpseps == NULL || i >= NGPsep || gpseps[i] == NULL)) {
    deleteGPsep(gpseps[i]);
    gpseps[i] = NULL;
  } else error("gpsep %d is not allocated\n", i);
}

void deleteGPseps(void)
{
  unsigned int i;
  for(i=0; i<NGPsep; i++) {
    if(gpseps[i]) {
      deleteGPsep(gpseps[i]);
    }
  }
  if(gpseps) free(gpseps);
  gpseps = NULL;
  NGPsep = 0;
}
extern"C"{
  void deleteGPsep_R(int *gpsep)
  {
    deleteGPsep_index(*gpsep);
  }
  void deleteGPseps_R(void)
  {
    if(gpseps) deleteGPseps();
  }
  void newGPsep_R(/* inputs */
    int *m_in,
    int *n_in,
    double *X_in,
    double *Z_in,
    double *d_in,
    double *g_in,
    int *dK_in,

    /* outputs */
    int *gpsep_index)
  {
    double **X;

    /* assign a new gp index */
    *gpsep_index = get_gpsep();

    /* create a new GP; */
    X = new_matrix_bones(X_in, *n_in, *m_in);
    try{
      gpseps[*gpsep_index] = newGPsep(*m_in, *n_in, X, Z_in, d_in, *g_in, *dK_in);
    }
    catch(cholException& e){
      free(X);
      error("bad cholesky decomposition, info=%d", e.getInfo());
    }
    free(X);
  }
  void llikGPsep_R(/* inputs */
    int *gpsepi_in,
    double *dab_in,
    double *gab_in,

    /* outputs */
    double *llik_out)
  {
    GPsep *gpsep;
    unsigned int gpsepi;

    /* get the cloud */
    gpsepi = *gpsepi_in;
    if(gpseps == NULL || gpsepi >= NGPsep || gpseps[gpsepi] == NULL)
      error("gpsep %d is not allocated\n", gpsepi);
    gpsep = gpseps[gpsepi];

    /* calculate log likelihood */
    *llik_out = llikGPsep(gpsep, dab_in, gab_in);
  }
  void getmGPsep_R(/* inputs */
    int *gpsepi_in,
    /* outputs */
    int *m_out)
  {
    GPsep *gpsep;
    unsigned int gpsepi;

    /* get the cloud */
    gpsepi = *gpsepi_in;
    if(gpseps == NULL || gpsepi >= NGPsep || gpseps[gpsepi] == NULL)
      error("gpsep %d is not allocated\n", gpsepi);
    gpsep = gpseps[gpsepi];

    *m_out = gpsep->m;
  }
  void getgGPsep_R(/* inputs */
    int *gpsepi_in,
    /* outputs */
    double *g_out)
  {
    GPsep *gpsep;
    unsigned int gpsepi;

    /* get the cloud */
    gpsepi = *gpsepi_in;
    if(gpseps == NULL || gpsepi >= NGPsep || gpseps[gpsepi] == NULL)
      error("gpsep %d is not allocated\n", gpsepi);
    gpsep = gpseps[gpsepi];

    *g_out = gpsep->g;
  }
  void getdGPsep_R(/* inputs */
    int *gpsepi_in,
    /* outputs */
    double *d_out)
  {
    GPsep *gpsep;
    unsigned int gpsepi;

    /* get the cloud */
    gpsepi = *gpsepi_in;
    if(gpseps == NULL || gpsepi >= NGPsep || gpseps[gpsepi] == NULL)
      error("gpsep %d is not allocated\n", gpsepi);
    gpsep = gpseps[gpsepi];

    /* double check that derivatives have been calculated */
    dupv(d_out, gpsep->d, gpsep->m);
  }
  void newparamsGPsep_R(/* inputs */
    int *gpsepi_in,
    double *d_in,
    double *g_in)
  {
    GPsep *gpsep;
    unsigned int gpsepi, k;
    int dsame;

    /* get the cloud */
    gpsepi = *gpsepi_in;
    if(gpseps == NULL || gpsepi >= NGPsep || gpseps[gpsepi] == NULL)
      error("gpsep %d is not allocated\n", gpsepi);
    gpsep = gpseps[gpsepi];

    /* check if any are old */
    dsame = 1;
    for(k=0; k<gpsep->m; k++) {
      if(d_in[k] <= 0) d_in[k] = gpsep->d[k];
      else if(d_in[k] != gpsep->d[k]) dsame = 0;
    }
    if(*g_in < 0) *g_in = gpsep->g;

    /* check if there is nothing to do bc the params are the same */
    if(dsame && *g_in == gpsep->g) return;

    /* call real C routine */
    try{
      newparamsGPsep(gpsep, d_in, *g_in);
    }
    catch(cholException& e){
      error("bad cholesky decomposition, info=%d", e.getInfo());
    }
  }
  void jmleGPsep_R(/* inputs */
    int *gpsepi_in,
    int *maxit_in,
    int *verb_in,
    double *dmin_in,
    double *dmax_in,
    double *grange_in,
    double *dab_in,
    double *gab_in,

    /* outputs */
    double *d_out,
    double *g_out,
    int *dits_out,
    int *gits_out,
    int *dconv_out)
  {
    GPsep *gpsep;
    unsigned int gpsepi, k;

    /* get the cloud */
    gpsepi = *gpsepi_in;
    if(gpseps == NULL || gpsepi >= NGPsep || gpseps[gpsepi] == NULL)
      error("gpsep %d is not allocated\n", gpsepi);
    gpsep = gpseps[gpsepi];

    /* check theta and tmax */
    assert(grange_in[0] >= 0 && grange_in[0] < grange_in[1]);
    for(k=0; k<gpsep->m; k++) {
      assert(dmin_in[k] >= 0 && dmin_in[k] < dmax_in[k]);
      if(gpsep->d[k] < dmin_in[k] || gpsep->d[k] > dmax_in[k])
	error("gpsep->d[%d]=%g outside drange[%d]=[%g,%g]",
	      k, gpsep->d[k], k, dmin_in[k], dmax_in[k]);
    }
    if(gpsep->g < grange_in[0] || gpsep->g > grange_in[1])
      error("gp->g=%g outside grange=[%g,%g]", gpsep->g, grange_in[0], grange_in[1]);

    /* double check that derivatives have been calculated */
    if(! gpsep->dK)
      error("derivative info not in gpsep; use newGPsep with dK=TRUE");

    /* call C-side MLE */
    try{
      myjmleGPsep(gpsep, *maxit_in, dmin_in, dmax_in, grange_in, dab_in, gab_in, *verb_in,
		  dits_out, gits_out, dconv_out);
    }
    catch(cholException& e){
      error("bad cholesky decomposition, info=%d", e.getInfo());
    }
    catch(optException& e){
      error("unable to opimize the nugget term in fmin()");
    }
    /* write back d and g */
    dupv(d_out, gpsep->d, gpsep->m);
    *g_out = gpsep->g;
  }
  void predGPsep_R(/* inputs */
    int *gpsepi_in,
    int *m_in,
    int *nn_in,
    double *XX_in,
    /* outputs */
    double *mean_out,
    double *Sigma_out,
    double *df_out,
    double *llik_out)
  {
    GPsep* gpsep;
    unsigned int gpsepi;
    double **XX;

    /* get the gp */
    gpsepi = *gpsepi_in;
    if(gpseps == NULL || gpsepi >= NGPsep || gpseps[gpsepi] == NULL)
      error("gpsep %d is not allocated\n", gpsepi);
    gpsep = gpseps[gpsepi];
    if((unsigned) *m_in != gpsep->m)
      error("ncol(X)=%d does not match GPsep/C-side (%d)", *m_in, gpsep->m);

    /* sanity check and XX representation */
    XX = new_matrix_bones(XX_in, *nn_in, *m_in);
    predGPsep_lite(gpsep, *nn_in, XX, mean_out, Sigma_out, df_out,
		   llik_out);
    /* clean up */
    free(XX);
  }
}
