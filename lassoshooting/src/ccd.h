#ifndef _CCD_H_
#  define _CCD_H_ 1

#  define _THIS_IS_AN_R_PACKAGE 1

#  define max(a,b) (((a)>(b))?(a):(b))

#  include <R.h>  /* to include Rconfig.h */
          
#  ifdef ENABLE_NLS
#    include <libintl.h>
#    define _(String) dgettext ("lassoshooting", String)
#  else
#    define _(String) (String)
#  endif

#  ifdef _THIS_IS_AN_R_PACKAGE
#    define myprintf Rprintf
#    define myprintferr REprintf
#  else
#    define myprintf printf
#    define myprintferr(x) (fprintf(stderr,x);)
#  endif

typedef struct params {
  double *XtX;
  double *Xty;
  double lambda;
  double infnorm;
  double *beta;
  double *s;
  int m;
  int p;

  int forcezero;
  int maxits;
  int its;
  double delta;
  double tol;
  int trace;
  double *w;
  int*nopenalize;

  double factor2;
} param_t;

double softthresh(double x,double t);
int ccd_common(param_t* params);

#endif
