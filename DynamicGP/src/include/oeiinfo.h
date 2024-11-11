#ifndef ORIEIINFO_H
#define ORIEIINFO_H
typedef struct
{
  int p;
  int L;
  double b;
  double iomemu2;
  double barval;
  double upb;
  const double *avec;
  const double *mub2star;
} parOei;
void oeiinfo(int n, int p, int L, double b, double barval,
	     const double* iomemu2, const double* upb,
	     const double* avec, const double* mub2star,
	     const double* mumk, double *info);
#endif
