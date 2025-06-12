#include <R.h>
#include <Rmath.h>

//order two subjects
void rankij(double *s1, double *z1, double *s2, double *z2,
            double *duration, double *cuts, double *cutz, int *rst) {

  double tmp;

  //printf("%5.3f, %5.3f, %5.3f, %5.3f\n", *s1, *z1, *s2, *z2);
  //both alive
  if ((*s1 > *duration) &&  (*s2 > *duration)) {
    tmp = *z1 - *z2;
    if (tmp > *cutz) {
	    *rst = 1;
    } else if (tmp < -*cutz) {
	    *rst = -1;
    } else {
	    *rst = 0;
    }
  } else if ((*s1 <= *duration) &&  (*s2 <= *duration)) {
    tmp = *s1 - *s2;
    if (tmp > *cuts) {
	    *rst = 1;
    } else if (tmp < -*cuts) {
	    *rst = -1;
    } else {
	    *rst = 0;
    }
  } else {
    if (*s1 > *duration) {
	    *rst = 1;
    } else {
	    *rst = -1;
    }
  }
}

//get rank statistic
void rankall(double *val1, double *val2, int *n1, int *n2,
	     double *duration, double *cuts, double *cutz, double *rst) {

    int i, j, rk, r2;

    rk = 0;
    for (i=0; i<*n1; i++) {
	for (j=0; j < *n2; j++) {
	    rankij(&val1[i*2], &val1[i*2+1],
		   &val2[j*2], &val2[j*2+1],
		   duration, cuts, cutz, &r2);
	    rk += r2;
	}
    }

    *rst = (double)rk/(*n1)/(*n2);
}

//bubble sort
void bsort(double *val, int *n, double *duration, double *cuts, double *cutz) {

  int hasChanged;
  int itemCount;
  int i, r2, k, ne;
  double tmp[3];

  ne         = 3; //surv z inx
  hasChanged = 1;
  itemCount  = *n;
  while (1 == hasChanged) {
    hasChanged = 0;
    itemCount--;
    for(i=0; i<itemCount; i++) {
      rankij(&val[i*ne], &val[i*ne+1],
             &val[(i+1)*ne], &val[(i+1)*ne+1],
             duration, cuts, cutz, &r2);
      if (1 == r2) {
        for (k=0; k<ne; k++) {
          tmp[k]          = val[i*ne+k];
          val[i*ne+k]     = val[(i+1)*ne+k];
          val[(i+1)*ne+k] = tmp[k];
          hasChanged      = 1;
        }
	    }
    }
  }
}

//get kernel density
void kdpdf(double *err, double *res, double *h, int *n, double *rst) {
  int i;
  double tmp;

  *rst = 0;
  for (i=0; i<*n; i++) {
    tmp  = (res[i] - *err) / (*h);
    *rst += exp(-pow(tmp/2,2));
  }

  *rst = (double)*rst/(*n)/(*h);
}
