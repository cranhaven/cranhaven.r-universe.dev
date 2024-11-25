double realdot2(const double *x, const int idx1, const double *y,
                const int idx2,  const int n);

void  prod1(int m, int n, int p, 
            double *A,  int *irA, int *jcA, int isspA, 
            double *B,  double *P,  int *irP, int *jcP, 
            int *list1, int *list2, int len);

void  prod2(int m, int n, int p,  
            double *A, int *irA, int *jcA, int isspA,
            double *B, int *irB, int *jcB, int isspB,
            double *P, int *irP, int *jcP, double *Btmp, 
            int *list1, int *list2, int len);
