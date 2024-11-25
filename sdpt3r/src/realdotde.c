double realdotde(double *x, int idx, double *y, int n)
{  int i;
   double r; 

   r=0.0;
   for (i=0; i<n-3; i++) {             /* LEVEL 4 */
      r += x[i+idx] * y[i]; i++;
      r += x[i+idx] * y[i]; i++;
      r += x[i+idx] * y[i]; i++;
      r += x[i+idx] * y[i]; }
   if (i<n-1) {                        /* LEVEL 2 */
      r += x[i+idx] * y[i]; i++;
      r += x[i+idx] * y[i]; i++; }
   if (i<n) {                          /* LEVEL 1 */
      r += x[i+idx] * y[i]; }
   return r; 
}
