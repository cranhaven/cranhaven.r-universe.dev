double realdot2(const double *x, const int idx1, const double *y, 
               const int idx2,  const int n)

{  int i;
  double r; 
  
  r=0.0;
  for(i=0; i< n-3; i++){                 /* LEVEL 4 */
      r+= x[i+idx1] * y[i+idx2]; i++;
      r+= x[i+idx1] * y[i+idx2]; i++;
      r+= x[i+idx1] * y[i+idx2]; i++;
      r+= x[i+idx1] * y[i+idx2]; 
  }
  if(i < n-1){                           /* LEVEL 2 */
      r+= x[i+idx1] * y[i+idx2]; i++;
      r+= x[i+idx1] * y[i+idx2]; i++;
  }
  if(i < n){                             /* LEVEL 1 */
      r+= x[i+idx1] * y[i+idx2]; 
  }
  return r; 
}
