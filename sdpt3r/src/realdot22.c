double realdot22(const double *x, int *irx, int *jcx, 
                const int col, const double *y)

{  int i, row, idxstart, idxend;
  double r; 
  
  idxstart=jcx[col]; idxend=jcx[col+1]; 
  r=0.0;
  for (i=idxstart; i<idxend-3; i++) {      /* LEVEL 4 */
      row = irx[i]; r += x[i]*y[row]; i++;
      row = irx[i]; r += x[i]*y[row]; i++;
      row = irx[i]; r += x[i]*y[row]; i++;
      row = irx[i]; r += x[i]*y[row]; }
  if (i<idxend-1) {                        /* LEVEL 2 */
      row = irx[i]; r += x[i]*y[row]; i++;
      row = irx[i]; r += x[i]*y[row]; i++; }
  if (i<idxend) {                          /* LEVEL 1 */
      row = irx[i]; r += x[i]*y[row]; }
  return r; 
}
