void saxpymat(double *y, int idx1, int istart, int iend, double alp, double *z, int idx2)
{  int i;
  
   for(i=istart; i< iend-3; i++){             /* LEVEL 4 */
      z[i+idx2] += alp * y[i+idx1]; i++;
      z[i+idx2] += alp * y[i+idx1]; i++;
      z[i+idx2] += alp * y[i+idx1]; i++;
      z[i+idx2] += alp * y[i+idx1]; 
   }
   if(i < iend-1){                            /* LEVEL 2 */
      z[i+idx2] += alp * y[i+idx1]; i++;
      z[i+idx2] += alp * y[i+idx1]; i++;
   }
   if(i < iend){                              /* LEVEL 1 */
      z[i+idx2] += alp * y[i+idx1]; 
   }
   return;
}
