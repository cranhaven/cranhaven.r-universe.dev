void ops3(double *x, double *y, double *z, 
          int numblk, int *cumblk, int options) 

{  int  j, l, jstart, jend;
   double tmp;
   
   if (options == 3) {
      for (l=0; l<numblk; ++l) {  
         jstart = cumblk[l]; jend = cumblk[l+1];
         tmp = x[l];
         for (j=jstart; j<jend; ++j) { 
             z[j] = tmp*y[j]; }
      }
   }
   else if (options == 4) { 
      for (l=0; l<numblk; ++l) {  
         jstart = cumblk[l]; jend = cumblk[l+1];
         tmp = x[l];
         for (j=jstart; j<jend; ++j) { 
             z[j] = tmp*y[j]; }
         z[jstart] = -z[jstart];
      }
   }
   return;
}
