void ops1(double *x, double *y, double *z, 
          int numblk, int *cumblk, int options) 

{  int  j, l, jstart, jend;
   double tmp; 
   
   if (options == 1) { 
      for (l=0; l<numblk; ++l) {  
          jstart = cumblk[l]; jend = cumblk[l+1];
          tmp = 0; 
          for (j=jstart; j<jend; ++j) { tmp += x[j]*y[j]; }
          z[l] = tmp; 
      }
   }
   else if (options == 2) { 
      for (l=0; l<numblk; ++l) {  
          jstart = cumblk[l]; jend = cumblk[l+1];
          tmp = x[jstart]*y[jstart]; 
          for (j=jstart+1; j<jend; ++j) { tmp -= x[j]*y[j]; }
          z[l] = tmp;
      }          
   }
   return;
}
