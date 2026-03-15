/* $Author: sinnwell $ */
/* $Date: 2003/10/07 21:21:27 $ */
/* $Header: /projects/genetics/cvs/cvsroot/haplo.stats/src/groupsum.c,v 1.2 2003/10/07 21:21:27 sinnwell Exp $ */
/* $Locker:  $ */
/* 
 * $Log: groupsum.c,v $
 * Revision 1.2  2003/10/07 21:21:27  sinnwell
 * fix $Log keyword comments
 *
 * Revision 1.1  2003/09/16 16:00:44  schaid
 * Initial revision
 * */
#include <math.h>
#include <R.h>


/* groupsum

Function Arguments:

x:        double pointer to an array. The values of x will be summed over a
          'group' indicator provided by indx

indx:     integer pointer on an array. It is assumed that this indx array
          as sequential integer values. This is imporant, becuase the values
          in indx are used as array indices when summing over values of x.

n:        integer pointer to scalar. n = length of x and indx.

grouptot: double pointer to array, which is the length of ngroup.

ngroup:   integer pointer to scalar; ngroup = length of grouptot.

The values of x are summed according to the group indx, and total sums
are stored in array grouptot. So, the ith element of grouptot is the total
of x's that have indx=(i+1), because we use the zero-offset in C.

Note that the error checking must be done is Splus, because this C code
is intended to simly sum, for speed.

*/

void groupsum(  double *x,
                int *indx,
                int *n,
                double *grouptot,
                int *ngroup ) {
 
 
  int i;
 
  for(i=0; i<*ngroup; i++) grouptot[i]=0.0;

  for (i=0; i<*n; i++){
      grouptot[indx[i]-1] += x[i];
    }
  
 
 return;

}





