#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>
#include "util.h"
#include "getDist.h"

void optgreed_c(double *data,
	      double *distvec,
	      int *nrow,
	      int *ncol,
	      double *vcovi,
	      unsigned int *ntr,
	      int *l2,
	      int *l1names,
	      int *valid,
	      double *validvar,
	      double *validlb,
	      double *validub,
	      int *verbose,
	      double *pairdist,
	      int *ismahal,
	      int *result,
	      int *p)
{

      /*                                */
      /*       set up distances         */
      /*                                */


  int n = choose(*nrow, 2);
  double *vec = calloc(n, sizeof(double));
  double *vec2 = calloc(n, sizeof(double));
  unsigned i;

  /* Compute distances between all units */
  if(*ismahal==1)
    {
      vec = allmahal(ncol, nrow, n, data, vcovi, vec);
    }
  else
    {
      /* put in user-supplied distances if mahalanobis distances are not wanted */
      for(i=1; i<n; i++)
	{
	  vec[i] = distvec[i];    
	}
    }
  for(i=1; i<n; i++)
    {
      vec2[i] = vec[i];    
    }
  /* Set some distances to INF to avoid unwanted matches if valid.var is set or if level.two=TRUE (return original distances otherwise) */
  
  vec = cleanUp(*l2, l1names, *valid, validvar, *validlb, *validub, n, vec);
 
      /*                        */
      /*       matching         */
      /*                        */

  unsigned j, k, mn[*ntr];
  //unsigned ii, cinf=n, t J, K, niter;
  int check=-1;
  double md, min;
  for(j=0; j<*p; j++)
    {
      unsigned matches[*ntr];

      if(*verbose==1)
	{
	  Rprintf("Block number is %i \n", j+1);
	}

      mn[0] = findMin(vec, 0, n, 0);
      min = vec[mn[0]];
      if(min == HUGE_VAL)
	{
	  check =0;
	}
      pairdist[j] = min;
      matches[0] = mycol(mn[0]);
      matches[1] = myrow(mn[0]);

      vec[mn[0]] = HUGE_VAL;


      /* if level.two=TRUE, then INF out distances corresponding to subunits within the same level one unit */
      if(*l2==1)
	{
	  for(i=0; i<n; i++)
	    {
	      for(k=0; k<2; k++)
		{
		  if(levelTwoCheck(i, matches[k], l1names)==1) /* levelTwoCheck is in util.c */
		    {
		      vec[i] = HUGE_VAL;
		    }
		}
	    }
	}

      int mm;
      /* ntr>2 block starts here */

      for(k=2; k<*ntr; k++) 
	{
	  int whichMatch;
	  md = HUGE_VAL;
	  for(i=0; i<k; i++)
	    {


	      /* find next match */

	      mm = findMin2(vec, *nrow, matches[i]);

	      if(vec[mm] < md)
		{
		  md = vec[mm];
		  mn[k-1] = mm;
		  whichMatch = i;
		}
	    }

	  /*record unit associated with new match */

	  if(mycol(mn[k-1]) == matches[whichMatch])
	    {
	      matches[k] = myrow(mn[k-1]);
	    }
	  else
	    {
	      matches[k] = mycol(mn[k-1]);
	    }
	  
	  /*record maximum distance in block */

	  pairdist[j] = maxDist(vec2, matches, k+1);

	  if(vec[mn[k-1]] == HUGE_VAL && check==-1)
	    {
	      check = k;
	    }

 	  /* INF out some distances */
	  
	  if(*l2 == 1) /* if level.two=TRUE, eliminate subunits within the same level two unit */
	    {
	      for(i=0; i<n; i++)
		{
		  if(levelTwoCheck(i, matches[k], l1names)==1)
		    {
		      vec[i] = HUGE_VAL;
		    }
		}
	    }


	  /* INF out the distance we just used */
	  vec[mn[k-1]] = HUGE_VAL; 

	  /* INF out distances between already paired units */
	  for(i=0; i<k; i++)
	    {
	      eliminatePairDist(matches[i], matches[k], vec);
	    }
	} /* end of n.tr>2 loop */
	  /* get rid of used units */ 
      for(i=0; i<*ntr; i++)
	{
	  vec = eliminate(matches[i],vec, *nrow);
	}

      if(check >= 0)
	{
	  for(i=check; i<*ntr; i++)
	    {
	      matches[i] = 0;
	    }
	}


      /* record matches for the R output */
       
      for(i=0; i<*ntr; i++)
	{
	  result[j*(*ntr) + i] = matches[i];
	}

      /* reset matches to zero for next iteration */
      for(i=0; i<*ntr; i++)
	{
	  matches[i] = 0;
	}    
    }
}
