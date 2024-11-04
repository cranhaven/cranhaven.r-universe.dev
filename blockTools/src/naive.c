#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>
#include "util.h"
#include "getDist.h"
#include "naive.h"

void naive_c(double *data,
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
  
  
  unsigned n = choose(*nrow, 2), i;
  double *vec = calloc(n, sizeof(double)); 
  double *vec2 = calloc(n, sizeof(double));
  
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
 
  unsigned unitlist[*nrow], unit, k, matches[*ntr], mm, mn[*ntr], check=0, j=0;

  for(unit=0; unit<*nrow; unit++)
    {
      if(unitlist[unit] != 1)
	{
	  matches[0] = unit+1;
	      if(*l2 == 1) /* if level.two=TRUE, eliminate subunits within the same level two unit */
		{
		  for(i=0; i<n; i++)
		    {
		      if(levelTwoCheck(i, unit+1, l1names)==1)
			{
			  vec[i] = HUGE_VAL;
			  if(l1names[myrow(i)-1]==l1names[unit])
			    {
			      unitlist[myrow(i)-1] = 1;
			    }
			  else
			    {
			      unitlist[mycol(i)-1] = 1;
			    }
			}
		    }
		}

	  for(k=0; k<(*ntr-1); k++)
	    {

	      /* find minimum distance */
	      mn[k] = findMin2(vec, *nrow, unit+1);

	      /* record row and column */
	      mm = mycol(mn[k]);
	      if(mm==matches[0])
		{
		  matches[k+1] = myrow(mn[k]);
		}
	      else
		{
		  matches[k+1] = mm;
		}
	      unitlist[matches[k+1]-1] = 1;
	      
      
	      if(vec[mn[k]] == HUGE_VAL && check==0)
		{
		  check = k+1;
		}

	       /* inf out things not to be reused */
	      vec[mn[k]] = HUGE_VAL;
	      if(*l2 == 1) /* if level.two=TRUE, eliminate subunits within the same level two unit */
		{
		  for(i=0; i<n; i++)
		    {
		      if(levelTwoCheck(i, matches[k+1], l1names)==1)
			{
			  vec[i] = HUGE_VAL;
			  if(l1names[myrow(i)-1]==l1names[matches[k+1]-1])
			    {
			      unitlist[myrow(i)-1] = 1;
			    }
			  else
			    {
			      unitlist[mycol(i)-1] = 1;
			    }
			}
		    }
		}
	    }
	  pairdist[j] = maxDist(vec2, matches, *ntr);

	  /* get rid of used units */ 
	  for(i=0; i<*ntr; i++)
	    {
	      vec = eliminate(matches[i],vec, *nrow);
	    }

	  
	  /* record matches for the R output */
	  if(check > 0)
	    {
	      for(i=check; i<*ntr; i++)
		{
		  matches[i] = 0;
		}
	    }
	  check = 0;

	  for(i=0; i<*ntr; i++)
	    {
	      result[j*(*ntr) + i] = matches[i];
	    }

  /* reset matches to zero for next iteration */
	  for(i=0; i<*ntr; i++)
	    {
	      matches[i] = 0;
	    }
	  j++;
	}
    }
}
