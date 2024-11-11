#include<math.h>
#include<float.h>
#include<stdlib.h>
#include<time.h>
#include"matrix.h"
#include"random.h"

static void initializeAvailableMatrix(int **avail, int nrow, int ncol)
{
  int i, j;
  for(i=0; i<nrow; i++)
    for(j=0; j<ncol; j++)
      avail[i][j]=j+1;
}

static void maximinLHS(int n, int k, int dup, int **result, uniformGenerator* generator)
{
  int len, *list1, *vec;
  int **avail, **point1;
  int minCandidateSquaredDistBtwnPts;
  unsigned int point_index, best, distSquared;
  unsigned int irow, jcol, idx, jdx, ucount;
  double dn, runif, minSquaredDistBtwnPts, squaredDistanceBtwnCorners;
  len = dup * (n-1);
  avail = new_imatrix(k,n);
  point1 = new_imatrix(k, len);
  list1 = new_ivector(len);
  vec = new_ivector(k);
  initializeAvailableMatrix(avail, k, n);
  dn = (double) n;
  squaredDistanceBtwnCorners = (double) (k*(n-1)*(n-1));
  /*
   * come up with an array of K integers from 1 to N randomly
   * and put them in the last column of result
   */
  for(irow = 0; irow < k; irow++)
  {
    runif = genrunif(generator) * dn + 1.0;
    runif = floor(runif);
    result[irow][n-1] = (int) floor(runif);
  }
  /*
   * use the random integers from the last column of result to place an N value
   * randomly through the avail matrix
   */
  for(irow = 0; irow < k; irow++)
  {
    idx = (unsigned int) (result[irow][n-1] - 1);
    avail[irow][idx] = n;
  }

  for(ucount = n - 1; ucount > 0; ucount--)
  {
    for(irow = 0; irow < k; irow++)
    {
      for(jcol = 0; jcol < dup; jcol++)
      {
	/* create the list1 vector */
	for(idx = 0; idx < ucount; idx++)
	  list1[idx+ucount*jcol]=avail[irow][idx];
      }
      /* create a set of points to choose from */
      for(jcol = ucount * dup; jcol > 0; jcol--)
      {
	runif = genrunif(generator);
	runif *= (double) jcol;
	runif = floor(runif);
	point_index = (unsigned int) runif;
	point1[irow][jcol-1] = list1[point_index];
	list1[point_index] = list1[jcol-1];
      }
    }
    minSquaredDistBtwnPts = DBL_MIN;
    best = 0;
    for(jcol = 0; jcol < dup*ucount - 1; jcol++)
    {
      minCandidateSquaredDistBtwnPts = (unsigned int) (ceil(squaredDistanceBtwnCorners));
      for(idx = ucount; idx < n; idx++)
      {
	distSquared = 0;
	/*
	 * find the distance between candidate points and the points already
	 * in the sample
	 */
	for(jdx = 0; jdx < k; jdx++)
	{
	  vec[jdx] = point1[jdx][jcol] - result[jdx][idx];
	  distSquared += vec[jdx] * vec[jdx];
	}
	/*
	 * if the distance squared value is the smallest so far, place it in the
	 * min candidate
	 */
	if(minCandidateSquaredDistBtwnPts > distSquared)
	  minCandidateSquaredDistBtwnPts = distSquared;
      }
      /*
       * if the candidate point is the largest minimum distance between points so
       * far, then keep that point as the best.
       */
      if((double)(minCandidateSquaredDistBtwnPts) > minSquaredDistBtwnPts)
      {
	minSquaredDistBtwnPts = (double) minCandidateSquaredDistBtwnPts;
	best = jcol;
      }
    }
    /* take the best point out of point1 and place it in the result */
    for(irow = 0; irow < k; irow++)
      result[irow][ucount-1] = point1[irow][best];
    /* update the numbers that are available for the future points */
    for(irow = 0; irow < k; irow++)
    {
      for(jcol = 0; jcol < n; jcol++)
	if(avail[irow][jcol] == result[irow][ucount-1])
	  avail[irow][jcol] = avail[irow][ucount - 1];
    }
  }
  /*
   * once all but the last points of result are filled in, there is only
   * one choice left
   */
  for(irow = 0; irow < k; irow++)
    result[irow][0] = avail[irow][0];
  delete_imatrix(avail);
  delete_imatrix(point1);
  free(list1);
  free(vec);
}

void genmaximinLHS(unsigned int n, unsigned int k, double **maximinlhs)
{
  unsigned int col, row;
  unsigned int seed1 = (int) time(NULL);
  int **result;
  double eps, dn;
  uniformGenerator generator={seed1, seed1+1024};
  result = new_imatrix(k, n);
  maximinLHS(n, k, 1, result, &generator);
  dn = (double) n;
  for(col = 0; col < k; ++col)
    for(row = 0; row < n; ++row)
    {
      eps = genrunif(&generator);
      maximinlhs[row][col] = (double)(result[col][row]-1) + eps;
      maximinlhs[row][col] /= dn;
    }
  delete_imatrix(result);
}
