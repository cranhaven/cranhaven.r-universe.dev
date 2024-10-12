/********************************************************************
 ********************************************************************
 **
 ** libhungarian by Cyrill Stachniss, 2004
 ** Modified by Benjamin Auder to work on floating point number,
 ** and to fit into a R package (2017)
 **
 ** Solving the Minimum Assignment Problem using the Hungarian Method.
 **
 ** ** This file may be freely copied and distributed! **
 **
 ** Parts of the used code was originally provided by the
 ** "Stanford GraphGase", but I made changes to this code.
 ** As asked by  the copyright node of the "Stanford GraphGase",
 ** I hereby proclaim that this file are *NOT* part of the
 ** "Stanford GraphGase" distrubition!
 **
 ** This file is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied
 ** warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 ** PURPOSE.
 **
 ********************************************************************
 ********************************************************************/

#include <stdio.h>
#include <stdlib.h>

#define HUNGARIAN_NOT_ASSIGNED 0
#define HUNGARIAN_ASSIGNED 1

#define HUNGARIAN_MODE_MINIMIZE_COST 0
#define HUNGARIAN_MODE_MAXIMIZE_UTIL 1

#define INF (1e32)
#define verbose (0)

typedef struct {
  int num_rows;
  int num_cols;
  double** cost;
  int** assignment;
} hungarian_problem_t;

int hungarian_imax(int a, int b) {
  return (a<b)?b:a;
}

/** This method initialize the hungarian_problem structure and init
 *  the  cost matrices (missing lines or columns are filled with 0).
 *  It returns the size of the quadratic(!) assignment matrix. **/
int hungarian_init(hungarian_problem_t* p, double** cost_matrix, int rows, int cols,
  int mode)
{
  int i,j;
  double max_cost = 0.;
  int org_cols = cols,
      org_rows = rows;

  // is the number of cols  not equal to number of rows ?
  // if yes, expand with 0-cols / 0-cols
  rows = hungarian_imax(cols, rows);
  cols = rows;

  p->num_rows = rows;
  p->num_cols = cols;

  p->cost = (double**)calloc(rows,sizeof(double*));
  p->assignment = (int**)calloc(rows,sizeof(int*));

  for(i=0; i<p->num_rows; i++) {
    p->cost[i] = (double*)calloc(cols,sizeof(double));
    p->assignment[i] = (int*)calloc(cols,sizeof(int));
    for(j=0; j<p->num_cols; j++) {
      p->cost[i][j] =  (i < org_rows && j < org_cols) ? cost_matrix[i][j] : 0.;
      p->assignment[i][j] = 0;

      if (max_cost < p->cost[i][j])
        max_cost = p->cost[i][j];
    }
  }

  if (mode == HUNGARIAN_MODE_MAXIMIZE_UTIL) {
    for(i=0; i<p->num_rows; i++) {
      for(j=0; j<p->num_cols; j++)
        p->cost[i][j] =  max_cost - p->cost[i][j];
    }
  }
  else if (mode == HUNGARIAN_MODE_MINIMIZE_COST) {
    // nothing to do
  }
//  else
//    fprintf(stderr,"%s: unknown mode. Mode was set to HUNGARIAN_MODE_MINIMIZE_COST !\n", __FUNCTION__);

  return rows;
}

/** Free the memory allocated by init. **/
void hungarian_free(hungarian_problem_t* p) {
  int i;
  for(i=0; i<p->num_rows; i++) {
    free(p->cost[i]);
    free(p->assignment[i]);
  }
  free(p->cost);
  free(p->assignment);
  p->cost = NULL;
  p->assignment = NULL;
}

/** This method computes the optimal assignment. **/
void hungarian_solve(hungarian_problem_t* p)
{
  int i, j, m, n, k, l, t, q, unmatched;
  double s; //,cost
  int* col_mate;
  int* row_mate;
  int* parent_row;
  int* unchosen_row;
  double* row_dec;
  double* col_inc;
  double* slack;
  int* slack_row;

//  cost = 0.;
  m =p->num_rows;
  n =p->num_cols;

  col_mate = (int*)calloc(p->num_rows,sizeof(int));
  unchosen_row = (int*)calloc(p->num_rows,sizeof(int));
  row_dec  = (double*)calloc(p->num_rows,sizeof(double));
  slack_row  = (int*)calloc(p->num_rows,sizeof(int));

  row_mate = (int*)calloc(p->num_cols,sizeof(int));
  parent_row = (int*)calloc(p->num_cols,sizeof(int));
  col_inc = (double*)calloc(p->num_cols,sizeof(double));
  slack = (double*)calloc(p->num_cols,sizeof(double));

  for (i=0; i<p->num_rows; i++) {
    col_mate[i]=0;
    unchosen_row[i]=0;
    row_dec[i]=0.;
    slack_row[i]=0;
  }
  for (j=0; j<p->num_cols; j++) {
    row_mate[j]=0;
    parent_row[j] = 0;
    col_inc[j]=0.;
    slack[j]=0.;
  }

  for (i=0; i<p->num_rows; ++i)
    for (j=0; j<p->num_cols; ++j)
      p->assignment[i][j]=HUNGARIAN_NOT_ASSIGNED;

  // Begin subtract column minima in order to start with lots of zeroes 12
  for (l=0; l<n; l++)
  {
    s=p->cost[0][l];
    for (k=1; k<m; k++)
      if (p->cost[k][l]<s)
        s=p->cost[k][l];
//    cost+=s;
    if (s!=0.)
      for (k=0; k<m; k++)
        p->cost[k][l]-=s;
  }
  // End subtract column minima in order to start with lots of zeroes 12

  // Begin initial state 16
  t=0;
  for (l=0; l<n; l++)
  {
    row_mate[l]= -1;
    parent_row[l]= -1;
    col_inc[l]=0.;
    slack[l]=INF;
  }
  for (k=0; k<m; k++)
  {
    s=p->cost[k][0];
    for (l=1; l<n; l++)
      if (p->cost[k][l]<s)
        s=p->cost[k][l];
    row_dec[k]=s;
    for (l=0; l<n; l++)
      if (s==p->cost[k][l] && row_mate[l]<0)
      {
        col_mate[k]=l;
        row_mate[l]=k;
        goto row_done;
      }
    col_mate[k]= -1;
    unchosen_row[t++]=k;
row_done:
    ;
  }
  // End initial state 16

  // Begin Hungarian algorithm 18
  if (t==0)
    goto done;
  unmatched=t;
  while (1)
  {
    q=0;
    while (1)
    {
      while (q<t)
      {
        // Begin explore node q of the forest 19
        {
          k=unchosen_row[q];
          s=row_dec[k];
          for (l=0; l<n; l++)
            if (slack[l] > 0.)
            {
              double del=p->cost[k][l]-s+col_inc[l];
              if (del<slack[l])
              {
                if (del==0.)
                {
                  if (row_mate[l]<0)
                    goto breakthru;
                  slack[l]=0.;
                  parent_row[l]=k;
                  unchosen_row[t++]=row_mate[l];
                }
                else
                {
                  slack[l]=del;
                  slack_row[l]=k;
                }
              }
            }
        }
        // End explore node q of the forest 19
        q++;
      }

      // Begin introduce a new zero into the matrix 21
      s=INF;
      for (l=0; l<n; l++)
        if (slack[l]>0. && slack[l]<s)
          s=slack[l];
      for (q=0; q<t; q++)
        row_dec[unchosen_row[q]]+=s;
      for (l=0; l<n; l++)
        if (slack[l]>0.)
        {
          slack[l]-=s;
          if (slack[l]==0.)
          {
            // Begin look at a new zero 22
            k=slack_row[l];
            if (row_mate[l]<0)
            {
              for (j=l+1; j<n; j++)
                if (slack[j]==0.)
                  col_inc[j]+=s;
              goto breakthru;
            }
            else
            {
              parent_row[l]=k;
              unchosen_row[t++]=row_mate[l];
            }
            // End look at a new zero 22
          }
        }
        else
          col_inc[l]+=s;
      // End introduce a new zero into the matrix 21
    }
breakthru:
    // Begin update the matching 20
    while (1)
    {
      j=col_mate[k];
      col_mate[k]=l;
      row_mate[l]=k;
      if (j<0)
        break;
      k=parent_row[j];
      l=j;
    }
    // End update the matching 20
    if (--unmatched==0)
      goto done;
    // Begin get ready for another stage 17
    t=0;
    for (l=0; l<n; l++)
    {
      parent_row[l]= -1;
      slack[l]=INF;
    }
    for (k=0; k<m; k++)
      if (col_mate[k]<0)
        unchosen_row[t++]=k;
    // End get ready for another stage 17
  }
done:

/*  // Begin doublecheck the solution 23
  for (k=0; k<m; k++)
    for (l=0; l<n; l++)
      if (p->cost[k][l]<row_dec[k]-col_inc[l])
        exit(0);
  for (k=0; k<m; k++)
  {
    l=col_mate[k];
    if (l<0 || p->cost[k][l]!=row_dec[k]-col_inc[l])
      exit(0);
  }
  k=0;
  for (l=0; l<n; l++)
    if (col_inc[l])
      k++;
  if (k>m)
    exit(0);
  // End doublecheck the solution 23
*/  // End Hungarian algorithm 18

  for (i=0; i<m; ++i)
  {
    p->assignment[i][col_mate[i]]=HUNGARIAN_ASSIGNED;
    /*TRACE("%d - %d\n", i, col_mate[i]);*/
  }
  for (k=0; k<m; ++k)
  {
    for (l=0; l<n; ++l)
    {
      /*TRACE("%d ",p->cost[k][l]-row_dec[k]+col_inc[l]);*/
      p->cost[k][l]=p->cost[k][l]-row_dec[k]+col_inc[l];
    }
    /*TRACE("\n");*/
  }
/*  for (i=0; i<m; i++)
    cost+=row_dec[i];
  for (i=0; i<n; i++)
    cost-=col_inc[i];
*/
  free(slack);
  free(col_inc);
  free(parent_row);
  free(row_mate);
  free(slack_row);
  free(row_dec);
  free(unchosen_row);
  free(col_mate);
}


/***************
 * Usage in R :
 **************/

// Auxiliary function for hungarianAlgorithm below
double** array_to_matrix(double* m, int rows, int cols)
{
  double** r = (double**)calloc(rows,sizeof(double*));
  for (int i=0; i<rows; i++)
  {
    r[i] = (double*)calloc(cols,sizeof(double));
    for (int j=0; j<cols; j++)
      r[i][j] = m[i*cols+j];
  }
  return r;
}

//TODO: re-code this algorithm in a more readable way, based on
//https://www.topcoder.com/community/data-science/data-science-tutorials/assignment-problem-and-hungarian-algorithm/
// Get the optimal assignment, by calling hungarian_solve above; "distances" in columns
void hungarianAlgorithm(double* distances, int* pn, int* assignment)
{
  int n = *pn;
  double** mat = array_to_matrix(distances, n, n);

  hungarian_problem_t p;
  hungarian_init(&p, mat, n, n, HUNGARIAN_MODE_MINIMIZE_COST);

  hungarian_solve(&p);

  // Copy the optimal assignment in a R-friendly structure
  for (int i=0; i < n; i++) {
    for (int j=0; j < n; j++) {
      if (p.assignment[i][j])
        assignment[i] = j+1; //add 1 since we work with R
    }
  }

  hungarian_free(&p);
  for (int i=0; i<n; i++)
    free(mat[i]);
  free(mat);
}
