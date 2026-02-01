#include <math.h>
#include <stdlib.h>


double **make_mat(int nrow, int ncol);
void delete_mat( double **mat);
				  
void blasso(double * Sin, double *Min, double *Omin, int * pin, 
            int * qin, double * lamin, double * tol, int * maxit, double * Bout, int * warm)
{
  int p=*pin;
  int q=*qin;
  int i,j,k, kit,r,c;
  	double bdiff, bnew, H, J, AH, tmp;
  double **lam=make_mat(p,q);
  double **S=make_mat(p,p);
  double **M=make_mat(p,q);
  double **Om=make_mat(q,q); 
  double **B=make_mat(p,q);
  
  // Read in input 
 for ( i=0; i < p; i++)
  {
    for(j=0; j < q; j++)
    {
     lam[i][j]=lamin[j*p+i];
    }
  }

  for ( i=0; i < p; i++)
  {
    for(j=0; j < q; j++)
    {
      B[i][j]=0;
      if( *warm == 1)
      {
        B[i][j]=Bout[j*p+i];
      }
      M[i][j]=Min[j*p+i];
    }
  }
  for(i=0; i < p; i++)
  {
    for ( j=0; j < p; j++)
    {
      S[i][j]=Sin[j*p+i];
    }
  }
  for(i=0; i < q; i++)
  {
    for ( j=0; j <q; j++)
    {
      Om[i][j]=Omin[j*q+i];
    }
  }
  kit=0;
  bdiff=*tol+1;
  while( (bdiff > *tol) || (kit < *maxit) )
  {
    kit+=1;
    bdiff = 0;
    for(r=0; r < p; r++)
    {
      for(c=0; c <q; c++)
      {
        //compute J
        J=0;
        for(j=0; j <p; j++)
        {
          for(k=0; k <q; k++)
          {
            if(B[j][k] != 0)
            {
              J+=B[j][k]*S[r][j]*Om[k][c];  
            }
          }
        }
        H = B[r][c] + (M[r][c]-J)/(S[r][r]*Om[c][c]);
        AH = fabs(H);
        tmp =AH - lam[r][c]/(S[r][r]*Om[c][c]);
        bnew=0;
        if(tmp  > 0 )
        {
          if(H > 0)
          {
            bnew = tmp;
          }
          if( H < 0 )
          {
            bnew = -tmp;
          }
        }
        bdiff+=fabs(B[r][c]-bnew);
        B[r][c]=bnew;
      }
    }
  }
  //prepare output into Bout	
  for(j=0; j <q; j++)
  {
    for (i=0; i < p; i++)
    {
      Bout[j*p+i] = B[i][j];
    }
  }
  delete_mat(S);
  delete_mat(M);
  delete_mat(Om);
  delete_mat(B);
}


double **make_mat(int nrow, int ncol)
{
  double ** mat;
  int k;
  mat = (double **) malloc(nrow*sizeof(double*));
  mat[0]=(double*) malloc(nrow*ncol*sizeof(double));
  for(k=1; k < nrow; k++)
    mat[k] = mat[k-1] + ncol;
  return mat;
}
void delete_mat( double **mat)
{
  free(mat[0]);
  free(mat);  
}


