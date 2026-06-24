
#include<math.h>
#include "array.h"
#include "MatTransMix.h"




void multiply(double **a, int arows, int acols,
	      double **b, int brows, int bcols, double **c)
{
  int i, j, k;
  
  for (i=0; i<arows; i++)
    for (j=0; j<bcols; j++) {
      c[i][j] = 0;
      for (k=0; k<acols; k++)
	c[i][j] += a[i][k] * b[k][j];
    }

}



void multiply2(double **a, int arows, int acols,
	       double **b, int brows, int bcols, double ***c, int m)
{
  int i, j, k;
  
  for (i=0; i<arows; i++)
    for (j=0; j<bcols; j++) {
      c[m][i][j] = 0;
      for (k=0; k<acols; k++)
	c[m][i][j] += a[i][k] * b[k][j];
    }

}


void cpy1(double ***a, int k, int nrows, int ncols, double **b)
{
  int i,j;
  for(i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      b[i][j]=a[k][i][j];
    }
  }

}



void cpy2(double **a, int nrows, int ncols, double ***b, int k)
{
  int i,j;
  for(i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      b[k][i][j]=a[i][j];
    }
  }

}



void cpyv(double **A, int col, int nrows, double *V){
  
  	int i;

	for(i=0; i<nrows; i++){
 		
      		V[i]=A[i][col];
    		
	}
}





int vecMin(double *x, int p, double (*min)){

	int i, minN;

	(*min) = x[0];
	minN = 0;

	for (i=0;i<p;i++){
		if (x[i] < (*min)){
			(*min) = x[i];
			minN = i;
		}
	}

	return minN;

}




int vecMax(double *x, int p, double (*max)){

	int i, maxN;

	(*max) = x[0];
	maxN = 0;

	for (i=0;i<p;i++){
		if (x[i] > (*max)){
			(*max) = x[i];
			maxN = i;
		}
	}

	return maxN;

}


int vec11vecSQ(double *y, int p, double **Res){
	
	int i,j;

	for (i=0;i<p;i++){
		for (j=0;j<p;j++){
			Res[i][j] = y[i]*y[j];
		}
	}
	
	return 0;
}



double vecNNvec(int p, double *y, double *x){
	
	int i;
	double Res;

	Res = 0;
	for (i=0;i<p;i++){
		Res = Res + y[i]*x[i];
	}

	return Res;
}



int vec_(int a, double *Res, double *Y){
	
	int i;

	for (i=0;i<a;i++){
		Res[i] = Res[i] - Y[i];
	}
	
	return 0;
}




double vAvt(double *v, int p, double **A){


	int i, j;
	double Res;
	double *Res1;

	MAKE_VECTOR(Res1, p);	


	anull(Res1, p);

	for(j=0; j<p; j++){

		for(i=0; i<p; i++){
		
			Res1[j] += v[i] * A[i][j];


		}

	}


	Res = 0;
	for(i=0; i<p; i++){
		Res += Res1[i] * v[i];
	}

	FREE_VECTOR(Res1);

	return(Res);

}





int mat_(int a, int b,double **Res, double **Y){
	
	int i,j;

	for (i=0;i<a;i++){
		for (j=0;j<b;j++){
			Res[i][j] = Res[i][j] - Y[i][j];
		}
	}
	
	return 0;
}


 

int vecsum(int a, int b,double **OO, double *Res){
	
	int i,j;

	for (i=0;i<a;i++){
		Res[i] = 0;
		for (j=0;j<b;j++){
			Res[i] = Res[i] + OO[i][j];
		}
	}
	
	return 0;
}



int MatrixProd(double **OO, int p, int m, double **Res){
     
     int i,j,k;

     for (i=0; i<p; i++){
         for (j=0; j<p; j++){
             Res[i][j]=0;
             for (k=0; k<m; k++){
                 Res[i][j]=Res[i][j]+OO[i][k]*OO[j][k];
             }
         }     
     }
     
     return 0;
}



int Kronecker(double **A, int a1, int a2, double **B, int b1, int b2, double **Res){

  int inda1, inda2, indb1, indb2, indRes1, indRes2;
  int i;
  int n;


  n = a1 * b1 * a2 * b2;

  indRes1 = 0;
  indRes2 = -1;

  inda1 = 0;
  inda2 = 0;
  indb1 = 0;
  indb2 = -1;

  for (i=0; i<n; i++){

    indb2++;
    indRes2++;

    if (indb2 == b2){

      indb2 = 0;
      inda2++;

      if (inda2 == a2){
	
	inda2 = 0;
	indb1++;
	indRes1++;
	indRes2 = 0;

	if (indb1 == b1){

	  indb1 = 0;
	  inda1++;

	}

      }
    }

    Res[indRes1][indRes2] = A[inda1][inda2] * B[indb1][indb2];

  }

  return 0;

}



int Gmat(int p, int m, double **Res){
     
     int a,b,i,i1,i2,n,ind;

	 n = 0;

     for (a=0; a<p; a++){
         for (b=0; b<p; b++){
         	
         	if (a < b){
         		i1 = b;
         		i2 = a;
         	} else {
         		i1 = a;
         		i2 = b;
         	}
         	
         	ind = m - (p - i2) * (p - i2 + 1) / 2 + i1 - i2;
         	
         	for (i=0; i<m; i++){
         	
         		if (i != ind ){
         			Res[n][i] = 0;	
         		} else {
         			Res[n][i] = 1;
         		}
         		
         	}
         	
         	n++;

         }     
     }
     
     return 0;
}




void tA(double **A, int a, int b, double **Res){

	int i,j;

   	for (i=0; i<a; i++){
		for (j=0; j<b; j++){
			Res[i][j] = A[j][i];
		}
	}
	
}




int ZXY(double **Z, int az, int bz, double **X, int ax, int bx, double **Y, int ay, int by, double **Res){

	double **Res1;

	MAKE_MATRIX(Res1, az, bx);	

	multiply(Z, az, bz, X, ax, bx, Res1);
	multiply(Res1, az, bx, Y, ay, by, Res);

	FREE_MATRIX(Res1);
 
        return 0;
    
}




void XAXt(double **X, int p, double **A, double **Res){

	double **Res1, **Res2;

	MAKE_MATRIX(Res1, p, p);	
	MAKE_MATRIX(Res2, p, p);

	tA(X, p, p, Res2);

	multiply(X, p, p, A, p, p, Res1);
	multiply(Res1, p, p, Res2, p, p, Res);

	FREE_MATRIX(Res1);
 	FREE_MATRIX(Res2);

}



void XAXt2(double **X, int p, double **A, double ***Res, int k){

	double **Res1, **Res2;

	MAKE_MATRIX(Res1, p, p);	
	MAKE_MATRIX(Res2, p, p);

	tA(X, p, p, Res2);

	multiply(X, p, p, A, p, p, Res1);
	multiply2(Res1, p, p, Res2, p, p, Res, k);

	FREE_MATRIX(Res1);
 	FREE_MATRIX(Res2);

}


void Anull(double **X, int ax, int bx){
     
     int i, j;

     for (i=0; i<ax; i++){
         for (j=0; j<bx; j++){
		X[i][j] = 0.0;
	 }
     }
}



void anull(double *x, int p){
     
     int i;

     for (i=0; i<p; i++){
	     x[i] = 0.0;
     }
}



void Anulli(int **X, int ax, int bx){
     
     int i, j;

     for (i=0; i<ax; i++){
         for (j=0; j<bx; j++){
		X[i][j] = 0;
	 }
     }
}



void anulli(int *x, int p){
     
     int i;

     for (i=0; i<p; i++){
	     x[i] = 0;
     }
}


void Anull3(double ***X, int ax, int bx, int cx){
     
     int i, j, k;

     for (i=0; i<ax; i++){
         for (j=0; j<bx; j++){
		for(k=0; k<cx; k++){
			X[i][j][k] = 0.0;
		}
	 }
     }
}



int asvector(double **X, int ax, int bx, double *ResVec){
     
    int i,j,k;

    k = 0;

    for (i=0; i<ax; i++){
         for (j=0; j<bx; j++){
		 	ResVec[k] = X[i][j];
		 	k++;
         }
     }

    return 0;
    
}



void cxS(int p, int K, double ***S, double c){

	int i, j, k;

	for (k=0; k<K; k++){
		for (i=0; i<p; i++){
			for (j=0; j<p; j++){
				S[k][i][j] = c * S[k][i][j];
			}
		}
	}

}




void AllPerms(int size,int **perms){

	int sch, i, j, v, w, finish, flag, ind;
	double **pat;
	int *cn;

	sch = 0;
	i = 0;
	j = -1;
	flag = 0;
	finish = 0;
	ind = 0;

	MAKE_MATRIX(pat, size, size);
	for (v=0; v<size; v++){
		for (w=0; w<size; w++){
			pat[v][w] = 0;
		}
	}

	MAKE_VECTOR(cn, size);
	for (v=0; v<size; v++){
		cn[v] = 0;
	}
  

	while (finish == 0){
    
		if (j != (size-1)){
			j = j+1;
		} else {
			if (flag == 1){
				j = 0;
				i = i+1;
				flag = 0;
			}
		}
    
		if (pat[i][j] == 0){
			for (v=0; v<size; v++){
				pat[i][v]=1;
				pat[v][j]=1;
			}
      
			sch = sch + 1;
			cn[sch-1] = j;
			flag = 1;
		}

		if ((sch == size) & (flag == 1)){
      
			for (v=0; v<size; v++){
				perms[ind][v] = cn[v];
			}

			ind++;
			flag = 0;
			sch = sch - 1;
			i = i - 1;
			j = cn[sch-1];
			sch = sch-1;
      
			for (v=0; v<size; v++){
				for (w=0; w<size; w++){
					pat[v][w] = 0;
				}
			}

			for (v=0; v<sch; v++){
				for (w=0; w<size; w++){
					pat[v][w] = 1;
					pat[w][cn[v]] = 1;
				}
			}    
      
		}


		if ((j == size - 1) & (flag == 0)){
			i = i - 1;
			
			sch = sch-1;

			if (sch >= 0){

				j = cn[sch];

				for (v=0; v<size; v++){
					for (w=0; w<size; w++){
						pat[v][w] = 0;
					}
				}

				if (sch > 0){
					for (v=0; v<sch; v++){
						for (w=0; w<size; w++){
							pat[v][w] = 1;
							pat[w][cn[v]] = 1;
						}
					}

				}
				
			}

			if (i >= 0){
				pat[i][j] = 1;
			}
		}

		if (sch == -1){
			finish = 1;
		}

	}

	FREE_MATRIX(pat);
	FREE_VECTOR(cn);

}


int Factorial(int a){
    int i;
    int res;
    
    res=1;
    for (i=1; i<(a+1); i++){
        res=res*i;
    }
    
    return res;
}





void extract(int n, int p, double **X, int *index, double **Y){
     
    int i,j,k;

    k = 0;

    for (i=0; i<n; i++){
        
	if(index[i] != 0){
	
	    for (j=0; j<p; j++){  
  
	        Y[k][j] = X[i][j];


	    }
	    k += 1;
	}
    }
}



