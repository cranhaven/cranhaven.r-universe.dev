#include <R.h>
#include<math.h> 


//Function that calculates the cost matrix of the EDR distance.
void erp(double *x, double *y, int *tamx, int *tamy, int *sigma, double *costMatrix, double *distMatrix, double *g){

	int i,j,tam1,tam2, siggma1, siggma2, max;
	tam1=*tamx+1;
	tam2=*tamy+1;
	siggma1=fmin(*sigma+2, tam1);
	siggma2=fmin(*sigma+2, tam2);

	double dist1, dist2, dist12;

	

	//The (0,0) position of the matrix is filled
	costMatrix[0]=0.0;

	//The edges of the matrix are filled.
	for(i=1;i<siggma1;i++){
	dist1=fabs(*g-x[i-1]);
	costMatrix[i*(*tamy+1)]= dist1+costMatrix[(i-1)*(*tamy+1)];}

	for(j=1;j<siggma2;j++){
	dist2=fabs(*g-y[j-1]);
	costMatrix[j]=dist2+costMatrix[j-1];}

	//Fill all lines until i=sigma+2
	for(i=1;i<siggma1;i++){
	max=fmin(i+*sigma+1,tam2);
		for(j=1;j<max;j++){
		dist1=fabs(*g-x[i-1]); //Cost if y[i-1] is a gap
		dist2=fabs(*g-y[j-1]); //Cost if x[i-1] is a gap
		dist12=distMatrix[(i-1)*(*tamy)+(j-1)]; // Cost if no gaps are left.
		costMatrix[i*(*tamy+1)+j]=fmin(fmin(dist1+costMatrix[(i-1)*(*tamy+1)+j],dist2+costMatrix[i*(*tamy+1)+(j-1)]),dist12+costMatrix[(i-1)*(*tamy+1)+(j-1)]);
		}
	}

	//Fill the rest of the matrix
	for(i=(siggma1);i<(tam1);i++){
	  max=fmin(i+*sigma+1,tam2);
		for(j=(i-*sigma);j<max;j++){
		dist1=fabs(*g-x[i-1]); //Cost if y[i-1] is a gap
		dist2=fabs(*g-y[j-1]); //Cost if x[i-1] is a gap
		dist12=distMatrix[(i-1)*(*tamy)+(j-1)]; // Cost if no gaps are left.
		costMatrix[i*(*tamy+1)+j]=fmin(fmin(dist1+costMatrix[(i-1)*(*tamy+1)+j],dist2+costMatrix[i*(*tamy+1)+(j-1)]),dist12+costMatrix[(i-1)*(*tamy+1)+(j-1)]);
		}
	}
}






