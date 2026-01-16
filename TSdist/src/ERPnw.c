#include <R.h>
#include<math.h> 


//Function that calculates the cost matrix of the EDR distance.
void erpnw(double *x, double *y, int *tamx, int *tamy, double *costMatrix, double *distMatrix, double *g){

	int i,j,tam1,tam2;
	tam1=*tamx+1;
	tam2=*tamy+1;
	double dist1, dist2, dist12;
	

	//The (0,0) position of the matrix is filled
	costMatrix[0]=0.0;

	//The edges of the matrix are filled.
	for(i=1;i<tam1;i++){
	dist1=fabs(*g-x[i-1]);
	costMatrix[i*(tam2)]= dist1+costMatrix[(i-1)*(tam2)];}

	for(j=1;j<tam2;j++){
	dist2=fabs(*g-y[j-1]);
	costMatrix[j]=dist2+costMatrix[j-1];}

	//The rest of the matrix is filled.
	for(i=1;i<tam1;i++){
		for(j=1;j<tam2;j++){
		dist1=fabs(*g-x[i-1]); //Cost if y[i-1] is a gap
		dist2=fabs(*g-y[j-1]); //Cost if x[i-1] is a gap
		dist12=distMatrix[(i-1)*(*tamy)+(j-1)]; // Cost if no gaps are left.
		costMatrix[i*(tam2)+j]=fmin(fmin(dist1+costMatrix[(i-1)*(tam2)+j],dist2+costMatrix[i*(tam2)+(j-1)]),dist12+costMatrix[(i-1)*(tam2)+(j-1)]);
	
		}
	}
}






