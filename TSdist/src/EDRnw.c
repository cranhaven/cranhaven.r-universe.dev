#include <R.h>
#include<math.h> 

//Function that calculates the cost matrix of the EDR distance.
void edrnw(int *tamx, int *tamy, double *costMatrix, double *subcost){

	int i,j,tam1,tam2;
	tam1=*tamx+1;
	tam2=*tamy+1;

	//The (0,0) position of the matrix is filled
	costMatrix[0]=0.0;

	//The edges of the matrix are filled.
	for(i=1;i<tam1;i++){
	costMatrix[i*(tam2)]=i;}

	for(j=1;j<tam2;j++){
	costMatrix[j]=j;}

	//The rest of the matrix is filled.
	for(i=1;i<tam1;i++){
		for(j=1;j<tam2;j++){
		costMatrix[i*(tam2)+j]=fmin(fmin(1+costMatrix[(i-1)*(tam2)+j],1+costMatrix[i*(tam2)+(j-1)]),subcost[(i-1)*(*tamy)+(j-1)]+costMatrix[(i-1)*(tam2)+(j-1)]);
	
		}
	}
}



    
