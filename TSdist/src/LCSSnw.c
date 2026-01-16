#include <R.h>
#include<math.h> 


//Function that calculates the cost matrix of the LCSS distance
//with no window.
void lcssnw(int *tamx, int *tamy, double *costMatrix, double *subcost){

	int i,j,tam1,tam2;
	tam1=*tamx+1;
	tam2=*tamy+1;
	
	//We fill the cost matrix step by step.

	//We fill position (0,0)
	costMatrix[0]=0.0;

	//We fill the edges.
	for(i=1;i<tam1;i++){
	costMatrix[i*(*tamy+1)]=0.0;}

	for(j=1;j<tam2;j++){
	costMatrix[j]=0.0;}

	//We fill the rest of the matrix.
	for(i=1;i<tam1;i++){
		for(j=1;j<tam2;j++){
			if(subcost[(i-1)*(*tamy)+(j-1)]==0.0){costMatrix[i*(*tamy+1)+j]=costMatrix[(i-1)*(*tamy+1)+(j-1)]+1.0;}
			else{costMatrix[i*(*tamy+1)+j]=fmax(costMatrix[(i-1)*(*tamy+1)+j],costMatrix[i*(*tamy+1)+(j-1)]);}
					
		}
	}
}


