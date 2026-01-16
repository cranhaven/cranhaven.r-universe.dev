#include <R.h>
#include<math.h> 


//Function that calculates the cost matrix of distance LCSS with 
// the chosen window.
void lcss(int *tamx, int *tamy, int *sigma, double *costMatrix, double *subcost){

	int i,j,tam1,tam2,siggma1, siggma2, max;
	tam1=*tamx+1;
	tam2=*tamy+1;
	siggma1=fmin(*sigma+2, tam1);
	siggma2=fmin(*sigma+2, tam2);

	
	//We fill the cost matrix step by step

	//We fill position (0,0)
	costMatrix[0]=0.0;

	//We fill the edges of the matrix
	for(i=1;i<siggma1;i++){
	costMatrix[i*(*tamy+1)]=0.0;}

	for(j=1;j<siggma2;j++){
	costMatrix[j]=0.0;}

	//Fill all lines until i=sigma+2
	for(i=1;i<siggma1;i++){
	max=fmin(i+*sigma+1,tam2);
		for(j=1;j<max;j++){
		  if(subcost[(i-1)*(*tamy)+(j-1)]==0.0){costMatrix[i*(*tamy+1)+j]=costMatrix[(i-1)*(*tamy+1)+(j-1)]+1.0;}
		  else{costMatrix[i*(*tamy+1)+j]=fmax(costMatrix[(i-1)*(*tamy+1)+j],costMatrix[i*(*tamy+1)+(j-1)]);}
		}
	}

	//Fill the rest of the matrix
	for(i=(siggma1);i<(tam1);i++){
	  max=fmin(i+*sigma+1,tam2);
		for(j=(i-*sigma);j<max;j++){
		 if(subcost[(i-1)*(*tamy)+(j-1)]==0.0){costMatrix[i*(*tamy+1)+j]=costMatrix[(i-1)*(*tamy+1)+(j-1)]+1.0;}
	 	 else{costMatrix[i*(*tamy+1)+j]=fmax(costMatrix[(i-1)*(*tamy+1)+j],costMatrix[i*(*tamy+1)+(j-1)]);}
		}
	}
}
