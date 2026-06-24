#include "MatTransMix.h"
#include <string.h>
#include "array.h"


void EigValDec(int size, double *W, double **A, double (*determinant))
{
  int i, j, INFO, N, LDA;
  char uplo='L';
  double *AT;
  char JOBZ='V';
  double *WORK;
  int LWORK;

  MAKE_VECTOR(AT,size*size);
  for (i=0; i<size; i++){
    for(j=0; j<size; j++) AT[j+size*i]=A[j][i];
  }

  N=size;
  LDA=size;

  LWORK=3*size-1;
  MAKE_VECTOR(WORK,LWORK);

  dsyev_(&JOBZ, &uplo, &N, AT, &LDA, W, WORK, &LWORK, &INFO);

  if (INFO==0){
    int i;
    (*determinant)=1.0;
    for (i=0;i<N;i++){
      (*determinant)*=W[i];
    }

  }

  for (i=0; i<size; i++){
    for(j=0; j<size; j++) A[j][i]=AT[j+size*i];
  }

  if (INFO!=0){

  }

  FREE_VECTOR(AT);
  FREE_VECTOR(WORK);

  return;
}
