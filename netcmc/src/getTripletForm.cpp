#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix getTripletForm(NumericMatrix membershipMatrix)
{
  int numberOfRowsInTripletForm = 0;
  int numberOfRowsInTripletFormIndex = 0;
  int numberOfRowsInMembershipMatrix = membershipMatrix.rows();
  int numberOfColumnsInMembershipMatrix = membershipMatrix.cols();
  
  for(int i = 0; i < numberOfRowsInMembershipMatrix; i++){
    for(int j = 0; j < numberOfColumnsInMembershipMatrix; j++){
      if(membershipMatrix(i,j) != 0){
        numberOfRowsInTripletForm += 1;    
      }
    }
  }
  
  NumericMatrix membershipMatrixInTripletForm(numberOfRowsInTripletForm, 3);
  
  for(int i = 0; i < numberOfRowsInMembershipMatrix; i++){
    for(int j = 0; j < numberOfColumnsInMembershipMatrix; j++){
      if(membershipMatrix(i,j) != 0){
        membershipMatrixInTripletForm(numberOfRowsInTripletFormIndex, 0) = i;
        membershipMatrixInTripletForm(numberOfRowsInTripletFormIndex, 1) = j;
        membershipMatrixInTripletForm(numberOfRowsInTripletFormIndex, 2) = membershipMatrix(i,j);
        numberOfRowsInTripletFormIndex ++;
      }
    }
  }
  
  return membershipMatrixInTripletForm;
  
}
