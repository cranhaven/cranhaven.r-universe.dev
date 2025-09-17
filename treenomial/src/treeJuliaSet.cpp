// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace arma;


int escapeTime(std::complex<double> z, std::vector<std::complex <double>> C, int maxIter) {
  int i = 1;
  double sum = 0;
  std::complex<double> temp (0,0);

  int Csize = C.size();
  for(int k = 0; k < (Csize-1); k++){
    sum += abs(C[k]);
  }

  while(i < maxIter){
    i ++;

    temp =  std::complex<double> (0,0);

    for(int j = 0; j < Csize; j++){
      temp += std::pow(z,j)*C[j];
    }
    z = temp;

    if(abs(z) > (2*((sum)/(abs(C.back()))))){
      break;
    }

  }

  return(i);
}

// [[Rcpp::export]]
arma::mat juliaSet(std::vector<std::complex < double> > coeffs, int pixelLength, std::complex<double> center, double maxZ, int maxIter){

  double inc = (2*maxZ)/pixelLength;

  mat res(pixelLength,pixelLength);

  double re = real(center) - maxZ;
  std::complex<double> im(0,0);
  std::complex<double> z(0,0);

  for(int i = 0; i < pixelLength; i++){

    im = imag(center) +  maxZ;

    for(int j = 0; j < pixelLength; j++){

     z = re + im*(std::complex<double>(0,1));

     res(i,j) = escapeTime(z,coeffs,maxIter);

     im -= inc;
    }
    re += inc;
  }

  return(res);
}



