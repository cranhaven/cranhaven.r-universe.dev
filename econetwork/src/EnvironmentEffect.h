/*
* This file is part of econetwork
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <https://www.gnu.org/licenses/>
*/

#ifndef ECONETWORK_ENVIRONMENTEFFECT_H
#define ECONETWORK_ENVIRONMENTEFFECT_H
#include <iostream>
#include <Eigen/Dense>
#include <gsl/gsl_vector.h>

namespace econetwork{

  
  class EnvironmentEffect
  {
  private:
    // observed data
    unsigned int _nbLocations;
    unsigned int _nbSpecies;
    unsigned int _nbCovariates;
    Eigen::MatrixXd _covarE; // environment covariates 
    Eigen::MatrixXd _covarE2; // square environment covariates 
    // parameters
    Eigen::MatrixXd _a; // coefficients for covariates
    Eigen::MatrixXd _b; // coefficients for square covariates 
    // utilities
    //Eigen::ArrayXXd _prediction;   ==> WOULD BE TOO OFTEN RECOMPYUTED
    //void updatePrediction(){
    //_prediction = _a*_covarE.transpose() + _b*_covarE2.transpose(); //of size _nbSpecies x _nbLocations
    //std::cout << "The matrix _prediction is of size " << _prediction.rows() << "x" << _prediction.cols() << std::endl << _prediction << std::endl;
    //}  
  public:
    EnvironmentEffect(unsigned int nbSpecies, unsigned int nbLocations, unsigned int nbCovariates) :
      _nbLocations(nbLocations), _nbSpecies(nbSpecies), _nbCovariates(nbCovariates), 
      _covarE(nbLocations,nbCovariates), _covarE2(nbLocations,nbCovariates),
      _a(Eigen::MatrixXd::Constant(nbSpecies,nbCovariates,0.)),
      _b(Eigen::MatrixXd::Constant(nbSpecies,nbCovariates,0.)) //, _prediction(nbSpecies,nbLocations)
    {
      //updatePrediction();
    }
    ~EnvironmentEffect(){}
    unsigned int nbCovariates() const{
      return(_nbCovariates);
    }
    double prediction(unsigned int i, unsigned int l) const{
      double p1 = _a.row(i)*_covarE.row(l).transpose();
      double p2 = _b.row(i)*_covarE2.row(l).transpose();
      return(p1+p2);
    }
    const Eigen::ArrayXXd getPrediction() const{
      return(_a*_covarE.transpose() + _b*_covarE2.transpose());
      //return(_prediction);
    }
    void loadEnvironmentData(const double* EnvironmentR){
      const double* ptr = EnvironmentR;
      for(unsigned int k=0; k<_nbCovariates; k++)
	for(unsigned int l=0; l<_nbLocations; l++){
	  _covarE(l,k) = *ptr; // EnvironmentR is supposed to be colwise, i.e. R-like
	  ptr++;
	}
      _covarE2 = _covarE.cwiseProduct(_covarE);
      //updatePrediction();
    }
    void loadCoefficientA(const Eigen::MatrixXd& a){
      _a = a;
      //updatePrediction();
    }
    void loadCoefficientB(const Eigen::MatrixXd& b){
      _b = b;
      //updatePrediction();
    }
    const Eigen::MatrixXd& getE() const{ return(_covarE);}
    const Eigen::MatrixXd& getE2() const{ return(_covarE2);}
    Eigen::MatrixXd& getCoefficientA() { return(_a);}
    Eigen::MatrixXd& getCoefficientB() { return(_b);}
  };
  
}

#endif
