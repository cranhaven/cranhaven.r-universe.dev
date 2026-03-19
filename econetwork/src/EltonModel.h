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

#ifndef ECONETWORK_ELTONMODEL_H
#define ECONETWORK_ELTONMODEL_H
#ifdef STATICRAND
#include <random>
#endif
#include <memory>
#include <iostream>
#include <Eigen/Dense>
#include <gsl/gsl_vector.h>
#include <EnvironmentEffect.h>
#include <functional>

#undef FIXEDBETA
//#define FIXEDBETA

namespace econetwork{
  
#ifdef STATICRAND
  static std::random_device rd;
  static std::mt19937 rng(rd());
  double staticrand(double low, double high);
#endif
  enum class SamplingType
    {
     speciesDependent,
     locationDependent,
     constant
    };
  
  class EltonModel
  {
  private:
    // (un)observed data
    unsigned int _nbLocations;
    unsigned int _nbSpecies;
    Eigen::MatrixXd _metaA; // species interactions, possibly weighted
    Eigen::MatrixXd _presX; // real presence/absence (stored in double to use Eigen operations)
    // parameters
    Eigen::VectorXd _alphaSpecies; // species ubiquity (the greater, the higher the number of locations the species is present)
    Eigen::VectorXd _alphaLocations; // locations attractivty (the greater, the higher the number of species is expected in a location)
    Eigen::VectorXd _beta; // influence of species interactions using copresence
    Eigen::VectorXd _betaAbs; // influence of species interactions using coabsence
    std::shared_ptr<EnvironmentEffect> _peffect; // effect of the environment covariates
    Eigen::MatrixXd _epsilon; // interaction probability between present species
    SamplingType _samplingType;
    Eigen::VectorXd _sampling; // species sampling
    // utilities
    Eigen::ArrayXXd _probaPresence; // current probability that any species is present at any location
    Eigen::ArrayXXd _weight;
    Eigen::ArrayXXd _compat; // compatibility of any species at any location
    EltonModel();
    void computeCompatibility(double extra=0.);
    double computeQ2(const Eigen::ArrayXXd& weight);
  public:
    EltonModel(unsigned int nbSpecies, unsigned int nbLocations,
	       double alphainit, double betainit, double samplinginit,
	       SamplingType samplingType) :
      _nbLocations(nbLocations), _nbSpecies(nbSpecies), _metaA(Eigen::MatrixXd(nbSpecies,nbSpecies)),
      _presX(nbSpecies,nbLocations),
      _alphaSpecies(Eigen::VectorXd::Constant(nbSpecies,alphainit/2)),
      _alphaLocations(Eigen::VectorXd::Constant(nbLocations,alphainit/2)),
      _beta(Eigen::VectorXd::Constant(nbLocations,betainit)),
      _betaAbs(Eigen::VectorXd::Constant(nbLocations,betainit)),
      _peffect(nullptr),
      _epsilon(Eigen::MatrixXd::Constant(nbSpecies,nbSpecies,1.0)),
      _samplingType(samplingType), _probaPresence(nbSpecies,nbLocations), _weight(nbSpecies,nbLocations),
      _compat(Eigen::ArrayXXd::Constant(nbSpecies,nbLocations,1))
    {
      switch(_samplingType){
      case SamplingType::speciesDependent:
	_sampling = Eigen::VectorXd::Constant(nbSpecies,samplinginit); break;
      case SamplingType::locationDependent:
	_sampling =  Eigen::VectorXd::Constant(nbLocations,samplinginit); break;
      case SamplingType::constant:
	_sampling =  Eigen::VectorXd::Constant(1,samplinginit); break;
      }
    }
    ~EltonModel(){}
    void loadEpsilon(const double* epsilonR);
    void loadAmeta(const double* AmetaR);
    void loadAlphaSpecies(const Eigen::VectorXd& alphaSpecies) {_alphaSpecies=alphaSpecies;}
    void loadAlphaLocations(const Eigen::VectorXd& alphaLocations) {
      _alphaLocations=alphaLocations;
    }
    void loadBeta(const Eigen::VectorXd& beta) {_beta=beta;}
    void loadBetaAbs(const Eigen::VectorXd& betaAbs) {_betaAbs=betaAbs;}
    void loadEnvironment(std::shared_ptr<EnvironmentEffect> const& peffect){
      _peffect = peffect;
    }
    void loadCompatibility(const Eigen::ArrayXXd& compat) {_compat=compat;}
    // if reinit=true, X=Y at first
    // if withY=false, Y and sampling are not used
    double simulateX(const Eigen::MatrixXd& Y, bool reinit=false, bool withY=true);
    void updateAlphaBeta();
    const Eigen::MatrixXd& getX() const {return(_presX);}
    const Eigen::VectorXd& getAlphaSpecies() const {return(_alphaSpecies);}
    const Eigen::VectorXd& getAlphaLocations() const {return(_alphaLocations);}
    const Eigen::VectorXd& getBeta() const {return(_beta);}
    const Eigen::VectorXd& getBetaAbs() const {return(_betaAbs);}
    const Eigen::ArrayXXd& getCompat() const {return(_compat);}
    double getQ2(){
      Eigen::ArrayXXd weight = (_metaA*_presX).array();
      return(computeQ2(weight));    
    }
    double sampling(unsigned int i, unsigned int s){
      double p = 0;
      switch(_samplingType){
      case SamplingType::speciesDependent:
	p = _sampling[i]; break;
      case SamplingType::locationDependent:
	p = _sampling[s]; break;
      case SamplingType::constant:
	p = _sampling[0]; break;
      }
      return(p);
    }
    Eigen::MatrixXd simulateY(const Eigen::MatrixXd& Yinit, unsigned int nbiter=300);
    friend void GSLBindingMinusQ2WithDerivatives(const gsl_vector *  currentModelParameters, void * gslparams, double * f, gsl_vector * df);
  };
  
  struct GSLParams{
    EltonModel* _model;
    Eigen::ArrayXXd* _weight;
  };
}
#endif
