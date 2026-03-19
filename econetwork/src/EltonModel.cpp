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
#include <iostream>
#include <vector>
#include <EltonModel.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_multiroots.h>
#include <Rcpp.h>

namespace econetwork{
  using namespace std;

#ifdef STATICRAND
  double staticrand(double low, double high){
    std::uniform_real_distribution<double> urd;
    return urd(rng, decltype(urd)::param_type{low,high});
  }
#endif
  int GSLBindingDerivQ2WithAlphaBeta(const gsl_vector * currentModelParameters, void * gslparams, gsl_vector * f);
  void GSLBindingMinusQ2WithDerivatives(const gsl_vector * currentModelParameters, void * gslparams, double * f, gsl_vector *df);
  double GSLBindingMinusQ2(const gsl_vector * currentModelParameters, void * gslparams);
  void GSLBindingMinusQ2Derivatives(const gsl_vector * currentModelParameters, void * gslparams, gsl_vector *df);

  //////////////////////////////////////////////////////////////
  void EltonModel::loadEpsilon(const double* epsilonR){
    const double* ptr = epsilonR;
    for(unsigned int j=0; j<_nbSpecies; j++)
      for(unsigned int i=0; i<_nbSpecies; i++){
	_epsilon(i,j) = *ptr; // epsilon is supposed to be colwise, i.e. R-like
	ptr++;
      }
  }

  void EltonModel::loadAmeta(const double* AmetaR){
    const double* ptr = AmetaR;
    for(unsigned int j=0; j<_nbSpecies; j++)
      for(unsigned int i=0; i<_nbSpecies; i++){
	_metaA(i,j) = *ptr; // Ameta is supposed to be colwise, i.e. R-like
	ptr++;
      }
  }
  
  //////////////////////////////////////////////////////////////
  void EltonModel::computeCompatibility(double extra){
    for(unsigned int i=0; i<_nbSpecies; i++){
      Eigen::VectorXd mini = Eigen::VectorXd::Constant(_peffect->nbCovariates(),1e16);
      Eigen::VectorXd maxi = Eigen::VectorXd::Constant(_peffect->nbCovariates(),-1e16);      
      for(unsigned int l=0; l<_nbLocations; l++){
	if(_presX(i,l)>0){ // using cwiseProduct, min/max can be 0. Filtering
	  for(unsigned int k=0; k<_peffect->nbCovariates(); k++){
	    if(_peffect->getE()(l,k)>maxi[k]) maxi[k] = _peffect->getE()(l,k);
	    if(_peffect->getE()(l,k)<mini(k)) mini[k] = _peffect->getE()(l,k);
	  }
	}
      }
      for(unsigned int k=0; k<_peffect->nbCovariates(); k++){
	auto lag = maxi[k]-mini[k];
	maxi[k] += lag*extra;
	mini[k] -= lag*extra;
      }
      for(unsigned int l=0; l<_nbLocations; l++){
	for(unsigned int k=0; k<_peffect->nbCovariates(); k++){
	  if(_peffect->getE()(l,k)>maxi[k])
	    _compat(i,l) = 0;
	  if(_peffect->getE()(l,k)<mini[k])
	    _compat(i,l) = 0;
	}
      }
    }
  }
  
  //////////////////////////////////////////////////////////////
  double EltonModel::simulateX(const Eigen::MatrixXd& Y, bool reinit, bool withY){
    if(reinit) _presX = Y;
    double logL = 0;
    std::vector<unsigned int> vrange(_nbSpecies,0);
    for(unsigned int i=0; i<_nbSpecies; i++) vrange[i] = i;
    // std::random_shuffle(vrange.begin(), vrange.end());
    for (std::vector<unsigned int>::iterator it=vrange.begin(); it!=vrange.end(); ++it){
      unsigned int i=*it;
      //for(unsigned int i=0; i<_nbSpecies; i++){
#pragma omp parallel for
      for(unsigned int l=0; l<_nbLocations; l++){
	if(_compat(i,l)>0){
	    if ((withY?Y(i,l):0)<1){ // not testing equality to 1 with float
	      double a = _betaAbs(l)*_metaA.row(i)*( (Eigen::VectorXd::Ones(_nbSpecies)-_presX.col(l)).array() * _compat.col(l) ).matrix();
	      double b = _alphaSpecies(i) + _alphaLocations(l) + _peffect->prediction(i,l) + _beta(l)*_metaA.row(i)*_presX.col(l);
	      double c = (1-(withY?sampling(i,l):0));
	      double probaabsil = 1/(1+c*exp(b-a));
	      double probapresil = 1/(exp(a-b)/c+1);
#ifdef STATICRAND
	      _presX(i,l) = ((staticrand(0,1)<probaabsil)?0:1);
#else
	      _presX(i,l) = ((R::unif_rand()<probaabsil)?0:1);
#endif
	      _probaPresence(i,l) = probapresil;
	    } else{
	      _presX(i,l) = 1;
	      _probaPresence(i,l) = 1;
	    }
	}
      }
    }
    // updating compatibility
    if(withY)
      computeCompatibility(0.);
    return(logL);
  }

  Eigen::MatrixXd EltonModel::simulateY(const Eigen::MatrixXd& Yinit, unsigned int nbiter){
    for(unsigned int k=0; k<=nbiter; k++){
      simulateX(Yinit, k==0, false);
#ifdef VERBOSE
      if(k%100==0){
	cout.precision(4);
	cout<<"# Y proposed"<<endl<<_presX.mean()<<endl;
      }
#endif
    }
    Eigen::MatrixXd Y =  Eigen::MatrixXd::Zero(_nbSpecies,_nbLocations);
    for(unsigned int i=0; i<_nbSpecies; i++){
      for(unsigned int l=0; l<_nbLocations; l++){
	if (_presX(i,l)==1){
#ifdef STATICRAND
	  if (staticrand(0,1)<sampling(i,l)) Y(i,l)=1;
#else
	  if (R::unif_rand()<sampling(i,l)) Y(i,l)=1;
#endif
	}
      }
    }
    // updating compatibility
    computeCompatibility(0.);
    return(Y);
  }
     
  //////////////////////////////////////////////////////////////
  void EltonModel::updateAlphaBeta(){
    // linking to function for which we search for the minimum
    _weight = (_metaA*_presX).array();
    GSLParams gslparams = {this, &_weight};
    gsl_multimin_function_fdf my_func;
    my_func.n = _nbSpecies+3*_nbLocations+2*_nbSpecies*_peffect->nbCovariates();
    gsl_vector *x = gsl_vector_alloc(my_func.n);
    for(unsigned int i=0; i<_nbSpecies; i++)
      gsl_vector_set(x,i,_alphaSpecies(i));
    for(unsigned int l=0; l<_nbLocations; l++)
      gsl_vector_set(x,_nbSpecies+l,_alphaLocations(l));
    for(unsigned int l=0; l<_nbLocations; l++)
      gsl_vector_set(x,_nbSpecies+_nbLocations+l,_beta(l));
    for(unsigned int l=0; l<_nbLocations; l++)
      gsl_vector_set(x,_nbSpecies+2*_nbLocations+l,_betaAbs(l));
    for(unsigned int i=0; i<_nbSpecies; i++)
      for(unsigned int k=0; k<_peffect->nbCovariates(); k++){
	gsl_vector_set(x,_nbSpecies+3*_nbLocations+i*_peffect->nbCovariates()+k,_peffect->getCoefficientA()(i,k));
      }
    for(unsigned int i=0; i<_nbSpecies; i++)
      for(unsigned int k=0; k<_peffect->nbCovariates(); k++){
	gsl_vector_set(x,_nbSpecies+3*_nbLocations+_nbSpecies*_peffect->nbCovariates()+i*_peffect->nbCovariates()+k,_peffect->getCoefficientB()(i,k));
      }
    my_func.f = &GSLBindingMinusQ2;
    my_func.df = &GSLBindingMinusQ2Derivatives;
    my_func.fdf = &GSLBindingMinusQ2WithDerivatives;
    my_func.params = &gslparams;    
    // solving
    const gsl_multimin_fdfminimizer_type * solverType = gsl_multimin_fdfminimizer_vector_bfgs2;
    gsl_multimin_fdfminimizer * solver = gsl_multimin_fdfminimizer_alloc(solverType, _nbSpecies+3*_nbLocations+2*_nbSpecies*_peffect->nbCovariates());
    int status;
    // The user-supplied tolerance tol corresponds to the parameter \sigma used by Fletcher
    // A value of 0.1 is recommended for typical use
    double initstepsize = 0.01;
    double tol = 0.05;
    status = gsl_multimin_fdfminimizer_set(solver, &my_func, x, initstepsize, tol);
#ifdef VERBOSE
    printf("status (init) = %s\n", gsl_strerror(status));
    cout.precision(6);
    cout<<"initial value: "<<solver->f<<endl;
    cout.precision(4);
#endif
    unsigned int iter = 0;
    double prevf = 1e9;
    bool convergence = false;
    do{
      iter++;
      status = gsl_multimin_fdfminimizer_iterate(solver);
      //printf("status = %s\n", gsl_strerror(status));
      if (status)   /* check if solver is stuck */
	break;	
#ifdef VERBOSE
      if(iter%10==0){
	printf ("%5d %10.5f\n", iter, solver->f);
      } 
#endif
      //gsl_vector_fprintf(stdout, solver->gradient, "%g");    
      status = gsl_multimin_test_gradient(solver->gradient, 1e-2);
      convergence = ((prevf-solver->f)/prevf < 1e-3);
      prevf = solver->f;
    } while(status == GSL_CONTINUE && iter < 500 && !convergence);
    // copying result
    for(unsigned int i=0; i<_nbSpecies; i++)
      _alphaSpecies(i) = gsl_vector_get(solver->x,i);
    for(unsigned int l=0; l<_nbLocations; l++)
      _alphaLocations(l) = gsl_vector_get(solver->x,_nbSpecies+l);
    for(unsigned int l=0; l<_nbLocations; l++)
      _beta(l) = gsl_vector_get(solver->x,_nbSpecies+_nbLocations+l);
    for(unsigned int l=0; l<_nbLocations; l++)
      _betaAbs(l) = gsl_vector_get(solver->x,_nbSpecies+2*_nbLocations+l);
    for(unsigned int i=0; i<_nbSpecies; i++)
      for(unsigned int k=0; k<_peffect->nbCovariates(); k++)
	_peffect->getCoefficientA()(i,k) = gsl_vector_get(solver->x,_nbSpecies+3*_nbLocations+i*_peffect->nbCovariates()+k);
    for(unsigned int i=0; i<_nbSpecies; i++)
      for(unsigned int k=0; k<_peffect->nbCovariates(); k++)
	_peffect->getCoefficientB()(i,k) = gsl_vector_get(solver->x,_nbSpecies+3*_nbLocations+_nbSpecies*_peffect->nbCovariates()+i*_peffect->nbCovariates()+k);
    // freeing
#ifdef VERBOSE
    cout.precision(6);
    cout<<"final value: "<<solver->f<<endl;
    printf("status (final) = %s\n", gsl_strerror(status));
    cout<<"after "<<iter<<" iterations "<<endl;
    cout.precision(4);
    cerr<<"After "<<iter<<" iterations, objective: "<<computeQ2(_weight)<<endl;; 
#endif
    gsl_multimin_fdfminimizer_free(solver);
    gsl_vector_free(x);
  }
  
  //////////////////////////////////////////////////////////////
  void GSLBindingMinusQ2WithDerivatives(const gsl_vector * currentModelParameters, void * gslparams, double * f, gsl_vector *df){
    EltonModel * ptrmodel = ((struct GSLParams *) gslparams)->_model;
    Eigen::ArrayXXd * ptrweight = ((struct GSLParams *) gslparams)->_weight;
    // Copying alpha/beta (after backup)
    Eigen::VectorXd alphaSpeciesBack = ptrmodel->_alphaSpecies;
    Eigen::VectorXd alphaLocationsBack = ptrmodel->_alphaLocations;
    Eigen::VectorXd betaBack = ptrmodel->_beta;
    Eigen::VectorXd betaAbsBack = ptrmodel->_betaAbs;
    Eigen::MatrixXd aBack = ptrmodel->_peffect->getCoefficientA();
    Eigen::MatrixXd bBack = ptrmodel->_peffect->getCoefficientB();
    // Copying parameters
    for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
      ptrmodel->_alphaSpecies(i) =  gsl_vector_get(currentModelParameters,i);
    for(unsigned int l=0; l<ptrmodel->_nbLocations; l++)
      ptrmodel->_alphaLocations(l) =  gsl_vector_get(currentModelParameters,ptrmodel->_nbSpecies+l);
    for(unsigned int l=0; l<ptrmodel->_nbLocations; l++)
      ptrmodel->_beta(l) =  gsl_vector_get(currentModelParameters,ptrmodel->_nbSpecies+ptrmodel->_nbLocations+l);
    for(unsigned int l=0; l<ptrmodel->_nbLocations; l++)
      ptrmodel->_betaAbs(l) =  gsl_vector_get(currentModelParameters,ptrmodel->_nbSpecies+2*ptrmodel->_nbLocations+l);
    for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
      for(unsigned int k=0; k<ptrmodel->_peffect->nbCovariates(); k++)
	ptrmodel->_peffect->getCoefficientA()(i,k) = gsl_vector_get(currentModelParameters, ptrmodel->_nbSpecies+3*ptrmodel->_nbLocations+i*ptrmodel->_peffect->nbCovariates()+k);
    for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
      for(unsigned int k=0; k<ptrmodel->_peffect->nbCovariates(); k++)
	ptrmodel->_peffect->getCoefficientB()(i,k) = gsl_vector_get(currentModelParameters, ptrmodel->_nbSpecies+3*ptrmodel->_nbLocations+ptrmodel->_nbSpecies*ptrmodel->_peffect->nbCovariates()+i*ptrmodel->_peffect->nbCovariates()+k);
    // Utilities
    Eigen::ArrayXXd arrAlpha, arrBeta, arrBetaAbs, weightdiff;
#pragma omp parallel
    {
#pragma omp single nowait
      {
#pragma omp task depend(out:arrAlpha)
	{
	  arrAlpha = ptrmodel->_alphaSpecies.array().replicate(1,ptrmodel->_nbLocations) + ptrmodel->_alphaLocations.array().transpose().replicate(ptrmodel->_nbSpecies,1) + ptrmodel->_peffect->getPrediction();
	}
#pragma omp task depend(out:arrBeta)
	{
	  arrBeta = ptrmodel->_beta.array().transpose().replicate(ptrmodel->_nbSpecies,1);
	}
#pragma omp task depend(out:arrBetaAbs)
	{
	  arrBetaAbs = ptrmodel->_betaAbs.array().transpose().replicate(ptrmodel->_nbSpecies,1);
	}
#pragma omp task depend(out:weightdiff)
	{
	  weightdiff = (ptrmodel->_metaA*(((1-ptrmodel->_probaPresence)*ptrmodel->_compat).matrix())).array();
	  //weightdiff = ptrmodel->_metaA.rowwise().sum().array().replicate(1,ptrmodel->_nbLocations) - *ptrweight;
	}
      }
    }
    // Computing Q2
    if(f){
      Eigen::ArrayXXd a, b, cmax, arrQ2a, arrQ2b;
#pragma omp parallel
      {
#pragma omp single nowait
	{
#pragma omp task depend(in:arrBetaAbs,weightdiff) depend(out:a)
	  {
	    a = weightdiff*arrBetaAbs;
	  }
#pragma omp task depend(in:arrAlpha,arrBeta) depend(out:b)
	  {
	    b = arrAlpha+(*ptrweight)*arrBeta;
	  }
#pragma omp task depend(in:arrBetaAbs,weightdiff) depend(out:arrQ2a)
	  {
	    arrQ2a = (1-ptrmodel->_probaPresence)*arrBetaAbs*weightdiff;
	  }
#pragma omp task depend(in:arrAlpha,arrBeta) depend(out:arrQ2b)
	  {
	    arrQ2b = (ptrmodel->_probaPresence)*(arrAlpha+(*ptrweight)*arrBeta);
	  }
	  
#pragma omp task depend(in:a,b) depend(out:arrQ2b)
	  {
	    cmax = a.cwiseMax(b);
	  }
	}
      }
      // log-sum trick https://gregorygundersen.com/blog/2020/02/09/log-sum-exp/
      Eigen::ArrayXXd arrQ2 = arrQ2a + arrQ2b - (cmax+log(exp(a-cmax)+exp(b-cmax))); // i.e. log(expoa+expob);
      arrQ2 = arrQ2*ptrmodel->_compat;
      // Output opposite to minimize -Q2
      auto Q2 = (arrQ2.rowwise().sum()).sum();
      *f = -Q2;
    }    
    // Computing derivatives
    if(df){
      auto c = weightdiff;
      auto d = *ptrweight;
      Eigen::ArrayXXd a, b, cmax, expaoversum, expboversum, derivAlphas, derivAlphal, term1a, term2a, term1b, term2b, derivBeta, derivBetaAbs;
      //Eigen::ArrayXXd tmp, derivCoeffA, derivCoeffB;
      Eigen::MatrixXd tmp, derivCoeffA, derivCoeffB;
#pragma omp parallel num_threads(4)
      {
#pragma omp single nowait
	{
#pragma omp task depend(in:arrBetaAbs,weightdiff) depend(out:a)
	  {
	    a = weightdiff*arrBetaAbs;
	  }
#pragma omp task depend(in:arrAlpha,arrBeta) depend(out:b)
	  {
	    b = arrAlpha+*ptrweight*arrBeta;
	  }
#pragma omp task depend(in:a,b) depend(out:cmax)
	  {
	    cmax = a.cwiseMax(b);
	  }
#pragma omp task depend(in:a,b,cmax) depend(out:expaoversum)
	  {
	    //auto expaoversum = exp(a) / (exp(a)+exp(b));
	    //expaoversum = 1 / (1+exp(b-a));
	    expaoversum = exp( a - (cmax+log(exp(a-cmax)+exp(b-cmax))) );
	  }
#pragma omp task depend(in:a,b,cmax) depend(out:expboversum)
	  {
	    expboversum = exp( b - (cmax+log(exp(a-cmax)+exp(b-cmax))) );
	  }
#pragma omp task depend(in:expboversum) depend(out:derivAlphas)
	  { 
	    //derivAlphas = ((ptrmodel->_probaPresence - 1/(exp(a-b)+1))*ptrmodel->_compat).rowwise().sum();
	    derivAlphas = ((ptrmodel->_probaPresence - expboversum)*ptrmodel->_compat).rowwise().sum();
	  }
#pragma omp task depend(in:expboversum) depend(out:derivAlphal)
	  {
	    //derivAlphal = ((ptrmodel->_probaPresence - 1/(exp(a-b)+1))*ptrmodel->_compat).colwise().sum();
	    derivAlphal = ((ptrmodel->_probaPresence - expboversum)*ptrmodel->_compat).colwise().sum();
	  }
#pragma omp task depend(out:term1a)
	  { 
	    term1a = (1-ptrmodel->_probaPresence)*weightdiff;
	  }
#pragma omp task depend(out:term1b)
	  { 
	    term1b = ptrmodel->_probaPresence*(*ptrweight);
	  }
#pragma omp task depend(in:c,expaoversum) depend(out:term2a)
	  { 
	    //term2a = c/(1+exp(b-a));
	    term2a = c*expaoversum; // /(1+exp(b-a));
	  }
#pragma omp task depend(in:d,expboversum) depend(out:term2b)
	  {
	    //term2b = d/(exp(a-b)+1);
	    term2b = d*expboversum; // /(exp(a-b)+1);
	  }
#pragma omp task depend(in:term1b,term2b) depend(out:derivBeta)
	  {
	    derivBeta = ((term1b-term2b)*ptrmodel->_compat).colwise().sum();
	  }
#pragma omp task depend(in:term1a,term2a) depend(out:derivBetaAbs)
	  {
	    derivBetaAbs = ((term1a-term2a)*ptrmodel->_compat).colwise().sum();
	  }
#pragma omp task depend(in:a,b,expboversum) depend(out:tmp)
	  {
	    tmp = ((ptrmodel->_probaPresence - expboversum)*ptrmodel->_compat).matrix();
	  }
#pragma omp task depend(in:tmp) depend(out:derivCoeffA)
	  {
	    derivCoeffA = tmp * ptrmodel->_peffect->getE();
	  }
#pragma omp task depend(in:tmp) depend(out:derivCoeffB)
	  {
	    derivCoeffB = tmp * ptrmodel->_peffect->getE2();
	  }
	}
      }
      // Output the derivative opposite to minimize -Q2
      //#define FIXEDALPHA
#ifdef FIXEDALPHA
      for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
	gsl_vector_set(df,i,-derivAlphas(i)); 
      for(unsigned int l=0; l<ptrmodel->_nbLocations; l++)
	gsl_vector_set(df,ptrmodel->_nbSpecies+l,-derivAlphal(l));
#else
      for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
	gsl_vector_set(df,i,-derivAlphas(i)); 
      for(unsigned int l=0; l<ptrmodel->_nbLocations; l++)
	gsl_vector_set(df,ptrmodel->_nbSpecies+l,-derivAlphal(l));
#endif
      //#define FIXEDBETA
#ifdef FIXEDBETA
      for(unsigned int l=0; l<ptrmodel->_nbLocations; l++){
	gsl_vector_set(df,ptrmodel->_nbSpecies+ptrmodel->_nbLocations+l,0);
	gsl_vector_set(df,ptrmodel->_nbSpecies+2*ptrmodel->_nbLocations+l,0);
      }
#else
      for(unsigned int l=0; l<ptrmodel->_nbLocations; l++){
	gsl_vector_set(df,ptrmodel->_nbSpecies+ptrmodel->_nbLocations+l,-derivBeta(l));
	gsl_vector_set(df,ptrmodel->_nbSpecies+2*ptrmodel->_nbLocations+l,-derivBetaAbs(l));
      }
#endif
      //#define NOENV
#ifdef NOENV
      for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
	for(unsigned int k=0; k<ptrmodel->_peffect->nbCovariates(); k++)
	  gsl_vector_set(df,ptrmodel->_nbSpecies+3*ptrmodel->_nbLocations+i*ptrmodel->_peffect->nbCovariates()+k,0);
      for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
	for(unsigned int k=0; k<ptrmodel->_peffect->nbCovariates(); k++)
	  gsl_vector_set(df,ptrmodel->_nbSpecies+3*ptrmodel->_nbLocations+ptrmodel->_nbSpecies*ptrmodel->_peffect->nbCovariates()+i*ptrmodel->_peffect->nbCovariates()+k,0);
#else
      for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
	for(unsigned int k=0; k<ptrmodel->_peffect->nbCovariates(); k++)
	  gsl_vector_set(df,ptrmodel->_nbSpecies+3*ptrmodel->_nbLocations+i*ptrmodel->_peffect->nbCovariates()+k,-derivCoeffA(i,k));
      for(unsigned int i=0; i<ptrmodel->_nbSpecies; i++)
	for(unsigned int k=0; k<ptrmodel->_peffect->nbCovariates(); k++)
	  gsl_vector_set(df,ptrmodel->_nbSpecies+3*ptrmodel->_nbLocations+ptrmodel->_nbSpecies*ptrmodel->_peffect->nbCovariates()+i*ptrmodel->_peffect->nbCovariates()+k,-derivCoeffB(i,k));
#endif
    }
    // Reinitializing       
    ptrmodel->_alphaSpecies = alphaSpeciesBack;  
    ptrmodel->_alphaLocations = alphaLocationsBack;
    ptrmodel->_beta = betaBack;
    ptrmodel->_betaAbs = betaAbsBack;
    ptrmodel->_peffect->loadCoefficientA(aBack);
    ptrmodel->_peffect->loadCoefficientB(bBack);
  }

  double GSLBindingMinusQ2(const gsl_vector * currentModelParameters, void * gslparams){
    double minusQ2;
    GSLBindingMinusQ2WithDerivatives(currentModelParameters,gslparams,&minusQ2,NULL);
    return(minusQ2);   
  }
  
  void GSLBindingMinusQ2Derivatives(const gsl_vector * currentModelParameters, void * gslparams, gsl_vector *df){
    GSLBindingMinusQ2WithDerivatives(currentModelParameters,gslparams,NULL,df);
  }

  //////////////////////////////////////////////////////////////
  double EltonModel::computeQ2(const Eigen::ArrayXXd& weight){ 
    Eigen::ArrayXXd arrAlpha = _alphaSpecies.array().replicate(1,_nbLocations) + _alphaLocations.array().transpose().replicate(_nbSpecies,1) + _peffect->getPrediction();
    Eigen::ArrayXXd arrBeta = _beta.array().transpose().replicate(_nbSpecies,1);
    Eigen::ArrayXXd arrBetaAbs = _betaAbs.array().transpose().replicate(_nbSpecies,1);
    Eigen::ArrayXXd weightdiff = (_metaA*(((1-_probaPresence)*_compat).matrix())).array();
    Eigen::ArrayXXd denom = exp(weightdiff*arrBetaAbs) + exp(arrAlpha+weight*arrBeta);
    Eigen::ArrayXXd a = weightdiff*arrBetaAbs;
    Eigen::ArrayXXd b = arrAlpha+weight*arrBeta;
    auto cmax = a.cwiseMax(b);
    Eigen::ArrayXXd arrQ2 = (1-_probaPresence)*arrBetaAbs*weightdiff
      + _probaPresence*(arrAlpha+weight*arrBeta)
      - (cmax+log(exp(a-cmax)+exp(b-cmax)));
    arrQ2 = arrQ2*_compat;
    return((arrQ2.rowwise().sum()).sum());
  }
  
}
