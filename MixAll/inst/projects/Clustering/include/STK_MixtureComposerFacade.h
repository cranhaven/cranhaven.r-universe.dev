/*--------------------------------------------------------------------*/
/*  Copyright (C) 2004-2015  Serge Iovleff, University Lille 1, Inria

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this program; if not, write to the
    Free Software Foundation, Inc.,
    59 Temple Place,
    Suite 330,
    Boston, MA 02111-1307
    USA

    Contact : S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
*/

/*
 * Project:  MixAll
 * created on: 11 Mars 2018
 * Author:   Iovleff, serge.iovleff@stkpp.org
 **/

/** @file STK_MixtureComposerFacade.h
 *  @brief In this file we define a facade class for p_composers.
 **/


#ifndef STK_MIXTURECOMPOSERFACADE_H
#define STK_MIXTURECOMPOSERFACADE_H

#include "STK_MixtureComposer.h"
#include "STK_MixtureSemiLearner.h"
#include "./CategoricalModels/STK_CategoricalMixtureManager.h"
#include "./DiagGaussianModels/STK_DiagGaussianMixtureManager.h"
#include "./GammaModels/STK_GammaMixtureManager.h"
#include "./KernelModels/STK_KernelMixtureManager.h"
#include "./PoissonModels/STK_PoissonMixtureManager.h"
#include "./PoissonModels/STK_PoissonMixtureManager.h"
#include "./PoissonModels/STK_PoissonMixtureManager.h"

namespace STK
{

/** The MixtureComposerFacade allows to interact with a composer
 *  for estimating a mixture model with less effort.
 *  This class stores also publicly an instance of a DataHandler and a KernelHandler.
 **/
template<class DataHandler_>
class MixtureComposerFacade
{
  public:
    /** pointer on the main p_composer */
    IMixtureComposer* p_composer_;
    /** data handler for model based mixture models */
    DataHandler_ handler_;
    /** handler for kernel mixture models */
    KernelHandler kerHandler_;
    /** Kernel Mixture Manager */
    KernelMixtureManager kmmManager_;

    /** constructor.
     *  @param p_composer p_composer to use
     **/
    MixtureComposerFacade( IMixtureComposer* p_composer = 0)
                         : p_composer_(p_composer)
                         , handler_()
                         , kerHandler_()
                         , kmmManager_(kerHandler_)
                         , diagGaussianManager_(handler_)
                         , poissonManager_(handler_)
                         , gammaManager_(handler_)
                         , categoricalManager_(handler_)
    {}
    /** destructor. */
    ~MixtureComposerFacade() { if (p_composer_) delete p_composer_;}

    // getters
    /** @return pointer on the composer */
    IMixtureComposer* const& p_composer() const
    { return p_composer_;}
    /** @return data handler */
    DataHandler_ const& handler() const
    { return handler_;}
    /** @return kernel handler */
    DataHandler_ const& kerHandler() const
    { return kerHandler_;}
    /** @return DiagGaussian mixtures  manager*/
    DiagGaussianMixtureManager<DataHandler_> const& diagGaussianManager() const
    { return diagGaussianManager_;}
    /** @return Poisson mixture models manager */
    PoissonMixtureManager<DataHandler_> const& poissonManager() const
    { return poissonManager_;}
    /** gamma mixture models manager */
    GammaMixtureManager<DataHandler_> const& gammaManager() const
    { return gammaManager_;}
    /** categorical mixture models manager */
    CategoricalMixtureManager<DataHandler_> const& categoricalManager() const
    { return categoricalManager_;}
    /** Kernel Mixture Manager */
    KernelMixtureManager const& kmmManager() const
    { return kmmManager_;}

    // setters
    /** set parameters of a component */
    bool setParameters( std::string const& idData, ArrayXX const& param);
    /** set diagonal Gaussian parameters */
    void setDiagGaussianParameters( std::string const& idData, ArrayXX const& param);
    /** set Poisson parameters */
    void setPoissonParameters( std::string const& idData, ArrayXX const& param);
    /** set gamma parameters */
    void setGammaParameters( std::string const& idData, ArrayXX const& param);
    /** set categorical parameters */
    void setCategoricalParameters( std::string const& idData, ArrayXX const& param);
    /** set Kernel Mixture Model parameters */
    void setKmmParameters( std::string const& idData, ArrayXX const& param);

    /** get parameters */
    bool getParameters( std::string const& idData, ArrayXX& param);
    /** get diagonal Gaussian parameters */
    void getDiagGaussianParameters( std::string const& idData, ArrayXX& param);
    /** get Poisson parameters */
    void getPoissonParameters( std::string const& idData, ArrayXX& param);
    /** get gamma parameters */
    void getGammaParameters( std::string const& idData, ArrayXX& param);
    /** get categorical parameters */
    void getCategoricalParameters( std::string const& idData, ArrayXX& param);
    /** get Kernel Mixture Model parameters */
    void getKmmParameters( std::string const& idData, ArrayXX& param);

    /** create a MixtureComposer
     *  @nbCluster number of cluster */
    void createMixtureComposer(int nbCluster);
    /** create a MixtureComposerFixedProp
     *  @nbCluster number of cluster */
    void createMixtureComposerFixedProp(int nbCluster);
    /** create a MixtureSemiLearner
     *  @nbCluster number of cluster */
    void createMixtureSemiLearner(int nbCluster);
    /** create a MixtureSemiLearnerFixedProp
     *  @nbCluster number of cluster */
    void createMixtureSemiLearnerFixedProp(int nbCluster);
    /** delete composer */
    void deleteComposer();
    /** create all mixtures */
    void createMixtures();

    /** set the mixture parameters using an array of posterior probabilities.
     *  Proportions, numbers in each class and class labels are computed
     *  using these posterior probabilities.
     *  @param tik posterior class probabilities
     **/
    template<class Array>
    void setMixtureParameters( Array const& tik);
    /** set the mixture parameters giving the posterior probabilities and
     *  the proportions.
     *  Numbers in each class and class labels are computed using the
     *  posterior probabilities.
     *  @param tik posterior class probabilities
     *  @param pk prior class proportion
     **/
    template<class Array, class RowVector>
    void setMixtureParameters( Array const& tik, RowVector const& pk);
    /** Set proportions of each classes
     *  @param pk prior class proportion
     **/
    template<class RowVector>
    void setProportions( RowVector const& pk);

  protected:
    /** diagonal Gaussian mixture models manager */
    DiagGaussianMixtureManager<DataHandler_> diagGaussianManager_;
    /** Poisson mixture models manager */
    PoissonMixtureManager<DataHandler_> poissonManager_;
    /** gamma mixture models manager */
    GammaMixtureManager<DataHandler_> gammaManager_;
    /** categorical mixture models manager */
    CategoricalMixtureManager<DataHandler_> categoricalManager_;
};

/* create a MixtureComposer */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::createMixtureComposer(int nbCluster)
{ p_composer_ = new MixtureComposer(handler().nbSample(), nbCluster) ;}

/* create a MixtureComposerFixedProp */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::createMixtureComposerFixedProp(int nbCluster)
{ p_composer_ = new MixtureComposerFixedProp(handler().nbSample(), nbCluster) ;}

/* create a MixtureComposer */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::createMixtureSemiLearner(int nbCluster)
{ p_composer_ = new MixtureSemiLearner(handler().nbSample(), nbCluster) ;}

/* create a MixtureComposerFixedProp */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::createMixtureSemiLearnerFixedProp(int nbCluster)
{ p_composer_ = new MixtureSemiLearnerFixedProp(handler().nbSample(), nbCluster) ;}

/* create a MixtureComposerFixedProp */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::deleteComposer()
{ if (p_composer_) delete p_composer_; p_composer_ = 0;}

/* create the mixtures in the given learner */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::createMixtures()
{
  p_composer_->createMixture(diagGaussianManager_);
  p_composer_->createMixture(poissonManager_);
  p_composer_->createMixture(gammaManager_);
  p_composer_->createMixture(categoricalManager_);
  p_composer_->createMixture(kmmManager_);
}

/* fill param with the parameters */
template<class DataHandler_>
bool MixtureComposerFacade<DataHandler_>::getParameters( std::string const& idData, ArrayXX& param)
{
  // get idModel from idData
  String idModel;
  if(!handler_.getIdModelName(idData, idModel)) { return false;} // should not happened

  // get mixture id
  Clust::Mixture mix = Clust::stringToMixture(idModel);
  if (mix == Clust::unknown_mixture_)
  {
    bool prop;
    mix = Clust::stringToMixture(idModel, prop);
  }

  // get mixture class (gamma, Gaussian, Poisson, etc.) and parameters for this class of mixture
  switch (Clust::mixtureToMixtureClass(mix))
  {
    case Clust::DiagGaussian_:
      getDiagGaussianParameters( idData, param);
      return true;
      break;
    case Clust::Poisson_:
      getPoissonParameters( idData, param);
      return true;
      break;
    case Clust::Gamma_:
      getGammaParameters( idData, param);
      return true;
      break;
    case Clust::Categorical_:
      getCategoricalParameters( idData, param);
      return true;
      break;
    case Clust::Kmm_:
      getKmmParameters( idData, param);
      return true;
      break;
    case Clust::unknown_mixture_class_:
      return false;
      break;
    default:
      return false;
      break;
  }
  return false; // avoid compiler warning
}

/* set model parameters with param */
template<class DataHandler_>
bool MixtureComposerFacade<DataHandler_>::setParameters( std::string const& idData, ArrayXX const& param)
{
    // get idModel from idData
    String idModel;
    if(!handler_.getIdModelName(idData, idModel)) { return false;}
    // get mixture id
    Clust::Mixture mix = Clust::stringToMixture(idModel);
    if (mix == Clust::unknown_mixture_)
    {
      bool prop;
      mix = Clust::stringToMixture(idModel, prop);
    }

    // get mixture class (gamma, Gaussian, Poisson, etc.) and parameters for this class of mixture
    switch (Clust::mixtureToMixtureClass(mix))
    {
      case Clust::DiagGaussian_:
        setDiagGaussianParameters( idData, param);
        return true;
        break;
      case Clust::Poisson_:
        setPoissonParameters( idData, param);
        return true;
        break;
      case Clust::Gamma_:
        setGammaParameters( idData, param);
        return true;
        break;
      case Clust::Categorical_:
        setCategoricalParameters( idData, param);
        return true;
        break;
      case Clust::Kmm_:
        setKmmParameters( idData, param);
        return true;
        break;
      case Clust::unknown_mixture_class_:
        return false;
        break;
      default:
        return false;
        break;
    }
    return true; // avoid compiler warning
}
/* get the diagonal Gaussian parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::getDiagGaussianParameters( std::string const& idData, ArrayXX& param)
{ p_composer_->getParameters(diagGaussianManager_,idData, param);}
/* get the Poisson parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::getPoissonParameters( std::string const& idData, ArrayXX& param)
{ p_composer_->getParameters(poissonManager_,idData, param);}
/* get the gamma parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::getGammaParameters( std::string const& idData, ArrayXX& param)
{ p_composer_->getParameters(gammaManager_,idData, param);}
/* get the Categorical parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::getCategoricalParameters( std::string const& idData, ArrayXX& param)
{ p_composer_->getParameters(categoricalManager_,idData, param);}
/* get the kernel parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::getKmmParameters( std::string const& idData, ArrayXX& param)
{ p_composer_->getParameters(kmmManager_,idData, param);}


// setters
/* set the diagonal Gaussian parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::setDiagGaussianParameters( std::string const& idData, ArrayXX const& param)
{ p_composer_->setParameters(diagGaussianManager_,idData, param);}
/* set the diagonal Gaussian parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::setPoissonParameters( std::string const& idData, ArrayXX const& param)
{ p_composer_->setParameters(poissonManager_,idData, param);}
/* set the gamma parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::setGammaParameters( std::string const& idData, ArrayXX const& param)
{ p_composer_->setParameters(gammaManager_,idData, param);}
/* set the Categorical parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::setCategoricalParameters( std::string const& idData, ArrayXX const& param)
{ p_composer_->setParameters(categoricalManager_,idData, param);}
/* set the Categorical parameters */
template<class DataHandler_>
void MixtureComposerFacade<DataHandler_>::setKmmParameters( std::string const& idData, ArrayXX const& param)
{ p_composer_->setParameters(kmmManager_,idData, param);}

template<class DataHandler_>
template<class Array>
void MixtureComposerFacade<DataHandler_>::setMixtureParameters(Array const& tik)
{ p_composer_->setMixtureParameters(tik);}

template<class DataHandler_>
template<class Array, class RowVector>
void MixtureComposerFacade<DataHandler_>::setMixtureParameters( Array const& tik, RowVector const& pk)
{ p_composer_->setMixtureParameters(tik, pk);}

template<class DataHandler_>
template<class RowVector>
void MixtureComposerFacade<DataHandler_>::setProportions( RowVector const& pk)
{ p_composer_->setProportions(pk);}


} // namespace STK

#endif /* STK_MIXTURECOMPOSERFACADE_H */
