#ifndef BAYESIAN_TRANSMISSION_MODELSETUP_H
#define BAYESIAN_TRANSMISSION_MODELSETUP_H


#include <string>
using std::string;

#include "util/util.h"
#include "infect/infect.h"
#include "modeling/modeling.h"
#include "lognormal/lognormal.h"

using namespace util;
using namespace infect;
using namespace models;
using namespace lognormal;

#include "wrap.h"

#include <Rcpp.h>
using namespace Rcpp;

#include "RRandom.h"

// void modelsetup(BasicModel * model, Rcpp::List modelParameters);

template <typename T>
inline void setParam(T* x, int i, Rcpp::List Param){
    x->set(i,
           Rcpp::as<double>(Param["init"]),
           Rcpp::as<bool>(Param["update"]),
           Rcpp::as<double>(Param["prior"]),
           Rcpp::as<double>(Param["weight"])
    );
}
template <typename T, typename itype1, typename itype2>
inline void setParam(T* x, itype1 i, itype2 j, Rcpp::List Param, bool dolog = false){
    if(dolog){
        x->set(i, j,
               log(Rcpp::as<double>(Param["init"])),
                   Rcpp::as<bool>(Param["update"]),
               log(Rcpp::as<double>(Param["prior"])),
                   Rcpp::as<double>(Param["weight"]))
        ;
    } else {
        x->set(i, j,
               Rcpp::as<double>(Param["init"]),
               Rcpp::as< bool >(Param["update"]),
               Rcpp::as<double>(Param["prior"]),
               Rcpp::as<double>(Param["weight"])
        );
    }
}
template <typename T, typename itype1, typename itype2>
inline void setParamWSig(T* x, itype1 i, itype2 j, Rcpp::List Param){
    x->set(i, j,
           Rcpp::as<double>(Param["init"]),
           Rcpp::as<bool>(Param["update"]),
           Rcpp::as<double>(Param["prior"]),
           Rcpp::as<double>(Param["weight"]),
           Rcpp::as<double>(Param["sigma"])
    );
}

inline void setupInsituParams(
        InsituParams * isp,
        std::vector<double> Probs,
        std::vector<double> Priors,
        std::vector<bool> Doit){
    isp->set(Probs[0], Probs[1], Probs[2]);
    isp->setPriors(Priors[0], Priors[1], Priors[2]);
    isp->setUpdate(Doit[0], Doit[1], Doit[2]);
}

inline void setupInsituParams(
        InsituParams * isp,
        Rcpp::NumericVector Probs,
        Rcpp::NumericVector Priors,
        Rcpp::LogicalVector Doit){
    setupInsituParams(isp,
                      as< std::vector<double> >(Probs),
                      as< std::vector<double> >(Priors),
                      as< std::vector<bool> >(Doit)
    );
}

inline void setupInsituParams(InsituParams * isp, Rcpp::List insituParameters)
{
    // std::ostringstream ss;
    // ss << "DEBUG InsituParams: probs=[" 
    //    << as<std::vector<double>>(insituParameters["probs"])[0] << ", "
    //    << as<std::vector<double>>(insituParameters["probs"])[1] << ", "
    //    << as<std::vector<double>>(insituParameters["probs"])[2] << "]";
    // Rcpp::message(Rcpp::wrap(ss.str()));
    setupInsituParams(isp,
                      as< std::vector<double> >(insituParameters["probs"]),
                      as< std::vector<double> >(insituParameters["priors"]),
                      as< std::vector<bool> >(insituParameters["doit"]));
}

inline void setupSurveillanceTestParams(
        TestParams * stp,
        Rcpp::List stpUncolonizedParam,
        Rcpp::List stpColonizedParam,
        Rcpp::List stpLatentParam
){
    setParam(stp, 0, stpUncolonizedParam);  // index 0 = uncolonized
    setParam(stp, 2, stpColonizedParam);    // index 2 = colonized (FIXED: was 1)
    setParam(stp, 1, stpLatentParam);        // index 1 = latent (FIXED: was 2)
}

inline void setupSurveillanceTestParams(TestParams * stp, Rcpp::List SurveillanceTestParameters)
{
    setupSurveillanceTestParams(stp,
                               Rcpp::as<Rcpp::List>(SurveillanceTestParameters["uncolonized"]),
                               Rcpp::as<Rcpp::List>(SurveillanceTestParameters["colonized"]),
                               Rcpp::as<Rcpp::List>(SurveillanceTestParameters["latent"]));
}

// Special setup for TestParamsAbx which has separate on/off antibiotic parameters
inline void setupSurveillanceTestParamsAbx(
        models::TestParamsAbx * stp,
        Rcpp::List stpUncolonizedParam,
        Rcpp::List stpColonizedParam,
        Rcpp::List stpLatentParam
){
    // TestParamsAbx::set(i, j, value, update, prival, prin)
    // where i = colonization state (0=unc, 1=lat, 2=col)
    //       j = abx status (0=off, 1=on)
    
    // For now, set both off-abx and on-abx to the same values
    // (matching constructor behavior)
    
    // Off-abx:
    stp->set(0, 0, 
             Rcpp::as<double>(stpUncolonizedParam["init"]),
             Rcpp::as<bool>(stpUncolonizedParam["update"]),
             Rcpp::as<double>(stpUncolonizedParam["prior"]),
             Rcpp::as<double>(stpUncolonizedParam["weight"]));
    stp->set(1, 0,
             Rcpp::as<double>(stpLatentParam["init"]),
             Rcpp::as<bool>(stpLatentParam["update"]),
             Rcpp::as<double>(stpLatentParam["prior"]),
             Rcpp::as<double>(stpLatentParam["weight"]));
    stp->set(2, 0,
             Rcpp::as<double>(stpColonizedParam["init"]),
             Rcpp::as<bool>(stpColonizedParam["update"]),
             Rcpp::as<double>(stpColonizedParam["prior"]),
             Rcpp::as<double>(stpColonizedParam["weight"]));
    
    // On-abx (same as off-abx for now):
    stp->set(0, 1,
             Rcpp::as<double>(stpUncolonizedParam["init"]),
             Rcpp::as<bool>(stpUncolonizedParam["update"]),
             Rcpp::as<double>(stpUncolonizedParam["prior"]),
             Rcpp::as<double>(stpUncolonizedParam["weight"]));
    stp->set(1, 1,
             Rcpp::as<double>(stpLatentParam["init"]),
             Rcpp::as<bool>(stpLatentParam["update"]),
             Rcpp::as<double>(stpLatentParam["prior"]),
             Rcpp::as<double>(stpLatentParam["weight"]));
    stp->set(2, 1,
             Rcpp::as<double>(stpColonizedParam["init"]),
             Rcpp::as<bool>(stpColonizedParam["update"]),
             Rcpp::as<double>(stpColonizedParam["prior"]),
             Rcpp::as<double>(stpColonizedParam["weight"]));
}


inline void setParamWRate(RandomTestParams* rtp, int i,
                          Rcpp::List p,
                          Rcpp::List r){
    setParam(rtp, false, i, p);
    setParam(rtp, true, i, r);
}
inline void setParamWRate(RandomTestParams* rtp, int i,
                          Rcpp::List pwr){
    setParamWRate(rtp, i, pwr["param"], pwr["rate"]);
}

inline void setupClinicalTestParams(
        RandomTestParams * ctp,
        Rcpp::List ctpUncolonizedParamWRate,
        Rcpp::List ctpColonizedParamWRate,
        Rcpp::List ctpLatentParamWRate
){
    setParamWRate(ctp, 0, ctpUncolonizedParamWRate);  // index 0 = uncolonized
    setParamWRate(ctp, 2, ctpColonizedParamWRate);    // index 2 = colonized (FIXED: was 1)
    setParamWRate(ctp, 1, ctpLatentParamWRate);        // index 1 = latent (FIXED: was 2)
}


inline void setupClinicalTestParams(RandomTestParams * ctp, Rcpp::List clinicalTestParameters)
{
    setupClinicalTestParams(ctp,
                            Rcpp::as<Rcpp::List>(clinicalTestParameters["uncolonized"]),
                            Rcpp::as<Rcpp::List>(clinicalTestParameters["colonized"]),
                            Rcpp::as<Rcpp::List>(clinicalTestParameters["latent"]));
}



inline void setupOutOfUnitParams(OutColParams * ocol, Rcpp::List outColParameters)
{
    if(ocol->nParam()==3){
        setParam(ocol, 0, outColParameters["acquisition"]);
        setParam(ocol, 1, outColParameters["progression"]);
        setParam(ocol, 2, outColParameters["clearance"]);
    } else {
        setParam(ocol, 0, outColParameters["acquisition"]);
        setParam(ocol, 2, outColParameters["clearance"]);
    }


}

inline void setupLogNormalICPAcquisition(
        LogNormalICP * icp,
        Rcpp::List AcquisitionParams
)
{
    for(auto i=0; i < AcquisitionParams.size(); i++)
    {
        setParam(icp, 0, i, AcquisitionParams[i]);
    }
}

inline void setupLinearAbxAcquisitionModel(
        LinearAbxICP* icp,
        Rcpp::List AcquisitionParams
)
{
    // LinearAbxICP::set() already handles log/logit transformations internally
    // Do NOT pass dolog=true as it would cause double transformation
    setParam(icp, 0, 0, AcquisitionParams["base"]);
    setParam(icp, 0, 1, AcquisitionParams["time"]);
    setParam(icp, 0, 2, AcquisitionParams["mass"]);
    setParam(icp, 0, 3, AcquisitionParams["freq"]);
    setParam(icp, 0, 4, AcquisitionParams["col_abx"]);
    setParam(icp, 0, 5, AcquisitionParams["suss_abx"]);
    setParam(icp, 0, 6, AcquisitionParams["suss_ever"]);
}


inline void setupAcquisitionParams(
        LogNormalICP * icp,
        Rcpp::List AcquisitionParams
){
    setupLogNormalICPAcquisition(icp, AcquisitionParams);
}

inline void setupAcquisitionParams(
        LinearAbxICP * icp,
        Rcpp::List AcquisitionParams
){
    setupLinearAbxAcquisitionModel(icp, AcquisitionParams);
}

inline void setupProgressionParams (
        LogNormalICP * icp,
        Rcpp::List ProgressionParams
){
    setParam(icp, 1, 0, ProgressionParams["rate"]);
    setParam(icp, 1, 1, ProgressionParams["abx"]);
    setParam(icp, 1, 2, ProgressionParams["ever_abx"]);
}

inline void setupClearanceParams (
        LogNormalICP * icp,
        Rcpp::List ClearanceParams
){
    setParam(icp, 2, 0, ClearanceParams["rate"]);
    setParam(icp, 2, 1, ClearanceParams["abx"]);
    setParam(icp, 2, 2, ClearanceParams["ever_abx"]);
}

inline void setupAbxRateParams(
        AbxParams * abxp,
        Rcpp::List AbxRateParams
){
    // AbxParams uses indices [0, 1, 2] = [uncolonized, latent, colonized]
    // For 2-state models, index 1 is unused (latent doesn't exist)
    setParam(abxp, 0, AbxRateParams["uncolonized"]);
    setParam(abxp, 1, AbxRateParams["latent"]);
    setParam(abxp, 2, AbxRateParams["colonized"]);
}

template <typename ModelType>
inline void setupAbxParams(
        ModelType * model,
        Rcpp::List AbxParams
){
    model -> setAbx(AbxParams["onoff"], AbxParams["delay"], AbxParams["life"]);
}

template <typename ICPType>
inline void setupInColParams(ICPType * icp, Rcpp::List inColParameters)
{
    setupAcquisitionParams(icp, inColParameters["acquisition"]);
    setupProgressionParams(icp, inColParameters["progression"]);
    setupClearanceParams(icp, inColParameters["clearance"]);
}

template <typename ModelType>
void modelsetup(ModelType * model, Rcpp::List modelParameters, bool verbose = false)
{

    // Antibiotics
    if(verbose) Rcpp::Rcout << std::endl << "  * Setting up Abx...";
    setupAbxParams(model, modelParameters["Abx"]);

    // In situ
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Insitu...";
    auto isp = model->getInsituParams();
    setupInsituParams(isp, modelParameters["Insitu"]);

    // Surveillance test parameters.
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Surveillance Test...";
    auto stp = model->getSurveillanceTestParams();
    // Check if this is a TestParamsAbx (used by LinearAbxModel, LinearAbxModel2, etc.)
    models::TestParamsAbx* stp_abx = dynamic_cast<models::TestParamsAbx*>(stp);
    if (stp_abx != nullptr) {
        Rcpp::List stParams = modelParameters["SurveillanceTest"];
        setupSurveillanceTestParamsAbx(stp_abx, 
                                       Rcpp::as<Rcpp::List>(stParams["uncolonized"]),
                                       Rcpp::as<Rcpp::List>(stParams["colonized"]),
                                       Rcpp::as<Rcpp::List>(stParams["latent"]));
    } else {
        setupSurveillanceTestParams(stp, modelParameters["SurveillanceTest"]);
    }

    //  Clinical test parameters.
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Clinical Test...";
    RandomTestParams * ctp = (RandomTestParams *) model->getClinicalTestParams();
    setupClinicalTestParams(ctp, modelParameters["ClinicalTest"]);

    // Out of unit infection parameters.
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Out of Unit...";
    auto ocol = model->getOutColParams();
    setupOutOfUnitParams(ocol, modelParameters["OutCol"]);

    // In unit infection parameters.
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up In Unit...";
    auto icp = model->getInColParams();
    setupInColParams(icp, modelParameters["InCol"]);

    // Abx rates
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Abx Rates...";
    auto abxp = model->getAbxParams();
    if(abxp != 0)
    {
        setupAbxRateParams(abxp, modelParameters["AbxRate"]);
    }
    if(verbose) Rcpp::Rcout << "Done" << std::endl;

}
template <>
void modelsetup(lognormal::LinearAbxModel * model, Rcpp::List modelParameters, bool verbose)
{
    if(verbose) Rcpp::Rcout << std::endl << "(In LinearAbxModel specialization)";

    // Antibiotics
    if(verbose) Rcpp::Rcout << std::endl << "  * Setting up Abx...";
    setupAbxParams(model, modelParameters["Abx"]);

    // In situ
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Insitu...";
    auto isp = model->getInsituParams();
    setupInsituParams(isp, modelParameters["Insitu"]);

    // Surveillance test parameters - LinearAbxModel uses TestParamsAbx
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Surveillance Test...";
    models::TestParamsAbx* stp = dynamic_cast<models::TestParamsAbx*>(model->getSurveillanceTestParams());
    if (stp != nullptr) {
        Rcpp::List stParams = modelParameters["SurveillanceTest"];
        setupSurveillanceTestParamsAbx(stp, 
                                       Rcpp::as<Rcpp::List>(stParams["uncolonized"]),
                                       Rcpp::as<Rcpp::List>(stParams["colonized"]),
                                       Rcpp::as<Rcpp::List>(stParams["latent"]));
    } else {
        // Fallback to regular setup if cast fails
        setupSurveillanceTestParams(model->getSurveillanceTestParams(), modelParameters["SurveillanceTest"]);
    }

    //  Clinical test parameters.
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Clinical Test...";
    RandomTestParams * ctp = (RandomTestParams *) model->getClinicalTestParams();
    setupClinicalTestParams(ctp, modelParameters["ClinicalTest"]);

    // Out of unit infection parameters.
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Out of Unit...";
    auto ocol = model->getOutColParams();
    setupOutOfUnitParams(ocol, modelParameters["OutCol"]);

    // In unit infection parameters.
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up In Unit...";
    lognormal::LinearAbxICP* icp = (lognormal::LinearAbxICP*)(model->getInColParams());
    setupInColParams(icp, modelParameters["InCol"]);

    // Abx rates
    if(verbose) Rcpp::Rcout << "Done" << std::endl
                            << "  * Setting up Abx Rates...";
    auto abxp = model->getAbxParams();
    if(abxp != 0)
    {
        setupAbxRateParams(abxp, modelParameters["AbxRate"]);
    }
    if(verbose) Rcpp::Rcout << "Done" << std::endl;

}


#endif //BAYESIAN_TRANSMISSION_MODELSETUP_H
