
#include "lognormal/lognormal.h"

#include "wrap.h"

#include <Rcpp.h>

#include "RRandom.h"

// Expose the classes to R
RCPP_EXPOSED_CLASS_NODECL(util::Object)
RCPP_EXPOSED_CLASS_NODECL(util::MapLink)

RCPP_EXPOSED_AS(util::Map)
RCPP_EXPOSED_AS(util::Random)

RCPP_EXPOSED_AS(infect::AbxLocationState)
RCPP_EXPOSED_AS(infect::AbxPatientState)
RCPP_EXPOSED_AS(infect::CountLocationState)
RCPP_EXPOSED_AS(infect::Episode)
RCPP_EXPOSED_AS(infect::EpisodeHistory)
RCPP_EXPOSED_AS(infect::Event)
RCPP_EXPOSED_AS(infect::Facility)
RCPP_EXPOSED_AS(infect::FacilityEpisodeHistory)
RCPP_EXPOSED_AS(infect::HistoryLink)
RCPP_EXPOSED_AS(infect::LocationState)
RCPP_EXPOSED_AS(infect::Model)
RCPP_EXPOSED_AS(infect::Patient)
RCPP_EXPOSED_AS(infect::PatientState)
RCPP_EXPOSED_AS(infect::RawEvent)
RCPP_EXPOSED_AS(infect::RawEventList)
RCPP_EXPOSED_AS(infect::Sampler)
RCPP_EXPOSED_AS(infect::SetLocationState)
RCPP_EXPOSED_AS(infect::System)
RCPP_EXPOSED_AS(infect::SystemEpisodeHistory)
RCPP_EXPOSED_AS(infect::SystemHistory)
RCPP_EXPOSED_AS(infect::Unit)
RCPP_EXPOSED_AS(infect::UnitEpisodeHistory)



void init_Module_lognormal(){
    using namespace Rcpp;
    using namespace lognormal;

    //virtual
    class_<lognormal::LogNormalICP>("CppLogNormalICP")
        .derives<models::InColParams>("CppInColParams")
        .method("logAcquisitionGap", &lognormal::LogNormalICP::logAcquisitionGap)
        .method("logAcquisitionRate", &lognormal::LogNormalICP::logAcquisitionRate)

        .method("logProgressionRate", &lognormal::LogNormalICP::logProgressionRate)
        .method("logProgressionGap", &lognormal::LogNormalICP::logProgressionGap)

        .method("logClearanceRate", &lognormal::LogNormalICP::logClearanceRate)
        .method("logClearanceGap", &lognormal::LogNormalICP::logClearanceGap)

        .method("logProbGap", &lognormal::LogNormalICP::logProbGap)
        .method("unTransform", &lognormal::LogNormalICP::unTransform)
        .property("names", &lognormal::LogNormalICP::paramNames)
        .property("values", &lognormal::LogNormalICP::getValues)
        .property("timeOrigin", &lognormal::LogNormalICP::getTimeOrigin, &lognormal::LogNormalICP::setTimeOrigin)
    ;

    class_<lognormal::LogNormalAbxICP>("CppLogNormalAbxICP")
        .derives<lognormal::LogNormalICP>("CppLogNormalICP")
        .constructor<int, int, int, int>()
        .property("header", &lognormal::LogNormalAbxICP::header)
        .method("logProgressionRate", &lognormal::LogNormalAbxICP::logProgressionRate)
        .method("logProgressionGap", &lognormal::LogNormalAbxICP::logProgressionGap)
        .method("logClearanceRate", &lognormal::LogNormalAbxICP::logClearanceRate)
        .method("logClearanceGap", &lognormal::LogNormalAbxICP::logClearanceGap)
        .method("logAcquisitionRate", &lognormal::LogNormalAbxICP::logAcquisitionRate)
        .method("logAcquisitionGap", &lognormal::LogNormalAbxICP::logAcquisitionGap)
        .property("names", &lognormal::LogNormalAbxICP::paramNames)
    ;


    class_<lognormal::LogNormalModel>("CppLogNormalModel")
        .derives<models::BasicModel>("CppBasicModel")
        .constructor<int, int, int, int, int>()
        .property("InColParams", &lognormal::LogNormalModel::getInColParams)
        .property("ClinicalTestParams", &lognormal::LogNormalModel::getClinicalTestParams)
        .property("IncolParams", &lognormal::LogNormalModel::getInColParams)
        .method("setAbx", &lognormal::LogNormalModel::setAbx)
    ;

    class_<lognormal::LinearAbxModel>("CppLinearAbxModel")
        .derives<lognormal::LogNormalModel>("CppLogNormalModel")
        .constructor<int, int, int, int>()
        .property("InColParams", &lognormal::LinearAbxModel::getInColParams)
    ;

    class_<lognormal::LinearAbxModel2>("CppLinearAbxModel2")
        .derives<lognormal::LogNormalModel>("CppLogNormalModel")
        .constructor<int, int, int, int>()
    ;

    class_<lognormal::MixedICP>("CppMixedICP")
        .derives<lognormal::LogNormalAbxICP>("CppLogNormalAbxICP")
        .constructor<int, int, int>()
        .constructor<int, int, int, int>()
        .method("acqRate", &lognormal::MixedICP::acqRate)
        .method("timePar", &lognormal::MixedICP::timePar)
        .method("unTransform", &lognormal::MixedICP::unTransform)
        .method("set", &lognormal::MixedICP::set)
    ;

    class_<lognormal::MixedModel>("CppMixedModel")
        .derives<lognormal::LogNormalModel>("CppLogNormalModel")
        .constructor<int, int, int, int>()
    ;
}
