
#include "modeling/modeling.h"
using namespace models;


#include "wrap.h"

#include <Rcpp.h>

#include "RRandom.h"

// Expose the classes to R
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
RCPP_EXPOSED_AS(models::TestParams)
RCPP_EXPOSED_AS(models::InsituParams)
RCPP_EXPOSED_AS(models::OutColParams)
RCPP_EXPOSED_AS(models::AbxParams)

void init_Module_models(){
    class_<models::TestParams>("CppTestParams")
        .derives<util::Object>("CppObject")
        .constructor<int>()
        .property("names", &models::TestParams::paramNames)
        .property("nStates", &models::TestParams::getNStates)
        .property("values", &models::TestParams::getValues)
        .method("set", static_cast<void (models::TestParams::*)(int, double, int, double, double)>(&models::TestParams::set))
        .method("update", static_cast<void (models::TestParams::*)(Random*, bool)>(&models::TestParams::update))
        .method("update_max", &models::TestParams::update_max)
        .method("getCount", &models::TestParams::getCount)
        .method("setCount", &models::TestParams::setCount)
    ;

    class_<models::InsituParams>("CppInsituParams")
        .derives<util::Object>("CppObject")
        .constructor<>()
        .constructor<int>()
        .constructor<std::vector<double>, std::vector<double>, std::vector<bool>>()
        .property("nStates", &models::InsituParams::getNStates)
        .property("paramNames", &models::InsituParams::paramNames)
        .property("values", &models::InsituParams::getValues)
        .property("counts", &models::InsituParams::getCounts, &models::InsituParams::setCounts)
        .method("set", &models::InsituParams::set)
        .method("setPriors", &models::InsituParams::setPriors)
        .method("setUpdate", &models::InsituParams::setUpdate)
        .method("update", static_cast<void (models::InsituParams::*)(Random*, bool)>(&models::InsituParams::update))
        ;
// IncolParams
    //virtual
    class_<models::InColParams>("CppInColParams")
        .derives<util::Object>("CppObject")
        .property("NStates", &models::InColParams::getNStates)
        .method("eventRate", &models::InColParams::eventRate)
    ;
    class_<OutColParams>("CppOutColParams")
        .derives<util::Object>("CppObject")
        .property("NStates", &models::OutColParams::getNStates)
        .property("values", &models::OutColParams::getValues)
        .property("names", &models::OutColParams::paramNames)
        .method("logProb", &models::OutColParams::logProb)
    ;
    
    class_<models::AbxParams>("CppAbxParams")
        .derives<util::Object>("CppObject")
        .property("NStates", &models::AbxParams::getNStates)
        .property("values", &models::AbxParams::getValues)
        .property("names", &models::AbxParams::paramNames)
    ;
    class_<models::UnitLinkedModel>("CppUnitLinkedModel")
        .derives<infect::Model>("CppModel")
        .property("InsituParams", &models::UnitLinkedModel::getInsituParams)
        .property("OutColParams", &models::UnitLinkedModel::getOutColParams)
        .property("SurveillanceTestParams", &models::UnitLinkedModel::getSurveillanceTestParams)
        .property("ClinicalTestParams", &models::UnitLinkedModel::getClinicalTestParams)
        .property("AbxParams", &models::UnitLinkedModel::getAbxParams)
        .method("logLikelihood_HL", (LogLikelihood_HL)&models::UnitLinkedModel::logLikelihood)
        .method("logLikelihood_EH", (LogLikelihood_EH)&models::UnitLinkedModel::logLikelihood)
        .method("logLikelihood_P_HL", (LogLikelihood_P_HL)&models::UnitLinkedModel::logLikelihood)
        .method("logLikelihood_P_HL_i", (LogLikelihood_P_HL_i)&models::UnitLinkedModel::logLikelihood)
        .method("logLikelihood_HL_i", (LogLikelihood_HL_i)&models::UnitLinkedModel::logLikelihood)
        .method("getHistoryLinkLogLikelihoods", &models::UnitLinkedModel::getHistoryLinkLogLikelihoods)
    ;


    class_<models::BasicModel>("CppBasicModel")
        .derives<models::UnitLinkedModel>("CppUnitLinkedModel")
        .method("forwardSimulate", &models::BasicModel::forwardSimulate)
        .method("initEpisodeHistory", &models::BasicModel::initEpisodeHistory)
        .method("sampleEpisodes", &models::BasicModel::sampleEpisodes)
    ;


}
