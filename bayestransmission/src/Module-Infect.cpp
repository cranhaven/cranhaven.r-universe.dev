#include "infect/infect.h"
using namespace infect;

#include "wrap.h"
#include <Rcpp.h>
#include <map>
#include <utility>

#include "RRandom.h"

// Compilation flags for optional class exposure
// Default to minimal exports (only critical classes)
#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
// BAYESTRANSMISSION_COMPREHENSIVE_TESTING defined - enable testing classes
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
// BAYESTRANSMISSION_ALL_CLASSES defined - enable all optional classes
#endif

// CRITICAL CLASSES - Always exposed (essential for R interface)
RCPP_EXPOSED_AS(RRandom)
RCPP_EXPOSED_AS(util::Random)
RCPP_EXPOSED_AS(infect::System)
RCPP_EXPOSED_AS(infect::SystemHistory)
RCPP_EXPOSED_AS(infect::Model)
RCPP_EXPOSED_AS(infect::EpisodeHistory)
RCPP_EXPOSED_AS(infect::FacilityEpisodeHistory)
RCPP_EXPOSED_AS(infect::SystemEpisodeHistory)
RCPP_EXPOSED_AS(infect::UnitEpisodeHistory)

RCPP_EXPOSED_CLASS_NODECL(util::Object)
RCPP_EXPOSED_CLASS_NODECL(util::MapLink)

#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
// NEEDED FOR COMPREHENSIVE TESTING - Important but not critical
RCPP_EXPOSED_AS(util::Map)
RCPP_EXPOSED_AS(infect::Event)
RCPP_EXPOSED_AS(infect::Facility)
RCPP_EXPOSED_AS(infect::Patient)
RCPP_EXPOSED_AS(infect::PatientState)
RCPP_EXPOSED_AS(infect::RawEventList)
RCPP_EXPOSED_AS(infect::Sampler)
RCPP_EXPOSED_AS(infect::Unit)
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
// OTHER CLASSES - Rarely used, conditional compilation
RCPP_EXPOSED_AS(util::IntMap)
RCPP_EXPOSED_AS(util::List)
RCPP_EXPOSED_AS(infect::AbxLocationState)
RCPP_EXPOSED_AS(infect::AbxPatientState)
RCPP_EXPOSED_AS(infect::CountLocationState)
RCPP_EXPOSED_AS(infect::Episode)
RCPP_EXPOSED_AS(infect::HistoryLink)
RCPP_EXPOSED_AS(infect::LocationState)
RCPP_EXPOSED_AS(infect::RawEvent)
RCPP_EXPOSED_AS(infect::SetLocationState)
#endif

//' Get compilation flags for exposed classes
//' 
//' Returns information about which optional classes are exposed in this build.
//' This allows tests to conditionally skip tests for unexposed classes.
//' 
//' @return List with logical flags:
//'   * `comprehensive_testing` - TRUE if comprehensive testing classes are exposed
//'   * `all_classes` - TRUE if all optional classes are exposed
//'   * `minimal` - TRUE if only critical classes are exposed
//' @export
// [[Rcpp::export]]
Rcpp::List getExposureFlags() {
    return Rcpp::List::create(
        Rcpp::Named("comprehensive_testing") = 
#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
            true,
#else
            false,
#endif
        Rcpp::Named("all_classes") = 
#ifdef BAYESTRANSMISSION_ALL_CLASSES
            true,
#else
            false,
#endif
        Rcpp::Named("minimal") = 
#if !defined(BAYESTRANSMISSION_COMPREHENSIVE_TESTING) && !defined(BAYESTRANSMISSION_ALL_CLASSES)
            true
#else
            false
#endif
    );
}

#ifdef BAYESTRANSMISSION_ALL_CLASSES
/**
 * @brief Wrapper to compute log likelihood for a single HistoryLink
 * 
 * @param model Pointer to the Model (must be castable to UnitLinkedModel)
 * @param link Pointer to the HistoryLink to compute likelihood for
 * @return double The log likelihood contribution of this link (including gap probability)
 */
static double Model_logLikelihoodLink_wrapper(infect::Model* model, infect::HistoryLink* link) {
    if (model == nullptr || link == nullptr) {
        return 0.0;
    }
    
    // Cast to UnitLinkedModel to access the HistoryLink-specific logLikelihood method
    UnitLinkedModel* ulm = dynamic_cast<UnitLinkedModel*>(model);
    if (ulm == nullptr) {
        Rcpp::stop("Model must be a UnitLinkedModel to compute likelihood for individual links");
    }
    
    return ulm->logLikelihood(link);
}
#endif


void init_Module_infect(){
    using namespace Rcpp;

    // CRITICAL CLASSES - Always exposed (essential for R interface)
    class_<infect::Model>("CppModel")
        .derives<util::Object>("CppObject")
        .property("AbxLife", &infect::Model::getAbxLife, &infect::Model::setAbxLife)
        .property("AbxDelay", &infect::Model::getAbxDelay, &infect::Model::setAbxDelay)
        .property("cheating", &infect::Model::isCheating)
        .method("logLikelihood", &infect::Model::logLikelihood)
        .method("update", &infect::Model::update)
        .method("forwardSimulate", &infect::Model::forwardSimulate)
        .method("sampleEpisodes", &infect::Model::sampleEpisodes)
#ifdef BAYESTRANSMISSION_ALL_CLASSES
        // Methods using EpisodeHistory/HistoryLink - only available with ALL_CLASSES
        .method("logLikelihoodLink", &Model_logLikelihoodLink_wrapper)
        .method("initEpisodeHistory", &infect::Model::initEpisodeHistory)
        .method("makeEpisodeHistory", &infect::Model::makeEpisodeHistory)
#endif
    ;

#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
    // COMPREHENSIVE TESTING CLASSES - Important but not critical
    class_<Event>("CppEvent")
        .derives<util::Object>("CppObject")
        .constructor()
        .property("Time", &Event::getTime)
        .property("Type", &Event::getTypeAsString)
        .property("Patient", &Event::getPatient)
        .property("Facility", &Event::getFacility)
        .property("Unit", &Event::getUnit)
        .property("isTest", &Event::isTest)
        .property("isPositiveTest", &Event::isPositiveTest)
        .property("isClinicalTest", &Event::isClinicalTest)
        .property("isAdmission", &Event::isAdmission)
    ;
    class_<infect::Facility>("CppFacility")
        .derives<util::Object>("CppObject")
        .constructor<int>()
        .property("id", &infect::Facility::getId)
        .method("getUnit", &infect::Facility::getUnit)
    ;
    class_<infect::Patient>("CppPatient")
        .derives<util::Object>("CppObject")
        .constructor<int>()
        .property("id", &Patient::getId)
        .property("group", &Patient::getGroup, &Patient::setGroup)
    ;
    class_<infect::PatientState>("CppPatientState")
        .derives<util::Object>("CppObject")
        .constructor<Patient*>()
        .constructor<Patient*, int>()
        .property("unit", &infect::PatientState::getUnit)
        .property("infectionStatus", &infect::PatientState::infectionStatus)
        .property("onAbx", &infect::PatientState::onAbx)
    ;
    class_<RawEventList>("CppRawEventList")
        .derives<util::SortedList>("CppSortedList")
        .constructor<
            std::vector<int>,    // facilities
            std::vector<int>,    // units
            std::vector<double>, // times
            std::vector<int>,    // patients
            std::vector<int>     // types
        >()
        .method("FirstTime", &RawEventList::firstTime)
        .method("LastTime", &RawEventList::lastTime)
    ;
    class_<infect::Unit>("CppUnit")
        .derives<util::Object>("CppObject")
        .constructor<Object*, int>()
        .property("id", &infect::Unit::getId)
        .method("getName", &infect::Unit::getName)
    ;
    class_<infect::Sampler>("CppSampler")
        .derives<util::Object>("CppObject")
        .constructor<SystemHistory*, Model*, Random*>()
        .method<void>("sampleModel", &infect::Sampler::sampleModel)
        .method<void>("sampleEpisodes", &infect::Sampler::sampleEpisodes)
    ;
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
    // ALL OPTIONAL CLASSES - Rarely used
    class_<infect::AbxLocationState>("CppAbxLocationState")
        .derives<util::Object>("CppObject")
        .property("nStates", static_cast<int (infect::AbxLocationState::*)() const>(&infect::LocationState::nStates))
        .property("Total", static_cast<int (infect::AbxLocationState::*)() const>(&infect::CountLocationState::getTotal))
        .property("Colonized", static_cast<int (infect::AbxLocationState::*)() const>(&infect::CountLocationState::getColonized))
        .property("Latent", static_cast<int (infect::AbxLocationState::*)() const>(&infect::CountLocationState::getLatent))
        .property("Susceptible", static_cast<int (infect::AbxLocationState::*)() const>(&infect::CountLocationState::getSusceptible))
        .method("onAbx", &infect::AbxLocationState::onAbx)
        .method("everAbx", &infect::AbxLocationState::everAbx)
        .property("AbxTotal", &infect::AbxLocationState::getAbxTotal)
        .property("EverAbxTotal", &infect::AbxLocationState::getEverAbxTotal)
        .property("AbxColonized", &infect::AbxLocationState::getAbxColonized)
        .property("EverAbxColonized", &infect::AbxLocationState::getEverAbxColonized)
        .property("AbxLatent", &infect::AbxLocationState::getAbxLatent)
        .property("EverAbxLatent", &infect::AbxLocationState::getEverAbxLatent)
        .property("AbxSusceptible", &infect::AbxLocationState::getAbxSusceptible)
        .property("EverAbxSusceptible", &infect::AbxLocationState::getEverAbxSusceptible)
        .property("NoAbxTotal", &infect::AbxLocationState::getNoAbxTotal)
        .property("NeverAbxTotal", &infect::AbxLocationState::getNeverAbxTotal)
        .property("NoAbxColonized", &infect::AbxLocationState::getNoAbxColonized)
        .property("NeverAbxColonized", &infect::AbxLocationState::getNeverAbxColonized)
        .property("NoAbxLatent", &infect::AbxLocationState::getNoAbxLatent)
        .property("NeverAbxLatent", &infect::AbxLocationState::getNeverAbxLatent)
        .property("NoAbxSusceptible", &infect::AbxLocationState::getNoAbxSusceptible)
        .property("NeverAbxSusceptible", &infect::AbxLocationState::getNeverAbxSusceptible)
    ;
    class_<infect::AbxPatientState>("CppAbxPatientState")
        .derives<util::Object>("CppObject")
        .property("unit", static_cast<infect::Unit* (infect::AbxPatientState::*)() const>(&infect::PatientState::getUnit))
        .property("infectionStatus", static_cast<infect::InfectionCoding::InfectionStatus (infect::AbxPatientState::*)() const>(&infect::PatientState::infectionStatus))
        .property("onAbx", &infect::AbxPatientState::onAbx)
        .property("everAbx", &infect::AbxPatientState::everAbx)
    ;
    class_<CountLocationState>("CppCountLocationState")
        .derives<util::Object>("CppObject")
        .property("nStates", static_cast<int (infect::CountLocationState::*)() const>(&infect::CountLocationState::nStates))
        .property("Total", &infect::CountLocationState::getTotal)
        .property("Colonized", &infect::CountLocationState::getColonized)
        .property("Latent", &infect::CountLocationState::getLatent)
        .property("Susceptible", &infect::CountLocationState::getSusceptible)
    ;
    class_<Episode>("CppEpisode")
        .derives<util::Object>("CppObject")
        .constructor()
        .property("Admission", &Episode::getAdmission)
        .property("Discharge", &Episode::getDischarge)
        .method("getEvents", &Episode::getEvents)
        .method("hasAdmission", &Episode::hasAdmission)
        .method("hasDischarge", &Episode::hasDischarge)
        .method("hasEvents", &Episode::hasEvents)
    ;
    class_<infect::HistoryLink>("CppHistoryLink")
        .derives<util::Object>("CppObject")
        .property("linked", &HistoryLink::isLinked, &infect::HistoryLink::setLinked)
        .property("hidden", &HistoryLink::isHidden, &infect::HistoryLink::setHidden)
        .property("PatientPrev", &HistoryLink::pPrev)
        .property("PatientNext", &HistoryLink::pNext)
        .property("HistoryPrev", &HistoryLink::hPrev)
        .property("HistoryNext", &HistoryLink::hNext)
        .property("UnitPrev", &infect::HistoryLink::uPrev)
        .property("UnitNext", &HistoryLink::uNext)
        .property("FacilityPrev", &HistoryLink::fPrev)
        .property("FacilityNext", &HistoryLink::fNext)
        .property("SystemPrev", &HistoryLink::sPrev)
        .property("SystemNext", &HistoryLink::sNext)
        .property("Event", &HistoryLink::getEvent)
        .property("PatientState", &HistoryLink::getPState)
        .property("UnitState", &HistoryLink::getUState)
        .property("FacilityState", &HistoryLink::getFState)
        .property("SystemState", &HistoryLink::getSState)
    ;
    class_<LocationState>("CppLocationState")
        .derives<util::Object>("CppObject")
        .property("Total", &infect::LocationState::getTotal)
        .property("Colonized", &infect::LocationState::getColonized)
        .property("Latent", &infect::LocationState::getLatent)
        .property("Susceptible", &infect::LocationState::getSusceptible)
        .property("nStates", &infect::LocationState::nStates)
    ;
    class_<TestParamsAbx>("CppTestParamsAbx")
        .derives<util::Object>("CppObject")
    ;
#endif

}
