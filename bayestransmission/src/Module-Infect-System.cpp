#include "infect/infect.h"
using namespace infect;

#include "wrap.h"
#include <Rcpp.h>
#include <map>
#include <utility>

#include "RRandom.h"

// Expose the classes to R
RCPP_EXPOSED_AS(RRandom)

RCPP_EXPOSED_CLASS_NODECL(util::Object)
RCPP_EXPOSED_CLASS_NODECL(util::MapLink)

// Critical classes (always exposed)
RCPP_EXPOSED_AS(util::Random)
RCPP_EXPOSED_AS(infect::Model)
RCPP_EXPOSED_AS(infect::System)
RCPP_EXPOSED_AS(infect::SystemHistory)
RCPP_EXPOSED_AS(infect::EpisodeHistory)
RCPP_EXPOSED_AS(infect::SystemEpisodeHistory)

#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
// Comprehensive testing classes
RCPP_EXPOSED_AS(infect::Event)
RCPP_EXPOSED_AS(infect::Patient)
RCPP_EXPOSED_AS(infect::Sampler)
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
// All optional classes
RCPP_EXPOSED_AS(util::IntMap)
RCPP_EXPOSED_AS(util::List)
RCPP_EXPOSED_AS(util::Map)
RCPP_EXPOSED_AS(infect::HistoryLink)
RCPP_EXPOSED_AS(infect::SetLocationState)
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
// Global cache to keep shared_ptrs alive as long as System exists
// This prevents the IntMap/Map from being deleted when returned to R
static std::map<infect::System*, std::shared_ptr<util::IntMap>> fac_cache;
static std::map<infect::System*, std::shared_ptr<util::IntMap>> pat_cache;
static std::map<std::pair<infect::System*, infect::Patient*>, std::shared_ptr<util::Map>> eps_cache;

// Wrapper functions that explicitly wrap returned pointers in reference classes
// with XPtr(p, false) to prevent R from deleting them (System owns them via shared_ptr)
// The shared_ptr is also cached to keep the container alive even if intermediate R objects are GC'd

static SEXP System_getFacilities_wrapper(infect::System* sys) {
    auto sp = sys->getFacilities();
    fac_cache[sys] = sp;  // Keep shared_ptr alive
    util::IntMap* p = sp.get();
    if (p == nullptr) {
        return R_NilValue;
    }
    Rcpp::Function methods_new = Rcpp::Environment::namespace_env("methods")["new"];
    return methods_new("Rcpp_CppIntMap", 
                      Rcpp::Named(".object_pointer") = Rcpp::XPtr<util::IntMap>(p, false));
}

static SEXP System_getPatients_wrapper(infect::System* sys) {
    auto sp = sys->getPatients();
    pat_cache[sys] = sp;  // Keep shared_ptr alive
    util::IntMap* p = sp.get();
    if (p == nullptr) {
        return R_NilValue;
    }
    Rcpp::Function methods_new = Rcpp::Environment::namespace_env("methods")["new"];
    return methods_new("Rcpp_CppIntMap", 
                      Rcpp::Named(".object_pointer") = Rcpp::XPtr<util::IntMap>(p, false));
}

static SEXP System_getEpisodes_wrapper(infect::System* sys, infect::Patient* p) {
    auto sp = sys->getEpisodes(p);
    eps_cache[std::make_pair(sys, p)] = sp;  // Keep shared_ptr alive
    util::Map* m = sp.get();
    if (m == nullptr) {
        return R_NilValue;
    }
    Rcpp::Function methods_new = Rcpp::Environment::namespace_env("methods")["new"];
    return methods_new("Rcpp_CppMap", 
                      Rcpp::Named(".object_pointer") = Rcpp::XPtr<util::Map>(m, false));
}
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
/**
 * @brief Wrapper function to extract all Events from a SystemHistory and return them as an Rcpp::List
 * 
 * This function traverses the SystemHistory via the system history chain (sNext links)
 * and collects all Event objects, wrapping each in its corresponding Rcpp reference class.
 * The Events are returned in chronological order as they appear in the history chain.
 * 
 * @param hist Pointer to the SystemHistory object to extract events from
 * @return SEXP An Rcpp::List containing wrapped CppEvent reference class objects.
 *              Returns empty list if hist is nullptr or has no events.
 *              Each Event pointer is wrapped with XPtr(event, false) to indicate
 *              that R should NOT delete these objects (SystemHistory owns them).
 * 
 * @note The returned Event objects share memory with the SystemHistory and should
 *       not be modified or deleted from R. They are read-only references.
 * 
 * @usage In R: event_list <- system_history$getEventList()
 */
static SEXP SystemHistory_getEventList_wrapper(infect::SystemHistory* hist) {
    if (hist == nullptr) {
        return R_NilValue;
    }
    
    Rcpp::List event_list;
    Rcpp::Function methods_new = Rcpp::Environment::namespace_env("methods")["new"];
    
    // Get the system head and traverse all history links
    infect::HistoryLink* link = hist->getSystemHead();
    
    if (link == nullptr) {
        return event_list;  // Return empty list
    }
    
    // Traverse the system history chain via sNext() to collect all events
    for (; link != nullptr; link = link->sNext()) {
        infect::Event* event = link->getEvent();
        
        if (event != nullptr) {
            // Wrap the Event pointer in an Rcpp reference class
            // Use XPtr with false flag to indicate R should not delete it
            // (the SystemHistory owns these events)
            SEXP event_obj = methods_new("Rcpp_CppEvent",
                                        Rcpp::Named(".object_pointer") = Rcpp::XPtr<infect::Event>(event, false));
            event_list.push_back(event_obj);
        }
    }
    
    return event_list;
}

/**
 * @brief Wrapper function to extract all HistoryLinks from a SystemHistory and return them as an Rcpp::List
 * 
 * This function traverses the SystemHistory via the system history chain (sNext links)
 * and collects all HistoryLink objects, wrapping each in its corresponding Rcpp reference class.
 * 
 * @param hist Pointer to the SystemHistory object to extract history links from
 * @return SEXP An Rcpp::List containing wrapped CppHistoryLink reference class objects.
 *              Returns empty list if hist is nullptr or has no history links.
 * 
 * @note The returned HistoryLink objects share memory with the SystemHistory and should
 *       not be modified or deleted from R. They are read-only references.
 * 
 * @usage In R: link_list <- system_history$getHistoryLinkList()
 */
static SEXP SystemHistory_getHistoryLinkList_wrapper(infect::SystemHistory* hist) {
    if (hist == nullptr) {
        return R_NilValue;
    }
    
    Rcpp::List link_list;
    Rcpp::Function methods_new = Rcpp::Environment::namespace_env("methods")["new"];
    
    // Get the system head and traverse all history links
    infect::HistoryLink* link = hist->getSystemHead();
    
    if (link == nullptr) {
        return link_list;  // Return empty list
    }
    
    // Traverse the system history chain via sNext() to collect all history links
    for (; link != nullptr; link = link->sNext()) {
        // Wrap the HistoryLink pointer in an Rcpp reference class
        // Use XPtr with false flag to indicate R should not delete it
        SEXP link_obj = methods_new("Rcpp_CppHistoryLink",
                                   Rcpp::Named(".object_pointer") = Rcpp::XPtr<infect::HistoryLink>(link, false));
        link_list.push_back(link_obj);
    }
    
    return link_list;
}
#endif

// TODO: Add System destructor hook to clear cache entries for deleted Systems

void init_Module_infect_system(){
    using namespace Rcpp;
    
#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
    class_<infect::Sampler>("CppSampler")
        .derives<util::Object>("CppObject")
        .constructor<SystemHistory*, Model*, Random*>()
        .method<void>("sampleModel", &infect::Sampler::sampleModel)
        .method<void>("sampleEpisodes", &infect::Sampler::sampleEpisodes)
    ;
#endif
    
    class_<System>("CppSystem")
        .derives<util::Object>("CppObject")
        .constructor<std::vector<int>, std::vector<int>, std::vector<double>, std::vector<int>, std::vector<int>>()
        .property("log", &System::get_log)
        .method("startTime", &System::startTime)
        .method("endTime", &System::endTime)
        .method("countEpisodes", (int (infect::System::*)() const)&infect::System::countEpisodes)
        .method("countEvents", (int (infect::System::*)() const)&infect::System::countEvents)
#ifdef BAYESTRANSMISSION_ALL_CLASSES
        .method("getFacilities", &System_getFacilities_wrapper)
        .method("getUnits", &System::getUnits)
        .method("getPatients", &System_getPatients_wrapper)
        .method("getEpisodes", &System_getEpisodes_wrapper)
        .method("getSystemCounts", (util::List* (infect::System::*)() const)&infect::System::getSystemCounts)
#endif
    ;
    
    class_<infect::SystemHistory>("CppSystemHistory")
        .derives<util::Object>("CppObject")
        .constructor<infect::System*, infect::Model*, bool>()
        .property("sumocc", &infect::SystemHistory::sumocc)
        .property("Episodes", &infect::SystemHistory::getEpisodes)
        .property("Admissions", &infect::SystemHistory::getAdmissions)
        .property("Discharges", &infect::SystemHistory::getDischarges)
#ifdef BAYESTRANSMISSION_ALL_CLASSES
        .property("UnitHeads", &infect::SystemHistory::getUnitHeads)
        .property("PatientHeads", &infect::SystemHistory::getPatientHeads)
        .property("FacilityHeads", &infect::SystemHistory::getFacilityHeads)
        .property("SystemHead", &infect::SystemHistory::getSystemHead)
        .method("getEventList", &SystemHistory_getEventList_wrapper)
        .method("getHistoryLinkList", &SystemHistory_getHistoryLinkList_wrapper)
#endif
    ;
    
}
