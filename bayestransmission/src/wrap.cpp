#include "wrap.h"
#include "RRandom.h"
#include <Rcpp.h>

using namespace Rcpp;

SEXP params2R(const Parameters * P){
    std::vector<double> values = P->getValues();
    Rcpp::NumericVector rtn(std::begin(values), std::end(values));
    rtn.attr("names") = P->paramNames();
    return rtn;
}

template <> SEXP Rcpp::wrap(const Parameters& P)
{
    return params2R(&P);
}


SEXP model2R(const lognormal::LogNormalModel * model)
{

    auto insitu = params2R(model->getInsituParams());
    auto surveillanceTest = params2R(model->getSurveillanceTestParams());
    auto clinicalTest = params2R(model->getClinicalTestParams());
    auto outCol = params2R(model->getOutColParams());
    auto inCol = params2R(model->getInColParams());
    auto abx = params2R(model->getAbxParams());

    return Rcpp::List::create(
        // _[""] = params2R(model->getParams()),
        _["Insitu"] = insitu, //params2R(model->getInsituParams()),
        _["SurveillanceTest"] = surveillanceTest, //params2R(model->getSurveillanceTestParams()),
        _["ClinicalTest"] = clinicalTest, //params2R(model->getClinicalTestParams()),
        _["OutCol"] = outCol, //params2R(model->getOutColParams()),
        _["InCol"] = inCol, //params2R(model->getInColParams()),
        _["Abx"] = abx //params2R(model->getAbxParams())
    );
}

template <> SEXP Rcpp::wrap(const lognormal::LogNormalModel& model)
{
    return model2R(&model);
}

SEXP Event2R(const infect::Event * e)
{
    return Rcpp::List::create(
        _["time"] = e->getTime(),
        _["type"] = infect::EventCoding::eventString(e->getType()),
        _["patient"] = e->getPatient()->getId(),
        _["unit"] = e->getUnit()->getId(),
        _["facility"] = e->getFacility()->getId()
    );

}
// SEXP HistoryLink2R(const infect::HistoryLink * hl)
// {
//     return Rcpp::List::create(
//         // _["time "]
//     );
//
// }

#define CREATE_WRAP_REFCLASS(NAMESPACE, CLASS)                                 \
template <>                                                                    \
SEXP Rcpp::wrap(const NAMESPACE::CLASS##_ptr& p) {                             \
    if(p == 0) {return R_NilValue;}                                            \
    Function methods_new = Rcpp::Environment::namespace_env("methods")["new"]; \
    return methods_new("Rcpp_Cpp" #CLASS, Rcpp::Named(".object_pointer") =     \
                                      Rcpp::XPtr<NAMESPACE::CLASS>(p, false)); \
}

#define EXPORT_POINTER(type) template <> SEXP Rcpp::wrap(const type##_ptr& p){ \
    return Rcpp::XPtr<type>(p, true);                                          \
}

// util
EXPORT_POINTER(util::Object)

CREATE_WRAP_REFCLASS(util,Map)

// infect - Critical classes (always exposed)
CREATE_WRAP_REFCLASS( infect, Model )
CREATE_WRAP_REFCLASS( infect, System )
CREATE_WRAP_REFCLASS( infect, SystemEpisodeHistory )
CREATE_WRAP_REFCLASS( infect, SystemHistory )
CREATE_WRAP_REFCLASS( infect, EpisodeHistory )
CREATE_WRAP_REFCLASS( infect, FacilityEpisodeHistory )
CREATE_WRAP_REFCLASS( infect, UnitEpisodeHistory )

#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
// Comprehensive testing classes
CREATE_WRAP_REFCLASS( infect, Event )
CREATE_WRAP_REFCLASS( infect, Facility )
CREATE_WRAP_REFCLASS( infect, Patient )
CREATE_WRAP_REFCLASS( infect, PatientState )
CREATE_WRAP_REFCLASS( infect, Sampler )
CREATE_WRAP_REFCLASS( infect, Unit )

// State classes for comprehensive testing
CREATE_WRAP_REFCLASS( infect, SetLocationState )
CREATE_WRAP_REFCLASS( infect, State )
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
// All optional classes
CREATE_WRAP_REFCLASS( infect, AbxLocationState )
CREATE_WRAP_REFCLASS( infect, AbxPatientState )
CREATE_WRAP_REFCLASS( infect, CountLocationState )
CREATE_WRAP_REFCLASS( infect, Episode )
CREATE_WRAP_REFCLASS( infect, HistoryLink )
CREATE_WRAP_REFCLASS( infect, LocationState )
#endif
// CREATE_WRAP_REFCLASS(infect,UnitTrackingModel )

// Models
CREATE_WRAP_REFCLASS( models, AbxParams )
CREATE_WRAP_REFCLASS( models, ConstrainedSimulator )
CREATE_WRAP_REFCLASS( models, InColParams )
CREATE_WRAP_REFCLASS( models, InsituParams )
CREATE_WRAP_REFCLASS( models, MassActionICP )
CREATE_WRAP_REFCLASS( models, MassActionModel )
CREATE_WRAP_REFCLASS( models, OutColParams )
CREATE_WRAP_REFCLASS( models, Parameters )
CREATE_WRAP_REFCLASS( models, RandomTestParams )
CREATE_WRAP_REFCLASS( models, TestParams )
#ifdef BAYESTRANSMISSION_ALL_CLASSES
CREATE_WRAP_REFCLASS( models, TestParamsAbx )
#endif
CREATE_WRAP_REFCLASS( models, UnitLinkedModel )

// LogNormal
CREATE_WRAP_REFCLASS( lognormal, LinearAbxICP)
CREATE_WRAP_REFCLASS( lognormal, LinearAbxModel)
CREATE_WRAP_REFCLASS( lognormal, LinearAbxModel2)
CREATE_WRAP_REFCLASS( lognormal, LogNormalAbxICP)
CREATE_WRAP_REFCLASS( lognormal, LogNormalICP )
CREATE_WRAP_REFCLASS( lognormal, LogNormalModel )
CREATE_WRAP_REFCLASS( lognormal, MixedICP )
CREATE_WRAP_REFCLASS( lognormal, MixedModel )


// Special Conversions
template <> SEXP Rcpp::wrap(const infect::InfectionCoding::InfectionStatus& status)
{
    return wrap(infect::InfectionCoding::codeString(status));
}

template <> SEXP Rcpp::wrap(const infect::EventCoding::EventCode& code)
{
    return wrap(infect::EventCoding::eventString(code));
}

#define MAKE_R_CONVERTER(CLASS)                                                \
    if (TYPEOF(x) == EXTPTRSXP) {                                          \
        Rcpp::XPtr<CLASS> Xptr = Rcpp::as<Rcpp::XPtr<CLASS>>(x);           \
        return wrap(dynamic_cast<CLASS *>(Xptr.get()));                    \
    }                                                                      \
    if (Rf_inherits(x, "Rcpp_CppObject")) {                                \
        Rcpp::RObject obj(x);                                              \
        Rcpp::Environment env(obj);                                        \
        Rcpp::XPtr<CLASS> Xptr = env[".pointer"];                          \
        return wrap(dynamic_cast<CLASS*>(Xptr.get()));                     \
    }                                                                      \
    Rcpp::stop("ptr must be an external pointer");

// [[Rcpp::export]]
SEXP asAbxLocationState(SEXP x){
#ifdef BAYESTRANSMISSION_ALL_CLASSES
MAKE_R_CONVERTER(infect::AbxLocationState)
#else
    Rcpp::stop("asAbxLocationState is not available in this build. Rebuild with BAYESTRANSMISSION_ALL_CLASSES defined.");
#endif
}

// [[Rcpp::export]]
SEXP asMap(SEXP x){
    if (TYPEOF(x) == EXTPTRSXP) {
        Rcpp::XPtr<util::Map> Xptr = Rcpp::as<Rcpp::XPtr<util::Map>>(x);
        return wrap(dynamic_cast<util::Map*>(Xptr.get()));
    }
    if (Rf_inherits(x, "Rcpp_CppObject")) {
        Rcpp::RObject obj(x);
        Rcpp::Environment env(obj);
        Rcpp::XPtr<util::Map> Xptr = env[".pointer"];
        return wrap(dynamic_cast<util::Map*>(Xptr.get()));
    }
    Rcpp::stop("ptr must be an external pointer");
}

// [[Rcpp::export]]
SEXP asHistoryLink(SEXP x)
{
#ifdef BAYESTRANSMISSION_ALL_CLASSES
    if (TYPEOF(x) == EXTPTRSXP) {
        Rcpp::XPtr<HistoryLink> Xptr = Rcpp::as<Rcpp::XPtr<HistoryLink>>(x);
        return wrap(dynamic_cast<HistoryLink*>(Xptr.get()));
    } else
    if (Rf_inherits(x, "Rcpp_CppObject") || Rf_inherits(x, "envRefClass")) {
        Rcpp::RObject obj(x);
        Rcpp::Environment env(obj);
        Rcpp::XPtr<HistoryLink> Xptr = env[".pointer"];
        return wrap(dynamic_cast<HistoryLink*>(Xptr.get()));
    } else
        Rcpp::stop("ptr must be an external pointer");
#else
    Rcpp::stop("asHistoryLink is not available in this build. Rebuild with BAYESTRANSMISSION_ALL_CLASSES defined.");
#endif
}

// [[Rcpp::export]]
std::string WhatAmI(SEXP x){
    // return x -> className();
    if (TYPEOF(x) == EXTPTRSXP) {
        Rcpp::XPtr<util::Object> Xptr = Rcpp::as<Rcpp::XPtr<util::Object>>(x);
    return Xptr.get() -> className();
    } else if (Rf_inherits(x, "envRefClass")) {
        Rcpp::RObject obj(x);
        Rcpp::Environment env(obj);
        Rcpp::XPtr<util::Object> Xptr = env[".pointer"];
        return (dynamic_cast<Object *>(Xptr.get()))->className();
    } else
        Rcpp::stop("ptr must be an external pointer");
}



// Expose the classes to R
RCPP_EXPOSED_AS(RRandom)

    RCPP_EXPOSED_CLASS_NODECL(util::Object)
    RCPP_EXPOSED_CLASS_NODECL(util::MapLink)

    RCPP_EXPOSED_AS(util::Map)
    RCPP_EXPOSED_AS(util::Random)

    // Critical classes (always exposed)
    RCPP_EXPOSED_AS(infect::Model)
    RCPP_EXPOSED_AS(infect::System)
    RCPP_EXPOSED_AS(infect::SystemEpisodeHistory)
    RCPP_EXPOSED_AS(infect::SystemHistory)
    RCPP_EXPOSED_AS(infect::EpisodeHistory)
    RCPP_EXPOSED_AS(infect::FacilityEpisodeHistory)
    RCPP_EXPOSED_AS(infect::UnitEpisodeHistory)

#ifdef BAYESTRANSMISSION_COMPREHENSIVE_TESTING
    // Comprehensive testing classes
    RCPP_EXPOSED_AS(infect::Event)
    RCPP_EXPOSED_AS(infect::Facility)
    RCPP_EXPOSED_AS(infect::Patient)
    RCPP_EXPOSED_AS(infect::PatientState)
    RCPP_EXPOSED_AS(infect::RawEvent)
    RCPP_EXPOSED_AS(infect::RawEventList)
    RCPP_EXPOSED_AS(infect::Sampler)
    RCPP_EXPOSED_AS(infect::Unit)
    
    // State classes
    RCPP_EXPOSED_AS(infect::SetLocationState)
#endif

#ifdef BAYESTRANSMISSION_ALL_CLASSES
    // All optional classes
    RCPP_EXPOSED_AS(infect::AbxLocationState)
    RCPP_EXPOSED_AS(infect::AbxPatientState)
    RCPP_EXPOSED_AS(infect::CountLocationState)
    RCPP_EXPOSED_AS(infect::Episode)
    RCPP_EXPOSED_AS(infect::HistoryLink)
    RCPP_EXPOSED_AS(infect::LocationState)
#endif


