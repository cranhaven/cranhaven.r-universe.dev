#include "infect/infect.h"
using namespace infect;

#include "lognormal/lognormal.h"
using namespace lognormal;

#include "modeling/modeling.h"


#include "wrap.h"

#include <Rcpp.h>

#include "RRandom.h"
// #include <functional>


void init_Module_util();
void init_Module_infect();
void init_Module_infect_system();
void init_Module_models();
void init_Module_lognormal();

// Expose the classes to R
// RCPP_EXPOSED_AS(RRandom)
//
// RCPP_EXPOSED_CLASS_NODECL(util::Object)
// RCPP_EXPOSED_CLASS_NODECL(util::MapLink)
//
// RCPP_EXPOSED_AS(util::Map)
// RCPP_EXPOSED_AS(util::Random)
//
// RCPP_EXPOSED_AS(infect::AbxLocationState)
// RCPP_EXPOSED_AS(infect::AbxPatientState)
// RCPP_EXPOSED_AS(infect::CountLocationState)
// RCPP_EXPOSED_AS(infect::Episode)
// RCPP_EXPOSED_AS(infect::EpisodeHistory)
// RCPP_EXPOSED_AS(infect::Event)
// RCPP_EXPOSED_AS(infect::Facility)
// RCPP_EXPOSED_AS(infect::FacilityEpisodeHistory)
// RCPP_EXPOSED_AS(infect::HistoryLink)
// RCPP_EXPOSED_AS(infect::LocationState)
// RCPP_EXPOSED_AS(infect::Model)
// RCPP_EXPOSED_AS(infect::Patient)
// RCPP_EXPOSED_AS(infect::PatientState)
// RCPP_EXPOSED_AS(infect::RawEvent)
// RCPP_EXPOSED_AS(infect::RawEventList)
// RCPP_EXPOSED_AS(infect::Sampler)
// RCPP_EXPOSED_AS(infect::SetLocationState)
// RCPP_EXPOSED_AS(infect::System)
// RCPP_EXPOSED_AS(infect::SystemEpisodeHistory)
// RCPP_EXPOSED_AS(infect::SystemHistory)
// RCPP_EXPOSED_AS(infect::Unit)
// RCPP_EXPOSED_AS(infect::UnitEpisodeHistory)


RCPP_MODULE(BayesianInfectiousDiseaseModelingModule){
    using namespace Rcpp;

    init_Module_util();
    init_Module_infect();
    init_Module_infect_system();
    init_Module_models();
    init_Module_lognormal();


    class_<RRandom>("RRandom")
        .constructor<>()
        .method("runif", (double(RRandom::*)()) &RRandom::runif)
        .method<double, double, double>("runif2", (double(RRandom::*)(double, double)) &RRandom::runif)
        .method("rexp", (double(RRandom::*)()) &RRandom::rexp)
        .method("rexp1", (double(RRandom::*)(double)) &RRandom::rexp)
        .method("rgamma", &RRandom::rgamma)
        .method("rnorm", (double(RRandom::*)()) &RRandom::rnorm)
        .method("rnorm2", (double(RRandom::*)(double, double))&RRandom::rnorm)
        .method("rpoisson", &RRandom::rpoisson)
    ;

}
