#ifndef bayesian_transmission_wrap_h
#define bayesian_transmission_wrap_h

#include <string>
using std::string;

#include "util/util.h"
#include "infect/infect.h"
#include "modeling/modeling.h"
#include "lognormal/lognormal.h"

#include <RcppCommon.h>

#define DECLARE_POINTER(type) typedef type* type##_ptr
#define DECLARE_WRAP(type) template<> SEXP wrap(const type##_ptr& p)

namespace util{
    DECLARE_POINTER(Object);
    DECLARE_POINTER(Map);
    DECLARE_POINTER(MapLink);
}

namespace infect{
    DECLARE_POINTER(AbxLocationState);
    DECLARE_POINTER(AbxPatientState);
    DECLARE_POINTER(CountLocationState);
    DECLARE_POINTER(Episode);
    DECLARE_POINTER(EpisodeHistory);
    DECLARE_POINTER(Event);
    DECLARE_POINTER(Facility);
    DECLARE_POINTER(FacilityEpisodeHistory);
    DECLARE_POINTER(HistoryLink);
    DECLARE_POINTER(LocationState);
    DECLARE_POINTER(Model);
    DECLARE_POINTER(Patient);
    DECLARE_POINTER(PatientState);
    DECLARE_POINTER(Sampler);
    DECLARE_POINTER(SetLocationState);
    DECLARE_POINTER(State);
    DECLARE_POINTER(System);
    DECLARE_POINTER(SystemEpisodeHistory);
    DECLARE_POINTER(SystemHistory);
    DECLARE_POINTER(Unit);
    DECLARE_POINTER(UnitEpisodeHistory);
    // typedef UnitTrackingModel* UnitTrackingModel_ptr;
}
namespace models{
    DECLARE_POINTER(AbxParams);
    DECLARE_POINTER(ConstrainedSimulator);
    DECLARE_POINTER(RandomTestParams);
    DECLARE_POINTER(InColParams);
    DECLARE_POINTER(InsituParams);
    DECLARE_POINTER(MassActionICP);
    DECLARE_POINTER(MassActionModel);
    DECLARE_POINTER(OutColParams);
    DECLARE_POINTER(Parameters);
    DECLARE_POINTER(RandomTestParams);
    DECLARE_POINTER(TestParams);
    DECLARE_POINTER(TestParamsAbx);
    DECLARE_POINTER(UnitLinkedModel);
    }
namespace lognormal{
    DECLARE_POINTER(LinearAbxICP);
    DECLARE_POINTER(LinearAbxModel);
    DECLARE_POINTER(LinearAbxModel2);
    DECLARE_POINTER(LogNormalAbxICP);
    DECLARE_POINTER(LogNormalICP);
    DECLARE_POINTER(LogNormalMassAct);
    DECLARE_POINTER(LogNormalModel);
    DECLARE_POINTER(MixedICP);
    DECLARE_POINTER(MixedModel);
    // DECLARE_POINTER(MultiUnitAbxIcp);
}


SEXP params2R(const Parameters *);
SEXP model2R(const lognormal::LogNormalModel * );
// SEXP HistoryLink2R(const infect::HistoryLink *);

typedef double (models::UnitLinkedModel::*LogLikelihood_SH)(infect::SystemHistory*);
typedef double (models::UnitLinkedModel::*LogLikelihood_HL)(infect::HistoryLink*);
typedef double (models::UnitLinkedModel::*LogLikelihood_EH)(infect::EpisodeHistory*);
typedef double (models::UnitLinkedModel::*LogLikelihood_P_HL)(infect::Patient*, infect::HistoryLink*);
typedef double (models::UnitLinkedModel::*LogLikelihood_P_HL_i)(infect::Patient*, infect::HistoryLink*, int);
typedef double (models::UnitLinkedModel::*LogLikelihood_HL_i)(infect::HistoryLink*, int);

namespace Rcpp {
    //util
    DECLARE_WRAP(util::Map);
    DECLARE_WRAP(util::Object);
    DECLARE_WRAP(util::MapLink);

    //infect
    DECLARE_WRAP(infect::AbxLocationState);
    DECLARE_WRAP(infect::AbxPatientState);
    DECLARE_WRAP(infect::CountLocationState);
    DECLARE_WRAP(infect::Episode);
    DECLARE_WRAP(infect::EpisodeHistory);
    DECLARE_WRAP(infect::Event);
    DECLARE_WRAP(infect::Facility);
    DECLARE_WRAP(infect::FacilityEpisodeHistory);
    DECLARE_WRAP(infect::HistoryLink);
    DECLARE_WRAP(infect::LocationState);
    DECLARE_WRAP(infect::Model);
    DECLARE_WRAP(infect::Patient);
    DECLARE_WRAP(infect::PatientState);
    DECLARE_WRAP(infect::Sampler);
    DECLARE_WRAP(infect::SetLocationState);
    DECLARE_WRAP(infect::State);
    DECLARE_WRAP(infect::System);
    DECLARE_WRAP(infect::SystemEpisodeHistory);
    DECLARE_WRAP(infect::SystemHistory);
    DECLARE_WRAP(infect::Unit);
    DECLARE_WRAP(infect::UnitEpisodeHistory);
    // DECLARE_WRAP(infect::UnitTrackingModel);



    //models
    DECLARE_WRAP(models::AbxParams);
    DECLARE_WRAP(models::ConstrainedSimulator);
    DECLARE_WRAP(models::RandomTestParams);
    DECLARE_WRAP(models::InColParams);
    DECLARE_WRAP(models::InsituParams);
    DECLARE_WRAP(models::MassActionICP);
    DECLARE_WRAP(models::MassActionModel);
    DECLARE_WRAP(models::OutColParams);
    DECLARE_WRAP(models::Parameters);
    DECLARE_WRAP(models::RandomTestParams);
    DECLARE_WRAP(models::TestParams);
    DECLARE_WRAP(models::TestParamsAbx);
    DECLARE_WRAP(models::UnitLinkedModel);

    // lognormal
    DECLARE_WRAP(lognormal::LinearAbxICP);
    DECLARE_WRAP(lognormal::LinearAbxModel);
    DECLARE_WRAP(lognormal::LinearAbxModel2);
    DECLARE_WRAP(lognormal::LogNormalAbxICP);
    DECLARE_WRAP(lognormal::LogNormalICP);
    DECLARE_WRAP(lognormal::LogNormalModel);
    DECLARE_WRAP(lognormal::MixedICP);
    DECLARE_WRAP(lognormal::MixedModel);
}
#define DECLARE_AS(CLASS) template <> inline CLASS* as<CLASS*>(SEXP sexp);
namespace Rcpp{

// DECLARE_AS(infect::AbxLocationState);
// DECLARE_AS(infect::AbxPatientState);
// DECLARE_AS(infect::CountLocationState);
// DECLARE_AS(infect::Episode);
// DECLARE_AS(infect::EpisodeHistory);
// DECLARE_AS(infect::Event);
// DECLARE_AS(infect::Facility);
// DECLARE_AS(infect::FacilityEpisodeHistory);
// DECLARE_AS(infect::HistoryLink);
// DECLARE_AS(infect::LocationState);
// DECLARE_AS(infect::Model);
// DECLARE_AS(infect::Patient);
// DECLARE_AS(infect::PatientState);
// DECLARE_AS(infect::Sampler);
// DECLARE_AS(infect::SetLocationState);
// DECLARE_AS(infect::State);
// DECLARE_AS(infect::System);
// DECLARE_AS(infect::SystemEpisodeHistory);
// DECLARE_AS(infect::SystemHistory);
// DECLARE_AS(infect::Unit);
// DECLARE_AS(infect::UnitEpisodeHistory);


}

namespace Rcpp { // Non-simplistic export
    template <> SEXP wrap(const infect::InfectionCoding::InfectionStatus& status);
    template <> SEXP wrap(const infect::EventCoding::EventCode& code);

    template <> SEXP wrap(const lognormal::LogNormalModel& model);

    // AS
    template <>
    inline infect::EventCoding::EventCode as<infect::EventCoding::EventCode>(SEXP sexp)
    {
        if (TYPEOF(sexp) == INTSXP) {
            // Convert the SEXP to an integer
            int code = Rcpp::as<int>(sexp);
            // Directly cast the integer to the enumeration type
            return static_cast<infect::EventCoding::EventCode>(code);
        } else if (TYPEOF(sexp) == STRSXP) {
            // Convert the SEXP to a string
            std::string codeStr = Rcpp::as<std::string>(sexp);
            // Use the conversion function to convert the string to EventCode
            return infect::EventCoding::toEventCode(codeStr);
        } else {
            throw std::invalid_argument("SEXP must be either an integer or a string");
        }
    }

}
#endif // bayesian_transmission_wrap_h
