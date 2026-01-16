
#include <string>
using std::string;

#include "util/util.h"
#include "infect/infect.h"
#include "modeling/modeling.h"
#include "lognormal/lognormal.h"

using namespace infect;
using namespace util;
using namespace lognormal;

#include "wrap.h"

using namespace Rcpp;

#include "RRandom.h"

#include "modelsetup.h"
lognormal::LogNormalModel* newModel(
        Rcpp::List modelParameters, //< Model specific options.
        bool verbose = false)
{
    lognormal::LogNormalModel *model = 0;
    std::string modname = modelParameters["modname"];
    int nstates = modelParameters["nstates"];

    if (modname == "LinearAbxModel")
    {
        model = new LinearAbxModel(
            nstates,
            modelParameters["nmetro"],
            modelParameters["forward"],
            modelParameters["cheat"]
        );
        modelsetup<LinearAbxModel>((LinearAbxModel*)model, modelParameters, verbose);
    } else
    if (modname == "LinearAbxModel2")
    {
        model = new LinearAbxModel2(
            nstates,
            modelParameters["nmetro"],
            modelParameters["forward"],
            modelParameters["cheat"]
        );
        modelsetup<LinearAbxModel2>((LinearAbxModel2*)model, modelParameters, verbose);
    } else
    // if (modname == "MultiUnitLinearAbxModel")
    // {
    //     model = new lognormal::MultiUnitLinearAbxModel(nstates,l,nmetro,forward,cheat);
    // } else
    if (modname == "MixedModel")
    {
        model = new MixedModel(
            nstates,
            modelParameters["nmetro"],
            modelParameters["forward"],
            modelParameters["cheat"]
        );
        modelsetup<MixedModel>((MixedModel*)model, modelParameters, verbose);
    } else
    // if (modname == "LogNormalAbxModel")
    // {
    //     model = new lognormal::LogNormalAbxModel(nstates,nmetro,forward,cheat);
    // } else
    // if (modname == "LNMassActionModel")
    // {
    //     model = new lognormal::LNMassActionModel(nstates,nmetro,forward,cheat);
    // } else
    if (modname == "LogNormalModel")
    {
        model = new LogNormalModel(
            nstates,
            0,  // abxtest - must be 0 for base LogNormalModel (creates TestParamsAbx with abx=false)
            modelParameters["nmetro"],
            modelParameters["forward"],
            modelParameters["cheat"]
        );
        modelsetup<LogNormalModel>((LogNormalModel*)model, modelParameters, verbose);
    }
    else
    {
        throw std::invalid_argument("Invalid model name");
    }

    //model->setup(modOptions);
    //modelsetup(model, modelParameters, verbose);

    return model;
}


//' Run Bayesian Transmission MCMC
//'
//' @param data Data frame with columns, in order: facility, unit, time, patient, and event type.
//' @param modelParameters List of model parameters, see [LogNormalModelParams()].
//' @param nsims Number of MCMC samples to collect after burn-in.
//' @param nburn Number of burn-in iterations.
//' @param outputparam Whether to output parameter values at each iteration.
//' @param outputfinal Whether to output the final model state.
//' @param verbose Print progress messages.
//'
//' @return A list with the following elements:
//'   * `Parameters` the MCMC chain of model parameters (if outputparam=TRUE)
//'   * `LogLikelihood` the log likelihood of the model at each iteration (if outputparam=TRUE)
//'   * `MCMCParameters` the MCMC parameters used
//'   * `ModelParameters` the model parameters used
//'   * `ModelName` the name of the model
//'   * `nstates` the number of states in the model
//'   * `waic1` the WAIC1 estimate
//'   * `waic2` the WAIC2 estimate
//'   * and optionally (if outputfinal=TRUE) `FinalModel` the final model state.
//' @seealso [mcmc_to_dataframe]
//' @examples
//'   # Minimal example: create parameters and run a very short MCMC
//'   params <- LinearAbxModel(nstates = 2)
//'   data(simulated.data_sorted, package = "bayestransmission")
//'   results <- runMCMC(
//'     data = simulated.data_sorted,
//'     modelParameters = params,
//'     nsims = 3,
//'     nburn = 0,
//'     outputparam = TRUE,
//'     outputfinal = FALSE,
//'     verbose = FALSE
//'   )
//'   str(results)
//' @export
// [[Rcpp::export]]
SEXP runMCMC(
    Rcpp::DataFrame data,
    Rcpp::List modelParameters,
    unsigned int nsims,
    unsigned int nburn = 100,
    bool outputparam = true,
    bool outputfinal = false,
    bool verbose = false
) {
    if(verbose)
        Rcpp::message(Rcpp::wrap(string("Initializing Variables")));

    // Make random number generator.

    if(verbose) Rcpp::Rcout << "Creating RNG...";

    RRandom *random = new RRandom();

    if(verbose) Rcpp::Rcout << "Done" << std::endl;

    if(verbose) Rcpp::Rcout << "Setting up System...";

    // Validate data is sorted by patient, then time
    std::vector<int> patients = as<std::vector<int>>(data[3]);
    std::vector<double> times = as<std::vector<double>>(data[2]);
    
    for (size_t i = 1; i < patients.size(); i++) {
        if (patients[i] < patients[i-1]) {
            Rcpp::stop("Data must be sorted by patient ID, then time. "
                      "Row %d has patient %d, but previous row had patient %d. "
                      "Please sort your data: data[order(data$patient, data$time), ]",
                      i+1, patients[i], patients[i-1]);
        }
        if (patients[i] == patients[i-1] && times[i] < times[i-1]) {
            Rcpp::stop("Data must be sorted by patient ID, then time. "
                      "For patient %d, row %d has time %.4f which is before row %d time %.4f. "
                      "Please sort your data: data[order(data$patient, data$time), ]",
                      patients[i], i+1, times[i], i, times[i-1]);
        }
    }

    System *sys = new System(
        as<std::vector<int>>(data[0]),//facility
        as<std::vector<int>>(data[1]),//unit
        times,//"time"
        patients,//"patient"
        as<std::vector<int>>(data[4])//"event type"
    );
    if (verbose) Rcpp::Rcout << "Done" << std::endl;


    //Model
    if (verbose) Rcpp::Rcout << "Creating model...";

    lognormal::LogNormalModel *model = newModel(modelParameters, verbose);
    if (verbose) Rcpp::Rcout << "Done" << std::endl;

    // Set time origin of model.
    LogNormalICP *icp = (LogNormalICP *) model->getInColParams();
    icp->setTimeOrigin((sys->endTime()-sys->startTime())/2.0);
    if (verbose) Rcpp::Rcout << "Set time origin" << std::endl;


    // Create state history.

    if (verbose) Rcpp::Rcout << "Building history structure...";

    SystemHistory *hist = new SystemHistory(sys, model, false);
    if (verbose) Rcpp::Rcout << "Done" << std::endl;

    // Find tests for posterior prediction and, hence, WAIC estimates.

    if (verbose) Rcpp::message(Rcpp::wrap(string("Finding tests for WAIC.\n")));

    util::List* tests = hist->getTestLinks();
    TestParams** testtype = new TestParams*[tests->size()];
    HistoryLink** histlink = new HistoryLink*[tests->size()];

    int wntests = 0;
    double wprob = 0;
    double wlogprob = 0;
    double wlogsqprob = 0;
    for (tests->init(); tests->hasNext(); wntests++)
    {
        histlink[wntests] = (HistoryLink *) tests->next();

        if (histlink[wntests]->getEvent()->isClinicalTest())
            testtype[wntests] = model->getClinicalTestParams();
        else
            testtype[wntests] = model->getSurveillanceTestParams();
    }

    // Make and runsampler.

    Rcpp::List paramchain(nsims);
    Rcpp::NumericVector llchain(nsims);
    if (verbose)
        Rcpp::message(Rcpp::wrap(string("Building sampler.\n")));

    Sampler *mc = new Sampler(hist,model,random);

    if (verbose)
    {
        Rcpp::Rcout << "\n=== INITIAL PARAMETERS ===" << std::endl;
        std::ostringstream ss;
        
        // Output parameter components
        model->getInsituParams()->write(ss);
        ss << "\t";
        model->getSurveillanceTestParams()->write(ss);
        ss << "\t";
        if (model->getClinicalTestParams() != model->getSurveillanceTestParams())
        {
            model->getClinicalTestParams()->write(ss);
            ss << "\t";
        }
        model->getOutColParams()->write(ss);
        ss << "\t";
        model->getInColParams()->write(ss);
        ss << "\t";
        if (model->getAbxParams() != 0)
        {
            model->getAbxParams()->write(ss);
            ss << "\t";
        }
        
        double initialLogLike = model->logLikelihood(hist);
        if (std::isinf(initialLogLike) || std::isnan(initialLogLike)) {
            Rcpp::Rcerr << "\nWARNING: Initial log likelihood is " << initialLogLike << "\n";
            Rcpp::Rcerr << "This suggests a problem with model initialization or data.\n";
        }
        Rcpp::Rcout << ss.str() << "\t\tLogLike=" << initialLogLike << std::endl;
        Rcpp::Rcout << "=== END INITIAL PARAMETERS ===\n" << std::endl;
    }

    if (verbose)
        Rcpp::message(Rcpp::wrap(string("burning in MCMC.\n")));
    for (unsigned int i=0; i<nburn; i++)
    {
        if(verbose) Rcout << i << ":sample episodes...";
        mc->sampleEpisodes();
        if(verbose) Rcout << "Sample Model...";
        mc->sampleModel();
        if(verbose) Rcout << "done." << std::endl;
    }

    if (verbose)
        Rcpp::message(Rcpp::wrap(string("Running MCMC.\n")));


    for (unsigned int i=0; i<nsims; i++)
    {
        if(verbose) Rcout << i << ":sample episodes...";
        mc->sampleEpisodes();
        if(verbose) Rcout << "Sample Model...";
        mc->sampleModel();

        if (outputparam)
        {
            if (verbose)
                Rcout << "Outputting parameters...";
            paramchain(i) = model2R(model);
            if (verbose) Rcout << "likelhood...";
            llchain(i) = model->logLikelihood(hist);
        }

        for (int j=0; j<wntests; j++)
        {
            HistoryLink *hh = histlink[j];
            double p = testtype[j]->eventProb(hh->getPState()->infectionStatus(),hh->getPState()->onAbx(),hh->getEvent()->getType());
            wprob += p;
            wlogprob += log(p);
            wlogsqprob += log(p)*log(p);
        }

        if(verbose) Rcout << "done." << std::endl;
    }

    if (verbose)
        Rcpp::message(Rcpp::wrap(string("MCMC done.\n")));

    wprob /= wntests * nsims;
    wlogprob /= wntests * nsims;
    wlogsqprob /= wntests * nsims;
    double waic1 = 2*log(wprob) - 4*wlogprob;
    double waic2 = -2 * log(wprob) - 2 * wlogprob*wlogprob + 2 * wlogsqprob;
    if (verbose) Rcout << "WAIC 1 2 = \t" << waic1 << "\t" << waic2 << "\n";

/*
*/

    // Reconstruct MCMCParameters list for return value
    Rcpp::List MCMCParameters = Rcpp::List::create(
        _["nsims"] = nsims,
        _["nburn"] = nburn,
        _["outputparam"] = outputparam,
        _["outputfinal"] = outputfinal
    );

    Rcpp::List ret = Rcpp::List::create(
        _["Parameters"] = paramchain,
        _["LogLikelihood"] = llchain,
        _["MCMCParameters"] = MCMCParameters,
        _["ModelParameters"] = modelParameters,
        // _["ModelName"] = modname,
        // _["nstates"] = nstates,
        _["waic1"] = waic1,
        _["waic2"] = waic2
    );

    if(outputfinal)
    {
        if (verbose) Rcout << "Writing complete form of final state." << std::endl;

        ret["FinalModel"] = model2R(model);
    }
    delete [] histlink;
    delete [] testtype;
    delete tests;
    delete mc;
    delete hist;
    delete sys;
    delete model;
    delete random;
    // Don't delete static members - they are shared across all invocations
    // Instead, clear them for the next run
    if (AbxCoding::sysabx != 0)
        AbxCoding::sysabx->clear();
    if (AbxCoding::syseverabx != 0)
        AbxCoding::syseverabx->clear();

    return ret;

}

//' Create a new model object
//'
//' Creates and initializes a model object based on the provided parameters.
//' This allows direct creation and inspection of model objects without running MCMC.
//' Returns a list with all model parameter values for verification.
//'
//' @param modelParameters List of model parameters, including:
//'   * `modname` Name of the model (e.g., "LogNormalModel", "LinearAbxModel", "LinearAbxModel2", "MixedModel")
//'   * `nstates` Number of states in the model
//'   * `nmetro` Number of metropolis steps
//'   * `forward` Forward parameter
//'   * `cheat` Cheat parameter
//' @param verbose Print progress messages (default: false)
//'
//' @return A list containing the initialized model parameters:
//'   * `Insitu` - In situ parameters
//'   * `SurveillanceTest` - Surveillance test parameters
//'   * `ClinicalTest` - Clinical test parameters
//'   * `OutCol` - Out of unit colonization parameters
//'   * `InCol` - In unit colonization parameters
//'   * `Abx` - Antibiotic parameters
//' @export
// [[Rcpp::export]]
SEXP newModelExport(
    Rcpp::List modelParameters,
    bool verbose = false
) {
    lognormal::LogNormalModel *model = newModel(modelParameters, verbose);
    
    // Use the existing model2R function to wrap the model parameters
    SEXP result = model2R(model);
    
    // Clean up the model object
    delete model;
    
    return result;
}

//' Get Individual HistoryLink Log Likelihoods (Diagnostic Function)
//'
//' This function creates a model and system history, then returns the log likelihood
//' contribution from each individual HistoryLink in the system. Useful for debugging
//' and verifying likelihood calculations.
//'
//' @param modelParameters List of model parameters (same format as runMCMC)
//' @return List containing:
//'   * `linkLogLikelihoods` - vector of log likelihoods for each link
//'   * `overallLogLikelihood` - total log likelihood from model->logLikelihood()
//'   * `numLinks` - number of history links
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
Rcpp::List testHistoryLinkLogLikelihoods(Rcpp::List modelParameters) {
    
    bool verbose = Rcpp::as<bool>(modelParameters.containsElementNamed("verbose") ? 
                                   modelParameters["verbose"] : Rcpp::wrap(true));
    
    if (verbose) Rcpp::Rcout << "Creating System from data..." << std::endl;
    
    // Extract data
    Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(modelParameters["Data"]);
    std::vector<double> times = as<std::vector<double>>(data[2]);
    std::vector<int> patients = as<std::vector<int>>(data[3]);
    
    System *sys = new System(
        as<std::vector<int>>(data[0]),//facility
        as<std::vector<int>>(data[1]),//unit
        times,//"time"
        patients,//"patient"
        as<std::vector<int>>(data[4])//"event type"
    );
    
    if (verbose) Rcpp::Rcout << "Building model..." << std::endl;
    lognormal::LogNormalModel *model = newModel(modelParameters, verbose);
    
    // Set time origin of model
    LogNormalICP *icp = (LogNormalICP *) model->getInColParams();
    icp->setTimeOrigin((sys->endTime()-sys->startTime())/2.0);
    
    if (verbose) Rcpp::Rcout << "Building history structure..." << std::endl;
    SystemHistory *hist = new SystemHistory(sys, model, false);
    
    if (verbose) Rcpp::Rcout << "Calling getHistoryLinkLogLikelihoods..." << std::endl;
    std::vector<double> lls = model->getHistoryLinkLogLikelihoods(hist);
    
    if (verbose) Rcpp::Rcout << "Calculating overall log likelihood..." << std::endl;
    double overall_ll = model->logLikelihood(hist);
    
    delete model;
    delete hist;
    delete sys;
    
    return Rcpp::List::create(
        Rcpp::Named("linkLogLikelihoods") = lls,
        Rcpp::Named("overallLogLikelihood") = overall_ll,
        Rcpp::Named("numLinks") = lls.size()
    );
}

//' Create a new C++ model object wrapped in appropriate reference class
//'
//' Creates and initializes a C++ model object based on the provided parameters,
//' then wraps it in the appropriate R reference class that exposes the model's
//' methods and properties.
//'
//' @param modelParameters List of model parameters (same format as runMCMC)
//' @param verbose Print progress messages (default: false)
//'
//' @return A reference class object wrapping the C++ model:
//'   * CppLogNormalModel - for "LogNormalModel"
//'   * CppLinearAbxModel - for "LinearAbxModel"
//'   * CppLinearAbxModel2 - for "LinearAbxModel2"
//'   * CppMixedModel - for "MixedModel" (note: needs Module exposure)
//'
//' The returned object provides access to model methods and properties including:
//'   * InColParams, OutColParams, InsituParams, etc.
//'   * logLikelihood(), getHistoryLinkLogLikelihoods(), etc.
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
SEXP newCppModelInternal(
    Rcpp::List modelParameters,
    bool verbose = false
) {
    if(verbose)
        Rcpp::message(Rcpp::wrap(string("Creating C++ model object...")));
    
    // Create the model using the existing newModel function
    lognormal::LogNormalModel *model = newModel(modelParameters, verbose);
    
    if(verbose)
        Rcpp::message(Rcpp::wrap(string("Model created successfully")));
    
    // Get the model name to determine which class to wrap in
    std::string modname = modelParameters["modname"];
    
    // Wrap the model in the appropriate reference class
    // The wrap.h and wrap.cpp files define how pointers are wrapped
    // We need to return an external pointer wrapped in the correct reference class
    
    if (modname == "LinearAbxModel")
    {
        lognormal::LinearAbxModel *linear_model = dynamic_cast<lognormal::LinearAbxModel*>(model);
        if (linear_model == nullptr) {
            delete model;
            throw std::runtime_error("Failed to cast to LinearAbxModel");
        }
        // Return wrapped in CppLinearAbxModel reference class
        return wrap(linear_model);
    }
    else if (modname == "LinearAbxModel2")
    {
        lognormal::LinearAbxModel2 *linear_model2 = dynamic_cast<lognormal::LinearAbxModel2*>(model);
        if (linear_model2 == nullptr) {
            delete model;
            throw std::runtime_error("Failed to cast to LinearAbxModel2");
        }
        // Return wrapped in CppLinearAbxModel2 reference class
        return wrap(linear_model2);
    }
    else if (modname == "MixedModel")
    {
        lognormal::MixedModel *mixed_model = dynamic_cast<lognormal::MixedModel*>(model);
        if (mixed_model == nullptr) {
            delete model;
            throw std::runtime_error("Failed to cast to MixedModel");
        }
        // Return wrapped in CppMixedModel reference class
        return wrap(mixed_model);
    }
    else if (modname == "LogNormalModel")
    {
        // For base LogNormalModel, we can return as-is
        return wrap(model);
    }
    else
    {
        delete model;
        throw std::invalid_argument("Invalid model name: " + modname);
    }
}
