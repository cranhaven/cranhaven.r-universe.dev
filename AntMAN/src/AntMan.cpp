/*
 *  AntMAN Package
 *
 */

#include "Mixtures.h"
#include "Priors.h"
#include "utils.h"



bool is_univariate (Rcpp::List mix_kernel_hyperparams) {
	VERBOSE_ASSERT (mix_kernel_hyperparams.containsElementNamed("type"), "mix_kernel_hyperparams does not contain a type field.");
	std::string mixture_type = mix_kernel_hyperparams["type"];
	return mixture_type.find("uni") != std::string::npos;
}

bool is_multivariate (Rcpp::List mix_kernel_hyperparams) {
	VERBOSE_ASSERT (mix_kernel_hyperparams.containsElementNamed("type"), "mix_kernel_hyperparams does not contain a type field.");
	std::string  mixture_type = mix_kernel_hyperparams["type"];
	return mixture_type.find("multi") != std::string::npos;
}




// ************************************************ AM_mcmc_fit ********************************************************************

Mixture* gen_mix (Rcpp::List mixture_parameters) {

	VERBOSE_ASSERT (mixture_parameters.containsElementNamed("type"), "In gen_mix mixture_parameters does not contain a type field.");

	if (mixture_parameters.containsElementNamed("alpha0") and mixture_parameters.containsElementNamed("beta0")) {
		return new MixtureUnivariatePoisson (mixture_parameters["alpha0"], mixture_parameters["beta0"]);
	}

	if (mixture_parameters.containsElementNamed("m0") and mixture_parameters.containsElementNamed("k0")
			and mixture_parameters.containsElementNamed("nu0")
			and mixture_parameters.containsElementNamed("sig02")) {
		return new MixtureUnivariateNormal (mixture_parameters["m0"], mixture_parameters["k0"], mixture_parameters["nu0"], mixture_parameters["sig02"]);
	}



	if (mixture_parameters.containsElementNamed("mu0") and mixture_parameters.containsElementNamed("ka0")
			and mixture_parameters.containsElementNamed("nu0")
			and mixture_parameters.containsElementNamed("Lam0")) {
		return new MixtureMultivariateNormal (mixture_parameters["mu0"], mixture_parameters["ka0"], mixture_parameters["nu0"], mixture_parameters["Lam0"]);
	}

	if (mixture_parameters.containsElementNamed("a0") and mixture_parameters.containsElementNamed("b0")) {
		return  new MixtureMultivariateBinomial (mixture_parameters["a0"], mixture_parameters["b0"]);
	}

	return NULL;
}

Prior* gen_poisson_gamma (Rcpp::List mix_components_prior, Rcpp::List  mix_weight_prior , Rcpp::RObject y) {

	// Q is Fixed lambda or Poisson or Binomial
	// H is gamma

	poisson_gamma_q_param_t * q;
	poisson_gamma_h_param_t * h;

	VERBOSE_ASSERT (mix_components_prior.containsElementNamed("type"), "In gen_prior mix_components_prior does not contain a type field.");
	VERBOSE_ASSERT (mix_weight_prior.containsElementNamed("type"), "In gen_prior mix_weight_prior does not contain a type field.");

	std::string prior_type   = mix_components_prior["type"];
	std::string weight_type  = mix_weight_prior["type"];

	VERBOSE_ASSERT(prior_type == "AM_mix_components_prior_pois", "Internal_error");
	VERBOSE_ASSERT(weight_type == "AM_mix_weights_prior_gamma", "Internal_error");

	int n = 0;
	if(Rcpp::is<Rcpp::NumericVector>(y) || Rcpp::is<Rcpp::IntegerVector>(y)){
		n = Rcpp::as<arma::vec>(y).size();
	} else  if (Rcpp::is<Rcpp::NumericMatrix>(y) || Rcpp::is<Rcpp::IntegerMatrix>(y)) {
		n = Rcpp::as<arma::mat>(y).n_rows;
	} else {
		VERBOSE_ERROR ("Unsupported type: y variable should be Matrix or Vector.");
	}


	/// ************ Q prior_components_type *******************

	if (mix_components_prior.containsElementNamed("init")
			and mix_components_prior.containsElementNamed("a")
			and mix_components_prior.containsElementNamed("b")) {
		q = new poisson_gamma_q_param_t(
				Rcpp::as<double>(mix_components_prior["init"]),
				Rcpp::as<double>(mix_components_prior["a"]),
				Rcpp::as<double>(mix_components_prior["b"]));
	} else if (mix_components_prior.containsElementNamed("a")
			and mix_components_prior.containsElementNamed("b")) {
		q = new poisson_gamma_q_param_t(
				Rcpp::as<double>(mix_components_prior["a"]),
				Rcpp::as<double>(mix_components_prior["b"]));
	} else if (mix_components_prior.containsElementNamed("Lambda")) {
		q = new poisson_gamma_q_param_t(
				Rcpp::as<double>(mix_components_prior["Lambda"]));
	} else {
		q = new poisson_gamma_q_param_t(n / 10);
	}



	if (mix_weight_prior.containsElementNamed("init")
			and mix_weight_prior.containsElementNamed("a")
			and mix_weight_prior.containsElementNamed("b")) {
		h = new poisson_gamma_h_param_t(
				Rcpp::as<double>(mix_weight_prior["init"]),
				Rcpp::as<double>(mix_weight_prior["a"]),
				Rcpp::as<double>(mix_weight_prior["b"]),
				1);
	} else if (mix_weight_prior.containsElementNamed("a")
			and mix_weight_prior.containsElementNamed("b")) {
		h = new poisson_gamma_h_param_t(
				1,
				Rcpp::as<double>(mix_weight_prior["a"]),
				Rcpp::as<double>(mix_weight_prior["b"]),
				1);
	} else if (mix_weight_prior.containsElementNamed("gamma")) {
		h = new poisson_gamma_h_param_t(
				Rcpp::as<double>(mix_weight_prior["gamma"]));
	} else {
		h = new poisson_gamma_h_param_t(1);
	}



	VERBOSE_ASSERT(q, "Prior error : q is not set.");
	VERBOSE_ASSERT(h, "Prior error : h is not set.");



	Prior* 	prior = new  PriorPoisson (*h,*q);


	return prior;
}


negbin_component gen_negbin_comp (Rcpp::List mix_components_prior, std::string suffix, double default_init ) {

	negbin_component C;

	std::string IC = "init"  + suffix;
	std::string MC = "fixed" + suffix;
	std::string aC = "a"     + suffix;
	std::string bC = "b"     + suffix;

	if (    mix_components_prior.containsElementNamed(IC.c_str())
			and mix_components_prior.containsElementNamed(aC.c_str())
			and mix_components_prior.containsElementNamed(bC.c_str())) {
		C.value = Rcpp::as<double>(mix_components_prior[IC]);
		C.a = Rcpp::as<double>(mix_components_prior[aC]);
		C.b = Rcpp::as<double>(mix_components_prior[bC]);
		C.fixed = false;
	} else if (!mix_components_prior.containsElementNamed(IC.c_str())
			and mix_components_prior.containsElementNamed(aC.c_str())
			and mix_components_prior.containsElementNamed(bC.c_str())) {
		C.value = default_init;
		C.a     = Rcpp::as<double>(mix_components_prior[aC]);
		C.b     = Rcpp::as<double>(mix_components_prior[bC]);
		C.fixed = false;

	} else if (mix_components_prior.containsElementNamed(MC.c_str())) {
		C.value = Rcpp::as<double>(mix_components_prior[MC]);
		C.fixed = true;
	} else {
		VERBOSE_ERROR("Missing value with " << suffix);
	}
	return C;
}

Prior* gen_negbin_gamma (Rcpp::List mix_components_prior, Rcpp::List  mix_weight_prior , Rcpp::RObject y) {

	// Q is Negative Binomial
	// H is gamma

	negative_binomial_gamma_q_param_t * q;
	negative_binomial_gamma_h_param_t * h;

	VERBOSE_ASSERT (mix_components_prior.containsElementNamed("type"), "In gen_prior mix_components_prior does not contain a type field.");
	VERBOSE_ASSERT (mix_weight_prior.containsElementNamed("type"), "In gen_prior mix_weight_prior does not contain a type field.");

	std::string prior_type   = mix_components_prior["type"];
	std::string weight_type  = mix_weight_prior["type"];

	VERBOSE_ASSERT(prior_type == "AM_mix_components_prior_negbin", "Internal_error");
	VERBOSE_ASSERT(weight_type == "AM_mix_weights_prior_gamma", "Internal_error");

	if(Rcpp::is<Rcpp::NumericVector>(y) || Rcpp::is<Rcpp::IntegerVector>(y)){
		const unsigned int n = Rcpp::as<arma::vec>(y).size();
		VERBOSE_INFO ("Input size is " << n);
	} else  if (Rcpp::is<Rcpp::NumericMatrix>(y) || Rcpp::is<Rcpp::IntegerMatrix>(y)) {
		const unsigned int n = Rcpp::as<arma::mat>(y).n_rows;
		VERBOSE_INFO ("Input size is " << n);
	} else {
		VERBOSE_ERROR ("Unsupported type: y variable should be Matrix or Vector.");
	}


	/// ************ Q prior_components_type *******************

	negbin_component R = gen_negbin_comp (mix_components_prior, "_R", 1);
	negbin_component P = gen_negbin_comp (mix_components_prior, "_P", 0.5); // TODO : DEFAULT AVERAGE (a, b)

	q = new negative_binomial_gamma_q_param_t(R,P);



	if (mix_weight_prior.containsElementNamed("init")
			and mix_weight_prior.containsElementNamed("a")
			and mix_weight_prior.containsElementNamed("b")) {
		h = new negative_binomial_gamma_h_param_t(
				Rcpp::as<double>(mix_weight_prior["init"]),
				Rcpp::as<double>(mix_weight_prior["a"]),
				Rcpp::as<double>(mix_weight_prior["b"]),
				1);
	} else if (mix_weight_prior.containsElementNamed("a")
			and mix_weight_prior.containsElementNamed("b")) {
		h = new negative_binomial_gamma_h_param_t(
				1,
				Rcpp::as<double>(mix_weight_prior["a"]),
				Rcpp::as<double>(mix_weight_prior["b"]),
				1);
	} else if (mix_weight_prior.containsElementNamed("gamma")) {
		h = new negative_binomial_gamma_h_param_t(
				Rcpp::as<double>(mix_weight_prior["gamma"]));
	} else {
		h = new negative_binomial_gamma_h_param_t(1);
	}



	VERBOSE_ASSERT(q, "Prior error : q is not set.");
	VERBOSE_ASSERT(h, "Prior error : h is not set.");
	VERBOSE_INFO(q);


	Prior* 	prior = new  PriorNegativeBinomial (*h,*q);


	return prior;
}


Prior* gen_dirac_gamma (Rcpp::List mix_components_prior, Rcpp::List  mix_weight_prior , Rcpp::RObject y) {

	// Q is Negative Binomial
	// H is gamma

	dirac_gamma_q_param_t * q;
	dirac_gamma_h_param_t * h;

	VERBOSE_ASSERT (mix_components_prior.containsElementNamed("type"), "In gen_prior mix_components_prior does not contain a type field.");
	VERBOSE_ASSERT (mix_weight_prior.containsElementNamed("type"), "In gen_prior mix_weight_prior does not contain a type field.");

	std::string prior_type   = mix_components_prior["type"];
	std::string weight_type  = mix_weight_prior["type"];

	VERBOSE_ASSERT(prior_type == "AM_mix_components_prior_dirac", "Internal_error");
	VERBOSE_ASSERT(weight_type == "AM_mix_weights_prior_gamma", "Internal_error");

	if(Rcpp::is<Rcpp::NumericVector>(y) || Rcpp::is<Rcpp::IntegerVector>(y)){
		const unsigned int n = Rcpp::as<arma::vec>(y).size();
		VERBOSE_INFO ("Input size is " << n);
	} else  if (Rcpp::is<Rcpp::NumericMatrix>(y) || Rcpp::is<Rcpp::IntegerMatrix>(y)) {
		const unsigned int n = Rcpp::as<arma::mat>(y).n_rows;
		VERBOSE_INFO ("Input size is " << n);
	} else {
		VERBOSE_ERROR ("Unsupported type: y variable should be Matrix or Vector.");
	}



	/// ************ Q prior_components_type *******************

	if (mix_components_prior.containsElementNamed("Mstar")) {
		q = new dirac_gamma_q_param_t(
				Rcpp::as<double>(mix_components_prior["Mstar"]));
	} else  {
		VERBOSE_ERROR("Mstar argument not found.");
	}



	if (mix_weight_prior.containsElementNamed("init")
			and mix_weight_prior.containsElementNamed("a")
			and mix_weight_prior.containsElementNamed("b")) {
		h = new dirac_gamma_h_param_t(
				Rcpp::as<double>(mix_weight_prior["init"]),
				Rcpp::as<double>(mix_weight_prior["a"]),
				Rcpp::as<double>(mix_weight_prior["b"]),
				1);
	} else if (mix_weight_prior.containsElementNamed("a")
			and mix_weight_prior.containsElementNamed("b")) {
		h = new dirac_gamma_h_param_t(
				1,
				Rcpp::as<double>(mix_weight_prior["a"]),
				Rcpp::as<double>(mix_weight_prior["b"]),
				1);
	} else if (mix_weight_prior.containsElementNamed("gamma")) {
		h = new dirac_gamma_h_param_t(
				Rcpp::as<double>(mix_weight_prior["gamma"]));
	} else {
		h = new dirac_gamma_h_param_t(1);
	}



	VERBOSE_ASSERT(q, "Prior error : q is not set.");
	VERBOSE_ASSERT(h, "Prior error : h is not set.");



	Prior* 	prior = new  PriorDirac (*h,*q);


	return prior;
}




Prior* gen_prior (Rcpp::List mix_components_prior, Rcpp::List  mix_weight_prior , Rcpp::RObject y) {

	VERBOSE_ASSERT (mix_components_prior.containsElementNamed("type"), "In gen_prior mix_components_prior does not contain a type field.");
	VERBOSE_ASSERT (mix_weight_prior.containsElementNamed("type"), "In gen_prior mix_weight_prior does not contain a type field.");

	std::string prior_type   = mix_components_prior["type"];
	std::string weight_type  = mix_weight_prior["type"];

	VERBOSE_INFO("prior_type = " << prior_type);
	VERBOSE_INFO("weight_type = " << weight_type);

	Prior* prior;

	if ((prior_type == "AM_mix_components_prior_pois") && (weight_type == "AM_mix_weights_prior_gamma")) {
		prior = gen_poisson_gamma(mix_components_prior, mix_weight_prior, y);
	} else if ((prior_type == "AM_mix_components_prior_negbin") && (weight_type == "AM_mix_weights_prior_gamma")) {
		prior = gen_negbin_gamma(mix_components_prior, mix_weight_prior, y);
	} else if ((prior_type == "AM_mix_components_prior_dirac") && (weight_type == "AM_mix_weights_prior_gamma")) {
		prior = gen_dirac_gamma(mix_components_prior, mix_weight_prior, y);
	} else{
		VERBOSE_ERROR("Unknown prior: Please could you make sure you properly used the AM_mix_components_prior and AM_mix_weights commands.");
	}

	return prior;

}



Rcpp::List getList (AntMANLogger& logger) {

	VERBOSE_INFO("Run getList");

	std::vector<std::string> names = logger.getNames();

	Rcpp::List my_list(names.size());
	my_list.attr("names") = names;

	int cnt = 0;
	for (auto name : names) {
		if (logger.haslog(name)) {
			switch (logger.getlogtype(name)) {
			case AntMANType::AM_INT : my_list[cnt++] = logger.getlog<int>(name) ; break;
			case AntMANType::AM_UINT : my_list[cnt++] = logger.getlog<unsigned int>(name) ; break;
			case AntMANType::AM_DOUBLE : my_list[cnt++] = logger.getlog<double>(name) ; break;
			case AntMANType::AM_ARMA_VEC : my_list[cnt++] = logger.getlog<arma::vec>(name) ; break;
			case AntMANType::AM_ARMA_MAT : my_list[cnt++] = logger.getlog<arma::mat>(name) ; break;
			case AntMANType::AM_ARMA_CUBE : my_list[cnt++] = logger.getlog<arma::cube>(name) ; break;
			case AntMANType::AM_ARMA_IVEC : my_list[cnt++] = logger.getlog<arma::ivec>(name) ; break;
			case AntMANType::AM_ARMA_IMAT : my_list[cnt++] = logger.getlog<arma::imat>(name) ; break;
			case AntMANType::AM_ARMA_ICUBE : my_list[cnt++] = logger.getlog<arma::icube>(name) ; break;
			case AntMANType::AM_VEC_DOUBLE : my_list[cnt++] = logger.getlog<std::vector<double>>(name) ; break;
			default :
				VERBOSE_ERROR("Unsupported type!");
			}



		}
	}

	VERBOSE_INFO("Finish getList");

	return my_list;



}


// INTERNAL FUNCTION - No DOCUMENTATION, Please only use AM_ refixed function.
// [[Rcpp::export]]
Rcpp::List IAM_mcmc_fit (
		Rcpp::RObject                        y                      , /* Not optional */
		Rcpp::List                           mix_kernel_hyperparams , /* Not optional */
		Rcpp::IntegerVector                  initial_clustering     , //  = Rcpp::IntegerVector::create() /* default will be 1for1 */
		bool                                 fixed_clustering       ,
		Rcpp::List                           mix_components_prior   , //  = Rcpp::List::create()          /* default will be Poisson ()  */
		Rcpp::List                           mix_weight_prior       , //  = Rcpp::List::create()          /* default will be Gamma ()    */
		Rcpp::List                           mcmc_parameters          //  = Rcpp::List::create()          /* (default niter=20000, ….)   */
) {
	// ################## mcmc_parameters ##################
	std::vector<std::string> required_items = {"parallel", "niter", "burnin", "thin", "verbose", "output","output_dir"};
	for (std::string item : required_items) {
		VERBOSE_ASSERT(mcmc_parameters.containsElementNamed(item.c_str()), "mcmc_parameters does not contains : " << item );
	}

	VERBOSE_LEVEL(mcmc_parameters["verbose"]) ;

	VERBOSE_DEBUG ("Debug mode is on.");

	// ################## output arguments ##################

	VERBOSE_INFO ("Start mcmc_fit");
	VERBOSE_INFO ("- y = " << y);
	VERBOSE_INFO ("- mix_kernel_hyperparams = " << mix_kernel_hyperparams.size());
	VERBOSE_INFO ("- initial_clustering = " << initial_clustering.size());
	VERBOSE_INFO ("- mix_components_prior = " << mix_components_prior.size());
	VERBOSE_INFO ("- mix_weight_prior = " << mix_weight_prior.size());
	VERBOSE_INFO ("- size(mcmc_parameters) = " << mcmc_parameters.size());

	// ################## y                ##################


	VERBOSE_INFO ("- Rcpp::is<Rcpp::NumericVector>(y) = " << Rcpp::is<Rcpp::NumericVector>(y));
	VERBOSE_INFO ("- Rcpp::is<Rcpp::IntegerVector>(y) = " << Rcpp::is<Rcpp::IntegerVector>(y));
	VERBOSE_INFO ("- Rcpp::is<Rcpp::NumericMatrix>(y) = " << Rcpp::is<Rcpp::NumericMatrix>(y));
	VERBOSE_INFO ("- Rcpp::is<Rcpp::IntegerMatrix>(y) = " << Rcpp::is<Rcpp::IntegerMatrix>(y));






	// ################## initial_clustering  ##################

	// ################## mix_kernel_hyperparams  ##################

	Mixture*              mixture  = gen_mix(mix_kernel_hyperparams);
	VERBOSE_ASSERT(mixture, "gen_mix returned NULL");

	// ################## mix_components_prior and mix_weight_prior  ##################



	Prior*                prior    = gen_prior(mix_components_prior,mix_weight_prior, y);
	VERBOSE_ASSERT(prior, "gen_prior returned NULL");


	std::vector<std::string> output_list = Rcpp::as<std::vector<std::string>>(mcmc_parameters["output"]);


	unsigned long niter  = mcmc_parameters["niter"];
	unsigned long burnin = mcmc_parameters["burnin"];
	unsigned long thin   = mcmc_parameters["thin"];
	bool parallel        = mcmc_parameters["parallel"];
	unsigned long stored = (niter - burnin)  / thin;

	VERBOSE_ASSERT(niter > burnin, "Please keep niter > burnin.");
	VERBOSE_ASSERT(niter > thin, "Please keep niter > thin.");
	VERBOSE_ASSERT(thin  > 0, "Please keep thin > 0.");

	VERBOSE_INFO(" - Stored observations " << stored);
	//GibbsResultRCpp res (  (niter - burnin)  / thin ,output_codes);

	AntMANLogger res(output_list, (niter - burnin)  / thin);
	if (Rcpp::is<Rcpp::NumericMatrix>(y)) {
			VERBOSE_ASSERT (is_multivariate(mix_kernel_hyperparams), "y argument is a Matrix while the technique is not MultiVariate.") ;

			VERBOSE_INFO ("-  Rcpp::as<arma::mat>(y).n_rows = " << Rcpp::as<arma::mat>(y).n_rows);
			MultivariateMixture* identifiedMixture = dynamic_cast<MultivariateMixture*>(mixture);
			VERBOSE_ASSERT (identifiedMixture, "The input data type does not match the selected mixture. Need MultivariateMixture.");
			identifiedMixture->fit(Rcpp::as<arma::mat>(y) , initial_clustering, fixed_clustering,
					prior ,
					niter ,burnin ,thin , parallel, &res );
			VERBOSE_INFO("End of Gibbs");
			return getList(res);

		}  else if (Rcpp::is<Rcpp::IntegerMatrix>(y)) {
			VERBOSE_ASSERT (is_multivariate(mix_kernel_hyperparams), "y argument is a Matrix while the technique is not MultiVariate.") ;

			VERBOSE_INFO ("-  Rcpp::as<arma::imat>(y).n_rows = " << Rcpp::as<arma::imat>(y).n_rows);
			MultivariateIntegerMixture* identifiedMixture = dynamic_cast<MultivariateIntegerMixture*>(mixture);
			VERBOSE_ASSERT (identifiedMixture, "The input data type does not match the selected mixture. Need MultivariateIntegerMixture.");
			identifiedMixture->fit(Rcpp::as<arma::imat>(y) , initial_clustering, fixed_clustering,
					prior ,
					niter ,burnin ,thin , parallel, &res );
			VERBOSE_INFO("End of Gibbs");
			return getList(res);

		}  else if(Rcpp::is<Rcpp::NumericVector>(y) ){
			VERBOSE_INFO ("-  Rcpp::as<arma::vec>(y).n_rows = " << Rcpp::as<arma::vec>(y).n_rows);
			VERBOSE_ASSERT (is_univariate(mix_kernel_hyperparams), "y argument is a Vector while the technique is not Univariate.") ;
			UnivariateMixture* identifiedMixture = dynamic_cast<UnivariateMixture*>(mixture);
			VERBOSE_ASSERT (identifiedMixture, "The input data type does not match the selected mixture. Need UnivariateMixture.");
			identifiedMixture->fit(Rcpp::as<arma::vec>(y) , initial_clustering, fixed_clustering, prior ,
					niter ,burnin ,thin,   parallel, &res );

			VERBOSE_INFO("End of Gibbs");
			return getList(res);
		} else if(Rcpp::is<Rcpp::IntegerVector>(y)){
			VERBOSE_INFO ("-  Rcpp::as<arma::ivec>(y).n_rows = " << Rcpp::as<arma::ivec>(y).n_rows);
			VERBOSE_ASSERT (is_univariate(mix_kernel_hyperparams), "y argument is a Vector while the technique is not Univariate.") ;
			UnivariateIntegerMixture* identifiedMixture = dynamic_cast<UnivariateIntegerMixture*>(mixture);
			VERBOSE_ASSERT (identifiedMixture, "The input data type does not match the selected mixture. Need UnivariateIntegerMixture.");
			identifiedMixture->fit(Rcpp::as<arma::ivec>(y) , initial_clustering, fixed_clustering, prior ,
					niter ,burnin ,thin,   parallel, &res );

			VERBOSE_INFO("End of Gibbs");
			return getList(res);
		} else {
		VERBOSE_ERROR("The parameter y must be a Matrix or a Vector and appriopriately Integer or Numeric.");
	}
	return Rcpp::List::create(Rcpp::Named("Error") = "Unexpected error."  );

}
