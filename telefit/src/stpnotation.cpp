#include "stpnotation.h"


List CompositionSamples::toSummarizedList() {
	
	List alpha_knots_sum, alpha_sum, eof_alpha_knots_sum;
	
	if(!localOnly) {
		
		// post process teleconnection knots
		
		alpha_knots_sum = List::create( _["est"] = mean(alpha_knots),
									    _["sd"] = stddev(alpha_knots, 1),
									    _["nSamples"] = alpha_knots.n_rows );
		
		// post process eof-mapped teleconnection field
		
		eof_alpha_knots_sum = List::create( _["est"] = eof_alpha_knots.mean(),
										    _["sd"] = eof_alpha_knots.stddev(),
										    _["nSamples"] = eof_alpha_knots.count(),
										    _["negProb"] = eof_alpha_knots_negprob.mean(),
										    _["posProb"] = eof_alpha_knots_posprob.mean() );
		
		// post process full teleconnection field
		
		if(return_full_alpha) {
			alpha_sum = List::create( _["est"] = alpha.mean(),
									  _["sd"] = alpha.stddev(),
									  _["nSamples"] = alpha.count() );
		}
	}
	
	
	// package forecast results
	
	List forecast_sum;
 
	if(localOnly) {
		forecast_sum = List::create( _["forecast"] = forecast,
									 _["cat_probs"] = cat_probs );
	} else {
		forecast_sum = List::create( _["forecast"] = forecast,
									 _["cat_probs"] = cat_probs,
									 _["local"] = local,
									 _["remote"] = remote );
	}
	
	
	// return results
	List ret;
	
	if( (!localOnly) & return_full_alpha & return_forecast ) {
		ret = List::create(
			_["alpha_knots"] = alpha_knots_sum,
			_["eof_alpha_knots"] = eof_alpha_knots_sum,
			_["alpha"] = alpha_sum,
			_["forecast"] = forecast_sum
		);
	} else if( (!localOnly) & return_full_alpha ) {
		ret = List::create(
			_["alpha_knots"] = alpha_knots_sum,
			_["eof_alpha_knots"] = eof_alpha_knots_sum,
			_["alpha"] = alpha_sum
		);
	} else if(return_forecast & (!localOnly)) {
		ret = List::create(
			_["alpha_knots"] = alpha_knots_sum,
			_["eof_alpha_knots"] = eof_alpha_knots_sum,
			_["forecast"] = forecast_sum
		);
	} else if(return_forecast & localOnly) {
		ret = List::create(
							_["forecast"] = forecast_sum
							);
	}
	
	return ret;
}
