#ifndef _CONJINVGAMMA_H
#define _CONJINVGAMMA_H

#include <RcppArmadillo.h>
#include "GibbsSampler.h"

namespace mcstat2 {

	//
	// conjugate inverse gamma sampler
	//

	class ConjInvGamma : public Sampler {

    protected:
        double post_shape, b;
        
	public:
		// sample a new parameter value given residual sum of squares
		double sample(double ss);

		ConjInvGamma(double t_a, double t_b, double t_n) {
			type = VECTOR;
            post_shape = t_a + t_n / 2;
            b =  t_b;
		}
        
	};

}

#endif
