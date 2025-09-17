#include "ConjInvGamma.h"

double mcstat2::ConjInvGamma::sample(double ss) {
    return 1.0 / R::rgamma( post_shape, 1.0 / (b + .5 * ss) );
}
