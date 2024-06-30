#ifndef SCALE_VARIABLES_H
#define SCALE_VARIABLES_H
#include <Rcpp.h>
#include "parameter_table.h"

void scale_latent_variances(parameter_table& pt);
void scale_loadings(parameter_table& pt);

#endif
