writeComment <- function(x) {
	n <- pmax(1,1+max(nchar(x))-nchar(x))
	return(sprintf("/* %s %*s */",x,n," "))
}

#' CRNN creates C code for a chemical reaction neural network
#'
#' This function creates a very general ODE (c source code), that can
#' be compiled and simulated using the UQSA package.
#'
#' Example: A + B <=> C
#' numReactions: n <- 1
#' initialValues: x <- c(A=2,B=3,C=0)
#' funcValues: f <- c("A+B","log(A)")
#'
#' The above definition would create a CRNN inspired ODE, where A+B
#' and log(A) are treated as observable (measurable) values
#' (functions of the state variables).
#'
#' Note: In addition to the state variables, the function values can
#' also reference the log-parameters of the model as `l[i]`, where `i` is
#' a 0-based offset to the reaction `i`; the backward rate, is stored at
#' position `l[i+numRct]`.
#'
#' @param numReactions the number of reversible mass action law
#'     reactions
#' @param initialValues named vector of initial values, names will be
#'     used as the names of the reacting compounds.
#' @param funcValues named character vector, can be any valid C
#'     expression (one line) of the available state variables (the
#'     names can be used literally).
#' @param model.name the prefix of all created model functions: CRNN_vf, CRNN_jac, ...
#' @export
#' @return a character vector suitable for writing to a file (.c)
#' @examples
#' C <- CRNN(4,c(A=1,B=2,C=3),c(out="A+B+C"),model.name="testmodel")
#' cat(head(C),sep='\n')
CRNN <- function(numReactions,initialValues,funcValues, model.name="CRNN"){
	headers <- c('stdlib.h','math.h','string.h','gsl/gsl_errno.h','gsl/gsl_odeiv2.h','gsl/gsl_math.h')
	C <- sprintf("#include <%s>",headers) # will be return value
	C <- c(C,"",
		writeComment(
			c("This is a general mass action law model,",
			"with a fixed number of reactions, where",
			"a reaction:",
			"    n[0] X[0] + n[1] X[1] -> n[2] X[2],","has the stoichiometry",
			"    nu = {-n[0]; -n[1]; +n[2]},",
			"and reaction flux:",
			"    flux = exp(log[k] + n[0]*log(X[0]) + n[1]*log(X[1])),",
			"which is the same as:",
			"    flux = exp(l - nu[0]*log(X[0]) - nu[1]*log(X[1])),",
			"but expressed in terms of stoichiometry.","",
			"Many quantities are expressed in logarithmic space, e.g.:",
			"    l = log(k).",
			"Specifically, reaction fluxes have to be sums of logarithms.",
			"For this reason, a reversible reaction has to be split up","into two reactions:",
			"    log(kf*A*B) = log(kf) + log(A) + log(B), whereas",
			"log(kf*A*B-kr*C) does not split up so neatly.",
			"The stoichiometry nu is a matrix in column major order, i.e.",
			"nu[a,b] = nu[a+A*b], where a and are offsets 0,...,A-1;",
			"a enumerates the state variables. and b the reactions.",
			"",
			"The stoichiometry of reverse reations nu_b = -nu is implied.",
			"We re-use nu for backward reaction fluxes, without copying it.",
			"",
			"Reactions can involve a modifier: a reactant that",
			"isn't consumed by the reaction (enzymes). These",
			"wouldn't normally appear in the stoichiometry",
			"because their number is conserved. But, they do affect",
			"the reaction flux.",
			" For these reasons, there is a second matrix of modifiers:",
			"    m[i+numStV*j] = {0,1,2,...};",
			"The values of m are only used in flux calculations.",
			"")
		)
	)
	C <- c(C,
		"inline static double LOG(double y){",
		"\treturn y>0?log(y):GSL_NEGINF;",
		"}",
		"",
		paste0(c("enum stateVars {",sprintf("_%s, ",names(initialValues)),"numStV};"),collapse=""),
		paste0(c("enum outputFun {",sprintf("_%s, ",names(funcValues)),"numFun};"),collapse="")
	)
	C <- c(C,
		sprintf("/* const int numStV = %i; */ /* number of state variables, see above */",length(initialValues)),
		sprintf("const int numPar = %i; /* number of reaction rate coefficients */",numReactions*2),
		sprintf("const int numRct = %i; /* number of reactions */",numReactions)
	)
	C <- c(C,
		"struct par {",
		"\tdouble *l;  /* log(k), rate coefficients */",
		sprintf("\tdouble *nu; /* stoichiometry (%i,%i*2)*/",length(initialValues),numReactions),
		"\tdouble *m;  /* modifiers (enzymes); not consumed or produced */",
		"};",
		""
	)
	C <- c(C,
		sprintf("int %s_flux(double t, double *y, double *fwdFlux, double *bwdFlux, struct par *p){", model.name),
		"\tdouble *nu=p->nu;",
		"\tdouble *l=p->l;",
		"\tdouble *m=p->m;",
		"\tint i,j;",
		writeComment("\tforward rections"),
		sprintf("\tfor (i = 0; i < numRct; i++) {"),
		"\t\tfwdFlux[i] = l[i];",
		sprintf("\t\tfor (j = 0; j < numStV; j++) {"),
		sprintf("\t\t\tif (nu[j+numStV*i]<0) {"),
		sprintf("\t\t\t\tfwdFlux[i] -= nu[j+numStV*i]*LOG(y[j]); /* a reactant */"),
		sprintf("\t\t\t}"),
		sprintf("\t\t\tif (m[j+numStV*i]>0){"),
		sprintf("\t\t\t\tfwdFlux[i] +=  m[j+numStV*i]*LOG(y[j]); /* a modifier */"),
		sprintf("\t\t\t}"),
		sprintf("\t\t}"),
		"\t\tfwdFlux[i] = exp(fwdFlux[i]);",
		sprintf("\t}"),
		writeComment("\tbackward rections"),
		sprintf("\tfor (i = 0; i < numRct; i++) {"),
		"\t\tbwdFlux[i] = l[i+numRct];",
		sprintf("\t\tfor (j = 0; j < numStV; j++) {"),
		sprintf("\t\t\tif (nu[j+numStV*i]>0) {"),
		sprintf("\t\t\t\tbwdFlux[i] += nu[j+numStV*i]*LOG(y[j]); /* a reactant */"),
		sprintf("\t\t\t}"),
		sprintf("\t\t\tif (m[j+numStV*i]>0){"),
		sprintf("\t\t\t\tbwdFlux[i] +=  m[j+numStV*i]*LOG(y[j]); /* a modifier */"),
		sprintf("\t\t\t}"),
		sprintf("\t\t}"),
		"\t\tbwdFlux[i] = exp(bwdFlux[i]);",
		sprintf("\t}"),
		"\treturn GSL_SUCCESS;",
		"}"
	)
	C <- c(C,
		sprintf("int %s_vf(double t, double *y, double *f, void *par){", model.name),
		"\tif (!y || !f) return(numStV);",
		"\tint i,j;",
		"\tstruct par *p = par;",
		"\tdouble *nu=p->nu;",
		"\tdouble fwdFlux[numRct];",
		"\tdouble bwdFlux[numRct];",
		sprintf("\t%s_flux(t,y,fwdFlux,bwdFlux,p);", model.name),
		"\tfor (i = 0; i < numStV; i++){",
		"\t\tf[i] = 0.0;",
		"/*\t\tif (y[i]<1e-10) fprintf(stderr,\"[%s] warning: y[%i] is too low: %g\\n\",__func__,i,y[i]);*/",
		"\t\tfor (j = 0; j < numRct; j++) {",
		writeComment("\t\t\tbackward reactions have the inverse stoichiometry: -nu"),
		"\t\t\tf[i] += nu[i+numStV*j]*(fwdFlux[j] - bwdFlux[j]);",
		"\t\t}",
		"\t}",
		"\treturn GSL_SUCCESS;",
		"}"
	)
	C <- c(C,"",
		sprintf("int %s_func(double t, double *y, double *func, void *par){", model.name),
		"\tif (!y || !func) return numFun;",
		"\tstruct par *p=par;",
		sprintf("\tdouble %s = y[_%s];",names(initialValues),names(initialValues)),
		sprintf("\tfunc[_%s] = %s;",names(funcValues),funcValues),
		"\treturn GSL_SUCCESS;",
		"}")
	C <- c(C,"",
		sprintf("int %s_jac(double t, double *y, double *jac, double dfdt[], void *par){", model.name),
		"\tif (!y || !jac) return numStV*numStV;",
		"\tint i,j,k;",
		"\tstruct par *p=par;",
		"\tdouble *l=p->l;",
		"\tdouble *nu=p->nu;",
		"\tdouble *m=p->m;",
		"\tdouble fwdFlux[numRct];",
		"\tdouble bwdFlux[numRct];",
		sprintf("\t%s_flux(t,y,fwdFlux,bwdFlux,p);", model.name),
		"\tfor (i = 0; i < numStV; i++){",
		"\t\tfor (j = 0; j < numStV; j++) {",
		"\t\t\tjac[i*numStV+j]=0;",
		"\t\t\tfor (k = 0; k < numRct; k++) {",
		"\t\t\t\tif (y[j]>0) {",
		"\t\t\t\t\tjac[i*numStV+j] -= nu[i+numStV*k]*(fwdFlux[k]*(nu[j+numStV*k]<0)+bwdFlux[k]*(nu[j+numStV*k]>0))*nu[j+numStV*k]/y[j];",
		"\t\t\t\t}",
		"\t\t\t}",
		"\t\t}",
		"\t}",
		"\treturn GSL_SUCCESS;",
		"}"
	)
	## initial values:
	C <- c(C,"",
		writeComment(c("These are the default initial conditions for y.","They may depend on the parameters,","and time of initialization t.")),
		sprintf("int %s_init(double t, double *y, void *par){", model.name),
		"\tif(!y || !par) return(numStV);",
		"\tstruct par *p=par;",
		sprintf("\ty[_%s] = %g;",names(initialValues),initialValues),
		"\treturn GSL_SUCCESS;",
		"}"
	)
	## default parameter values:
	C <- c(C,"",
		writeComment(c("These are default values for the parameters.","They may depend on the initialization time t.")),
		sprintf("int %s_default(double t, void *par){", model.name),
		"\tif(!par) return numRct;",
		"\tdouble *p=par;",
		"/*\tIt is a bit unclear what to do here for CRNNs, as they have more complex parameters. */",
		"/*\tmemset(p,0,lpar*sizeof(double)); */",
		"\treturn GSL_SUCCESS;",
		"}"
	)
}
