#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_randist.h>
#include <math.h>

/* This is an attempt to make this package work on windows */
#ifdef _WIN32
/* begin: Prevent windows.h from including things not needed here, */
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
/* end */
#include <windows.h>
typedef HMODULE shared_library;
#define DLSYM(lib, fn) GetProcAddress((lib), (fn))
#else
#include <dlfcn.h>
#define DLSYM(lib, fn) dlsym((lib), (fn))
typedef void* shared_library;
#endif
/* the rest of the code must make considerations as well */

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <time.h>
#include <stdint.h>

typedef SEXP Rdata;
enum status {success, timeout, nstep_max};


/* The standard 64-bit FNV-1a hash function */
static uint64_t FNV1a(const char* str) {
	const uint64_t FNV_offset_basis =  0xcbf29ce484222325ULL;
	const uint64_t FNV_prime = 0x100000001b3ULL;
	uint64_t hash = FNV_offset_basis;
	for (const char* ptr = str; *ptr != '\0'; ++ptr) {
		hash ^= (uint64_t)(unsigned char)(*ptr);
		hash *= FNV_prime;
	}
	return hash;
}


int (*model_effects)(double t, int *x, double *c, double* a,  int j);
int (*model_propensities)(double t, int *x, double *c, double *a);
int (*model_reaction_coefficients)(double *c);
int (*model_initial_counts)(int *x, double *c);
int (*model_func)(double t, int *x, double *c, double *f);
int (*model_particle_count)(double t, double *molarity, int *x);
int (*model_stochastic_parameters)(double t, double *par);
/* The union below exists because dlsym always returns a (void*)
 * pointer.  With this unit type, we can convert the void-pointer to
 * any of the functions above using one of the other union members.
 * This is only necessary to suppress a pedantic warning.  The ptr
 * component accepts a void pointer from dlsym.  and the above
 * function pointers are filled from the rest of the union members.
 */
union symbol {
	void *ptr;                                         /* <- input   */
	int (*model_effects)(double t, int *x, double *c, double *a, int j);     /* outputs -> */
	int (*model_propensities)(double t, int *x, double *c, double*a);
	int (*model_reaction_coefficients)(double *c);
	int (*model_initial_counts)(int *x, double *c);
	int (*model_func)(double t, int *x, double *c, double *f);
	int (*model_particle_count)(double t, double *molarity, int *x);
	int (*model_stochastic_parameters)(double t, double *par);
};

int load_model(const char *model_so){
	static shared_library lib = NULL; // this happens once
	static uint64_t old_path_hash = 0;
	uint64_t current_path_hash = FNV1a(model_so);
	const char *err_msg = NULL;
	union symbol conversion; /* pointer conversion */
	if (!lib || old_path_hash != current_path_hash){ // a new library was specified
#ifdef _WIN32
		if (lib) FreeLibrary(lib); // discard old library
		lib = LoadLibrary(model_so);
#else
		/* do normal UNIX/POSIX things */
		if (lib) dlclose(lib);     // discard old library
		lib = dlopen(model_so,RTLD_LAZY);
		err_msg = dlerror();
#endif
		old_path_hash = current_path_hash;
	}
	if (lib){
		if ((conversion.ptr=DLSYM(lib,"model_effects"))==NULL){
			REprintf("[%s] loading model_effects has failed.\n",__func__);
		} else {
			model_effects = conversion.model_effects;
		}
		if ((conversion.ptr=DLSYM(lib,"model_propensities"))==NULL){
			REprintf("[%s] loading model_propensities has failed.\n",__func__);
		} else {
			model_propensities = conversion.model_propensities;
		}
		if ((conversion.ptr=DLSYM(lib,"model_reaction_coefficients"))==NULL){
			REprintf("[%s] loading model_reaction_coefficients has failed.\n",__func__);
		} else {
			model_reaction_coefficients = conversion.model_reaction_coefficients;
		}
		if ((conversion.ptr=DLSYM(lib,"model_initial_counts"))==NULL){
			REprintf("[%s] loading model_initial_counts has failed.\n",__func__);
		} else {
			model_initial_counts = conversion.model_initial_counts;
		}
		if ((conversion.ptr=DLSYM(lib,"model_func"))==NULL){
			REprintf("[%s] loading model_func has failed.\n",__func__);
		} else {
			model_func = conversion.model_func;
		}
		if ((conversion.ptr=DLSYM(lib,"model_particle_count"))==NULL){
			REprintf("[%s] loading model_particle_count has failed.\n",__func__);
		} else {
			model_particle_count = conversion.model_particle_count;
		}
		if ((conversion.ptr = DLSYM(lib,"model_stochastic_parameters"))==NULL){
			REprintf("[%s] loading model_stochastic_parameters has failed.\n",__func__);
		} else {
			model_stochastic_parameters = conversion.model_stochastic_parameters;
		}
	} else {
		error("[%s] %s.\n",__func__,err_msg);
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}

int pick_reaction(gsl_vector *a, double r_sum_a){
	int j;
	size_t m=a->size;
	double psum = 0.0;
	//gsl_sort_vector(a);
	for (j=0; j<m; j++){
		psum += gsl_vector_get(a,j);
		if (psum > r_sum_a) break;
	}
	return j;
}

void print_counts(double t, gsl_vector_int *x){
	Rprintf("%10f ",t);
	size_t n = x->size;
	int i;
	for (i=0;i<n;i++){
		Rprintf("%10i ",x->data[i]);
	}
	Rprintf("\n");
}

int advance_in_time(double *t, gsl_vector_int *x, gsl_vector *c, gsl_vector *a, gsl_rng *RNG){
	double tau;
	double r;
	double sum_a=0.0;
	int j;
	//model_propensities(*t,x->data,c->data,a->data);
	// 1. calculate sum(a)
	sum_a = gsl_vector_sum(a);
	if (sum_a==0) return -1; /* failure */
	// 2. pick a time step forward (tau)
	tau = gsl_ran_exponential(RNG,1.0/sum_a);
	// 3. pick a reaction to perform:
	r = gsl_rng_uniform(RNG);
	j = pick_reaction(a,r*sum_a);
	model_effects(*t,x->data,c->data,a->data,j);
	*t += tau;
	return 0; /* success */
}

int in_list(Rdata List, const char *name);
Rdata from_list(Rdata List, const char *name);
void set_names(Rdata list, const char *names[]);

int cat_parameters(gsl_vector *c, double *p, size_t np, Rdata input){
	memcpy(c->data,p,sizeof(double)*np);
	if (input && input != R_NilValue) {
		memcpy(c->data+(c->size - length(input)),REAL(AS_NUMERIC(input)),sizeof(double)*length(input));
	}
	return 0;
}

double seconds(double clock_a, double clock_b){
	return fabs(clock_b-clock_a)/CLOCKS_PER_SEC;
}

Rdata gillespie(Rdata model_so, Rdata experiments, Rdata parameters, Rdata time_limit_s, Rdata nstep){
	gsl_set_error_handler_off();
	int i,j,k;
	//int l;
	if (load_model(CHAR(STRING_ELT(model_so,0))) != EXIT_SUCCESS) return R_NilValue;
	size_t M = ncols(parameters);
	size_t nrp = nrows(parameters);
	size_t n = model_initial_counts(NULL,NULL);
	size_t m = model_reaction_coefficients(NULL);
	size_t na = model_propensities(0,NULL,NULL,NULL);
	size_t nf = model_func(0,NULL,NULL,NULL);
	gsl_vector_int *x = gsl_vector_int_alloc(n);
	gsl_vector *c = gsl_vector_alloc(m);
	gsl_vector *krc = gsl_vector_alloc(m);
	gsl_vector *a = gsl_vector_alloc(na);
	double t0 = 0;
	double tf = 10;
	double t = t0;
	double *p;
	double limit_seconds = REAL(time_limit_s)[0];
	int limit_nstep = asInteger(nstep);
	clock_t cpu_t[2];
	Rdata time, input, E, initialState, initialTime, yf, y, f, numSteps, cpuTime;
	const char *snames[] = {"state","func","status","numSteps","cpuSeconds",NULL};
	Rdata solution = PROTECT(NEW_LIST(length(experiments)));
	SET_NAMES(solution,GET_NAMES(experiments));
	gsl_vector_int_view column;
	Rdata status;
	int count;
	int status_t;
	gsl_rng *RNG = gsl_rng_alloc(gsl_rng_ranlxs0);
	gsl_rng_set(RNG, 1337);
	model_reaction_coefficients(c->data);
	model_initial_counts(x->data,c->data);
	/* main solver loop: */
	for (i=0; i<length(experiments); i++){
		E = VECTOR_ELT(experiments,i);
		initialState = from_list(E,"initialState");
		initialTime = from_list(E,"initialTime");
		time = from_list(E,"outputTimes");
		input = from_list(E,"input");
		t0 = *REAL(AS_NUMERIC(initialTime));
		y = PROTECT(alloc3DArray(INTSXP,n,length(time),M));
		f = PROTECT(alloc3DArray(REALSXP,nf,length(time),M));
		status = PROTECT(NEW_INTEGER(M));
		numSteps = PROTECT(NEW_INTEGER(M));
		cpuTime = PROTECT(NEW_NUMERIC(M));
		for (k=0;k<M;k++){
			t = t0;
			p = REAL(parameters)+(k*nrp);
			cat_parameters(c,p,nrp,input); /* c <- cat(p,input)*/
			gsl_vector_memcpy(krc,c);
			if (model_stochastic_parameters){
				model_stochastic_parameters(t0,c->data);
			}
			if (model_particle_count){
				model_particle_count(t0,REAL(initialState),x->data);
			}
			cpu_t[0] = clock();
			INTEGER(status)[k]=success; // success
			count=0;
			for (j=0; j<length(time); j++){
				tf = REAL(AS_NUMERIC(time))[j];
				model_propensities(t0,x->data,c->data,a->data);
				while (t < tf && (status_t = advance_in_time(&t,x,c,a,RNG))==0){
					count++;
					cpu_t[1] = clock();
					INTEGER(status)[k]=status_t;
					if (seconds(cpu_t[1],cpu_t[0]) > limit_seconds) {
						INTEGER(status)[k]=timeout; // timeout reached
						break;
					} else if (count > limit_nstep && limit_nstep > 0) {
						INTEGER(status)[k]=nstep_max; // nstep maximum reached
						break;
					}
				}
				column = gsl_vector_int_view_array(INTEGER(y)+(n*length(time)*k+n*j),n);
				model_func(t,x->data,krc->data,REAL(f)+(nf*length(time)*k+nf*j));
				gsl_vector_int_memcpy(&(column.vector),x);
			}
			INTEGER(numSteps)[k] = count;
			REAL(cpuTime)[k] = seconds(clock(),cpu_t[0]);
		}
		yf = PROTECT(NEW_LIST(5));
		SET_VECTOR_ELT(yf,0,y);
		SET_VECTOR_ELT(yf,1,f);
		SET_VECTOR_ELT(yf,2,status);
		SET_VECTOR_ELT(yf,3,numSteps);
		SET_VECTOR_ELT(yf,4,cpuTime);
		set_names(yf,snames);
		SET_VECTOR_ELT(solution,i,yf);
		UNPROTECT(6);
	}
	UNPROTECT(1);
	gsl_rng_free(RNG);
	return solution;
}
