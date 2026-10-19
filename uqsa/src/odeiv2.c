#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_sf.h>
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
#include <stdint.h>

#define MATCH 0
#define NO_DIFFERENCE 0
#define RCOND_LIMIT 1e-10
#define TIME_LIMIT_ERROR 1<<11
/* SEXP stands for S-Expression, and it can be any R data object (or
 * function) in this program, we'll only use data from R. However SEXP is
 * a bad type name; I am compelled to pronounce it in my head ...
 */
typedef SEXP Rdata;


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



static char* pcpy(char *dest, const char *src, size_t n){
	return ((char*) memcpy(dest,src,n)+n);
}

/* an ODE model y'=f(t,y,p) will have a name, and several functions, prefixed with
 * that name, e.g.: ${name}_vf().
 * The possible suffixes are:
 * _vf          the ODE right-hand-side function f (vector-field)
 * _jac         the y-jacobian df/dy (with respect to y)
 * _jacp        the p-jacobian df/dp (with respect to p)
 * _init        sets the initial values of the state variables y
 * _default     sets the default parameter values p
 * _func        output functions F (observable values)
 * _funcJac     output function y-jacobian dF/dy
 * _funcJacp    output function p-jacobian dF/dp
 * all of these functions return integer error codes and GSL_SUCCESS on success (0).
 *
 * We load these functions by name from a shared library (.so) and
 * give them new, generic names, where we replace the model's name with "ODE".
 *
 */
int (*ODE_vf)(double t, const double y_[], double f_[], void *par);
int (*ODE_jac)(double t, const double y_[], double *jac_, double *dfdt_, void *par);
int (*ODE_jacp)(double t, const double y_[], double *jacp_, double *dfdt_, void *par);
int (*ODE_func)(double t, const double y_[], double *func_, void *par);
int (*ODE_funcJac)(double t, const double y_[], double *funcJac_, void *par);
int (*ODE_funcJacp)(double t, const double y_[], double *funcJacp_, void *par);
int (*ODE_default)(double t, void *par);
int (*ODE_init)(double t, double *y_, void *par);
int (*ODE_event)(double t, double *y_, void *par, int EventLabel, double dose);

union symbol {
	void *ptr;
	int (*ODE_vf)(double t, const double y_[], double f_[], void *par);
	int (*ODE_jac)(double t, const double y_[], double *jac_, double *dfdt_, void *par);
	int (*ODE_jacp)(double t, const double y_[], double *jacp_, double *dfdt_, void *par);
	int (*ODE_func)(double t, const double y_[], double *func_, void *par);
	int (*ODE_funcJac)(double t, const double y_[], double *funcJac_, void *par);
	int (*ODE_funcJacp)(double t, const double y_[], double *funcJacp_, void *par);
	int (*ODE_default)(double t, void *par);
	int (*ODE_init)(double t, double *y_, void *par);
	int (*ODE_event)(double t, double *y_, void *par, int EventLabel, double dose);
};


void transition_matrix(gsl_matrix *Ji, gsl_matrix *Jf, double ti, double tf, gsl_matrix *phi);

typedef enum {SCALE,DIAG,MATVEC} tf_t;

typedef struct {
	tf_t type;
	int length_b;
	int l;
	double *A;
	double *b;
	gsl_vector *y; /* result vector */
} affine_tf;

enum event_type {affine_tf_event, model_func_event};
struct event {
	enum event_type type;
	int *label;
	int nt;
	int nL;
	int nDose;
	double *time;
	double *dose;
	affine_tf *state;
	affine_tf *par;
};

static double sec(clock_t c){
	double t=(double) c;
	return t/CLOCKS_PER_SEC;
}

/* finds named item in List, `name` can be a space separated list of possible names */
static int in_list(Rdata List, const char *name){
	if (!isVector(List)) return -1;
	int i;
	R_xlen_t N=length(List);
	int l=strlen(name);
	char *str=malloc(l+1);
	char *context=NULL;
	*(((char*) memcpy(str,name,l))+l)='\0';
	char *t=strtok_r(str," ",&context);
	while (t){
		for (i=0;i<N;i++){
			if (strcmp(CHAR(STRING_ELT(List,i)),t) == MATCH){
				free(str);
				return i;
			}
		}
		t=strtok_r(NULL," ",&context);
	}
	free(str);
	return -1;
}

Rdata from_list(Rdata List, const char *name){
	if (!isVector(List)) return R_NilValue;
	Rdata names = GET_NAMES(List);
	int i=in_list(names,name);
	Rdata E=R_NilValue;
	if (i>=0){
		E=VECTOR_ELT(List,i);
	}
	return E;
}

/* creates an affine transformation struct from R objects, Rdata
	 objects need to be kept alive as A and b arfe taken from R via
	 pointers. Some memory is allocated to store an intermediate result. */
static affine_tf* /* an affine transformation (linear with offset) map: x -> A*x+b */
affine_transformation(
 Rdata A,/*a series of matrices, possibly just a set of diagonals*/
 Rdata b)/*a series of offsets*/
{
	if (A == R_NilValue || b == R_NilValue) return NULL;

	Rdata dA=GET_DIM(A);
	Rdata db=GET_DIM(b);
	assert(length(dA)==length(db));
	R_xlen_t n=length(dA);
	int *dim=INTEGER(dA);
	int *dim_b=INTEGER(db);
	int j;
	if (dim[0] != dim_b[0]) return NULL;
	affine_tf *L=malloc(sizeof(affine_tf));
	if (n==3) {
		L->l=dim[2];
	} else {
		L->l=1;
	}

	if (dim[0]==dim[1]){
		L->type=MATVEC;
	} else if (dim[1] == 1) {
		L->type=DIAG;
	} else {
		REprintf("[%s] A and b have weird dimensionality: A is ",__func__);
		for (j=0;j<n;j++) Rprintf("%i%s",dim[j],j==n-1?"×":" ");
		REprintf(" and b is ");
		for (j=0;j<n;j++) Rprintf("%i%s",dim_b[j],j==n-1?"×":" ");
		REprintf("\nThey should be both three dimensional (length(dim(A))==3).\n");
		return NULL;
	}
	L->A=REAL(A);
	L->b=REAL(b);
	L->length_b=dim[0];
	L->y=gsl_vector_alloc(dim[0]);
	return L;
}

static void free_tf(affine_tf *L){
	if (L){
		gsl_vector_free(L->y);
		free(L);
	}
}

/* This function applies a affine transformation to the vector z. We
	assume that A and b and z in the input are all of sufficient size:
	n×n for matrices and n for vectors. A can be n-sized as well, if A
	is a diagonal matrix, then we only store the diagonal. n is stored
	in the transformation structure as length_b. */
static int /* the returned status of the gsl operations */
apply_tf(
	affine_tf *L, /* a transformation struct: A and b are cast to gsl_vectors here */
	double *z,/* an array of size n, it is updated using L */
	int t_index /* if A and b are each a series of matrices, pick the one with this offset */
)
{
	if (!L) return GSL_SUCCESS; /* nothing to be done */
	if (!z || !(t_index >=0 && L->l > 0)) {
		REprintf("[%s] (%p) z must be a pointer to a double array. t_index must be a valid index (0<=%i<%i), L must be applicable to z\n",__func__,(void*) z,t_index,L->l);
		return GSL_EINVAL;
	}
	int n=L->length_b;
	int i=t_index % (L->l);
	int status=GSL_SUCCESS;
	gsl_vector_view x=gsl_vector_view_array(z,n);
	gsl_vector_view b=gsl_vector_view_array((L->b)+i*n,n);
	gsl_vector_view a;
	gsl_matrix_view A;
	switch (L->type){
	case SCALE:
		status|=gsl_vector_memcpy(L->y,&(x.vector));
		status|=gsl_vector_scale(L->y,L->A[i]);
		break;
	case DIAG:
		/* y <- diag(A)*x + b */
		a=gsl_vector_view_array((L->A)+i*n,n);
		status|=gsl_vector_memcpy(L->y,&(x.vector));
		status|=gsl_vector_mul(L->y,&(a.vector));
		break;
	case MATVEC:
		/* y <- A*x + b */
		A=gsl_matrix_view_array((L->A)+i*n*n,n,n);
		status|=gsl_blas_dgemv(CblasNoTrans, 1.0, &(A.matrix), &(x.vector), 0.0, L->y);
		break;
	default:
		REprintf("[%s] unknown transformation type %i.\n",__func__,L->type);
		return GSL_EINVAL;
	}
	if (status==GSL_SUCCESS){
		gsl_vector_add(L->y,&(b.vector));
		gsl_vector_memcpy(&(x.vector),L->y);
	}
	return status;
}

struct event* event_from_R(Rdata E){
	if (E == R_NilValue) return NULL;
	if (!IS_VECTOR(E)) return NULL;
	Rdata tf=from_list(E,"tf");
	Rdata time = from_list(E,"time");
	Rdata label = from_list(E,"label");
	Rdata dose = from_list(E,"dose");
	if (time == R_NilValue) return NULL;
	struct event *event=malloc(sizeof(struct event));
	if (label != R_NilValue){
		event->nL = length(label);
		event->label = INTEGER(AS_INTEGER(label));
		event->dose = REAL(AS_NUMERIC(dose));
		event->nDose = length(dose);
		event->type = model_func_event;
	} else if (tf != R_NilValue){
		Rdata state_tf=from_list(tf,"state");
		Rdata param_tf=from_list(tf,"param");
		event->state=affine_transformation(from_list(state_tf,"A"),from_list(state_tf,"b"));
		event->par=affine_transformation(from_list(param_tf,"A"),from_list(param_tf,"b"));
		event->type=affine_tf_event;
	} else {
		REprintf("[%s] unknown type of event.\n",__func__);
	}
	ssize_t lt=length(time);
	if (event->type == model_func_event && lt != event->nL){
		REprintf("[%s] event time vector and event label vector must be the same length for this type of event. (%zd != %i)\n",__func__,lt,event->nL);
	}
	event->time = REAL(AS_NUMERIC(time));
	event->nt=lt;
	return event;
}

/* this function takes the address of an event structure pointer, clears the */
/* memory and changes the pointer to NULL, so that the event cannot be       */
/* accessed after being freed (except through a different pointer).          */
static void event_free(struct event **ev){
	if (ev && *ev){
		switch ((*ev)->type){
			case affine_tf_event:
			free_tf((*ev)->state);
			free_tf((*ev)->par);
			break;
			case model_func_event:
			/* for now, no action is needed */
			break;
		}
		free(*ev);
		*ev=NULL;
	}
}

/* Loads the ODE system from an `.so` file, the file is given by name, */
/* the returned structure is intended for the `gsl_odeiv2` library of  */
/* solvers. The jacobian dfdx is loaded alongside the right hand side; */
/* `gsl_odeiv2_system`. Other model functions (_jacp, _func, _init,    */
/* _default, funcJac, funcJacp) are assigned to global variables.      */
gsl_odeiv2_system /* the system structure, see gsl documentation.      */
load_system(
	const char *model_name, /* the name of the model, function names will be inferred from that */
	const char *model_so  /* the path to the shared library that contains the model. */
)
{
	static shared_library lib = NULL; // this happens once
	static uint64_t old_path_hash = 0;
	uint64_t current_path_hash = FNV1a(model_so);
	const char *err_msg = NULL;
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
	gsl_odeiv2_system sys={NULL,NULL,0,NULL};
	size_t n,l;
	size_t m=strlen(model_name);
	char *symbol_name=malloc(m+32); // symbol name in .so
	char *suffix=pcpy(symbol_name,model_name,m);
	*suffix='\0';
	union symbol conversion;
	if (lib){
		/* In this block, we use the DLSYM macro defined in the preamble (this file) */
		/* DLSYM is dlsym on UNIX systems and the windows equivalent on windows      */
		*((char*) pcpy(suffix,"_vf",3))='\0';
		if ((conversion.ptr=DLSYM(lib,symbol_name))==NULL){
			REprintf("[%s] loading «%s» is required.\n",__func__,symbol_name);
			REprintf("%p: %p\n",(void*) lib,conversion.ptr);
			free(symbol_name);
			return sys;
		}
		ODE_vf = conversion.ODE_vf;

		*((char*) pcpy(suffix,"_jac",4))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_jac = conversion.ODE_jac;

		*((char*) pcpy(suffix,"_jacp",5))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_jacp = conversion.ODE_jacp;

		*((char*) pcpy(suffix,"_func",5))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_func = conversion.ODE_func;

		*((char*) pcpy(suffix,"_funcJac",8))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_funcJac = conversion.ODE_funcJac;

		*((char*) pcpy(suffix,"_funcJacp",9))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_funcJacp = conversion.ODE_funcJacp;

		*((char*) pcpy(suffix,"_default",8))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_default = conversion.ODE_default;

		*((char*) pcpy(suffix,"_init",5))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_init = conversion.ODE_init;

		*((char*) pcpy(suffix,"_event",6))='\0';
		conversion.ptr = DLSYM(lib,symbol_name);
		ODE_event = conversion.ODE_event;
	} else {
		if (err_msg){
			Rf_error("[dlopen] %s.",err_msg);
		} else {
			Rf_error("[dlopen] unknown error.");
		}
		error("[%s] library «%s» could not be loaded: %s\n",__func__,model_so,err_msg);
		return sys; // just in case error fails for some reason
	}
	n=ODE_vf(0,NULL,NULL,NULL);
	l=ODE_default(0,NULL);
	sys.function=ODE_vf;
	sys.jacobian=ODE_jac;
	sys.dimension=n;
	double *p=malloc(sizeof(double)*l);
	ODE_default(0,p);
	sys.params=p;
	free(symbol_name);
	return sys;
}

/* Intergrates the system `sys` using the specified `driver` and
   simulation instructions. */
int /* error code if any, otherwise GSL_SUCCESS */
simulate_timeseries(
	const gsl_odeiv2_system sys, /* the system to integrate */
	gsl_odeiv2_driver* driver, /* the driver that is used to integrate `sys` */
	double t0, /* the initial time: y(t0) = y0 */
	const gsl_vector *y0, /* initial value */
	const gsl_vector *time, /* a vector of time-points */
	const struct event *event, /*a struct array with scheduled events */
	gsl_matrix *Yout,  /* (OUT) return vaule, pre-allocated */
	double time_limit_seconds, /* time limit for solution */
	int *n) /* (OUT) number of steps taken */
{
	gsl_set_error_handler_off();
	int nt=time->size;
	int ny=(int) sys.dimension;
	gsl_vector *y=gsl_vector_alloc(ny);
	gsl_vector_memcpy(y,y0);

	gsl_vector_view Yout_row;
	int i=0,j,nL,nDose;
	double t=t0;
	double tf,te;
	int status=GSL_SUCCESS;
	double dose=0;
	int label=0;
	clock_t ct0=clock();
	double elapsed_sec;
	*n = 0;
	for (j=0, i=0; j<nt; j++){
		tf=gsl_vector_get(time,j);
		while (event && i<event->nt && event->time[i] <= tf) {
			te=event->time[i];
			status=gsl_odeiv2_driver_apply(driver, &t, te, y->data);
			*n += driver->e->count; //gsl_odeiv2_step_n_actual(driver->s); // next we definitely reset
			if (status!=GSL_SUCCESS){
				gsl_vector_free(y);
				gsl_odeiv2_driver_reset(driver);
				return(status);
			}
			switch (event->type){
			case affine_tf_event:
				apply_tf(event->state,y->data,i);
				apply_tf(event->par,(double*) sys.params,i);
				break;
			case model_func_event:
				nL = event->nL;
				nDose = event->nDose;
				if (event->dose && nDose) dose = event->dose[i % nDose];
				if (event->label && nL) label = event->label[i % nL];
				if (ODE_event) ODE_event(te,y->data,sys.params,label,dose);
				break;
			}
			status=gsl_odeiv2_driver_reset(driver);
			if (status!=GSL_SUCCESS){
				gsl_vector_free(y);
				gsl_odeiv2_driver_reset(driver);
				return(status);
			}
			i++;
		}
		if (tf>t) status=gsl_odeiv2_driver_apply(driver, &t, tf, y->data);
		elapsed_sec = sec(clock()-ct0);
		if (elapsed_sec > time_limit_seconds){
			status = TIME_LIMIT_ERROR;
		}
		if(status==GSL_SUCCESS){
			Yout_row = gsl_matrix_row(Yout,j);
			gsl_vector_memcpy(&(Yout_row.vector),y);
		} else {
			REprintf("[%s] %s\n",__func__,gsl_strerror(status));
			break;
		}
	}
	gsl_vector_free(y);
	*n += driver->e->count; //gsl_odeiv2_step_n_actual(driver->s);
	gsl_odeiv2_driver_reset(driver);
	return status;
}

// allocates a matrix of parameters, onw row per experiment;
gsl_matrix* params_alloc(Rdata experiments){
	int i;
	ssize_t N;
	gsl_matrix *P=NULL;
	int np=0,nu=0;
	gsl_vector *p=NULL;
	Rdata input;
	double* u;
	gsl_vector_view row;
	if (ODE_default) {
		np=ODE_default(0.0,NULL);
		p=gsl_vector_alloc(np);
		ODE_default(0.0,p->data);
	} else {
		warning("the seemingly has no parameters (no default parameter function).");
		return NULL;
	}
	if (experiments != R_NilValue && IS_VECTOR(experiments)){
		N=length(experiments);
		P=gsl_matrix_alloc(N,p->size);
		for (i=0;i<N;i++){
			input = from_list(VECTOR_ELT(experiments,i),"input Input u inputs Inputs");
			nu = (input != R_NilValue) ? length(input) : 0;
			row = gsl_matrix_row(P,i);
			gsl_vector_memcpy(&row.vector,p);
			if (nu>0 && np>=nu){
				u = gsl_matrix_ptr(P,i,np-nu);
				memcpy(u, REAL(input), nu*sizeof(double));
			}
		}
	} else {
		P = gsl_matrix_alloc(1,p->size);
		row = gsl_matrix_row(P,0);
		gsl_vector_memcpy(&row.vector,p);
	}
	if (p) gsl_vector_free(p);
	return P;
}

// allocates a matrix of initial values, one row per experiment
gsl_matrix* init_alloc(const gsl_odeiv2_system sys, gsl_matrix *p){
	int ny = sys.dimension;
	int i,N = p->size1;
	gsl_matrix *Y0 = gsl_matrix_alloc(N,ny);
	double t = 0.0;
	gsl_matrix_set_zero(Y0);
	if (ODE_init){
		for (i=0;i<N;i++){
			ODE_init(t,gsl_matrix_ptr(Y0,i,0),gsl_matrix_ptr(p,i,0));
		}
	}
	return Y0;
}

void update_initial_values(gsl_vector *y0, gsl_odeiv2_system sys, Rdata iv){
	gsl_vector_view r_init;
	if (!y0) return;
	if (!sys.params) return;
	if (iv != R_NilValue && IS_VECTOR(iv) && length(iv) == y0->size){
		r_init = gsl_vector_view_array(REAL(AS_NUMERIC(iv)),length(iv));
		gsl_vector_memcpy(y0,&(r_init.vector));
	} else if (ODE_init!=NULL && y0->size == sys.dimension) {
		ODE_init(0.0,y0->data,(double*) sys.params);
	}
}


void set_names(Rdata list, const char *names[])
{
	ssize_t i=0;
	ssize_t n=0;
	while (names && names[i++]){
		n++;
	}
	if (n==0) return;
	Rdata rnames=PROTECT(allocVector(STRSXP, n));
	Rdata elt;
	for (i=0;i<n;i++){
		elt=PROTECT(mkChar(names[i]));
		SET_STRING_ELT(rnames,i,elt);
	}
	SET_NAMES(list,rnames);
	UNPROTECT(1+n);
}

struct sensApproxMem {
	gsl_matrix *A;
	gsl_matrix *LU;
	gsl_matrix *B;
	gsl_matrix *E;
	gsl_matrix *S_AB;
	gsl_permutation *P;
	gsl_matrix *FA;
	gsl_matrix *Sy;
	gsl_matrix *Sf;
};

/* n: number of state variables (ODE_vf);
 * l: number of parameters (ODE_default);
 * f: number of output values (ODE_func);
 */
struct sensApproxMem sensApproxMemAlloc(size_t n, size_t l, size_t f){
	struct sensApproxMem M; /* contains a bunch of pointers */
	M.A=gsl_matrix_alloc(n,n);
	M.LU=gsl_matrix_alloc(n,n);
	M.B=gsl_matrix_alloc(n,l);
	M.E=gsl_matrix_alloc(n,n);
	M.S_AB=gsl_matrix_alloc(n,l);
	M.P=gsl_permutation_alloc(n);
	M.FA=gsl_matrix_alloc(f,n);
	M.Sy=gsl_matrix_alloc(n,l);
	M.Sf=gsl_matrix_alloc(f,l);
	return M; /* bunch of pointers, by value */
}

void sensApproxMemFree(struct sensApproxMem M){
	if (M.A) gsl_matrix_free(M.A);
	if (M.LU) gsl_matrix_free(M.LU);
	if (M.B) gsl_matrix_free(M.B);
	if (M.E) gsl_matrix_free(M.E);
	if (M.S_AB) gsl_matrix_free(M.S_AB);
	if (M.P) gsl_permutation_free(M.P);
	if (M.FA) gsl_matrix_free(M.FA);
	if (M.Sy) gsl_matrix_free(M.Sy);
	if (M.Sf) gsl_matrix_free(M.Sf);
}

void transition_matrix(gsl_matrix *Ji, /* the jacobian at t=ti */
 gsl_matrix *Jf, /* the jacobian at t=tf */
 double ti, /* initial time of the interval (left bondary) */
 double tf,/* final time of the interval (right boundary) */
 gsl_matrix *phi) /* return buffer */
{
  double s=0.5*(tf-ti);
  size_t ny=Ji->size1;
  int i,n=3;
  gsl_matrix *V=gsl_matrix_alloc(ny,ny);
  gsl_matrix *W=gsl_matrix_alloc(ny,ny);
  gsl_matrix *I1=gsl_matrix_alloc(ny,ny);  // I1(tf;ti) = 0.5*(tf-ti)*(Jf+Ji)
  gsl_matrix_set_identity(phi);
  // I1
  gsl_matrix_memcpy(I1,Jf);
  gsl_matrix_add(I1,Ji);
  gsl_matrix_scale(I1,s);
  gsl_matrix_set_identity(W);
  for (i=0;i<n;i++){
    gsl_matrix_set_identity(V);
    gsl_blas_dgemm(CblasNoTrans,CblasNoTrans, s, W, Jf, 1.0, V);
    gsl_matrix_memcpy(W,V);
  }
  gsl_blas_dgemm(CblasNoTrans,CblasNoTrans, 1.0, W, I1, 1.0, phi);

  gsl_matrix_free(I1);
  gsl_matrix_free(V);
  gsl_matrix_free(W);
}

int sensitivityApproximation(double t0, gsl_vector *t, gsl_vector *p, gsl_matrix *Y, double *dYdp, double *dFdp, struct sensApproxMem M)
{
	int j,k;
	size_t m=t->size;
	size_t n=Y->size2;
	size_t l=p->size;
	int f=ODE_func(0,NULL,NULL,NULL);
	double tj,tj_1=t0,delta_t;
	gsl_vector_view col, diag;
	gsl_matrix *A=M.A;
	gsl_matrix *LU=M.LU;
	gsl_matrix *B=M.B;
	gsl_matrix *E=M.E;
	gsl_matrix *S_AB=M.S_AB;
	gsl_permutation *P=M.P;
	gsl_matrix *FA=M.FA;
	gsl_matrix_view SY, SF; /* sensitivity matrices (array-views) */
	int sign;
	const double *y;
	int status=GSL_SUCCESS;
	if (m != Y->size1) REprintf("[%s] t has length %zu, but Y has %zu rows.\n",__func__,m,Y->size1);

	for (j=0;j<m;j++){
		tj=gsl_vector_get(t,j);
		delta_t=tj-tj_1;
		tj_1=tj;
		y=gsl_matrix_ptr(Y,j,0);
		ODE_jac(tj,y,A->data,NULL,p->data);                                           /* A <- df/dy */
		ODE_jacp(tj,y,B->data,NULL,p->data);                                          /* B <- df/dp */
		gsl_matrix_memcpy(LU,A);                                                      /* LU <- A */
		// dirty hack .. maybe this will work better? Will not work if some eigenvalues are positive.
		diag = gsl_matrix_diagonal(LU);
		gsl_vector_add_constant(&(diag.vector),-1e-9); // all eigenvalues should be negative, if some are close to zero, we force them
		// .. dirty hack
		status=gsl_linalg_LU_decomp(LU, P, &sign);                                           /* make P*A = L*U */
		if (status) Rprintf("[%s] %s\n",__func__,gsl_strerror(status));
		for (k=0;k<l;k++){
			col=gsl_matrix_column(B,k);
			status=gsl_linalg_LU_svx(LU, P, &(col.vector));                                    /* B <- A\B*/
			if (status) Rprintf("[%s] %s\n",__func__,gsl_strerror(status));
		}
		gsl_matrix_scale(A,delta_t);                                                  /* A <- (df/dy)*(t-t0)*/
		gsl_linalg_exponential_ss(A,E,GSL_PREC_SINGLE);                               /* E <- exp(A*(t-t0))*/
		if (j==0){
			gsl_matrix_memcpy(S_AB,B);
		} else {
			gsl_matrix_memcpy(S_AB,M.Sy);
			gsl_matrix_add(S_AB,B);
		}
		gsl_matrix_memcpy(M.Sy,B);
		gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, E, S_AB, -1.0, M.Sy);         /* S is now the piece-wise-constant approximation of the sensitivity */
		/* write result to output buffer: */
		SY=gsl_matrix_view_array(dYdp+n*l*j,l,n);
		gsl_matrix_transpose_memcpy(&(SY.matrix),M.Sy);

		ODE_funcJac(tj,y,FA->data,p->data);
		ODE_funcJacp(tj,y,M.Sf->data,p->data);

		status=gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, FA, M.Sy, 1.0, M.Sf);
		if (status) Rprintf("[%s] %s\n",__func__,gsl_strerror(status));
		SF=gsl_matrix_view_array(dFdp+f*l*j,l,f);
		status=gsl_matrix_transpose_memcpy(&(SF.matrix),M.Sf);
		if (status) Rprintf("[%s] %s\n",__func__,gsl_strerror(status));
	}
	return GSL_SUCCESS;
}

/* experiment: one experiment, with data and standard error        */
/* f: calculated output function values, from the simulation       */
/* returns the value of the log-likelihood assuming gaussian noise */
/* Details:                                                        */
/* Likelihood = 1/sqrt(2*pi*sd)*exp(-0.5*((f-d)/sd)**2)            */
/*            = exp(-0.5*((f-d)/sd)**2 - 0.5*(log(2*pi*sd)))       */
/* log(Likelihood) = -0.5*(((f-d)/sd)**2 + log(2*pi*sd))           */
double logLikelihood(Rdata experiment, double *f){
	/* these two are supposed to be matrices, without missing values */
	Rdata data=from_list(experiment,"data measuredData experimentalData");
	Rdata stdv=getAttrib(data,install("errors"));
	if (!IS_NUMERIC(stdv)){
		stdv = from_list(experiment,"stdv sd standardError standardDeviationOfTheMean");
	}
	Rdata time=from_list(experiment,"time times t outputTimes");
	double ll=0;
	double d,s;
	int i;
	R_xlen_t nt=length(time);
	int n=ODE_func(0,NULL,NULL,NULL);
	double C;
	for (i=0;i<n*nt;i++){
		d = REAL(data)[i];
		s = REAL(stdv)[i];
		C = (isnan(d) || isinf(s) || s==0.0) ? 0.0 : log(2*M_PI*s*s);
		ll+= (isnan(d) || isinf(s) || s==0.0) ? 0.0 : (gsl_pow_2((f[i]-d)/s) + C);
	}
	return -0.5*ll;
}

/* experiment: one experiment, with data and standard error        */
/* f: calculated output function values, from the simulation       */
/* returns the value of the log-likelihood assuming gaussian noise */
/* Details:                                                        */
/* Likelihood = 1/sqrt(2*pi*sd)*exp(-0.5*((f-d)/sd)**2)            */
/*            = exp(-0.5*((f-d)/sd)**2 - 0.5*(log(2*pi*sd)))       */
/* grad(log(Likelihood)) = ((d-f)/(sd*sd))*df/dp */
int gradLogLikelihood(double *gll, Rdata experiment, double *func, double *funcSens, size_t m, gsl_vector *v){
	Rdata data=from_list(experiment,"data measurements measuredData experimentalData");
	Rdata stdv=getAttrib(data,install("errors"));
	if (!IS_NUMERIC(stdv)){
		stdv = from_list(experiment,"stdv sd standardError standardDeviationOfTheMean");
	}
	//Rdata stdv=from_list(experiment,"stdv standardError standardDeviationOfTheMean");
	Rdata time=from_list(experiment,"time times t outputTimes");
	R_xlen_t nt=length(time);
	int n=v->size;
	gsl_vector_view d,s,g,f;
	gsl_matrix_view Sf;
	//gsl_vector *v=gsl_vector_alloc(n);
	int j;
	int status = GSL_SUCCESS;
	g = gsl_vector_view_array(gll,m);
	gsl_vector_set_zero(&(g.vector));
	//gsl_vector_set_zero(v);
	for (j=0;j<nt;j++){
		d = gsl_vector_view_array(REAL(data)+(j*n),n);
		s = gsl_vector_view_array(REAL(stdv)+(j*n),n);
		f = gsl_vector_view_array(func+(j*n),n);
		Sf = gsl_matrix_view_array(funcSens+(j*n*m),m,n);
		gsl_vector_memcpy(v,&d.vector);
		gsl_vector_sub(v,&f.vector);
		gsl_vector_div(v,&s.vector);
		gsl_vector_div(v,&s.vector); // v = ((d-f)/(sd*sd))
		status |= gsl_blas_dgemv(CblasNoTrans, 1.0, &(Sf.matrix), v, 1.0, &(g.vector));
	}
	//gsl_vector_free(v);
	return status;
}

/* experiment: one experiment, with data and standard error        */
/* f: calculated output function values, from the simulation       */
/* returns the value of the log-likelihood assuming gaussian noise */
/* Details:                                                        */
/* FisherInf = t(Sf)*solve(Sigma)*Sf                               */
/* for solve(Sigma) == diag(sd**(-2))                              */
/* with Sf_sd[,j] = Sf[,j]/sd -> FisherInf = t(Sf_sd)*Sf_sd        */
int FisherInformation(double *FI, Rdata experiment, double *funcSens, gsl_matrix *Sf_sd){
	Rdata data=from_list(experiment,"data measurements measuredData");
	Rdata stdv=getAttrib(data,install("errors"));
	if (!IS_NUMERIC(stdv)){
		stdv = from_list(experiment,"stdv sd standardError standardDeviationOfTheMean");
	}
	Rdata time=from_list(experiment,"time times t outputTimes OutputTimes");
	R_xlen_t nt=length(time);
	size_t n=Sf_sd->size2; //ODE_func(0,NULL,NULL,NULL);
	size_t m=Sf_sd->size1;
	gsl_vector_view s,row;
	gsl_matrix_view Sf,fi;
	//gsl_matrix *Sf_sd=gsl_matrix_alloc(m,n);
	int i,j;
	int status = GSL_SUCCESS;
	//gsl_matrix_set_zero(Sf_sd);
	fi = gsl_matrix_view_array(FI,m,m);
	gsl_matrix_set_zero(&(fi.matrix));
	for (j=0;j<nt;j++){
		s = gsl_vector_view_array(REAL(stdv)+(j*n),n);
		Sf = gsl_matrix_view_array(funcSens+(j*n*m),m,n);
		if (gsl_matrix_memcpy(Sf_sd,&Sf.matrix) != GSL_SUCCESS){
			REprintf("[%s] memcpy didn't work for Sf.matrix(%zu,%zu).",__func__,(&Sf.matrix)->size1,(&Sf.matrix)->size2);
		}
		for (i=0;i<m;i++) {
			row = gsl_matrix_row(Sf_sd,i);
			gsl_vector_div(&row.vector,&s.vector);
		}
		status |= gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1.0, Sf_sd, Sf_sd, 1.0, &(fi.matrix));
	}
	//gsl_matrix_free(Sf_sd);
	return status;
}

/* This program loads an ODE model, with functions compatible with `gsl_odeiv2`
	(see odeiv2 documentation on the GSL webpage). It
	 simulates the model for each entry in a list of named items, each
	 describing a single initial value problem (`y0`, `t`, parameters `p`, events).
	 ```
	 y'=f(y,t;p) y0=y(t[0])
	 ```
	 This function also estimates the Fisher Information and gradient of a hypothetical Gaussian noise model.
	 If the noise model for the data contained within the experiments variable is more complicated,
	 then these estimates will be wrong and must be re-done in higher level code.
*/
Rdata /* the trajectories as a list (same size as experiments) */
r_gsl_odeiv2_outer_fi(
 Rdata modelName, /* a string */
 Rdata experiments, /* a list of simulation experiments */
 Rdata parameters, /* a matrix of parameterization columns*/
 Rdata absolute_tolerance, /* absolute tolerance for GSL's solver */
 Rdata relative_tolerance, /* relative tolerance for GSL's solver */
 Rdata initial_step_size, /* initial guess for the step size */
 Rdata optional_outputs, /* a value that indicates whether ll, gl, fi are to be calculated or not */
 Rdata method,  /* integration method (integer) */
 Rdata time_limit_seconds, /* time limit for single experiments */
 Rdata nmax) /* set a maximum number of steps for the solver */
{
	gsl_set_error_handler_off();
	const gsl_odeiv2_step_type* step_types[] = {gsl_odeiv2_step_msbdf, gsl_odeiv2_step_msadams, gsl_odeiv2_step_bsimp, gsl_odeiv2_step_rk4imp, gsl_odeiv2_step_rk2imp, gsl_odeiv2_step_rk1imp, gsl_odeiv2_step_rk8pd, gsl_odeiv2_step_rkck, gsl_odeiv2_step_rkf45, gsl_odeiv2_step_rk4, gsl_odeiv2_step_rk2, NULL};
	const char* model_so=CHAR(asChar(getAttrib(modelName,install("comment"))));
	const char* model_name=CHAR(STRING_ELT(modelName,0));
	int i,j,k,status;
	double abs_tol=asReal(absolute_tolerance);
	double rel_tol=asReal(relative_tolerance);
	double h=asReal(initial_step_size);
	R_xlen_t N=GET_LENGTH(experiments);
	size_t M=ncols(parameters);
	const gsl_odeiv2_step_type * T=step_types[asInteger(method)];
	int OUTPUTS = asInteger(optional_outputs);                        /* 0,1,2,3 */
	enum possible_outputs {output_fisher_information, output_grad_log_likelihood, output_log_likelihood, output_functions, numOptOutputs};
	Rdata res_list = PROTECT(NEW_LIST(N));                            /* use VECTOR_ELT and SET_VECTOR_ELT */
	SET_NAMES(res_list,GET_NAMES(experiments));
	Rdata yf_list, state=R_NilValue, func=R_NilValue, iv, t, cpuSeconds, numSteps, Status;
	double t0;
	clock_t ct0, ct1;
	const char *yf_names[]={"cpuSeconds","numSteps","status","state","func","logLikelihood","gradLogLikelihood","FisherInformation",NULL};
	gsl_vector_view initial_value, time;
	gsl_matrix_view y;
	R_xlen_t nt;
	unsigned long NMAX = asInteger(nmax);
	struct event *ev=NULL;
	double *f;
	gsl_vector_view p;
	double *sy_k=NULL, *sf_k=NULL;
	Rdata FI=R_NilValue, gll=R_NilValue, ll=R_NilValue;
	gsl_odeiv2_system sys = load_system(model_name, model_so); /* also sets ODE_*() functions */
	if (sys.dimension == 0 || ODE_default==NULL || ODE_init==NULL){
		REprintf("[%s] loading model has failed (system dimension: «%zu»).\n",__func__,sys.dimension);
		UNPROTECT(1); /* res_list */
		return R_NilValue;
	}
	gsl_matrix *P = params_alloc(experiments);
	gsl_matrix *Y0 = init_alloc(sys,P);
	size_t nf = ODE_func ? abs(ODE_func(0,NULL,NULL,NULL)):0;
	size_t ny = sys.dimension;
	size_t np = P->size2;
	gsl_matrix *Sf_sd=gsl_matrix_alloc(np,nf);
	gsl_vector *v=gsl_vector_alloc(nf);
	struct sensApproxMem saMem = sensApproxMemAlloc(ny,np,nf);
	gsl_odeiv2_driver *driver = gsl_odeiv2_driver_alloc_y_new(&sys,T,h,abs_tol,rel_tol);
	if (NMAX) gsl_odeiv2_driver_set_nmax(driver,NMAX);
	for (i=0; i<N; i++){
		iv = from_list(VECTOR_ELT(experiments,i),"initial_value initialState initialValue initialValues");
		t = from_list(VECTOR_ELT(experiments,i),"time times outputTimes t");
		t0 = REAL(AS_NUMERIC(from_list(VECTOR_ELT(experiments,i),"intial_time initialTime t0 T0")))[0];
		ev = event_from_R(from_list(VECTOR_ELT(experiments,i),"event events scheduledEvents scheduledEvent scheduled_event"));
		initial_value = gsl_matrix_row(Y0,0);
		sys.params = gsl_matrix_ptr(P,i,0);
		nt = length(t);
		time=gsl_vector_view_array(REAL(AS_NUMERIC(t)),nt);
		ny = Y0->size2;
		state=PROTECT(alloc3DArray(REALSXP,ny,nt,M));
		cpuSeconds=PROTECT(NEW_NUMERIC(M));
		numSteps=PROTECT(NEW_INTEGER(M));
		Status=PROTECT(NEW_INTEGER(M));

		for (j=0; j<ny*nt*M; j++) REAL(state)[j]=NA_REAL; /* initialize to NA */
		/* here we initialize optional return values */
		/* each output element implies the ones following it (fisherInformation implies all the rest) */
		/* For this reason, the switch statement has no breaks.*/
		switch(OUTPUTS){ /* with fall-through on purpose */
		case output_fisher_information:
			FI=PROTECT(alloc3DArray(REALSXP,np,np,M));
			// fall-through
		case output_grad_log_likelihood:
			gll=PROTECT(allocMatrix(REALSXP,np,M));        /* gradient of log-likelihood */
			// fall-through
		case output_log_likelihood:
			ll=PROTECT(NEW_NUMERIC(M));                    /* log-likelihood */
			sy_k=malloc(sizeof(double)*ny*np*nt);          /* sensitivity of state: d state/dp */
			sf_k=malloc(sizeof(double)*nf*np*nt);          /* sensitivity of  func: d  func/dp */
			// fall-through
		case output_functions:
			func=PROTECT(alloc3DArray(REALSXP,nf,nt,M));   /* output functions */
			for (j=0;j<nf*nt*M;j++) REAL(func)[j]=NA_REAL;     /* initialize to NA */
		}
		for (k=0;k<M;k++){
			y=gsl_matrix_view_array(REAL(AS_NUMERIC(state))+(nt*ny*k),nt,ny);
			memcpy((double*) sys.params, REAL(AS_NUMERIC(parameters))+abs(nrows(parameters))*k, abs(nrows(parameters))*sizeof(double));
			update_initial_values(&initial_value.vector,sys,iv);
			ct0=clock();
			status=simulate_timeseries(
				sys,
				driver,
				t0,
				&(initial_value.vector),
				&(time.vector),
				ev,
				&(y.matrix),
				REAL(time_limit_seconds)[0],
				INTEGER(numSteps)+k
			);
			ct1=clock();
			REAL(cpuSeconds)[k] = sec(ct1-ct0);
			INTEGER(Status)[k] = status;
			if (status==GSL_SUCCESS) {
				p = gsl_matrix_row(P,i);
				if (OUTPUTS<=output_functions){
					for (j=0;j<nt;j++){
						f=REAL(func)+(0+j*nf+k*nf*nt);
						ODE_func(gsl_vector_get(&(time.vector),j),gsl_matrix_ptr(&(y.matrix),j,0),f,sys.params);
					}
				}
				if (OUTPUTS<=output_grad_log_likelihood){
					sensitivityApproximation(t0,&(time.vector),&(p.vector),&(y.matrix),sy_k,sf_k,saMem);
				}
				switch(OUTPUTS){
				case output_fisher_information:
					FisherInformation(REAL(FI)+(k*np*np),VECTOR_ELT(experiments,i),sf_k,Sf_sd);
				case output_grad_log_likelihood:
					gradLogLikelihood(REAL(gll)+(k*np),VECTOR_ELT(experiments,i),REAL(func)+(k*nf*nt),sf_k,np,v);
				case output_log_likelihood:
					REAL(ll)[k]=logLikelihood(VECTOR_ELT(experiments,i),REAL(func)+(k*nf*nt));
				}
			} else {
				REprintf(
					"[%s] simulation of provided parameters failed for experiment %i with error %i: %s\n",
					__func__,
					i,
					status,
					gsl_strerror(status)
				);
				switch(OUTPUTS){
				case output_fisher_information:
					memset(REAL(FI)+(k*np*np),0,sizeof(double)*np*np);
				case output_grad_log_likelihood:
					memset(REAL(gll)+(k*np),0,sizeof(double)*np);
				case output_log_likelihood:
					REAL(ll)[k] = -INFINITY;
				}
			}
		}
		if (OUTPUTS<=output_grad_log_likelihood){
			free(sy_k);
			free(sf_k);
		}
		yf_list=PROTECT(NEW_LIST(8));
		SET_VECTOR_ELT(yf_list,0,cpuSeconds);
		SET_VECTOR_ELT(yf_list,1,numSteps);
		SET_VECTOR_ELT(yf_list,2,Status);

		SET_VECTOR_ELT(yf_list,3,state);
		if (OUTPUTS <= output_functions){
			SET_VECTOR_ELT(yf_list,4,func);
		}
		if (OUTPUTS <= output_log_likelihood){
			SET_VECTOR_ELT(yf_list,5,ll);
		}
		if (OUTPUTS <= output_grad_log_likelihood){
			SET_VECTOR_ELT(yf_list,6,gll);
		}
		if (OUTPUTS <= output_fisher_information){
			SET_VECTOR_ELT(yf_list,7,FI);
		}
		set_names(yf_list,yf_names);
		SET_VECTOR_ELT(res_list,i,yf_list);
		event_free(&ev);

		UNPROTECT(1); /* yf_list */
		UNPROTECT(1); /* cpuSeconds */
		UNPROTECT(1); /* numSteps*/
		UNPROTECT(1); /* Status */
		UNPROTECT(1); /* state */
		if (OUTPUTS <= output_functions) UNPROTECT(1); /* func */
		switch (OUTPUTS){
		case output_fisher_information:
			UNPROTECT(1);
		case output_grad_log_likelihood:
			UNPROTECT(1);
		case output_log_likelihood:
			UNPROTECT(1);
		}
		if (status!=GSL_SUCCESS){
			REprintf("[%s] parameter set lead to solver errors (%s) in experiment %i/%zd\n",__func__,gsl_strerror(status),i,N);
		}
	} // experiments: 0 to N-1
	sensApproxMemFree(saMem); /* frees the memory of temporary matrices of sensitivity approximation */
	UNPROTECT(1); /* res_list */
	gsl_odeiv2_driver_free(driver);
	gsl_matrix_free(P);
	gsl_matrix_free(Y0);
	gsl_matrix_free(Sf_sd);
	gsl_vector_free(v);
	return res_list;
}

/* the parameter struct for CRNNs */
struct par {
	double *l;  /* log(k), rate coefficients */
	double *nu; /* stoichiometry (4×10)*/
	double *m;  /* modifiers (enzymes); not consumed or produced */
};


/* This function simulates a CRNN, for now, only one parameter set is used (M=1)
 * It is currently unclear in what way a series of simulations could be useful.
 * Would a matrix/array of parameters make sense in any way?
 */
Rdata /* the trajectories as a list (same size as experiments) */
r_gsl_odeiv2_outer_CRNN(
 Rdata modelName, /* a string */
 Rdata experiments, /* a list of simulation experiments */
 Rdata lparameters, /* a matrix of parameterization columns, in log-space */
 Rdata stoichiometry, /* a matrix of stoichiometric coefficients */
 Rdata modifiers, /* a matrix of rate modifiers (catalytic enzymes) */
 Rdata absolute_tolerance, /* absolute tolerance for GSL's solver */
 Rdata relative_tolerance, /* relative tolerance for GSL's solver */
 Rdata initial_step_size, /* initial guess for the step size */
 Rdata method, /* integration method (integer) */
 Rdata time_limit_seconds, /* time limit in seconds */
 Rdata nmax) /* nmax for the odeiv2 driver */
{
	gsl_set_error_handler_off();
	const gsl_odeiv2_step_type* step_types[] = {gsl_odeiv2_step_msbdf, gsl_odeiv2_step_msadams, gsl_odeiv2_step_bsimp, gsl_odeiv2_step_rk4imp, gsl_odeiv2_step_rk2imp, gsl_odeiv2_step_rk1imp, gsl_odeiv2_step_rk8pd, gsl_odeiv2_step_rkck, gsl_odeiv2_step_rkf45, gsl_odeiv2_step_rk4, gsl_odeiv2_step_rk2, NULL};
	const char* model_so=CHAR(asChar(getAttrib(modelName,install("comment"))));
	const char* model_name=CHAR(STRING_ELT(modelName,0));
	int i,j,k,status=GSL_SUCCESS;
	double abs_tol=asReal(absolute_tolerance);
	double rel_tol=asReal(relative_tolerance);
	double h=asReal(initial_step_size);
	R_xlen_t N=GET_LENGTH(experiments);
	size_t M=1; //dim(lparameters)[3];
	const gsl_odeiv2_step_type * T=step_types[asInteger(method)]; //gsl_odeiv2_step_msbdf;
	struct par p = {REAL(lparameters), REAL(stoichiometry), REAL(modifiers)}; /* l, nu, m */
	Rdata res_list = PROTECT(NEW_LIST(N)); /* use VECTOR_ELT and SET_VECTOR_ELT */
	SET_NAMES(res_list, GET_NAMES(experiments));
	Rdata yf_list, Y, F, iv, t, cpuSeconds, numSteps, Status;
	double t0;
	clock_t ct0, ct1;
	const char *yf_names[]={"state","func","cpuSeconds","numSteps","status",NULL};
	gsl_vector_view initial_value, time;
	gsl_matrix_view y;
	size_t nt;
	struct event *ev=NULL;
	double *f;
	gsl_odeiv2_system sys = load_system(model_name, model_so); /* also sets ODE_*() functions */
	if (sys.dimension == 0 || ODE_default==NULL || ODE_init==NULL){
		REprintf("[%s] loading model has failed (system dimension: «%zu»).\n",__func__,sys.dimension);
		UNPROTECT(1); /* res_list */
		return R_NilValue;
	}
	free(sys.params); /* auto-allocated by load system */
	sys.params = (void *) &p;
	size_t nf = abs(ODE_func(0,NULL,NULL,NULL));
	size_t ny = sys.dimension;
	size_t np = abs(nrows(lparameters)); /* also number of reactions */
	unsigned long NMAX=asInteger(nmax);
	gsl_odeiv2_driver *driver = gsl_odeiv2_driver_alloc_y_new(&sys,T,h,abs_tol,rel_tol);
	if (NMAX) gsl_odeiv2_driver_set_nmax(driver,NMAX);
	for (i=0; i<N; i++){
		iv = from_list(
			VECTOR_ELT(experiments,i),
			"initial_value initialState initialValue initialValues iv"
		);
		t = from_list(VECTOR_ELT(experiments,i),"time times t outputTimes");
		t0 = asReal(
			AS_NUMERIC(
				from_list(
					VECTOR_ELT(experiments,i),
					"intial_time initialTime t0 T0"
				)
			)
		);
		ev = event_from_R(
			from_list(
				VECTOR_ELT(experiments,i),
				"event events scheduledEvents scheduledEvent scheduled_event"
			)
		);
		initial_value = gsl_vector_view_array(REAL(iv),ny);
		nt = length(t);
		time=gsl_vector_view_array(REAL(AS_NUMERIC(t)),nt);
		Y=PROTECT(alloc3DArray(REALSXP,ny,nt,M));
		cpuSeconds=PROTECT(NEW_NUMERIC(M));
		numSteps=PROTECT(NEW_INTEGER(M));
		Status=PROTECT(NEW_INTEGER(M));
		for (j=0; j<ny*nt*M; j++) REAL(Y)[j]=NA_REAL; /* initialize to NA */
		F=PROTECT(alloc3DArray(REALSXP,nf,nt,M));
		for (j=0;j<nf*nt*M;j++) REAL(F)[j]=NA_REAL;   /* initialize to NA */
		for (k=0;k<M;k++){
			p.l = (REAL(lparameters)+np*2*k); /* number of reactions, fwd & bwd */
			y=gsl_matrix_view_array(REAL(AS_NUMERIC(Y))+(nt*ny*k),nt,ny);
			ct0=clock();
			status=simulate_timeseries(
				sys,
				driver,
				t0,
				&(initial_value.vector),
				&(time.vector),
				ev,
				&(y.matrix),
				REAL(time_limit_seconds)[0],
				INTEGER(numSteps)+k
			);
			ct1=clock();
			REAL(cpuSeconds)[k] = sec(ct1-ct0);
			INTEGER(Status)[k] = status;
			if (status==GSL_SUCCESS) {
				for (j=0;j<nt;j++){
					f=REAL(F)+(0 + j*nf + k*nf*nt);
					ODE_func(
						gsl_vector_get(&(time.vector),j),
						gsl_matrix_ptr(&(y.matrix),j,0),
						f,
						sys.params
					);
				}
			}
		}
		yf_list=PROTECT(NEW_LIST(5));
		SET_VECTOR_ELT(yf_list,0,Y);
		SET_VECTOR_ELT(yf_list,1,F);
		SET_VECTOR_ELT(yf_list,2,cpuSeconds);
		SET_VECTOR_ELT(yf_list,3,numSteps);
		SET_VECTOR_ELT(yf_list,4,Status);
		set_names(yf_list,yf_names);
		SET_VECTOR_ELT(res_list,i,yf_list);
		if (ev) event_free(&ev);
		UNPROTECT(6);
	} // experiments: 0 to N-1
	UNPROTECT(1); /* res_list */
	gsl_odeiv2_driver_free(driver);
	return res_list;
}
