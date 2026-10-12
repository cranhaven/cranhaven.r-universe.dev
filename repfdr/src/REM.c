#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <pthread.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rversion.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

// due to an extremely annoying bug, I am not using joinable threads
#define JOIN_THREADS_MANUALLY



// BUILD NOTE:
// On Windows, need to compile with R CMD SHLIB REM.c -pthread

// Globals (this is a standalone module so pardon my French)
static int* H_3d;
static double* pi_initial;
static double* pz_mat;
static int* binned_z;

static int max_iter;
static double tol;

static int nr_outcomes;
static int nr_studies;
static int nr_bins;
static int nr_tests;

static int nr_threads;
static int e_step_batch_size;
static int m_step_batch_size;
static int verbose;

int EMi;

static double* pi_EM;

static double* pr_h_mat;
static double* pr_h_mat_col_norms;

// This is a naive initial implementation that just mirrors exactly what Ruth and Dani do in R
#define ACCESS_H_3d(i, j) H_3d[(i) + nr_outcomes * (j)]
#define ACCESS_pz_mat(i, j, k) pz_mat[(i) + nr_studies * ((j) + nr_bins * (k))]
#define ACCESS_binned_z(i, j) binned_z[(i) + nr_tests * (j)]
#define ACCESS_pr_h_mat(i, j) pr_h_mat[(i) + nr_outcomes * (j)]
#define ACCESS_pi_EM(i, j) pi_EM[(i) + nr_outcomes * (j)]

// Functions invocated as threads
void* e_step(void* worker_args) {
	int i, j, k;
	double term;

	int first_test = ((int*)worker_args)[0];
	int last_test  = ((int*)worker_args)[1];

	for (k = first_test; k <= last_test; ++k) {
		pr_h_mat_col_norms[k] = 0;

		for (i = 0; i < nr_outcomes; ++i) {
			term = 1.0;

			for (j = 0; j < nr_studies; ++j) {
				term *= ACCESS_pz_mat(j, ACCESS_binned_z(k, j), ACCESS_H_3d(i, j));
			}

			ACCESS_pr_h_mat(i, k) = term * ACCESS_pi_EM(i, EMi - 1);

			pr_h_mat_col_norms[k] += ACCESS_pr_h_mat(i, k);
		}
    }
	
	((int*)worker_args)[2] = 1; // signals we are done and thread can be manually joined
	return NULL;
}

void* m_step(void* worker_args) {
	int i, k;

  int first_outcome = ((int*)worker_args)[0];
	int last_outcome  = ((int*)worker_args)[1];

	for (i = first_outcome; i <= last_outcome; ++i) {
		ACCESS_pi_EM(i, EMi) = 0;

		for (k = 0; k < nr_tests; ++k) {
			ACCESS_pi_EM(i, EMi) += ACCESS_pr_h_mat(i, k) / pr_h_mat_col_norms[k];
		}
	}

  ((int*)worker_args)[2] = 1; // signals we are done and thread can be manually joined
	return NULL;
}

// Main entry point called from R
SEXP REM(SEXP R_H_3d, SEXP R_pi_initial, SEXP R_pz_mat, SEXP R_binned_z, SEXP R_max_iter, SEXP R_tol, SEXP R_nr_threads, SEXP R_verbose) {
	int i;
	double s, mad, ad;

	H_3d = INTEGER(R_H_3d);
	pi_initial = REAL(R_pi_initial);
	pz_mat = REAL(R_pz_mat);
	binned_z = INTEGER(R_binned_z);
	max_iter = *INTEGER(R_max_iter);
	tol = *REAL(R_tol);
  nr_threads = *INTEGER(R_nr_threads);
  verbose = *INTEGER(R_verbose);

	SEXP Rdim = getAttrib(R_H_3d, R_DimSymbol);
	nr_outcomes = INTEGER(Rdim)[0];
	nr_studies  = INTEGER(Rdim)[1];

	Rdim = getAttrib(R_pz_mat, R_DimSymbol);
	nr_bins = INTEGER(Rdim)[1];

	Rdim = getAttrib(R_binned_z, R_DimSymbol);
	nr_tests = INTEGER(Rdim)[0];

	// Allocate matrix of estimated probabilities (we also track the
	// solution as it evolves through the EM iterations).
	SEXP R_pi_EM;
	PROTECT(R_pi_EM = allocMatrix(REALSXP, nr_outcomes, max_iter + 1)); // in the last column I'll pass control info
	pi_EM = REAL(R_pi_EM);

	// Allocate missing data matrix: probabilities of each outcome
	// for each test repeated in all studies.
	pr_h_mat = (double*)R_alloc(nr_outcomes * nr_tests, sizeof (double));

	// To optimize the computation I'm holding the pre-test sums in the current E-step
	pr_h_mat_col_norms = (double*)R_alloc(nr_tests, sizeof (double));

  if (nr_threads <= 0) {
#ifdef _WIN32
  	SYSTEM_INFO sysinfo;
	  GetSystemInfo(&sysinfo);
	  nr_threads = sysinfo.dwNumberOfProcessors;
#else
	  nr_threads = sysconf(_SC_NPROCESSORS_ONLN);
#endif
  }

#ifdef JOIN_THREADS_MANUALLY  
  pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
#else
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
#endif

	pthread_t* worker_threads = (pthread_t*)R_alloc(nr_threads, sizeof (pthread_t));
	e_step_batch_size = floor((double)nr_tests / nr_threads);
	m_step_batch_size = floor((double)nr_outcomes / nr_threads);

	int* worker_args = (int*)R_alloc(3 * nr_threads, sizeof (int));

	// Initialize to specified starting values
	for (i = 0; i < nr_outcomes; ++i) {
		ACCESS_pi_EM(i, 0) = pi_initial[i];
	}

	// E-M iterations
	for (EMi = 1; EMi < max_iter; ++EMi) {
		if (verbose) Rprintf("EM iteration %d\n", EMi + 1);

		// E-step
		for (i = 0; i < nr_threads; ++i) {
			worker_args[3 * i + 0] = i * e_step_batch_size;
			worker_args[3 * i + 1] = (i < nr_threads - 1) ? ((i + 1) * e_step_batch_size - 1) : (nr_tests - 1);
			worker_args[3 * i + 2] =  0;

			//if (verbose) Rprintf("   spawning a thread for e-step batch (%d, %d)\n", e_step_args[0], e_step_args[1]);

#ifdef JOIN_THREADS_MANUALLY  
			pthread_create(&(worker_threads[i]), &attr, e_step, (void*) (&worker_args[3 * i]));
#else
  		pthread_create(&(worker_threads[i]),  NULL, e_step, (void*) (&worker_args[3 * i]));
#endif
		}

#ifdef JOIN_THREADS_MANUALLY  
  	// Manually join the worker threads (might be unnecessarily slow for small datasets)
		int all_done = 0;
		while (!all_done) {
			all_done = 1;
			for (int t = 0; t < nr_threads; ++t) {
				all_done &= worker_args[3 * t + 2];
			}

#ifdef _WIN32
			Sleep(1);
#else
			usleep(1);
#endif
		}
#else
    for (i = 0; i < nr_threads; ++i) {
			pthread_join(worker_threads[i], NULL);
		}
#endif

		// M-step
		for (i = 0; i < nr_threads; ++i) {
			worker_args[3 * i + 0] = i * m_step_batch_size;
			worker_args[3 * i + 1] = (i < nr_threads - 1) ? ((i + 1) * m_step_batch_size - 1) : (nr_outcomes - 1);
			worker_args[3 * i + 2] = 0;

			//if (verbose) Rprintf("   spawning a thread for m-step batch (%d, %d)\n", m_step_batch_range[0], m_step_batch_range[1]);

#ifdef JOIN_THREADS_MANUALLY  
			pthread_create(&(worker_threads[i]), &attr, m_step, (void*) (&worker_args[3 * i]));
#else
  		pthread_create(&(worker_threads[i]),  NULL, m_step, (void*) (&worker_args[3 * i]));
#endif
		}

#ifdef JOIN_THREADS_MANUALLY  
  	// Manually join the worker threads (might be unnecessarily slow for small datasets)
		all_done = 0;
		while (!all_done) {
			all_done = 1;
			for (int t = 0; t < nr_threads; ++t) {
				all_done &= worker_args[3 * t + 2];
			}

#ifdef _WIN32
			Sleep(1);
#else
			usleep(1);
#endif
		}
#else
		for (i = 0; i < nr_threads; ++i) {
			pthread_join(worker_threads[i], NULL);
		}
#endif

		s = 0;
		for (i = 0; i < nr_outcomes; ++i) {
			s += ACCESS_pi_EM(i, EMi);
		}

		mad = -1;
		for (i = 0; i < nr_outcomes; ++i) {
			ACCESS_pi_EM(i, EMi) /= s;

			ad = fabs(ACCESS_pi_EM(i, EMi) - ACCESS_pi_EM(i, EMi - 1));
			if (ad > mad) {
				mad = ad;
			}
		}

		// Check for convergence
		if (mad < tol) {
			break;
		}
	}

	// Pass control information in the last column
	ACCESS_pi_EM(0, max_iter) = EMi;

	UNPROTECT(1);

	return(R_pi_EM);
}
