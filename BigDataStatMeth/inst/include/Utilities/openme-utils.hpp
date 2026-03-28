/**
 * @file openme-utils.hpp
 * @brief OpenMP threading utilities and configuration management
 *
 * This file provides comprehensive thread management utilities for OpenMP
 * parallelization in the BigDataStatMeth package. It handles thread
 * initialization, configuration, forking behavior, and runtime thread
 * count determination based on system resources and environment variables.
 *
 * Key features:
 * - Dynamic thread count management
 * - Environment variable configuration
 * - Fork-safe thread handling
 * - Thread throttling for performance optimization
 * - System resource detection and limits
 *
 * Environment variables:
 * - R_DATATABLE_NUM_THREADS: Direct thread count control
 * - R_DATATABLE_NUM_PROCS_PERCENT: CPU utilization percentage
 * - R_DATATABLE_THROTTLE: Thread activation threshold
 * - OMP_THREAD_LIMIT: OpenMP thread limit
 * - OMP_NUM_THREADS: OpenMP thread count
 *
 * @note This implementation prioritizes safe and efficient thread management
 * while respecting system and user-defined constraints.
 */

#include <Rcpp.h>
#include "H5Cpp.h"

#ifdef _OPENMP
#include <pthread.h>
#endif
#include <errno.h>     // errno
#include <ctype.h>     // isspace


#ifndef _INITVARS
#define _INITVARS

/**
 * @brief Global thread management variables
 * @{
 */
static int  DTthreads = -1;    //!< Current thread count, initialized to -1
static int  DTthrottle = -1;   //!< Thread activation threshold
static bool RestoreAfterFork = true; //!< Thread restoration behavior after fork
/** @} */

/**
 * @brief Retrieves integer value from environment variable
 *
 * @param name Environment variable name
 * @param def Default value if not found or invalid
 * @return int Parsed value or default
 *
 * @details
 * - Handles empty or missing environment variables
 * - Validates numeric format
 * - Ensures positive integer values
 * - Issues warning for invalid values
 */
static int getIntEnv(const char *name, int def);
    
    
static int getIntEnv(const char *name, int def)
{
    const char *val = getenv(name);
    if (val==NULL) return def;
    size_t nchar = strlen(val);
    if (nchar==0) return def;
    char *end;
    errno = 0;
    long int ans = strtol(val, &end, 10);  // ignores leading whitespace. If it fully consumed the string, *end=='\0' and isspace('\0')==false
    while (isspace(*end)) end++;  // ignore trailing whitespace
    if (errno || (size_t)(end-val)!=nchar || ans<1 || ans>INT_MAX) {
        Rcpp::warning(("Ignoring invalid %s==\"%s\". Not an integer >= 1. Please remove any characters that are not a digit [0-9]."), name, val);
        return def;
        
    }
    return (int)ans;
}
    
/**
 * @brief Minimum of two integers
 * @param a First integer
 * @param b Second integer
 * @return int Smaller value
 */
static inline int imin(int a, int b) { return a < b ? a : b; }

/**
 * @brief Maximum of two integers
 * @param a First integer
 * @param b Second integer
 * @return int Larger value
 */
static inline int imax(int a, int b) { return a > b ? a : b; }

#endif 


#ifndef _INITDTTHREADS
#define _INITDTTHREADS

/**
 * @brief Initializes thread configuration based on environment and system state
 *
 * @details Thread count determination algorithm:
 * 1. Check R_DATATABLE_NUM_THREADS
 * 2. If unset, use R_DATATABLE_NUM_PROCS_PERCENT
 * 3. Apply system limits (num_procs, thread_limit)
 * 4. Honor OpenMP settings
 * 5. Ensure at least one thread
 *
 * Environment variable hierarchy:
 * 1. R_DATATABLE_NUM_THREADS
 * 2. R_DATATABLE_NUM_PROCS_PERCENT
 * 3. OMP_THREAD_LIMIT
 * 4. OMP_NUM_THREADS
 *
 * @note Called at package startup and by setDTthreads()
 */
inline void initDTthreads() {
    // called at package startup from init.c
    // also called by setDTthreads(threads=NULL) (default) to reread environment variables; see setDTthreads below
    // No verbosity here in this setter. Verbosity is in getDTthreads(verbose=TRUE)
    int ans = getIntEnv("R_DATATABLE_NUM_THREADS", INT_MIN);
    if (ans>=1) {
        ans = imin(ans, omp_get_num_procs());  // num_procs is a hard limit; user cannot achieve more. ifndef _OPENMP then myomp.h defines this to be 1
    } else {
        // Only when R_DATATABLE_NUM_THREADS is unset (or <=0) do we use PROCS_PERCENT; #4514
        int perc = getIntEnv("R_DATATABLE_NUM_PROCS_PERCENT", 50); // use "NUM_PROCS" to use the same name as the OpenMP function this uses
        // 50% of logical CPUs by default; half of 8 is 4 on laptop with 4 cores. Leaves plenty of room for other processes: #3395 & #3298
        if (perc<=1 || perc>100) {
            Rcpp::warning(("Ignoring invalid R_DATATABLE_NUM_PROCS_PERCENT==%d. If used it must be an integer between 2 and 100. Default is 50. See ?setDTtheads."), perc);
            // not allowing 1 is to catch attempts to use 1 or 1.0 to represent 100%.
            perc = 50;
        }
        ans = imax(omp_get_num_procs()*perc/100, 1); // imax for when formula would result in 0.
    }
    ans = imin(ans, omp_get_thread_limit());  // honors OMP_THREAD_LIMIT when OpenMP started; e.g. CRAN sets this to 2. Often INT_MAX meaning unlimited/unset
    ans = imin(ans, omp_get_max_threads());   // honors OMP_NUM_THREADS when OpenMP started, plus reflects any omp_set_* calls made since
    // max_threads() -vs- num_procs(): https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/302866
    ans = imin(ans, getIntEnv("OMP_THREAD_LIMIT", INT_MAX));  // user might expect `Sys.setenv(OMP_THREAD_LIMIT=2);setDTthreads()` to work. Satisfy this
    ans = imin(ans, getIntEnv("OMP_NUM_THREADS", INT_MAX));   //   expectation by reading them again now. OpenMP just reads them on startup (quite reasonably)
    ans = imax(ans, 1);  // just in case omp_get_* returned <=0 for any reason, or the env variables above are set <=0
    DTthreads = ans;
    DTthrottle = imax(1, getIntEnv("R_DATATABLE_THROTTLE", 1024)); // 2nd thread is used only when n>1024, 3rd thread when n>2048, etc
}
#endif 

#ifndef _GETDTHREADS
#define _GETDTHREADS

/**
 * @brief Determines number of threads for parallel operations
 *
 * @param n Number of iterations/items to process
 * @param throttle Whether to apply thread throttling
 * @return int Number of threads to use
 *
 * @details Thread allocation strategy:
 * - With throttling: Incremental thread activation based on workload
 * - Without throttling: Direct allocation based on available threads
 * - Respects global thread limits
 *
 * Throttling behavior:
 * - One thread for n ≤ DTthrottle
 * - Two threads for n ≤ 2*DTthrottle
 * - And so on up to DTthreads maximum
 */
inline int getDTthreads(const int64_t n, const bool throttle) {
    
    initDTthreads();
    
    // throttle==true  : a number of iterations per thread (DTthrottle) is applied before a second thread is utilized
    // throttle==false : parallel region is already pre-chunked such as in fread; e.g. two batches intended for two threads
    if (n<1) return 1; // 0 or negative could be deliberate in calling code for edge cases where loop is not intended to run at all
    int64_t ans = throttle ? 1+(n-1)/DTthrottle :  // 1 thread for n<=1024, 2 thread for n<=2048, etc
        n;                    // don't use 20 threads for just one or two batches
    return ans>=DTthreads ? DTthreads : (int)ans;  // apply limit in static local DTthreads saved there by initDTthreads() and setDTthreads()
}
#endif 


#ifndef _MYGETENV
#define _MYGETENV

/**
 * @brief Safe environment variable retrieval with default
 *
 * @param name Environment variable name
 * @param unset Default value if not found
 * @return const char* Value or default string
 */
static const char *mygetenv(const char *name, const char *unset) {
    const char *ans = getenv(name);
    return (ans==NULL || ans[0]=='\0') ? unset : ans;
}
#endif 

#ifndef _GETDTHREADS_R
#define _GETDTHREADS_R

/**
 * @brief R interface for thread configuration inspection
 *
 * @param verbose Whether to print detailed configuration
 * @return SEXP Integer SEXP with thread count
 *
 * @details When verbose:
 * - Prints OpenMP version
 * - Shows processor count
 * - Lists all relevant environment variables
 * - Displays current thread settings
 */
inline SEXP getDTthreads_R(SEXP verbose) {
    if(!IS_TRUE_OR_FALSE(verbose))
        Rf_error(("%s must be TRUE or FALSE"), "verbose");
    if (LOGICAL(verbose)[0]) {
#ifndef _OPENMP
        Rprintf(("This installation of BigDataStatMeth has not been compiled with OpenMP support.\n"));
#else
        Rprintf(("  OpenMP version (_OPENMP)       %d\n"), _OPENMP); // user can use Google to map 201511 to 4.5; it's odd that OpenMP API does not provide 4.5
#endif
        Rprintf(("  omp_get_num_procs()            %d\n"), omp_get_num_procs());
        Rprintf(("  R_DATATABLE_NUM_PROCS_PERCENT  %s\n"), mygetenv("R_DATATABLE_NUM_PROCS_PERCENT", "unset (default 50)"));
        Rprintf(("  R_DATATABLE_NUM_THREADS        %s\n"), mygetenv("R_DATATABLE_NUM_THREADS", "unset"));
        Rprintf(("  R_DATATABLE_THROTTLE           %s\n"), mygetenv("R_DATATABLE_THROTTLE", "unset (default 1024)"));
        Rprintf(("  omp_get_thread_limit()         %d\n"), omp_get_thread_limit());
        Rprintf(("  omp_get_max_threads()          %d\n"), omp_get_max_threads());
        Rprintf(("  OMP_THREAD_LIMIT               %s\n"), mygetenv("OMP_THREAD_LIMIT", "unset"));  // CRAN sets to 2
        Rprintf(("  OMP_NUM_THREADS                %s\n"), mygetenv("OMP_NUM_THREADS", "unset"));
        Rprintf(("  RestoreAfterFork               %s\n"), RestoreAfterFork ? "true" : "false");
        Rprintf(("  BigDataStatMeth is using %d threads with throttle==%d.\n"), getDTthreads(INT_MAX, false), DTthrottle);
    }
    return Rf_ScalarInteger(getDTthreads(INT_MAX, false));
}
#endif


#ifndef _PREFORKDTTHREADS
#define _PREFORKDTTHREADS
    static int pre_fork_DTthreads = 0;
#endif

#ifndef _WHENFORK
#define _WHENFORK
    inline void when_fork() {
        pre_fork_DTthreads = DTthreads;
        DTthreads = 1;
    }
#endif

#ifndef _AFTERFORK
#define _AFTERFORK
    inline void after_fork() {
        if (RestoreAfterFork) DTthreads = pre_fork_DTthreads;
    }
#endif


#ifndef _AVOID_OPENMP_HANG
#define _AVOID_OPENMP_HANG
    inline void avoid_openmp_hang_within_fork() {
        // Called once on loading BigDataStatMeth from init.c
    #ifdef _OPENMP
        pthread_atfork(&when_fork, &after_fork, NULL);
    #endif
    }
#endif
    
    
#ifndef _GET_FINAL_THREADS
#define _GET_FINAL_THREADS
    inline unsigned int get_number_threads(Rcpp::Nullable<int> threads, Rcpp::Nullable<bool> bparal) {
        
        // unsigned int ithreads = std::thread::hardware_concurrency();
        unsigned int ithreads = getDTthreads(INT_MAX, false);
        
        if( bparal.isNotNull() ) {
            if(Rcpp::as<bool>(bparal) == false) {
                ithreads = 1;
                return(ithreads);
            }
        }
        
        if(threads.isNotNull()) {
            if (Rcpp::as<int> (threads) <= (int)ithreads){
                ithreads = Rcpp::as<int> (threads);
            }
        } else {
            ithreads =  getDTthreads(0, false);
        }    
        
        return(ithreads);
    }
#endif
