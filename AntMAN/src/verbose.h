/*
 *  AntMAN Package
 *
 */


#ifndef ANTMAN_SRC_MIXTURE_CPP_VERBOSE_HPP_
#define ANTMAN_SRC_MIXTURE_CPP_VERBOSE_HPP_

#include <iostream>
#define VERBOSE_BINARY

inline int VERBOSE_LEVEL(int nv = -1) {
	static int v = 0;
	if (nv >= 0) v = nv;
	return v;
}

#ifdef HAS_RCPP
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#define COUT_STREAM Rcpp::Rcout
#define CERR_STREAM Rcpp::Rcerr
static inline void stop_cmd () {Rcpp::stop("Error inside the package.\n");}
#else
#include <cstdlib>
#define COUT_STREAM std::cout
#define CERR_STREAM std::cerr
static inline void stop_cmd () {abort();} // Commented for R Package
#endif



static inline void flush_output () {
#ifdef HAS_RCPP
	R_FlushConsole();
	R_ProcessEvents();
	R_CheckUserInterrupt();
#else
	COUT_STREAM << std::flush;
	CERR_STREAM << std::flush;
#endif

}



#define VERBOSE_COLOR true

#define PURPLE_COLOR (VERBOSE_COLOR?"\033[1;35m":"")
#define RED_COLOR    (VERBOSE_COLOR?"\033[1;31m":"")
#define YELLOW_COLOR (VERBOSE_COLOR?"\033[0;33m":"")
#define GREEN_COLOR  (VERBOSE_COLOR?"\033[1;32m":"")
#define BLUE_COLOR   (VERBOSE_COLOR?"\033[1;34m":"")
#define RESET_COLOR  (VERBOSE_COLOR?"\033[0m":"")

#define EXTRA_LEVEL   4
#define DEBUG_LEVEL   3
#define INFO_LEVEL    2
#define LOG_LEVEL     1
#define WARNING_LEVEL 1
#define ERROR_LEVEL   0

#define VERBOSE_GENERIC_MSG(thr, out, color, msg)      {if (VERBOSE_LEVEL() >= thr)    out  << "[" << thr << "] " << "[" << __FILE__ << ":" << __LINE__ << "] " << color  << msg << RESET_COLOR << std::endl;              };
#define VERBOSE_GENERIC_END(thr, out, color, msg)      {VERBOSE_GENERIC_MSG(thr, out, color, msg); stop_cmd () ;};


#ifdef VERBOSE_BINARY
#define VERBOSE_EXTRA(msg)                          VERBOSE_GENERIC_MSG(EXTRA_LEVEL,    CERR_STREAM, BLUE_COLOR,  msg)
#define VERBOSE_DEBUG(msg)                          VERBOSE_GENERIC_MSG(DEBUG_LEVEL,    CERR_STREAM, BLUE_COLOR,  msg)
#else
#define VERBOSE_EXTRA(msg)                         {};
#define VERBOSE_DEBUG(msg)                         {};
#endif

#define VERBOSE_INFO(msg)                            VERBOSE_GENERIC_MSG(INFO_LEVEL,    CERR_STREAM, GREEN_COLOR,  msg)
#define VERBOSE_LOG(msg)                             VERBOSE_GENERIC_MSG(LOG_LEVEL,     CERR_STREAM, RESET_COLOR,  msg)
#define VERBOSE_WARNING(msg)                         VERBOSE_GENERIC_MSG(WARNING_LEVEL, CERR_STREAM, YELLOW_COLOR, msg)
#define VERBOSE_ERROR(msg)                           VERBOSE_GENERIC_END(ERROR_LEVEL,   CERR_STREAM, RED_COLOR,    msg)
#define VERBOSE_ASSERT(test,msg)  {if (not (test)) { VERBOSE_GENERIC_END(ERROR_LEVEL,   CERR_STREAM, RED_COLOR,    msg) }};


static int const VERBOSE_PROGRESS_BAR_SIZE =  51;

static inline void VERBOSE_PROGRESS_START()    {
	if (not (VERBOSE_LEVEL() >= LOG_LEVEL)) {return;}
	CERR_STREAM << "0%   10   20   30   40   50   60   70   80   90   100%" << std::endl;
    CERR_STREAM << "[----|----|----|----|----|----|----|----|----|----|" << std::endl;
}
static inline void VERBOSE_PROGRESS_UPDATE(int v)  {
	if (not (VERBOSE_LEVEL() >= LOG_LEVEL)) {return;}
	const int progress = (v * VERBOSE_PROGRESS_BAR_SIZE) / 100;
	CERR_STREAM << std::string(progress, '\r')
	            <<  std::string(progress, '*') ;
	flush_output ();
}
static inline void VERBOSE_PROGRESS_STOP()   {
	if (not (VERBOSE_LEVEL() >= LOG_LEVEL)) {return;}
	CERR_STREAM << "" << std::endl;
}





#endif /* ANTMAN_SRC_MIXTURE_CPP_VERBOSE_HPP_ */
