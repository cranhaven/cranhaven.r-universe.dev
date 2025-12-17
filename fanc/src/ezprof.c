/*  ezprof.c
 *  - 2013/10/08
 *  - Haruhisa Nagata 
 */
#define USE_FC_LEN_T

#include <math.h>
#include <stdint.h>
#include <string.h>
//#include <time.h>
#include <sys/time.h>
#include <R.h>
#include <Rinternals.h>
#include "ezprof.h"

uint64_t get_nanotime(void) {
//	struct timespec ts;
//	return clock_gettime(CLOCK_MONOTONIC, &ts) == 0 ?
//		(uint64_t)ts.tv_sec * 1000000000 + ts.tv_nsec : (uint64_t)-1;
	struct timeval t;
	return gettimeofday(&t, NULL) == 0 ?
		(uint64_t)t.tv_sec * 1000000000 + t.tv_usec * 1000 : (uint64_t)-1;
}

void ezprof_init(struct ezprof_t *prof) {
	uint64_t t = get_nanotime();
	for (int i = 0; i < NUM_PROF_SLOT; i++) {
		prof->start[i] = t;
		prof->time[i] = 0;
		prof->name[i] = "";
	}
	prof->n = 0;
}

void ezprof_dispose(struct ezprof_t *prof) {
}

void ezprof_start(struct ezprof_t *prof, int index, const char *name) {
	if (index < 0 || index >= NUM_PROF_SLOT) return;
	if (index >= prof->n) prof->n = index + 1;
	prof->name[index] = name;
	prof->start[index] = get_nanotime();
}

void ezprof_stop(struct ezprof_t *prof, int index) {
	if (index < 0 || index >= NUM_PROF_SLOT) return;
	uint64_t t = get_nanotime();
	prof->time[index] += t - prof->start[index];
	prof->start[index] = t;
}

void ezprof_print(struct ezprof_t *prof) {
	for (int i = 0; i < prof->n; i++) {
		if (prof->name[i] == NULL || strcmp(prof->name[i], "") == 0) {
			Rprintf("%8.3fsec: %d\n", (double)(prof->time[i]), i + 1);
		} else {
			Rprintf("%8.3fsec: %s\n", (double)(prof->time[i]), prof->name[i]);
		}
	}
}

static int imin(int a, int b) {
	return a < b ? a : b;
}

SEXP ezprof_as_SEXP(struct ezprof_t *prof, SEXP R_time, SEXP R_name) {
	int n = imin(prof->n, length(R_time));
	for (int i = 0; i < n; i++) {
		REAL(R_time)[i] = (double)prof->time[i] / 1000000000L;
		if (prof->name[i] != NULL || strcmp(prof->name[i], "") == 0) {
			SET_STRING_ELT(R_name, i, mkChar(prof->name[i]));
		}
	}
	return R_time;
}
