/*  ezprof.h
 *  - 2013/10/08
 *  - Haruhisa Nagata 
 */

#ifndef EZPROF_H
#define EZPROF_H

#include <stdint.h>
#include <R.h>
#include <Rinternals.h>

#define NUM_PROF_SLOT 64
struct ezprof_t {
	int n;
	uint64_t start[NUM_PROF_SLOT];
	uint64_t time[NUM_PROF_SLOT];
	const char *name[NUM_PROF_SLOT];
};

void ezprof_init(struct ezprof_t *prof);
void ezprof_dispose(struct ezprof_t *prof);
void ezprof_start(struct ezprof_t *prof, int index, const char *name);
void ezprof_stop(struct ezprof_t *prof, int index);
void ezprof_print(struct ezprof_t *prof);
SEXP ezprof_as_SEXP(struct ezprof_t *prof, SEXP R_time, SEXP R_name);

#endif // EZPROF_H
