/* LocalMem.h - local memory routines. 
 * 
 * These routines are meant for the sort of scenario where
 * a lot of little to medium size pieces of memory are
 * allocated, and then disposed of all at once.
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#ifndef LOCALMEM_H
#define LOCALMEM_H

struct lm *lmInit(int blockSize);
/* Create a local memory pool. Parameters are:
 *      blockSize - how much system memory to allocate at a time.  Can
 *                  pass in zero and a reasonable default will be used.
 */

void lmCleanup(struct lm **pLm);
/* Clean up a local memory pool. */

void *lmAlloc(struct lm *lm, size_t size);
/* Allocate memory from local pool. */

#define lmAllocVar(lm, pt) (pt = lmAlloc(lm, sizeof(*pt)));
/* Shortcut to allocating a single variable in local mem and
 * assigning pointer to it. */

#define lmAllocArray(lm, pt, size) (pt = lmAlloc(lm, sizeof(*pt) * (size)))
/* Shortcut to allocating an array in local mem and
 * assigning pointer to it. */

#endif//ndef LOCALMEM_H
