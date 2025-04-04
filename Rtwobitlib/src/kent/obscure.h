/* Obscure.h  - stuff that's relatively rarely used
 * but still handy. 
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#ifndef OBSCURE_H
#define OBSCURE_H

int digitsBaseTwo(unsigned long x);
/* Return base two # of digits. */

void writeGulp(char *file, char *buf, int size);
/* Write out a bunch of memory. */

int ptToInt(void *pt);
/* Convert pointer to integer.  Use when really want to store a
 * pointer in an int. */

#endif /* OBSCURE_H */
