/* Obscure stuff that is handy every now and again. 
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#include "common.h"
#include "obscure.h"

int digitsBaseTwo(unsigned long x)
/* Return base two # of digits. */
{
int digits = 0;
while (x)
    {
    digits += 1;
    x >>= 1;
    }
return digits;
}

void writeGulp(char *file, char *buf, int size)
/* Write out a bunch of memory. */
{
FILE *f = mustOpen(file, "w");
mustWrite(f, buf, size);
carefulClose(&f);
}

int ptToInt(void *pt)
/* Convert pointer to integer.  Use when really want to store a
 * pointer in an int. */
{
char *a = NULL, *b = pt;
return b - a;
}

