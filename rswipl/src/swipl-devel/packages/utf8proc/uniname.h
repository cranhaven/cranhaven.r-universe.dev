/* Public interface to the compact Unicode name table.  See uniname.c
 * for the implementation and uniname_data.h for the table format.
 */

#ifndef UNINAME_H_INCLUDED
#define UNINAME_H_INCLUDED

#include <stdint.h>
#include <stddef.h>
#include "uniname_data.h"

/* code point -> name: writes a NUL-terminated name into buf and
 * returns its length, or 0 if cp has no Unicode name.
 */
size_t uniname_get(uint32_t cp, char *buf, size_t buflen);

/* name -> code point: returns 1 and sets *cp, or 0.  Semidet. */
int    uniname_lookup(const char *name, uint32_t *cp);

/* Enumeration over every named code point. */
typedef struct uniname_iter
{ const uint8_t *p;
  uint32_t       cur;
  unsigned       ri;
  uint32_t       rc;
  int            phase;
} uniname_iter;

void uniname_iter_init(uniname_iter *it);
int  uniname_iter_next(uniname_iter *it, uint32_t *cp,
                       char *buf, size_t buflen);

#endif /*UNINAME_H_INCLUDED*/
