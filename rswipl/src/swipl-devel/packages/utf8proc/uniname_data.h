/* Declarations for the compact Unicode name table.
 *
 * GENERATED FILE -- DO NOT EDIT.
 * Regenerate with `ninja regen-uniname` in the build dir.
 *
 * Source: UnicodeData.txt, Unicode 17.0.0.
 * Input file vendored under packages/utf8proc/data/.
 */

#ifndef UNINAME_DATA_H_INCLUDED
#define UNINAME_DATA_H_INCLUDED

#include <stdint.h>
#include <stddef.h>

#define UNINAME_UNICODE_VERSION "17.0.0"
#define UNINAME_WORD_COUNT   13795
#define UNINAME_RANGE_COUNT  23
#define UNINAME_PREFIX_COUNT 7
#define UNINAME_NAMED_COUNT  34594
#define UNINAME_WORDS_LEN    101152
#define UNINAME_NAMES_LEN    267267

#define UNINAME_K_HEX    0  /* PREFIX + '-' + upperhex(cp) */
#define UNINAME_K_HANGUL 1  /* HANGUL SYLLABLE <jamo>      */

/* uniname_names is a flat byte stream.  For each explicit name,
 * in ascending code-point order: an unsigned-LEB128 code-point
 * delta, then one unsigned-LEB128 (word-id+1) per word, then a
 * 0x00 terminator.  uniname_words is the word table, NUL-
 * separated in ascending id (descending frequency) order.
 */

typedef struct {
    uint32_t start;
    uint32_t end;
    uint16_t prefix;   /* index into uniname_prefixes */
    uint8_t  kind;
} uniname_range_t;

extern const char            uniname_words[UNINAME_WORDS_LEN];
extern const char            uniname_prefixes[];
extern const uniname_range_t uniname_ranges[UNINAME_RANGE_COUNT];
extern const uint8_t         uniname_names[UNINAME_NAMES_LEN];

#endif /*UNINAME_DATA_H_INCLUDED*/
