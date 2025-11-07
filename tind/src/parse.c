/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 *****************************
 * parsing character strings *
 *****************************
 */


#include <string.h>
#include "parse.h"
#include "calendar.h"
#include "datetime.h"
#include "tdiff.h"
#include "compiler.h"


#ifndef R_SHORT_LEN_MAX /* will be the case when there's no long vector support */
#define R_SHORT_LEN_MAX 2147483647
#endif /* R_SHORT_LEN_MAX */




// ==================================================================
// parsing character strings
// ==================================================================

// is_
// ========================================================

static inline
TIND__ATTRIBUTE_INLINE
int
is_digit(char c)
{
    return (c >= '0') && (c <= '9');
}


static inline
TIND__ATTRIBUTE_INLINE
int
is_alpha(char c)
{
    return ((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z')) ||
           (c & 0x80); // NOTE: a shortcut to handle non-ASCII characters (letters).
}


// skip_
// ========================================================

static inline
TIND__ATTRIBUTE_INLINE
void
skip_ws(const char **c)
{
    while (**c == ' ' || **c == '_' || **c == '\t' || **c == '\n') ++*c;
}


static inline
TIND__ATTRIBUTE_INLINE
void
skip_non_alnum(const char **c)
{
    while ((**c) && !is_digit(**c) && !is_alpha(**c)) ++*c;
}


static inline
TIND__ATTRIBUTE_INLINE
void
skip_non_num(const char **c)
{
    while ((**c) && !is_digit(**c)) ++*c;
}


// check NA and empty strings
// ========================================================

static inline
TIND__ATTRIBUTE_INLINE
int
is_na_empty(const char *c)
{
    skip_ws(&c);
    if (!*c) return 1;
    if (strncmp(c, "NA", 2)) return 0; else c += 2;
    skip_ws(&c);
    return !*c;
}


// parse numbers
// ========================================================

static inline
TIND__ATTRIBUTE_INLINE
int
parse_ni(const char **c, int n, int exact)
{
    const char *c0 = *c;
    int i = n - 1, buf = **c - '0';
    ++*c;
    while (i && is_digit(**c)) {
        buf *= 10;
        buf += **c - '0';
        ++*c; --i;
    }
    if (exact && i) { *c = c0; return -1; }
    return buf;
}


static inline
TIND__ATTRIBUTE_INLINE
double
parse_nd(const char **c, int n, int exact)
{
    const char *c0 = *c;
    int i = n - 1, buf = **c - '0', buf1;
    ++*c;
    while (i && is_digit(**c)) {
        buf *= 10;
        buf += **c - '0';
        ++*c; --i;
    }
    if (exact && i) { *c = c0; return -1.; }
    if (**c != '.') return (double) buf;
    if (!is_digit(*(++*c))) return (double) buf;
    buf1 = *((*c)++) - '0';
    i = 1;
    while (is_digit(**c) && (i < 6)) {
        buf1 *= 10;
        buf1 += **c - '0';
        ++*c; ++i;
    }
    while (is_digit(**c)) ++*c;
    switch (i) {
        case 1: return (double) buf + (double) buf1 / 10.;
        case 2: return (double) buf + (double) buf1 / 100.;
        case 3: return (double) buf + (double) buf1 / 1000.;
        case 4: return (double) buf + (double) buf1 / 10000.;
        case 5: return (double) buf + (double) buf1 / 100000.;
        default: ;
    }
    return (double) buf + (double) buf1 / 1000000.;
}



// parsing names
// ========================================================

// match character strings (month names, weekday names)
static inline
TIND__ATTRIBUTE_INLINE
int
match_str(const char **x, SEXP table, int start, int length)
{
    int n = 0;
    int m = 0;
    const char *x0 = *x, *t;

    while (is_alpha(**x) || (**x == '.')) { ++*x; ++n; }

    for (int i = 0; i < length; ++i) {
        t = CHAR(STRING_ELT(table, start + i));
        if (strlen(t) != n) continue;
        if (!strncmp(x0, t, n)) { m = i + 1; break; }
    }

    if (!m) *x = x0;

    return m;
}


// match am/pm indicators
static inline
TIND__ATTRIBUTE_INLINE
int
match_ampm(const char **x, SEXP table, int start)
{
    // am / pm
    if (!strncmp(*x, "am", 2)) { *x += 2; return 1; }
    if (!strncmp(*x, "pm", 2)) { *x += 2; return 2; }
    // AM / PM
    if (!strncmp(*x, "AM", 2)) { *x += 2; return 1; }
    if (!strncmp(*x, "PM", 2)) { *x += 2; return 2; }
    // a.m. / p.m.
    if (!strncmp(*x, "a.m.", 4)) { *x += 4; return 1; }
    if (!strncmp(*x, "p.m.", 4)) { *x += 4; return 2; }
    // A.M. / P.M.
    if (!strncmp(*x, "A.M.", 4)) { *x += 4; return 1; }
    if (!strncmp(*x, "P.M.", 4)) { *x += 4; return 2; }

    return match_str(x, table, start, 2);
}



// time index parsing
// ========================================================


// parse
TIND__ATTRIBUTE_FUNCTION
SEXP
parse(SEXP sx, SEXP sfmt, SEXP stype, SEXP sbeh, SEXP scnames, SEXP sznames,
      SEXP sabbyst)
{
    size_t n = XLENGTH(sx), nok = 0, i;
    int nof = length(sfmt);
    SEXP sy, shms, sz;
    int *py = 0, *pz = 0;
    double *phms = 0;
    // behavior on error:
    // 0 - return NULL immediately (for automatic parsing / automatic type detection)
    // 1 - return NAs + record position of the 1st error
    int errbeh = *INTEGER(sbeh);
    int abbyst = *INTEGER(sabbyst);
    int nz = length(sznames);
    int typet = (*CHAR(STRING_ELT(stype, 0)) == 't');
    int typeh = (*CHAR(STRING_ELT(stype, 0)) == 'h');
    int allna = 1;

    if (typet) {
        sy = PROTECT(allocVector(INTSXP, n));
        shms = PROTECT(allocVector(REALSXP, n));
        sz = PROTECT(allocVector(INTSXP, n));
        py = INTEGER(sy);
        phms = REAL(shms);
        pz = INTEGER(sz);
    } else if (typeh) {
        sy = R_NilValue;
        shms = PROTECT(allocVector(REALSXP, n));
        sz = R_NilValue;
        phms = REAL(shms);
    } else {
        sy = PROTECT(allocVector(INTSXP, n));
        shms = R_NilValue;
        sz = R_NilValue;
        py = INTEGER(sy);
    }

    for (i = 0; i < n; ++i) {
        const char *x0 = CHAR(STRING_ELT(sx, i));
#ifdef DEBUG_TIND_PARSE
Rprintf("  X: %s\n", x0);
#endif /* DEBUG_TIND_PARSE */
        // handle empty / NA
        if (is_na_empty(x0)) {
            if (typeh) phms[i] = NA_REAL; else py[i] = NA_INTEGER;
            continue;
        }

        allna = 0;
        int ok = 0;
        int y = 0, q = 0, m = 0, w = 0, d = 0, u = 0, j = 0, yqmwd = 0;
        int q_fl = 0, m_fl = 0, w_fl = 0, d_fl = 0, u_fl = 0, j_fl = 0;
        double s = 0., hms = 0.;
        int h = 0, I = 0, p = 0, min = 0, z = 0;
        int h_fl = 0, I_fl = 0, p_fl = 0, min_fl = 0, s_fl = 0, z_fl = 0,
            zh_fl = 0, zhm_fl = 0;

        for (int k = 0; k < nof; ++k) {
            const char *pfmt = CHAR(STRING_ELT(sfmt, k));
            const char *x = x0;
#ifdef DEBUG_TIND_PARSE
Rprintf("  FMT: %s\n", pfmt);
#endif /* DEBUG_TIND_PARSE */
            ok = 1;
            q_fl = 0, m_fl = 0, w_fl = 0, d_fl = 0, u_fl = 0, j_fl = 0;
            h_fl = 0, I_fl = 0, p_fl = 0, min_fl = 0, s_fl = 0;
            z_fl = 0, zh_fl = 0, zhm_fl = 0;
            while (1) {
#ifdef DEBUG_TIND_PARSE
Rprintf("  X: %s\n", x);
#endif /* DEBUG_TIND_PARSE */
                if (*pfmt == 'z') skip_ws(&x);
                else if (*pfmt == 'm' || *pfmt == 'u' || *pfmt == 'p') skip_non_alnum(&x);
                else skip_non_num(&x);
                if (!*x) { ok = 0; break; }
                switch (*pfmt) {
                    case 'y':
                        if (!is_digit(*x)) ok = 0;
                        else if ((y = parse_ni(&x, 4, 1)) >= 0) { ; }
                        else if ((y = parse_ni(&x, 2, 1)) >= 0) {
                            y += (y < abbyst) ? 2000 : 1900;
                        } else ok = 0;
                        break;
                    case 'q':
                        if ((*x == 'q') || (*x == 'Q')) ++x;
                        if (!is_digit(*x)) ok = 0;
                        else { q = parse_ni(&x, 1, 0); q_fl = 1; }
                        break;
                    case 'm':
                        if (is_digit(*x)) ok = m_fl = ((m = parse_ni(&x, 2, 0)) >= 0);
                        else if (is_alpha(*x)) {
                            m = match_str(&x, scnames, 0, 24);
                            if (m) {
                                m = (m - 1) % 12 + 1;
                                m_fl = 1;
                            } else ok = 0;
                        } else ok = 0;
                        break;
                    case 'w':
                        // if ((*x == 'w') || (*x == 'W')) ++x;
                        if (!is_digit(*x)) ok = 0;
                        else ok = w_fl = ((w = parse_ni(&x, 2, 0)) >= 0);
                        break;
                    case 'd':
                        if (!is_digit(*x)) ok = 0;
                        else ok = d_fl = ((d = parse_ni(&x, 2, 0)) >= 0);
                        break;
                    case 'j':
                        if (!is_digit(*x)) ok = 0;
                        else ok = j_fl = ((j = parse_ni(&x, 3, 0)) >= 0);
                        break;
                    case 'u':
                        if (is_digit(*x)) { u = parse_ni(&x, 1, 0); u_fl = 1; }
                        else if (is_alpha(*x)) {
                            u = match_str(&x, scnames, 24, 14);
                            if (u) {
                                u = (u - 1) % 7 + 1;
                                u_fl = 1;
                            } else ok = 0;
                        } else ok = 0;
                        break;
                    case 'H':
                        if (!is_digit(*x)) ok = 0;
                        else ok = h_fl = ((h = parse_ni(&x, 2, 1)) >= 0);
                        break;
                    case 'I':
                        if (!is_digit(*x)) ok = 0;
                        else ok = I_fl = ((I = parse_ni(&x, 2, 0)) >= 0);
                        break;
                    case 'p':
                        if (!is_alpha(*x)) ok = 0;
                        else ok = p_fl = ((p = match_ampm(&x, scnames, 38)) > 0);
                        break;
                    case 'M':
                        if (!is_digit(*x)) ok = 0;
                        else ok = min_fl = ((min = parse_ni(&x, 2, 1)) >= 0);
                        break;
                    case 'S':
                        if (!is_digit(*x)) ok = 0;
                        else ok = s_fl = ((s = parse_nd(&x, 2, 1)) >= 0.);
                        break;
                    case 'z':
                        if (*x == 'Z') {
                            z = 0; ok = z_fl = 1; ++x;
                        } else if ((*x >= 'A') && (*x <= 'Z')) {
                            z = match_str(&x, sznames, 0, nz);
                            if (z) {
                                z *= 100000;
                                ok = z_fl = 1;
                            } else ok = 0;
                        } else if ((*x == '+') || (*x == '-')) {
                            int sgn = (*x == '-') ? -1 : 1;
                            ++x;
                            if ((z = parse_ni(&x, 4, 1)) >= 0) {
                                ok = zhm_fl = 1;
                            } else if ((z = parse_ni(&x, 2, 1)) >= 0) {
                                if (*x == ':') {
                                    ++x;
                                    int zm = parse_ni(&x, 2, 1);
                                    if (zm >= 0) { z = 100 * z + zm; ok = zhm_fl = 1; }
                                    else ok = 0;
                                } else ok = zh_fl = 1;
                            } else ok = 0;
                            if (ok) z = sgn * z;
                        } else ok = 0;
                        break;
                    default:
                        ok = 0;
                }
#ifdef DEBUG_TIND_PARSE
Rprintf("  -> FMT: %c, OK: %d\n", *pfmt, ok);
#endif /* DEBUG_TIND_PARSE */
                if (!ok) break;
                ++pfmt;
                if (!*pfmt) {
                    skip_non_alnum(&x);
                    ok = !*x;
                    break;
                }
            }

            if (!ok) continue;

            if (ok && !typeh) {
                if (m_fl && d_fl) { // year, month, day
                    yqmwd = validateymd(y, m, d);
                } else if (j_fl) { // year, day of year
                    yqmwd = validateyj(y, j);
                } else if (w_fl && u_fl) { // year, week, day of week
                    yqmwd = validateywu(y, w, u);
                } else if (w_fl) { // weeks
                    yqmwd = validateyw(y, w);
                } else if (m_fl) { // months
                    yqmwd = validateym(y, m);
                } else if (q_fl) { // quarters
                    yqmwd = validateyq(y, q);
                } else { // years
                    yqmwd = y;
                }

                if (yqmwd == NA_INTEGER) ok = 0;
            }
            if (ok && (typeh || typet)) {
                if (I_fl && p_fl) {
                    h = validateip(I, p);
                    if (h == NA_INTEGER) ok = h_fl = 0; else h_fl = 1;
                }
                if (h_fl) {
                    hms = (min_fl && s_fl) ? validatehms(h, min, s) :
                                    (min_fl ? validatehms(h, min, 0.) :
                                                validatehms(h, 0, 0.));
                    if (!(R_FINITE(hms))) ok = 0;
                }
            }
            if (ok && typet) {
                if (zhm_fl) {
                    z = validatezhm(z);
                    if (z == NA_INTEGER) ok = 0;
                } else if (zh_fl) {
                    z = validatezh(z);
                    if (z == NA_INTEGER) ok = 0;
                } else if (!z_fl) z = NA_INTEGER;
            }
            if (ok) break;
#ifdef DEBUG_TIND_PARSE
Rprintf("  => OK: %d\n", ok);
#endif /* DEBUG_TIND_PARSE */
        }

        if (!ok) {
            if (errbeh) {
                if (!nok) nok = i + 1;
                if (typeh) phms[i] = NA_REAL; else py[i] = NA_INTEGER;
            }
            else { UNPROTECT(1 + 2 * typet); return R_NilValue; }
        } else {
            if (!typeh) py[i] = yqmwd;
            if (typeh || typet) phms[i] = hms;
            if (typet) pz[i] = z;
        }
    }

    if (allna && !errbeh) { UNPROTECT(1 + 2 * typet); return R_NilValue; }

    if (typeh) setAttrib(shms, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    else setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    SEXP sres = PROTECT(allocVector(VECSXP, nok ? 4 : 3));
    SET_VECTOR_ELT(sres, 0, sy);
    SET_VECTOR_ELT(sres, 1, shms);
    SET_VECTOR_ELT(sres, 2, sz);
    if (nok) SET_VECTOR_ELT(sres, 3, PROTECT((nok <= R_SHORT_LEN_MAX) ?
                                             ScalarInteger(nok) : ScalarReal(nok)));
    UNPROTECT((nok ? 3 : 2) + 2 * typet);
    return sres;
}


// strptind
TIND__ATTRIBUTE_FUNCTION
SEXP
strptind(SEXP sx, SEXP sfmt, SEXP stype, SEXP sbeh, SEXP scnames, SEXP sznames,
         SEXP sabbyst)
{
    size_t n = XLENGTH(sx), nok = 0, i;
    int nof = length(sfmt);
    SEXP sy, shms, sz;
    int *py = 0, *pz = 0;
    double *phms = 0;
    // behavior on error:
    // 0 - return NULL immediately (for automatic parsing / automatic type detection)
    // 1 - return NAs + record position of the 1st error
    int errbeh = *INTEGER(sbeh);
    int abbyst = *INTEGER(sabbyst);
    int nz = length(sznames);
    int typet = (*CHAR(STRING_ELT(stype, 0)) == 't');
    int typeh = (*CHAR(STRING_ELT(stype, 0)) == 'h');
    int allna = 1;

    if (typet) {
        sy = PROTECT(allocVector(INTSXP, n));
        shms = PROTECT(allocVector(REALSXP, n));
        sz = PROTECT(allocVector(INTSXP, n));
        py = INTEGER(sy);
        phms = REAL(shms);
        pz = INTEGER(sz);
    } else if (typeh) {
        sy = R_NilValue;
        shms = PROTECT(allocVector(REALSXP, n));
        sz = R_NilValue;
        phms = REAL(shms);
    } else {
        sy = PROTECT(allocVector(INTSXP, n));
        shms = R_NilValue;
        sz = R_NilValue;
        py = INTEGER(sy);
    }

    for (i = 0; i < n; ++i) {
        const char *x0 = CHAR(STRING_ELT(sx, i));
#ifdef DEBUG_TIND_PARSE
Rprintf("  X: %s\n", x0);
#endif /* DEBUG_TIND_PARSE */
        // handle empty / NA
        if (is_na_empty(x0)) {
            if (typeh) phms[i] = NA_REAL; else py[i] = NA_INTEGER;
            continue;
        }

        allna = 0;
        int ok = 0;
        int y = 0, g = 0, q = 0, m = 0, V = 0, d = 0, u = 0, j = 0, yqmwd = 0;
        int y_fl = 0, g_fl = 0, q_fl = 0, m_fl = 0, V_fl = 0, d_fl = 0,
            u_fl = 0, j_fl = 0;
        double s = 0., hms = 0.;
        int h = 0, I = 0, p = 0, min = 0, z = 0;
        int h_fl = 0, I_fl = 0, p_fl = 0, min_fl = 0, s_fl = 0, z_fl = 0,
            zh_fl = 0, zhm_fl = 0;

        for (int k = 0; k < nof; ++k) {
            const char *pfmt = CHAR(STRING_ELT(sfmt, k));
            const char *x = x0;
#ifdef DEBUG_TIND_PARSE
Rprintf("  FMT: %s\n", pfmt);
#endif /* DEBUG_TIND_PARSE */
            ok = 1;
            q_fl = 0, m_fl = 0, V_fl = 0, d_fl = 0, u_fl = 0, j_fl = 0;
            h_fl = 0, I_fl = 0, p_fl = 0, min_fl = 0, s_fl = 0;
            z_fl = 0, zh_fl = 0, zhm_fl = 0;
            while (1) {
#ifdef DEBUG_TIND_PARSE
Rprintf("    FMT: \"%s\"\n", pfmt);
Rprintf("    X: \"%s\"\n", x);
#endif /* DEBUG_TIND_PARSE */
                if (*pfmt != '%') {
                    if (*pfmt == *x) {
                        if (*x) { ++pfmt; ++x; continue; } else break;
                    } else if (!*pfmt) { // tolerate spaces at the end of input
                        skip_ws(&x); ok = !*x; break;
                    } else { ok = 0; break; }
                } else ++pfmt;
                switch (*pfmt) {
                    case '%':
                        if (*x == '%') ++x; else ok = 0;
                        break;
                    case 't':
                        if (*x == '\t') ++x; else ok = 0;
                        break;
                    case 'n':
                        if (*x == '\n') ++x; else ok = 0;
                        break;
                    case 'y':
                        if (!is_digit(*x)) ok = 0;
                        else if ((y = parse_ni(&x, 2, 1)) >= 0) {
                            y += (y < abbyst) ? 2000 : 1900;
                            y_fl = 1;
                        } else ok = 0;
                        break;
                    case 'Y':
                        if (!is_digit(*x)) ok = 0;
                        else ok = y_fl = ((y = parse_ni(&x, 4, 1)) >= 0);
                        break;
                    case 'q':
                        if (!is_digit(*x)) ok = 0;
                        else { q = parse_ni(&x, 1, 0); q_fl = 1; }
                        break;
                    case 'm':
                        if (!is_digit(*x)) ok = 0;
                        else ok = m_fl = ((m = parse_ni(&x, 2, 1)) > 0);
                        break;
                    case 'b':
                        if (!is_alpha(*x)) ok = 0;
                        else ok = m_fl = ((m = match_str(&x, scnames, 0, 12)) > 0);
                        break;
                    case 'B':
                        if (!is_alpha(*x)) ok = 0;
                        else ok = m_fl = ((m = match_str(&x, scnames, 12, 12)) > 0);
                        break;
                    case 'g':
                        if (!is_digit(*x)) ok = 0;
                        else if ((g = parse_ni(&x, 2, 1)) >= 0) {
                            g += (g < abbyst) ? 2000 : 1900;
                            g_fl = 1;
                        } else ok = 0;
                        break;
                    case 'G':
                        if (!is_digit(*x)) ok = 0;
                        else ok = g_fl = ((g = parse_ni(&x, 4, 1)) >= 0);
                        break;
                    case 'V':
                        if (!is_digit(*x)) ok = 0;
                        else ok = V_fl = ((V = parse_ni(&x, 2, 1)) >= 0);
                        break;
                    case 'd':
                        if (!is_digit(*x)) ok = 0;
                        else ok = d_fl = ((d = parse_ni(&x, 2, 1)) >= 0);
                        break;
                    case 'e':
                        if (is_digit(*x)) ok = d_fl = ((d = parse_ni(&x, 2, 1)) >= 0);
                        else if (*x == ' ') {
                            if (is_digit(*++x))
                                ok = d_fl = ((d = parse_ni(&x, 1, 0)) > 0);
                            else ok = 0;
                        } else ok = 0;
                        break;
                    case 'j':
                        if (!is_digit(*x)) ok = 0;
                        else ok = j_fl = ((j = parse_ni(&x, 3, 1)) > 0);
                        break;
                    case 'u':
                        if (!is_digit(*x)) ok = 0;
                        else ok = u_fl = ((u = parse_ni(&x, 1, 1)) > 0);
                        break;
                    case 'a':
                        if (!is_alpha(*x)) ok = 0;
                        else ok = u_fl = ((u = match_str(&x, scnames, 24, 7)) > 0);
                        break;
                    case 'A':
                        if (!is_alpha(*x)) ok = 0;
                        else ok = u_fl = ((u = match_str(&x, scnames, 31, 7)) > 0);
                        break;
                    case 'H':
                        if (!is_digit(*x)) ok = 0;
                        else ok = h_fl = ((h = parse_ni(&x, 2, 1)) >= 0);
                        break;
                    case 'I':
                        if (!is_digit(*x)) ok = 0;
                        else ok = I_fl = ((I = parse_ni(&x, 2, 0)) >= 0);
                        break;
                    case 'p':
                        if (!is_alpha(*x)) ok = 0;
                        else ok = p_fl = ((p = match_ampm(&x, scnames, 38)) > 0);
                        break;
                    case 'M':
                        if (!is_digit(*x)) ok = 0;
                        else ok = min_fl = ((min = parse_ni(&x, 2, 1)) >= 0);
                        break;
                    case 'S':
                        if (!is_digit(*x)) ok = 0;
                        else ok = s_fl = ((s = (double) parse_ni(&x, 2, 1)) >= 0.);
                        break;
                    case 'O':
                        ++pfmt;
                        if (!is_digit(*x)) ok = 0;
                        else ok = s_fl = ((s = parse_nd(&x, 2, 1)) >= 0.);
                        break;
                    case 'z':
                        if (*x == 'Z') {
                            z = 0; ok = z_fl = 1; ++x;
                        } else if ((*x == '+') || (*x == '-')) {
                            int sgn = (*x == '-') ? -1 : 1;
                            ++x;
                            if ((z = parse_ni(&x, 4, 1)) >= 0) {
                                ok = zhm_fl = 1;
                            } else if ((z = parse_ni(&x, 2, 1)) >= 0) {
                                if (*x == ':') {
                                    ++x;
                                    int zm = parse_ni(&x, 2, 1);
                                    if (zm >= 0) {
                                        z = 100 * z + zm;
                                        ok = zhm_fl = 1;
                                    } else ok = 0;
                                } else ok = zh_fl = 1;
                            } else ok = 0;
                            if (ok) z = sgn * z;
                        } else ok = 0;
                        break;
                    case 'Z':
                        if ((*x >= 'A') && (*x <= 'Z')) {
                            z = match_str(&x, sznames, 0, nz);
                            if (z) {
                                z *= 100000;
                                ok = z_fl = 1;
                            } else ok = 0;
                        } else ok = 0;
                        break;
                    default:
                        ok = 0;
                }
#ifdef DEBUG_TIND_PARSE
Rprintf("    -> FMT: %c, OK: %d\n", *pfmt, ok);
#endif /* DEBUG_TIND_PARSE */
                if (!ok) break;
                ++pfmt;
            }
            if (ok) break;
        }
#ifdef DEBUG_TIND_PARSE
Rprintf("  => OK: %d\n", ok);
#endif /* DEBUG_TIND_PARSE */

        if (ok && !typeh) {
            if (y_fl && m_fl && d_fl) { // year, month, day
                yqmwd = validateymd(y, m, d);
            } else if (y_fl && j_fl) { // year, day of year
                yqmwd = validateyj(y, j);
            } else if (g_fl && V_fl && u_fl) { // year, week, day of week
                yqmwd = validateywu(g, V, u);
            } else if (g_fl && V_fl) { // weeks
                yqmwd = validateyw(g, V);
            } else if (y_fl && m_fl) { // months
                yqmwd = validateym(y, m);
            } else if (y_fl && q_fl) { // quarters
                yqmwd = validateyq(y, q);
            } else if (y_fl) { // years
                yqmwd = y;
            }
            if (yqmwd == NA_INTEGER) ok = 0;
        }
        if (ok && (typeh || typet)) {
            if (I_fl && p_fl) {
                h = validateip(I, p);
                if (h == NA_INTEGER) ok = h_fl = 0; else h_fl = 1;
            }
            if (h_fl) {
                hms = (min_fl && s_fl) ? validatehms(h, min, s) :
                               (min_fl ? validatehms(h, min, 0.) :
                                         validatehms(h, 0, 0.));
                if (!(R_FINITE(hms))) ok = 0;
            }
        }
        if (ok && typet) {
            if (zhm_fl) {
                z = validatezhm(z);
                if (z == NA_INTEGER) ok = 0;
            } else if (zh_fl) {
                z = validatezh(z);
                if (z == NA_INTEGER) ok = 0;
            } else if (!z_fl) z = NA_INTEGER;
        }

        if (!ok) {
            if (errbeh) {
                if (!nok) nok = i + 1;
                if (typeh) phms[i] = NA_REAL; else py[i] = NA_INTEGER;
            }
            else { UNPROTECT(1 + 2 * typet); return R_NilValue; }
        } else {
            if (!typeh) py[i] = yqmwd;
            if (typeh || typet) phms[i] = hms;
            if (typet) pz[i] = z;
        }
    }

    if (allna && !errbeh) { UNPROTECT(1 + 2 * typet); return R_NilValue; }

    if (typeh) setAttrib(shms, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    else setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    SEXP sres = PROTECT(allocVector(VECSXP, nok ? 4 : 3));
    SET_VECTOR_ELT(sres, 0, sy);
    SET_VECTOR_ELT(sres, 1, shms);
    SET_VECTOR_ELT(sres, 2, sz);
    if (nok) SET_VECTOR_ELT(sres, 3, PROTECT((nok <= R_SHORT_LEN_MAX) ?
                                             ScalarInteger(nok) : ScalarReal(nok)));
    UNPROTECT((nok ? 3 : 2) + 2 * typet);
    return sres;
}



// parsing time differences
// ========================================================

// match units
static inline
TIND__ATTRIBUTE_INLINE
char
match_unit(const char **x)
{
    const char *unit_y[] = { "year", "years", "" };
    const char *unit_q[] = { "quarter", "quarters", "" };
    const char *unit_m[] = { "month", "months", "" };
    const char *unit_w[] = { "week", "weeks", "" };
    const char *unit_d[] = { "day", "days", "" };
    char u = **x;
    const char **tab;
    switch (u) {
        case 'y': tab = unit_y; break;
        case 'q': tab = unit_q; break;
        case 'm': tab = unit_m; break;
        case 'w': tab = unit_w; break;
        case 'd': tab = unit_d; break;
        default: return '\0';
    }
    ++*x;
    if (!is_alpha(**x)) return u;
    --*x;
    while (**tab) {
        int ul = strlen(*tab);
        if (strncmp(*x, *tab, ul)) { ++tab; continue; }
        if (is_alpha(*(*x + ul))) { ++tab; continue; } else { *x += ul; return u; }
    }
    return '\0';
}


// match units - time
static inline
TIND__ATTRIBUTE_INLINE
char
match_unit_t(const char **x)
{
    const char *unit_d[] = { "day", "days", "" };
    const char *unit_h[] = { "hour", "hours", "" };
    const char *unit_min[] = { "min", "mins", "minute", "minutes", "" };
    const char *unit_s[] = { "sec", "secs", "second", "seconds", "" };
    char u = **x;
    const char **tab;
    switch (u) {
        case 'd': tab = unit_d; break;
        case 'h': tab = unit_h; break;
        case 'm': tab = unit_min; break;
        case 's': tab = unit_s; break;
        default: return '\0';
    }
    ++*x;
    if (!is_alpha(**x)) return u;
    --*x;
    while (**tab) {
        int ul = strlen(*tab);
        if (strncmp(*x, *tab, ul)) { ++tab; continue; }
        if (is_alpha(*(*x + ul))) { ++tab; continue; } else { *x += ul; return u; }
    }
    return '\0';
}


// These two functions return 2-element lists with time difference values and type.
// On error NULL is returned.

SEXP
parse_tdiff_yqmwd(SEXP sx)
{
    size_t n = XLENGTH(sx), i;
    SEXP sy;
    int *y;
    char type = '\0';
    int ok = 0;

    sy = PROTECT(allocVector(INTSXP, n));
    y = INTEGER(sy);

    for (i = 0; i < n; ++i) {
        const char *x = CHAR(STRING_ELT(sx, i));
        char ntype;
        // handle empty / NA
        if (is_na_empty(x)) { y[i] = NA_INTEGER; continue; }
        // leading space
        skip_ws(&x);
        // unit without preceding number
        if (is_alpha(*x)) {
            ntype = match_unit(&x);
            if (!ntype) { ok = 0; break; } else ok = 1;
            if (!type) type = ntype; else if (type != ntype) { ok = 0; break; }
            skip_ws(&x);
            if (*x) { ok = 0; break; } else { y[i] = 1; continue; }
        }
        // minus sign
        int neg = 0;
        if (*x == '-') { neg = 1; ++x; }
        if (!is_digit(*x)) { ok = 0; break; }
        y[i] = parse_ni(&x, 7, 0);
        if (neg) y[i] = -y[i];
        // parse unit
        skip_ws(&x);
        ntype = match_unit(&x);
        if (!ntype) { ok = 0; break; } else ok = 1;
        if (!type) type = ntype; else if (type != ntype) { ok = 0; break; }
        skip_ws(&x);
        // finish or go through '()'
        if (*x) {
            if (type == 'y') { ok = 0; break; }
            if (*x != '(') { ok = 0; break; }
            else {
                while (*x && *x != ')') ++x;
                if (*x != ')') { ok = 0; break; }
                ++x;
                skip_ws(&x);
                if (*x) { ok = 0; break; }
            }
        }
        switch (ntype) {
            case 'y': if (!valid_tdiff_y(y[i])) y[i] = NA_INTEGER; break;
            case 'q': if (!valid_tdiff_q(y[i])) y[i] = NA_INTEGER; break;
            case 'm': if (!valid_tdiff_m(y[i])) y[i] = NA_INTEGER; break;
            case 'w': if (!valid_tdiff_w(y[i])) y[i] = NA_INTEGER; break;
            case 'd': if (!valid_tdiff_d(y[i])) y[i] = NA_INTEGER; break;
        }
        if (!ok) break;
    }

    if (!ok) { UNPROTECT(1); return R_NilValue; }

    SEXP res = PROTECT(allocVector(VECSXP, 2));
    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    SET_VECTOR_ELT(res, 0, sy);
    char buf[2] = "x";
    buf[0] = type;
    SET_VECTOR_ELT(res, 1, mkString(buf));
    UNPROTECT(2);
    return res;
}


SEXP
parse_tdiff_t(SEXP sx)
{
    size_t n = XLENGTH(sx), i;
    SEXP sy;
    double *y;
    int ok = 0;

    sy = PROTECT(allocVector(REALSXP, n));
    y = REAL(sy);

    for (i = 0; i < n; ++i) {
        char unit = '\0';
        const char *x, *x0;
        x = CHAR(STRING_ELT(sx, i));
        // handle empty / NA
        if (is_na_empty(x)) { y[i] = NA_REAL; continue; }
        // leading space
        skip_ws(&x);
        // unit without preceding number
        if (is_alpha(*x)) {
            unit = match_unit_t(&x);
            if (!unit || (unit == 'd')) { ok = 0; break; }
            skip_ws(&x);
            if (*x) { ok = 0; break; }
            ok = 1;
            switch (unit) {
                case 'h': y[i] = 3600.; break;
                case 'm': y[i] = 60.; break;
                case 's': y[i] = 1.; break;
                default: ;
            }
            continue;
        }
        // minus sign
        int neg = 0;
        if (*x == '-') { neg = 1; ++x; }
        // parse
        y[i] = 0.;
        int yint = 0, hasdigits;
        double ydbl = 0.;
        while (*x) {
            if (!is_digit(*x)) { ok = 0; break; } else ok = 1;
            x0 = x;
            while (is_digit(*x0)) ++x0;
            hasdigits = (*x0 == '.');
            if (hasdigits) ydbl = parse_nd(&x, 7, 0); else yint = parse_ni(&x, 7, 0);
            skip_ws(&x);
            char nunit = match_unit_t(&x);
            if (!nunit) { ok = 0; break; } else ok = 1;
            switch (nunit) {
                case 'd': ok = !hasdigits; y[i] += 86400. * yint; break;
                case 'h': ok = !hasdigits; y[i] += 3600. * yint; break;
                case 'm': ok = !hasdigits; y[i] += 60. * yint; break;
                case 's': ok = 1; y[i] += (hasdigits ? ydbl : yint); break;
                default: ;
            }
            if (!ok) break;
            if (unit && (nunit <= unit)) { ok = 0; break; }
            unit = nunit;
            skip_ws(&x);
            // finish, go through '()', or continue parsing
            if (!*x || *x == '(') {
                if (*x == '(') {
                    while (*x && *x != ')') ++x;
                    if (!*x) { ok = 0; break; } else { ++x; skip_ws(&x); }
                    if (*x) { ok = 0; break; }
                }
                if (unit == 1) { ok = 0; break; } // cannot have days only
                if (neg) y[i] = -y[i];
                if (!valid_tdiff_t(y[i])) y[i] = NA_REAL;
                continue;
            } else ok = 0;
        }
        if (!ok) break;
    }

    if (!ok) { UNPROTECT(1); return R_NilValue; }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    SEXP res = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, sy);
    SET_VECTOR_ELT(res, 1, mkString("t"));
    UNPROTECT(2);
    return res;
}



// parsing time intervals
// ========================================================

static inline
TIND__ATTRIBUTE_INLINE
int
is_tinterval_open(const char *c)
{
    if (is_na_empty(c)) return 1;
    while (*c && (*c == ' ' || *c == '\t' || *c == '\n' || *c == '.')) ++c;
    return !*c;
}


// This function returns list of character vectors of starts and ends of
// time intervals  'sep' string is understood as a separator, addtional info
// in parentheses is stripped and NAs or fields with '.' and/or whitespace fields
// are returned as NA_STRING. Reults are then to be parsed as time indices.

SEXP
parse_tinterval(SEXP sx, SEXP ssep)
{
    size_t n = XLENGTH(sx), i;
    int nchar = 0;
    int ok = 1;

    const char *sep = CHAR(STRING_ELT(ssep, 0));
    int nchsep = strlen(sep);

    // check first
    for (i = 0; i < n; ++i) {
        const char *x, *x0;
        x0 = x = CHAR(STRING_ELT(sx, i));
        int nch = strlen(x);
        int count = 0;

        int nch0 = nch - nchsep, nch1, nch2;
        for (int j = 0; j <= nch0; ++j, ++x)
            if (!strncmp(x, sep, nchsep)) { ++count; nch1 = j; }
        if (count != 1) { ok = 0; break; }

        for (x = x0 + nch1 + nchsep, nch2 = 0; *x && (*x != '('); ++x, ++nch2) { ; }
        if (*x == '(') {
            while (*x && *x != ')') ++x;
            if (*x != ')') { ok = 0; break; }
            ++x;
            skip_ws(&x);
            if (*x) { ok = 0; break; }
        }

        if (nchar < nch1) nchar = nch1;
        if (nchar < nch2) nchar = nch2;
    }

    if (!ok) return R_NilValue;

    char *buf = R_Calloc(nchar + 1, char);
    SEXP ss = PROTECT(allocVector(STRSXP, n));
    SEXP se = PROTECT(allocVector(STRSXP, n));

    // split and rm additional info
    for (i = 0; i < n; ++i) {
        const char *x;
        char *b;
        x = CHAR(STRING_ELT(sx, i));
        b = buf;
        // 1st part
        while (strncmp(x, sep, nchsep)) *b++ = *x++;
        *b = '\0';
        if (is_tinterval_open(buf)) SET_STRING_ELT(ss, i, NA_STRING);
        else SET_STRING_ELT(ss, i, mkChar(buf));
        // 2nd part
        x += nchsep;
        b = buf;
        while (*x && *x != '(') *b++ = *x++;
        *b = '\0';
        if (is_tinterval_open(buf)) SET_STRING_ELT(se, i, NA_STRING);
        else SET_STRING_ELT(se, i, mkChar(buf));
    }

    R_Free(buf);

    setAttrib(ss, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    setAttrib(se, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    SEXP sres = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(sres, 0, ss);
    SET_VECTOR_ELT(sres, 1, se);
    SEXP snms = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(snms, 0, mkChar("start"));
    SET_STRING_ELT(snms, 1, mkChar("end"));
    setAttrib(sres, R_NamesSymbol, snms);
    UNPROTECT(4);
    return sres;
}

