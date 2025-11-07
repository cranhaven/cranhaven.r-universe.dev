/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ***********************************
 * conversion to character strings *
 ***********************************
 */


#include "compiler.h"
#include "tochar.h"
#include "calendar.h"


// inlines
// ==================================================================

// print fixed-length non-negative number to a buffer
static inline
TIND__ATTRIBUTE_INLINE
void
print_n0(char *c, int num, int n)
{
    switch (n) {
        case 7: *c++ = '0' + num / 1000000; num %= 1000000;
        case 6: *c++ = '0' + num / 100000; num %= 100000;
        case 5: *c++ = '0' + num / 10000; num %= 10000;
        case 4: *c++ = '0' + num / 1000; num %= 1000;
        case 3: *c++ = '0' + num / 100; num %= 100;
        case 2: *c++ = '0' + num / 10; num %= 10;
        case 1: *c++ = '0' + num;
    }
}


// print fixed-length non-negative number to a buffer and increment pointer
static inline
TIND__ATTRIBUTE_INLINE
void
print_n(char **c, int num, int n)
{
    switch (n) {
        case 7: *(*c)++ = '0' + num / 1000000; num %= 1000000;
        case 6: *(*c)++ = '0' + num / 100000; num %= 100000;
        case 5: *(*c)++ = '0' + num / 10000; num %= 10000;
        case 4: *(*c)++ = '0' + num / 1000; num %= 1000;
        case 3: *(*c)++ = '0' + num / 100; num %= 100;
        case 2: *(*c)++ = '0' + num / 10; num %= 10;
        case 1: *(*c)++ = '0' + num;
    }
}


// number of characters in representation of a number
static inline
TIND__ATTRIBUTE_INLINE
int
nchar_num(int num)
{
    int nc = 1;
    if (num < 0) { num = -num; ++nc; }
    if (num >= 1000000) nc += 6;
    else if (num >= 100000) nc += 5;
    else if (num >= 10000) nc += 4;
    else if (num >= 1000) nc += 3;
    else if (num >= 100) nc += 2;
    else if (num >= 10) nc += 1;
    return nc;
}


// print variable-length number to a buffer and increment pointer
static inline
TIND__ATTRIBUTE_INLINE
void
print_num(char **c, int num)
{
    if (num < 0) { **c = '-'; ++*c; num = -num; }
    print_n(c, num, nchar_num(num));
}


// determine the number of significant digits (up to 6), assumes x >= 0
static inline
TIND__ATTRIBUTE_INLINE
int
digits(double x)
{
    int p = 0, pow10[7] = { 1000000, 100000, 10000, 1000, 100, 10, 1 };
    int dd = (int) round((x - floor(x)) * 1e6);
    while (dd % pow10[p]) ++p;
    return p;
}


// print decimal places (up to 6) to a buffer
static inline
TIND__ATTRIBUTE_INLINE
void
print_d0(char *c, double x, int n)
{
    int pow10[6] = { 100000, 10000, 1000, 100, 10, 1 };
    int dd = (int) round((x - floor(x)) * 1e6);
    print_n0(c, dd / pow10[n - 1], n);
}


// print decimal places (up to 6) to a buffer and increment pointer
static inline
TIND__ATTRIBUTE_INLINE
void
print_d(char **c, double x, int n)
{
    int pow10[6] = { 100000, 10000, 1000, 100, 10, 1 };
    int dd = (int) round((x - floor(x)) * 1e6);
    print_n(c, dd / pow10[n - 1], n);
}


// just like strcpy but does not copy '\0'
static inline
TIND__ATTRIBUTE_INLINE
void
strcpy0(char *dest, const char *src)
{
    while (*src) *(dest++) = *(src++);
}


// copy centered string given destination's length
static inline
TIND__ATTRIBUTE_INLINE
void
strcenter(char *dest, const char *src, int len)
{
    int ls, ll, lr;
    ls = strlen(src);
    if (ls == len) { strcpy0(dest, src); return; }
    ll = (len - ls) / 2; lr = (len - ls + 1) / 2;
    for (int i = 0; i < ll; ++i) *dest++ = ' ';
    strcpy0(dest, src);
    dest += ls;
    for (int i = 0; i < lr; ++i) *dest++ = ' ';
}



// printing time indices
// ==================================================================

static inline
TIND__ATTRIBUTE_INLINE
void
print_yqmwd(char *c, int x, char type)
{
    switch (type) {
        case 'y':
            print_n0(c, x, 4); break;
        case 'q':
            print_n0(c, x / 4, 4); c[4] = 'Q';
            print_n0(c + 5, x % 4 + 1, 1); break;
        case 'm':
            print_n0(c, x / 12, 4); c[4] = '-';
            print_n0(c + 5, x % 12 + 1, 2); break;
        case 'w': {
            int y, w;
            w2yw(x, &y, &w);
            print_n0(c, y, 4); c[4] = '-'; c[5] = 'W';
            print_n0(c + 6, w, 2); break;
        }
        case 'd': {
            int y, m, d;
            d2ymd(x, &y, &m, &d);
            print_n0(c, y, 4); c[4] = '-';
            print_n0(c + 5, m, 2); c[7] = '-';
            print_n0(c + 8, d, 2); break;
        }
        default: break;
    }
}


static inline
TIND__ATTRIBUTE_INLINE
void
print_hms(char *c, double hms, int sdigits)
{
    int hmsi = (int) hms;
    print_n0(c, hmsi / 3600, 2);
    c[2] = ':';
    print_n0(c + 3, hmsi % 3600 / 60, 2);
    if (sdigits != -1) {
        c[5] = ':';
        print_n0(c + 6, hmsi % 60, 2);
        if (sdigits) {
            c[8] = '.';
            print_d0(c + 9, hms, sdigits);
        }
    }
}


static inline
TIND__ATTRIBUTE_INLINE
void
print_off(char *c, int off)
{
    if (off >= 0) c[0] = '+'; else { c[0] = '-'; off = -off; }
    off /= 60;
    c[1] = '0' + off / 600;
    c[2] = '0' + off / 60 % 10;
    c[3] = '0' + off % 60 / 10;
    c[4] = '0' + off % 10;
}



// x2char
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
yqmwd2char(SEXP sx, SEXP stype)
{
    size_t i, n;
    int len, *x;
    char buf[16], type = *CHAR(STRING_ELT(stype, 0));
    SEXP sy;

    n = XLENGTH(sx);
    x = INTEGER(sx);
    sy = PROTECT(allocVector(STRSXP, n));

    switch (type) {
        case 'y': len = 4; break; case 'q': len = 6; break;
        case 'm': len = 7; break; case 'w': len = 8; break;
        case 'd': len = 10; break; default: len = 0;
    }
    buf[len] = '\0';
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { SET_STRING_ELT(sy, i, NA_STRING); continue; }
        print_yqmwd(buf, x[i], type);
        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
t2char(SEXP sdate, SEXP shms, SEXP sgmtoff, SEXP szone)
{
    size_t i, n;
    int *date, *gmtoff, hasgmtoff, haszone;
    double *hms;
    char buf[64] = "YYYY-MM-DD HH:MM:SS.SSSSSS";
    SEXP sy;

    n = XLENGTH(sdate);
    date = INTEGER(sdate);
    hms = REAL(shms);
    sy = PROTECT(allocVector(STRSXP, n));
    hasgmtoff = !isNull(sgmtoff);
    haszone = !isNull(szone);

    // determine precision
    int secdig = -1;
    for (i = 0; i < n; ++i) {
        if (date[i] == NA_INTEGER) continue;
        int hm = (int) hms[i];
        hm /= 60;
        double sec = hms[i] - 60. * (double) hm;
        if (sec < 1e-6) continue; else if (secdig == -1) { secdig = 0; }
        int sd = digits(sec);
        if (sd > secdig) secdig = sd;
    }

    // construct buffer
    int end = 16;
    if (secdig > 0) end = 20 + secdig; else if (!secdig) end = 19;
    buf[end] = '\0';
    if (hasgmtoff) {
        if (!XLENGTH(sgmtoff)) { // not NULL but 0-length means UTC
            strcpy(buf + strlen(buf), "Z");
            hasgmtoff = 0;
        } else {
            hasgmtoff = strlen(buf); // keep pos where offset starts
            strcpy(buf + strlen(buf), "+HHMM");
            gmtoff = INTEGER(sgmtoff);
        }
    } else if (haszone) {
        if (!XLENGTH(szone)) { // not NULL but 0-length means UTC
            strcpy(buf + strlen(buf), " UTC");
            haszone = 0;
        } else {
            haszone = strlen(buf) + 1; // keep pos where zone info starts
            strcpy(buf + strlen(buf), " ZZZZ");
        }
    }

    // loop
    for (i = 0; i < n; ++i) {
        if (date[i] == NA_INTEGER) { SET_STRING_ELT(sy, i, NA_STRING); continue; }
        // YYYY-MM-DD HH:MM(:SS(.SSSSS)?)?
        print_yqmwd(buf, date[i], 'd');
        print_hms(buf + 11, hms[i], secdig);
        // offset / time zone abbreviation
        if (hasgmtoff) {
            print_off(buf + hasgmtoff, gmtoff[i]);
        } else if (haszone) {
            const char *zn = CHAR(STRING_ELT(szone, i));
            strcpy(buf + haszone, zn);
        }

        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sdate, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
h2char(SEXP sh)
{
    size_t i, n;
    double *h;
    char buf[16] = "HH:MM:SS.SSSSSS";
    SEXP sy;

    n = XLENGTH(sh);
    h = REAL(sh);
    sy = PROTECT(allocVector(STRSXP, n));

    // determine precision
    int secdig = -1;
    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(h[i]))) continue;
        int hm = (int) h[i];
        hm /= 60;
        double sec = h[i] - 60. * (double) hm;
        if (sec < 1e-6) continue; else if (secdig == -1) { secdig = 0; }
        int sd = digits(sec);
        if (sd > secdig) secdig = sd;
    }

    // buffer
    int end = 5;
    if (secdig > 0) end = 9 + secdig; else if (!secdig) end = 8;
    buf[end] = '\0';

    // loop
    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(h[i]))) { SET_STRING_ELT(sy, i, NA_STRING); continue; }
        print_hms(buf, h[i], secdig);
        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sh, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}



// format
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
format(SEXP sx, SEXP shms, SEXP soff, SEXP szn,
       SEXP stype, SEXP sfmt, SEXP scnames)
{
    size_t i, n;
    int bufsz, utc = 0, secdig = 0;
    int *x = 0;
    SEXP sy;
    char *buf, type;
    const char *fmt0;
    int *off = 0;
    double *hms = 0;

    type = *CHAR(STRING_ELT(stype, 0));
    fmt0 = CHAR(STRING_ELT(sfmt, 0));

    if (type == 'h') {
        n = XLENGTH(shms);
    } else {
        n = XLENGTH(sx);
        x = INTEGER(sx);
    }
    if ((type == 't') || (type == 'h')) {
        hms = REAL(shms);
        // determine precision
        for (i = 0; i < n; ++i) {
            if (!(R_FINITE(hms[i]))) continue;
            int hm = (int) hms[i];
            hm /= 60;
            double sec = hms[i] - 60. * (double) hm;
            if (sec < 1e-6) continue;
            int sd = digits(sec);
            if (sd > secdig) secdig = sd;
        }
    }
    if (type == 't') {
        // 0-length means UTC
        if (XLENGTH(soff)) { off = INTEGER(soff); utc = 0; } else utc = 1;
    }
    sy = PROTECT(allocVector(STRSXP, n));

    // buffer
    bufsz = 16;
    for (int c = 0; c < length(scnames); ++c) {
        int clen = strlen(CHAR(STRING_ELT(scnames, c))) + 1;
        if (clen > bufsz) bufsz = clen;
    }
    bufsz *= 2;
    buf = R_Calloc(bufsz + 1, char);

    for (i = 0; i < n; ++i) {
        if (((type == 'h') && !(R_FINITE(hms[i]))) ||
            ((type != 'h') && (x[i] == NA_INTEGER))) {
            SET_STRING_ELT(sy, i, NA_STRING); continue;
        }
        memset(buf, 0, bufsz + 1);
        const char *fmt = fmt0;
        char *b = buf;
        int y = 0, q = 0, m = 0, g = 0, w = 0, d = 0, wd = 0, yd = 0, h = 0,
            min = 0;
        double s = 0.;

        switch (type) {
            case 'y': y = x[i]; break;
            case 'q': y = x[i] / 4; q = x[i] % 4 + 1; break;
            case 'm': y = x[i] / 12; m = x[i] % 12 + 1; q = 1 + x[i] % 12 / 3; break;
            case 'w': w2yw(x[i], &g, &w); break;
            case 'd':
            case 't':
                d2ymd(x[i], &y, &m, &d);
                q = 1 + (m - 1) / 3;
                wd = dayofweek(x[i]);
                yd = dayofyear(x[i]);
                int dy = yd + 4 - wd;
                if (dy <= 0) { g = y - 1; w = weeksinyear(g);
                } else if (dy > 365 + isleapyear(y)) { g = y + 1; w = 1;
                } else { g = y; w = 1 + (dy - 1) / 7; }
                break;
            default: break;
        }
        if ((type == 't') || (type == 'h')) {
            int hm;
            hm = (int) hms[i] / 60;
            h = hm / 60;
            min = hm % 60;
            s = hms[i] - 60. * hm;
        }

        while (*fmt) {
            if (*fmt != '%') { *b++ = *fmt++;  continue; }
            ++fmt;
            switch (*fmt) {
                case '%': *b++ = '%'; break;
                case 't': *b++ = '\t'; break;
                case 'n': *b++ = '\n'; break;
                case 'y': print_n(&b, y % 100, 2); break;
                case 'Y': print_n(&b, y, 4); break;
                case 'q': print_n(&b, q, 1); break;
                case 'm': print_n(&b, m, 2); break;
                case 'b':
                    strcpy(b, CHAR(STRING_ELT(scnames, m - 1)));
                    b = buf + strlen(buf); break;
                case 'B':
                    strcpy(b, CHAR(STRING_ELT(scnames, m + 11)));
                    b = buf + strlen(buf); break;
                case 'g': print_n(&b, g % 100, 2); break;
                case 'G': print_n(&b, g, 4); break;
                case 'V': print_n(&b, w, 2); break;
                case 'd': print_n(&b, d, 2); break;
                case 'e':
                    if (d > 9) print_n(&b, d, 2);
                    else { *b++ = ' ';  print_n(&b, d, 1); }
                    break;
                case 'j': print_n(&b, yd, 3); break;
                case 'u': print_n(&b, wd, 1); break;
                case 'a':
                    strcpy(b, CHAR(STRING_ELT(scnames, wd + 23)));
                    b = buf + strlen(buf); break;
                case 'A':
                    strcpy(b, CHAR(STRING_ELT(scnames, wd + 30)));
                    b = buf + strlen(buf); break;
                case 'H': print_n(&b, h, 2); break;
                case 'I': {
                    int I;
                    I = (h >= 12) ? h - 12 : h;
                    if (!I) I = 12;
                    print_n(&b, I, 2); break; }
                case 'p':
                    strcpy(b, CHAR(STRING_ELT(scnames, (h >= 12) ? 39 : 38)));
                    b = buf + strlen(buf);
                    break;
                case 'M': print_n(&b, min, 2); break;
                case 'S': print_n(&b, (int) s, 2); break;
                case 'O': {
                    print_n(&b, (int) s, 2);
                    fmt += 2;
                    int sdig;
                    if ((*fmt >= '0') && (*fmt <= '6')) sdig = *fmt - '0';
                    else { sdig = secdig; --fmt; }
                    if (!sdig) break;
                    *b++ = '.';
                    print_d(&b, s, sdig);
                    break; }
                case 'z':
                    if (utc) {
                        *b++ = 'Z';
                    } else if (off[i] == NA_INTEGER) {
                        strcpy(b, "+-????");
                        b += 6;
                    } else {
                        int neg = 0, offhm = off[i];
                        if (offhm < 0) { neg = 1; offhm = -offhm; }
                        offhm = (offhm / 3600) * 100 + (offhm % 3600) / 60;
                        *b++ = neg ? '-' : '+';
                        print_n(&b, offhm, 4);
                    }
                    break;
                case 'Z':
                    strcpy(b, CHAR(STRING_ELT(szn, i)));
                    b = buf + strlen(buf); break;
            }
            ++fmt;
            int buflen = strlen(buf);
            if (2 * buflen >= bufsz) {
                bufsz *= 2;
                buf = R_Realloc(buf, bufsz + 1, char);
                b = buf + buflen;
                memset(b, 0, bufsz - buflen + 1);
            }
        }

        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    R_Free(buf);

    if (type == 'h') setAttrib(sy, R_NamesSymbol, getAttrib(shms, R_NamesSymbol));
    else setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}



// tdiff_x2char
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
tdiff_yqmwd2char(SEXP sx, SEXP st)
{
    size_t i, n;
    int *x;
    char buf[16], type = *CHAR(STRING_ELT(st, 0));
    SEXP sy;

    n = XLENGTH(sx);
    x = INTEGER(sx);
    sy = PROTECT(allocVector(STRSXP, n));

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { SET_STRING_ELT(sy, i, NA_STRING); continue; }
        char *b = buf;
        print_num(&b, x[i]);
        *b++ = type;
        *b = '\0';
        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


static inline
TIND__ATTRIBUTE_INLINE
void
tdiff_t_flags(double xx, int *all0, int *flag_m, int *flag_s, int *secdig)
{
    if (!(R_FINITE(xx))) return;
    if (*all0) { if (xx != 0.) *all0 = 0; else return; }
    if (xx < 0) xx = -xx;
    int hm;
    hm = (int) floor(xx / 60.);
    if (hm % 60) *flag_m = 1;
    xx -= hm * 60.;
    int sd = digits(xx);
    if (sd) {
        if (sd > *secdig) *secdig = sd;
        *flag_s = 1;
    } else if ((int) floor(xx)) *flag_s = 1;
}


static inline
TIND__ATTRIBUTE_INLINE
void
print_tdiff_t(char **c, double xx, int flag_m, int flag_s, int secdig)
{
    int d = 0, h = 0, m = 0, s = 0, started = 0;
    // sign
    if (xx < 0) { xx = fabs(xx); *(*c)++ = '-'; }
    // days
    if (xx >= 86400.) {
        d = (int) floor(xx / 86400.);
        xx -= 86400. * d;
    }
    if (d) { print_num(c, d); *(*c)++ = 'd'; started = 1; }
    // hours, minutes, seconds
    h = (int) floor(xx / 3600.);
    if (flag_m || flag_s) {
        xx -= 3600. * h;
        m = (int) floor(xx / 60.);
    }
    if (flag_s) {
        xx -= 60. * m;
        s = (int) floor(xx);
    }
    // hours
    if (started) { print_n(c, h, 2); *(*c)++ = 'h'; }
    else if ((!flag_m && !flag_s) || h || m || (flag_m && !flag_s)) {
        print_num(c, h); *(*c)++ = 'h'; started = 1;
    }
    if (!flag_m && !flag_s) return;
    // minutes
    if (started) { print_n(c, m, 2); *(*c)++ = 'm'; }
    if (!flag_s) return;
    // seconds
    if (started) print_n(c, s, 2); else print_num(c, s);
    if (secdig) { *(*c)++ = '.'; print_d(c, xx, secdig); }
    *(*c)++ = 's';
}


TIND__ATTRIBUTE_FUNCTION
SEXP
tdiff_t2char(SEXP sx)
{
    size_t i, n;
    int all0 = 1, flag_m = 0, flag_s = 0, secdig = 0;
    const double *x;
    char buf[32];
    SEXP sy;

    n = XLENGTH(sx);
    x = REAL(sx);
    sy = PROTECT(allocVector(STRSXP, n));

    // flags
    for (i = 0; i < n; ++i)
        tdiff_t_flags(x[i], &all0, &flag_m, &flag_s, &secdig);
    if (all0) { flag_s = 1; flag_m = 0; }
    // print
    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) { SET_STRING_ELT(sy, i, NA_STRING); continue; }
        char *b = buf;
        print_tdiff_t(&b, x[i], flag_m, flag_s, secdig);
        *b = '\0';
        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}



// tdiff_aux
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
aux_tdiff(SEXP sx, SEXP st)
{
    size_t i, n;
    int *x;
    char buf[16], type = *CHAR(STRING_ELT(st, 0));
    SEXP sy;

    n = XLENGTH(sx);
    x = INTEGER(sx);
    sy = PROTECT(allocVector(STRSXP, n));

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { SET_STRING_ELT(sy, i, mkChar("")); continue; }
        char *b = buf;
        int y, r, xx = x[i], neg = 0;
        char u = 'y';
        if (xx < 0) { xx = -xx; neg = 1; }
        switch (type) {
            case 'q':
                if (xx < 4) break;
                if (neg) *b++ = '-';
                print_num(&b, xx / 4);
                r = xx % 4;
                if (r == 2) { *b++ = '.'; print_num(&b, 5); }
                else if (r) { *b++ = '.'; print_num(&b, 25 * r); }
                *b++ = u;
                break;
            case 'm':
                if (xx < 12) break;
                if (neg) *b++ = '-';
                print_num(&b, xx / 12);
                r = xx % 12;
                if (r == 6) { *b++ = '.'; print_num(&b, 5); }
                else if (r) { *b++ = '.'; print_n(&b, (100 * r + 5) / 12, 2); }
                *b++ = u;
                break;
            case 'w':
                if (xx < 52) break;
                *b++ = '~';
                if (neg) *b++ = '-';
                y = (int) round((double) xx / 5.21775);
                r = y % 10; y /= 10;
                print_num(&b, y);
                if (r) { *b++ = '.'; print_n(&b, r, 1); }
                *b++ = u;
                break;
            case 'd':
                if (xx < 30) break;
                *b++ = '~';
                if (neg) *b++ = '-';
                y = (int) round((double) xx / 3.044); // month actually
                if (y >= 120) y = (int) round((double) xx / 36.52425); else u = 'm';
                r = y % 10; y /= 10;
                print_num(&b, y);
                if (r) { *b++ = '.'; print_n(&b, r, 1); }
                *b++ = u;
                break;
            default: break;
        }
        *b = '\0';
        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    UNPROTECT(1);
    return sy;
}



// format_tdiff
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
format_tdiff(SEXP sx, SEXP sa)
{
    size_t i, n;
    int lx = 0, la = 0;
    char buf[32];
    SEXP sy;

    n = XLENGTH(sx);
    sy = PROTECT(allocVector(STRSXP, n));

    // determine lengths
    for (i = 0; i < n; ++i) {
        int ilx, ila;
        ilx = (STRING_ELT(sx, i) == NA_STRING) ? 2 : strlen(CHAR(STRING_ELT(sx, i)));
        ila = strlen(CHAR(STRING_ELT(sa, i)));
        if (ilx > lx) lx = ilx;
        if (ila > la) la = ila;
    }
    // format
    for (i = 0; i < n; ++i) {
        int ilx, ila, isnax;
        char *b = buf;
        isnax = (STRING_ELT(sx, i) == NA_STRING);

        if (isnax) {
            ilx = 2;
            for (; ilx < lx; ++ilx) *b++ = ' ';
            strcpy0(b, "NA");
        } else {
            const char *x = CHAR(STRING_ELT(sx, i));
            ilx = strlen(x);
            for (; ilx < lx; ++ilx) *b++ = ' ';
            strcpy0(b, x);
        }

        b = buf + lx;
        if (la) {
            const char *a = CHAR(STRING_ELT(sa, i));
            ila = strlen(a);
            *b++ = ' ';
            if (ila) {
                *b++ = '(';
                strcpy0(b, a);
                b += ila;
                *b++ = ')';
                for (; ila < la; ++ila) *b++ = ' ';
            } else for (ila = -2; ila < la; ++ila) *b++ = ' ';
        }
        *b = '\0';
        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}



// tinterval2char
// ==================================================================

// this function takes concatenated start and end indices after as.character
TIND__ATTRIBUTE_FUNCTION
SEXP
tinterval2char(SEXP sx)
{
    size_t i, n2, n;
    int lx = 0;
    n2 = XLENGTH(sx); n = n2 / 2;
    char buf[128], dots[] = "...", dash[] = " -- ";
    SEXP sy;

    sy = PROTECT(allocVector(STRSXP, n));

    // determine lengths
    for (i = 0; i < n2; ++i) {
        int ilx;
        ilx = (STRING_ELT(sx, i) == NA_STRING) ? 3 : strlen(CHAR(STRING_ELT(sx, i)));
        if (ilx > lx) lx = ilx;
    }
    // format
    for (i = 0; i < n; ++i) {
        char *b = buf;
        const char *x;
        int isnax;
        isnax = (STRING_ELT(sx, i) == NA_STRING);
        x = isnax ? dots : CHAR(STRING_ELT(sx, i));
        strcenter(b, x, lx);
        b += lx;
        strcpy0(b, dash);
        b += 4;
        isnax = (STRING_ELT(sx, n + i) == NA_STRING);
        x = isnax ? dots : CHAR(STRING_ELT(sx, n + i));
        strcenter(b, x, lx);
        b += lx;
        *b = '\0';
        SET_STRING_ELT(sy, i, mkChar(buf));
    }

    UNPROTECT(1);
    return sy;
}



// format_tinterval
// ==================================================================

// this function takes concatenated start and end indices, auxiliary info,
// separator string, and the string used for denoting open ends
TIND__ATTRIBUTE_FUNCTION
SEXP
format_tinterval(SEXP sx, SEXP sa, SEXP ssep, SEXP sopen)
{
    size_t i, n2, n;
    int lx = 0, la = 0, lsep, lopen;
    char *buf;
    const char *sep, *open;
    SEXP sy;

    n2 = XLENGTH(sx); n = n2 / 2;
    sy = PROTECT(allocVector(STRSXP, n));

    sep = CHAR(STRING_ELT(ssep, 0));
    open = CHAR(STRING_ELT(sopen, 0));

    // determine lengths
    lsep = strlen(sep);
    lopen = strlen(open);
    for (i = 0; i < n2; ++i) {
        int ilx = (STRING_ELT(sx, i) == NA_STRING) ? lopen :
                                            strlen(CHAR(STRING_ELT(sx, i)));
        if (ilx > lx) lx = ilx;
    }
    for (i = 0; i < n; ++i) {
        int ila = strlen(CHAR(STRING_ELT(sa, i)));
        if (ila > la) la = ila;
    }

    // format
    buf = R_Calloc(2 * lx + lsep + la + 4, char);
    for (i = 0; i < n; ++i) {
        int isnax;
        char *b = buf;
        const char *x;
        isnax = (STRING_ELT(sx, i) == NA_STRING);
        x = isnax ? open : CHAR(STRING_ELT(sx, i));
        strcenter(b, x, lx);
        b += lx;
        strcpy0(b, sep);
        b += lsep;
        isnax = (STRING_ELT(sx, n + i) == NA_STRING);
        x = isnax ? open : CHAR(STRING_ELT(sx, n + i));
        strcenter(b, x, lx);
        b += lx;
        if (la) {
            const char *a = CHAR(STRING_ELT(sa, i));
            int ila = strlen(a);
            *b++ = ' ';
            if (ila) {
                *b++ = '(';
                strcpy0(b, a);
                b += ila;
                *b++ = ')';
                for (; ila < la; ++ila) *b++ = ' ';
            } else for (ila = -2; ila < la; ++ila) *b++ = ' ';
        }
        *b = '\0';
        SET_STRING_ELT(sy, i, mkChar(buf));
    }
    R_Free(buf);

    UNPROTECT(1);
    return sy;
}

