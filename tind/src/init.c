/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 *******************************
 * init - routine registration *
 *******************************
 */

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "calendar.h"
#include "datetime.h"
#include "tlimits.h"
#include "resolution.h"
#include "num.h"
#include "tochar.h"
#include "parse.h"
#include "range.h"
#include "ordered.h"
#include "tdiff.h"
#include "tinterval.h"



static const
R_CallMethodDef callMethods[] = {
    // calendar
    {"validate_y",          (DL_FUNC) &validate_y,          1},
    {"is_leap_year",        (DL_FUNC) &is_leap_year,        1},
    {"validate_q",          (DL_FUNC) &validate_q,          1},
    {"q2y",                 (DL_FUNC) &q2y,                 1},
    {"y2q",                 (DL_FUNC) &y2q,                 1},
    {"q2qrtr",              (DL_FUNC) &q2qrtr,              1},
    {"validate_m",          (DL_FUNC) &validate_m,          1},
    {"m2y",                 (DL_FUNC) &m2y,                 1},
    {"y2m",                 (DL_FUNC) &y2m,                 1},
    {"m2q",                 (DL_FUNC) &m2q,                 1},
    {"q2m",                 (DL_FUNC) &q2m,                 1},
    {"m2mnth",              (DL_FUNC) &m2mnth,              1},
    {"weeks_in_year",       (DL_FUNC) &weeks_in_year,       1},
    {"validate_w",          (DL_FUNC) &validate_w,          1},
    {"w2week",              (DL_FUNC) &w2week,              1},
    {"w2y",                 (DL_FUNC) &w2y,                 1},
    {"y2w",                 (DL_FUNC) &y2w,                 1},
    {"days_in_quarter",     (DL_FUNC) &days_in_quarter,     1},
    {"days_in_month",       (DL_FUNC) &days_in_month,       1},
    {"validate_d",          (DL_FUNC) &validate_d,          1},
    {"day_of_year",         (DL_FUNC) &day_of_year,         1},
    {"day_of_week",         (DL_FUNC) &day_of_week,         1},
    {"d2day",               (DL_FUNC) &d2day,               1},
    {"d2y",                 (DL_FUNC) &d2y,                 1},
    {"y2d",                 (DL_FUNC) &y2d,                 1},
    {"d2q",                 (DL_FUNC) &d2q,                 1},
    {"q2d",                 (DL_FUNC) &q2d,                 1},
    {"d2m",                 (DL_FUNC) &d2m,                 1},
    {"m2d",                 (DL_FUNC) &m2d,                 1},
    {"last_day_in_month",   (DL_FUNC) &last_day_in_month,   1},
    {"w2d",                 (DL_FUNC) &w2d,                 1},
    {"d2w",                 (DL_FUNC) &d2w,                 1},
    {"d2jdn",               (DL_FUNC) &d2jdn,               1},
    {"jdn2d",               (DL_FUNC) &jdn2d,               1},
    {"easter",              (DL_FUNC) &easter,              1},
    {"yqm2yf",              (DL_FUNC) &yqm2yf,              2},
    {"w2yf",                (DL_FUNC) &w2yf,                1},
    {"d2yf",                (DL_FUNC) &d2yf,                1},
    {"floor_yqm",           (DL_FUNC) &floor_yqm,           2},
    {"ceiling_y",           (DL_FUNC) &ceiling_y,           2},
    {"ceiling_q",           (DL_FUNC) &ceiling_q,           2},
    {"ceiling_m",           (DL_FUNC) &ceiling_m,           2},
    {"floor_w",             (DL_FUNC) &floor_w,             2},
    {"ceiling_w",           (DL_FUNC) &ceiling_w,           2},
    {"floor_d",             (DL_FUNC) &floor_d,             2},
    {"ceiling_d",           (DL_FUNC) &ceiling_d,           2},
    {"validate_yq",         (DL_FUNC) &validate_yq,         2},
    {"validate_ym",         (DL_FUNC) &validate_ym,         2},
    {"validate_yw",         (DL_FUNC) &validate_yw,         2},
    {"inc_w_by_y",          (DL_FUNC) &inc_w_by_y,          2},
    {"validate_yj",         (DL_FUNC) &validate_yj,         2},
    {"inc_d_by_m",          (DL_FUNC) &inc_d_by_m,          2},
    {"lastdwinmonth",       (DL_FUNC) &lastdwinmonth,       2},
    {"validate_ymd",        (DL_FUNC) &validate_ymd,        3},
    {"validate_ywu",        (DL_FUNC) &validate_ywu,        3},
    {"nthdwinmonth",        (DL_FUNC) &nthdwinmonth,        3},
    // datetime
    {"validate_t",          (DL_FUNC) &validate_t,          1},
    {"validate_h",          (DL_FUNC) &validate_h,          1},
    {"validate_hms",        (DL_FUNC) &validate_hms,        3},
    {"plt_ymd2d",           (DL_FUNC) &plt_ymd2d,           4},
    {"plt_hms2h",           (DL_FUNC) &plt_hms2h,           4},
    {"plt_midnightdiff",    (DL_FUNC) &plt_midnightdiff,    7},
    {"dh2t_aux",            (DL_FUNC) &dh2t_aux,            3},
    {"h2hour",              (DL_FUNC) &h2hour,              1},
    {"h2min",               (DL_FUNC) &h2min,               1},
    {"h2sec",               (DL_FUNC) &h2sec,               1},
    {"floor_h",             (DL_FUNC) &floor_h,             2},
    {"ceiling_h",           (DL_FUNC) &ceiling_h,           2},
    // tlimits
    {"tlimits",             (DL_FUNC) &tlimits,             1},
    // resolution
    {"res_y",               (DL_FUNC) &res_y,               1},
    {"res_q",               (DL_FUNC) &res_q,               1},
    {"res_m",               (DL_FUNC) &res_m,               1},
    {"res_w",               (DL_FUNC) &res_w,               1},
    {"res_d",               (DL_FUNC) &res_d,               1},
    {"res_s",               (DL_FUNC) &res_s,               1},
    {"res_subs",            (DL_FUNC) &res_subs,            1},
    {"res_min",             (DL_FUNC) &res_min,             1},
    {"res_h",               (DL_FUNC) &res_h,               1},
    {"tunit",               (DL_FUNC) &tunit,               1},
    {"res_i",               (DL_FUNC) &res_i,               1},
    // num
    {"autoparse_num",       (DL_FUNC) &autoparse_num,       1},
    {"num2q",               (DL_FUNC) &num2q,               1},
    {"num2m",               (DL_FUNC) &num2m,               1},
    {"num2w",               (DL_FUNC) &num2w,               1},
    {"num2d",               (DL_FUNC) &num2d,               1},
    {"q2num",               (DL_FUNC) &q2num,               1},
    {"m2num",               (DL_FUNC) &m2num,               1},
    {"w2num",               (DL_FUNC) &w2num,               1},
    {"d2num",               (DL_FUNC) &d2num,               1},
    // tochar
    {"yqmwd2char",          (DL_FUNC) &yqmwd2char,          2},
    {"t2char",              (DL_FUNC) &t2char,              4},
    {"h2char",              (DL_FUNC) &h2char,              1},
    {"format",              (DL_FUNC) &format,              7},
    {"tdiff_yqmwd2char",    (DL_FUNC) &tdiff_yqmwd2char,    2},
    {"tdiff_t2char",        (DL_FUNC) &tdiff_t2char,        1},
    {"aux_tdiff",           (DL_FUNC) &aux_tdiff,           2},
    {"format_tdiff",        (DL_FUNC) &format_tdiff,        2},
    {"tinterval2char",      (DL_FUNC) &tinterval2char,      1},
    {"format_tinterval",    (DL_FUNC) &format_tinterval,    4},
    // parse
    {"parse",               (DL_FUNC) &parse,               7},
    {"strptind",            (DL_FUNC) &strptind,            7},
    {"parse_tdiff_yqmwd",   (DL_FUNC) &parse_tdiff_yqmwd,   1},
    {"parse_tdiff_t",       (DL_FUNC) &parse_tdiff_t,       1},
    {"parse_tinterval",     (DL_FUNC) &parse_tinterval,     2},
    // range
    {"range",               (DL_FUNC) &range,               2},
    // ordered
    {"fi2match",            (DL_FUNC) &fi2match,            5},
    {"is_ordered",          (DL_FUNC) &is_ordered,          2},
    {"is_regular_t",        (DL_FUNC) &is_regular_t,        2},
    {"is_regular_th",       (DL_FUNC) &is_regular_th,       2},
    {"unique_ord",          (DL_FUNC) &unique_ord,          1},
    {"intersect_ord",       (DL_FUNC) &intersect_ord,       2},
    {"union_ord",           (DL_FUNC) &union_ord,           2},
    {"setdiff_ord",         (DL_FUNC) &setdiff_ord,         2},
    {"match_ord",           (DL_FUNC) &match_ord,           3},
    {"merge_in_ord",        (DL_FUNC) &merge_in_ord,        2},
    {"merge_out_ord",       (DL_FUNC) &merge_out_ord,       2},
    // tdiff
    {"validate_tdiff_y",    (DL_FUNC) &validate_tdiff_y,    1},
    {"validate_tdiff_q",    (DL_FUNC) &validate_tdiff_q,    1},
    {"validate_tdiff_m",    (DL_FUNC) &validate_tdiff_m,    1},
    {"validate_tdiff_w",    (DL_FUNC) &validate_tdiff_w,    1},
    {"validate_tdiff_d",    (DL_FUNC) &validate_tdiff_d,    1},
    {"validate_tdiff_t",    (DL_FUNC) &validate_tdiff_t,    1},
    // tinterval
    {"intersect_tint",      (DL_FUNC) &intersect_tint,      4},
    {"match_tint",          (DL_FUNC) &match_tint,          4},
    {"in_tint_ord",         (DL_FUNC) &in_tint_ord,         3},
    {NULL,                  NULL,                           0}
};


void
R_init_tind(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

