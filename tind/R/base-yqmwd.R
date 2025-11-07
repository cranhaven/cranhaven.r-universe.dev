#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ##################################### #
# years, quarters, months, weeks, dates #
# ##################################### #


# .Call wrappers
# ###################################################################

# 2-arg ops, diff, inc, wd2d, yj2d, ...
.Call_2 <- function(.NAME, X, Y)
{
    nx <- length(X)
    ny <- length(Y)
    if (!.check_lengths(nx, ny)) return (integer())

    return (switch(.NAME,
                   validate_yq =    .Call(C_validate_yq,   X, Y),
                   validate_ym =    .Call(C_validate_ym,   X, Y),
                   validate_yw =    .Call(C_validate_yw,   X, Y),
                   inc_w_by_y =     .Call(C_inc_w_by_y,    X, Y),
                   validate_yj =    .Call(C_validate_yj,   X, Y),
                   inc_d_by_m =     .Call(C_inc_d_by_m,    X, Y),
                   lastdwinmonth =  .Call(C_lastdwinmonth, X, Y)))
}


# 3-arg ops, validate_ymd, ... + validate_hms from base-th.R
.Call_3 <- function(.NAME, X, Y, Z)
{
    nx <- length(X)
    ny <- length(Y)
    nz <- length(Z)
    if (!.check_lengths(nx, ny, nz)) return (if (is.double(Z)) double()
                                             else integer())

    return (switch(.NAME,
                   validate_ymd = .Call(C_validate_ymd, X, Y, Z),
                   validate_ywu = .Call(C_validate_ywu, X, Y, Z),
                   nthdwinmonth = .Call(C_nthdwinmonth, X, Y, Z),
                   validate_hms = .Call(C_validate_hms, X, Y, Z)))
}


## NOTE: validate_x returns the original vector with NAs replacing invalid
## indices. validate_xy[z]? creates indices from components replacing invalid
## indices with NAs.


# years
# ###################################################################

.validate_y <- function(y) .Call(C_validate_y, .require_mode(y, "integer"))

.is.leap_year <- function(y) .Call(C_is_leap_year, y)

.y2char <- function(y) .Call(C_yqmwd2char, y, "y")

.y2yf <- function(y) .Call(C_yqm2yf, y, "y")

.floor_y <- function(y, n) .Call(C_floor_yqm, y, n)

.ceiling_y <- function(y, n) .Call(C_ceiling_y, y, n)


# quarters
# ###################################################################

.validate_q <- function(q) .Call(C_validate_q, .require_mode(q, "integer"))

.q2char <- function(q) .Call(C_yqmwd2char, q, "q")

.q2qrtr <- function(q) .Call(C_q2qrtr, q)

.q2y <- function(q) .Call(C_q2y, q)

.y2q <- function(y) .Call(C_y2q, y)

.q2yf <- function(q) .Call(C_yqm2yf, q, "q")

.floor_q <- function(q, n) .Call(C_floor_yqm, q, n)

.ceiling_q <- function(q, n) .Call(C_ceiling_q, q, n)

.validate_yq <- function(y, q)
    .Call_2("validate_yq", .require_mode(y, "integer"),
                           .require_mode(q, "integer"))


# months
# ###################################################################

.validate_m <- function(m) .Call(C_validate_m, .require_mode(m, "integer"))

.m2char <- function(m) .Call(C_yqmwd2char, m, "m")

.m2mnth <- function(m) .Call(C_m2mnth, m)

.m2y <- function(m) .Call(C_m2y, m)

.y2m <- function(y) .Call(C_y2m, y)

.m2q <- function(m) .Call(C_m2q, m)

.q2m <- function(q) .Call(C_q2m, q)

.m2yf <- function(m) .Call(C_yqm2yf, m, "m")

.floor_m <- function(m, n) .Call(C_floor_yqm, m, n)

.ceiling_m <- function(m, n) .Call(C_ceiling_m, m, n)

.validate_ym <- function(y, m)
    .Call_2("validate_ym", .require_mode(y, "integer"),
                           .require_mode(m, "integer"))


# weeks
# ###################################################################

.weeks_in_year <- function(y) .Call(C_weeks_in_year, y)

.validate_w <- function(w) .Call(C_validate_w, .require_mode(w, "integer"))

.w2char <- function(w) .Call(C_yqmwd2char, w, "w")

.w2week <- function(w) .Call(C_w2week, w)

.w2y <- function(w) .Call(C_w2y, w)

.y2w <- function(y) .Call(C_y2w, y)

.w2yf <- function(w) .Call(C_w2yf, w)

.floor_w <- function(w, n) .Call(C_floor_w, w, n)

.ceiling_w <- function(w, n) .Call(C_ceiling_w, w, n)

.validate_yw <- function(y, w)
    .Call_2("validate_yw", .require_mode(y, "integer"),
                           .require_mode(w, "integer"))

.inc_w_by_y <- function(w, by) .Call_2("inc_w_by_y", w, by)


# dates
# ###################################################################

.days_in_year <- function(y) 365L + .is.leap_year(y)

.days_in_quarter <- function(q) .Call(C_days_in_quarter, q)

.days_in_month <- function(m) .Call(C_days_in_month, m)

.validate_d <- function(d) .Call(C_validate_d, .require_mode(d, "integer"))

.d2char <- function(d) .Call(C_yqmwd2char, d, "d")

.day_of_year <- function(d) .Call(C_day_of_year, d)

.day_of_week <- function(d) .Call(C_day_of_week, d)

.d2day <- function(d) .Call(C_d2day, d)

.d2y <- function(d) .Call(C_d2y, d)

.y2d <- function(y) .Call(C_y2d, y)

.d2q <- function(d) .Call(C_d2q, d)

.q2d <- function(q) .Call(C_q2d, q)

.d2m <- function(d) .Call(C_d2m, d)

.m2d <- function(m) .Call(C_m2d, m)

.last_day_in_month <- function(m) .Call(C_last_day_in_month, m)

.w2d <- function(w) .Call(C_w2d, w)

.d2w <- function(d) .Call(C_d2w, d)

.d2jdn <- function(d) .Call(C_d2jdn, d)

.jdn2d <- function(jdn) .Call(C_jdn2d, jdn)

.easter <- function(y) .Call(C_easter, y)

.d2yf <- function(d) .Call(C_d2yf, d)

.floor_d <- function(d, n) .Call(C_floor_d, d, n)

.ceiling_d <- function(d, n) .Call(C_ceiling_d, d, n)

.validate_yj <- function(y, j)
    .Call_2("validate_yj", .require_mode(y, "integer"),
                           .require_mode(j, "integer"))

.inc_d_by_m <- function(d, by) .Call_2("inc_d_by_m", d, by)

.last_dw_in_month <- function(dw, m)
    .Call_2("lastdwinmonth", .require_mode(dw, "integer"), m)

.validate_ymd <- function(y, m, d)
    .Call_3("validate_ymd", .require_mode(y, "integer"),
                            .require_mode(m, "integer"),
                            .require_mode(d, "integer"))

.validate_ywu <- function(y, w, u)
    .Call_3("validate_ywu", .require_mode(y, "integer"),
                            .require_mode(w, "integer"),
                            .require_mode(u, "integer"))

.nth_dw_in_month <- function(nth, dw, m)
    .Call_3("nthdwinmonth", .require_mode(nth, "integer"),
                            .require_mode(dw, "integer"), m)

