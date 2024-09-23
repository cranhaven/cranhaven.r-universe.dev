#' Return a list of the function parameters to hook into
#'
#' @return List of function pointers
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .rxode2parseFunPtrs()
.rxode2parseFunPtrs <- function() {
  .Call(`_rxode2parse_funPtrs`)
}
#' Convert a factor/char to an id
#'
#' @param a value to convert to an id
#' @return id factor
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .convertId("a")
.convertId <- function(a) {
  .Call(`_rxode2parse_convertId_`, a)
}

#' Get the internal breakdown of an evid
#'
#' @param i evid to breakdown
#' @return named evid integer vector
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' .getWh(1001)
#' .getWh(10401)
#'
.getWh <- function(i) {
  checkmate::assertIntegerish(i,len=1, any.missing=FALSE)
  .Call(`_rxode2parse_getWh`, as.integer(i))
}

#' This converts NONMEM-style EVIDs to classic RxODE events
#'
#' @param cmt compartment flag
#' @param amt dose amount
#' @param rate dose rate
#' @param dur dose duration
#' @param ii inter-dose interval
#' @param evid event id
#' @param ss steady state
#' @return classic evids, excluding evids that are added (you need to
#'   add them manually) or simply use etTran.  This is mostly for
#'   testing and really shouldn't be used directly.
#' @export
#' @author Matthew L. Fidler
#' @examples
#' .toClassicEvid(cmt=10, amt=3, evid=1)
#' .toClassicEvid(cmt=10, amt=3, rate=2, evid=1)
#' .toClassicEvid(cmt=10, amt=3, rate=-1, evid=1)
#' .toClassicEvid(cmt=10, amt=3, rate=-2, evid=1)
#' .toClassicEvid(cmt=10, amt=3, dur=2, evid=1)
#' .toClassicEvid(cmt=304, amt=3, dur=2, evid=1)
#' .toClassicEvid(cmt=7, amt=0, rate=2, evid=1, ss=1)
#' .toClassicEvid(cmt=-10, amt=3, evid=1)
#' .toClassicEvid(cmt=10, amt=3, evid=5)
#' .toClassicEvid(cmt=6, amt=3, evid=6)
#' .toClassicEvid(cmt=6, amt=3, evid=7)
#' .toClassicEvid(evid=2)
#' .toClassicEvid(evid=4)
.toClassicEvid <- function(cmt=1L, amt=0.0, rate=0.0, dur=0.0, ii=0.0, evid=0L, ss=0.0) {
  .w <- which(is.na(cmt))
  if (length(.w) > 0) cmt[.w] <- 1
  checkmate::assertIntegerish(cmt)
  checkmate::assertIntegerish(evid, any.missing=FALSE)
  checkmate::assertNumeric(amt)
  checkmate::assertNumeric(dur, any.missing=FALSE)
  checkmate::assertNumeric(ii)
  checkmate::assertNumeric(ss)
  .df <- data.frame(cmt=as.integer(cmt), evid=as.integer(evid), amt=as.double(amt),
                    rate=as.double(rate), dur=as.double(dur),
                    ii=as.double(ii),
                    ss=as.double(ss))
  .Call(`_rxode2parse_getClassicEvid`,
        .df$cmt, .df$amt, .df$rate, .df$dur,
        .df$ii, .df$evid, .df$ss)
}

rex::register_shortcuts("rxode2parse")
.rxDerivedReg <- rex::rex(
  start,
  or(
    group(or("V", "Q", "VP", "VT", "CLD"), number),
    "KA", "VP", "VT", "CLD", "V", "VC", "CL", "VSS", "K", "KE", "KEL",
    "Q", "VT", group("K", number, number), "AOB", "ALPHA", "BETA", "GAMMA",
    "A", "B", "C"
  ),
  end
)


#' Calculate derived parameters for the 1-, 2-, and 3- compartment
#' linear models.
#'
#' This calculates the derived parameters based on what is provided
#' in a data frame or arguments
#'
#' @param ... The input can be:
#'
#'
#'  * A data frame with PK parameters in it; This should ideally
#'  be a data frame with one pk parameter per row since it will
#'  output a data frame with one PK parameter per row.
#'
#'  * PK parameters as either a vector or a scalar
#'
#'
#' @param verbose boolean that when TRUE provides a message about the detected pk parameters
#'   and the detected compartmental model.  By default this is `FALSE`.
#'
#' @param digits represents the number of significant digits for the
#'   output; If the number is zero or below (default), do not round.
#'
#' @return Return a data.frame of derived PK parameters for a 1-, 2-,
#'   or 3-compartment linear model given provided clearances and
#'   volumes based on the inferred model type.
#'
#' The model parameters that will be provided in the data frame are:
#'
#' * `vc`: Central Volume (for 1-, 2- and 3-
#'   compartment models)
#'
#' * `kel`: First-order elimination rate (for 1-, 2-, and
#'   3-compartment models)
#'
#' * `k12`: First-order rate of transfer from central to
#'   first peripheral compartment; (for 2- and 3-compartment models)
#'
#' * `k21`: First-order rate of transfer from first
#'   peripheral to central compartment, (for 2- and 3-compartment
#'   models)
#'
#' * `k13`: First-order rate of transfer from central to
#'   second peripheral compartment; (3-compartment model)
#'
#' * `k31`: First-order rate of transfer from second
#'   peripheral to central compartment (3-compartment model)
#'
#' * `vp`: Peripheral Volume (for 2- and 3- compartment models)
#'
#' * `vp2`: Peripheral Volume for 3rd compartment (3- compartment model)
#'
#' * `vss`: Volume of distribution at steady state; (1-, 2-, and 3-compartment models)
#'
#' * `t12alpha`: \eqn{t_{1/2,\alpha}}; (1-, 2-, and 3-compartment models)
#'
#' * `t12beta`: \eqn{t_{1/2,\beta}}; (2- and 3-compartment models)
#'
#' * `t12gamma`: \eqn{t_{1/2,\gamma}}; (3-compartment model)
#'
#' * `alpha`: \eqn{\alpha}; (1-, 2-, and 3-compartment models)
#'
#' * `beta`: \eqn{\beta}; (2- and 3-compartment models)
#'
#' * `gamma`: \eqn{\beta}; (3-compartment model)
#'
#' * `A`: true `A`; (1-, 2-, and 3-compartment models)
#'
#' * `B`: true `B`; (2- and 3-compartment models)
#'
#' * `C`: true `C`; (3-compartment model)
#'
#' * `fracA`: fractional A; (1-, 2-, and 3-compartment models)
#'
#' * `fracB`: fractional B; (2- and 3-compartment models)
#'
#' * `fracC`: fractional C; (3-compartment model)
#'
#' @author Matthew Fidler and documentation from Justin Wilkins, \email{justin.wilkins@@occams.com}
#'
#' @references Shafer S. L. `CONVERT.XLS`
#'
#' @references Rowland M, Tozer TN. Clinical Pharmacokinetics and Pharmacodynamics: Concepts and Applications (4th). Clipping Williams & Wilkins, Philadelphia, 2010.
#'
#' @examples
#'
#' ## Note that rxode2 parses the names to figure out the best PK parameter
#'
#' params <- rxDerived(cl = 29.4, v = 23.4, Vp = 114, vp2 = 4614, q = 270, q2 = 73)
#'
#' ## That is why this gives the same results as the value before
#'
#' params <- rxDerived(CL = 29.4, V1 = 23.4, V2 = 114, V3 = 4614, Q2 = 270, Q3 = 73)
#'
#' ## You may also use micro-constants alpha/beta etc.
#'
#' params <- rxDerived(k12 = 0.1, k21 = 0.2, k13 = 0.3, k31 = 0.4, kel = 10, v = 10)
#'
#' ## or you can mix vectors and scalars
#'
#' params <- rxDerived(CL = 29.4, V = 1:3)
#'
#' ## If you want, you can round to a number of significant digits
#' ## with the `digits` argument:
#'
#' params <- rxDerived(CL = 29.4, V = 1:3, digits = 2)
#' @export
rxDerived <- function(..., verbose = FALSE, digits = 0) {
  .lst <- list(...)
  if (inherits(.lst[[1]], "data.frame")) {
    .lst <- .lst[[1]]
  }
  .namesU <- toupper(names(.lst))
  .w <- which(regexpr(.rxDerivedReg, .namesU) != -1)
  if (length(.w) > 1L) {
    if (verbose) {
      message("parameters: ", paste(names(.lst)[.w], collapse = ","))
    }
    .linCmt <- .Call(
      `_rxode2parse_linCmtParse`, names(.lst)[.w],
      c(
        "with(.lst,.Call(`_rxode2parse_calcDerived`, ", "list(", "0, 0, 0, 0, ",
        ", 0, 0, 0, 0),digits))"
      ),
      verbose
    )$str
    .env <- environment()
    return(eval(parse(text = .linCmt), envir = .env))
  } else {
    stop("cannot figure out PK parameters to convert", call. = FALSE)
  }
}

#' Get the information about the rxode2 derived parameter transformation
#'
#'
#' @param ... Parameters translated, should be unquoted and not assigned to anything.
#' @return Translation information; This list contains:
#'
#' - `$str` A named string of the parameters as seen in the underlying C/C++
#'   code. The parameters that are NA are not used in the linear
#'   compartment model calculations.
#'
#' - `$ncmt` the number of compartments in the model
#'
#' - `$trans` the rxode2 translation number of the parameterization
#'
#' This contains the linCmt()
#'   translation number, the number of compartments and the parameters
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' .rxTransInfo(cl, v , Vp, vp2, q, q2)
#'
#' .rxTransInfo(k12, k21, k13, k31, kel, v)
#'
#' .rxTransInfo(k12, k21, k13, k31, kel, v, ka)
#'
#' .rxTransInfo(CL, V)
#'
.rxTransInfo <- function(...) {
  .args <- as.list(match.call(expand.dots = TRUE))[-1]
  .args <- as.character(.args)
  .namesU <- toupper(as.character(.args))
  .w <- which(regexpr(.rxDerivedReg, .namesU) != -1)
  if (length(.w) > 1L) {
    .linCmt <- .Call(
      `_rxode2parse_linCmtParse`, .args[.w],
      c(
        "", "", "tlag, F, rate1, dur1, ",
        ", tlag2, F2, rate2, dur2"
      ),
      FALSE
    )
    .str <- .linCmt$str
    .str <- strsplit(.str, ", +")[[1]]
    .str <- .str[-(1:2)]
    .str <- .str[c(1:6, 11)]
    .str <- vapply(seq_along(.str), function(i) {
      .num <- suppressWarnings(as.numeric(.str[i]))
      if (is.na(.num)) return(.str[i])
      NA_character_
    }, character(1), USE.NAMES=FALSE)
    names(.str) <- c("p1", "v1", "p2", "p3","p4", "p5", "ka")
    .linCmt$str <- .str
    .linCmt
  } else {
    stop("cannot figure out PK parameters to use", call. = FALSE)
  }
}

## nocov start
.dummy <- function() {
  #dummy import to make check() and CRAN happy
  .r <- rex::rex(start, end)
  .d <- data.table::data.table(a=1)
}
#' Is the linear systems with gradients built-in
#'
#' @return logical (TRUE) if the solved systems with gradients are
#'   built-in. (FALSE) if the solves systems with gradients are absent
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .linCmtSens()
.linCmtSens <- function() {
  as.logical(.Call(`_rxode2parse_linCmtB`))
}
## nocov end
