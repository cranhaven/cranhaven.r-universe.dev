methods::setClassUnion(
    name = "numbers",
    members = c("numeric", "complex")
)


methods::setMethod(
    f = "coerce",
    signature = c(from = "ANY", to = "numbers"),
    definition = function (from, to, strict = TRUE)
{
    value <- as.numbers(from)
    if (strict)
        attributes(value) <- NULL
    value
})


as.scalar.logical <- function (x)
.Call(C_as.scalar.logical, x)


as.scalar.integer <- function (x)
.Call(C_as.scalar.integer, x)


as.scalar.real <- as.scalar.double <- as.scalar.numeric <- function (x)
.Call(C_as.scalar.real, x)


as.scalar.complex <- function (x)
.Call(C_as.scalar.complex, x)


as.scalar.number <- function (x, strict = TRUE)
.Call(C_as.scalar.number, x, strict)


as.scalar.string <- as.scalar.character <- function (x)
.Call(C_as.scalar.string, x)


as.scalar <- function (x)
.Call(C_as.scalar, x)


aslength1 <- function (x)
{
    if (!is.vector(x))
        x <- as.vector(x)
    len <- length(x)
    if (len == 1L) {
        x
    }
    else if (len > 1L) {
        warning(gettextf("first element used of '%s' argument",
            deparse(substitute(x), nlines = 1L)[1L], domain = NA))
        x[1L]
    }
    else stop(gettextf("'%s' must be of length 1", domain = NA,
        deparse(substitute(x), nlines = 1L)[1L]))
}





numbers <- function (length = 0L)
numeric(length = length)


as.numbers <- function (x = NULL, strict = TRUE, ...)
.Call(C_as.numbers, x, strict)


is.numbers <- function (x)
is.numeric(x) || is.complex(x)





hypot <- function (x, y)
{
    if (missing(y))
        .Call(C_hypot, Re(x), Im(x))
    else .Call(C_hypot, x, y)
}
