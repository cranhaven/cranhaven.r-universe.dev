assert <- function(condition,
                   msg=c(
                     "assertion failed: ",
                     deparse(src),
                     if(!is.null(getSrcref(src))) c(
                       "at ", getSrcDirectory(src), "/", getSrcFilename(src),
                       ":", getSrcLocation(src, "line")))) {
  if (!isTRUE(condition)) {
    src <- arg_expr(condition)
    stop(msg)
  }
}

`%is%` <- expect_equal

test_that("deque", {

  x <- deque()
  x$getFirst(NA) %is% NA
  x$append("a")
  x$getFirst(NA) %is% "a"
  x$getFirst(NA) %is% NA
  x$length() %is% 0
  x$append("a")
  x$append("b")
  x$prepend("c")
  x$extract() %is% list("c", "a", "b")
  x$prepend("d")
  x$prepend("e")
  x$length() %is% 5
  x$peek(1) %is% "e"
  x$peek(2) %is% "d"
  x$peek(3) %is% "c"
  x$peek(4) %is% "a"
  x$peek(5) %is% "b"
  x$peek(6, NA) %is% NA
  x$peek(-1, NA) %is% "b"
  x$peek(-2, NA) %is% "a"
  x$peek(-3, NA) %is% "c"
  x$peek(-4, NA) %is% "d"
  x$peek(-5, NA) %is% "e"
  x$extract(-3:-1) %is% list("c", "a", "b")
  x$extract(1:3) %is% list("e", "d", "c")
  x$extract(c(1, NA, 3)) %is% list("e", NULL, "c")
  x$extract(c(1, 0, 3), 0) %is% list("e", "c")
  x$extract(c(1, 100, 3), 0) %is% list("e", "c")
  x$extract(c(1, 100, 3), NA) %is% list("e", NULL, "c")
  x$peek(0, NA) %is% NA
  x$getLast(NA) %is% "b"
  x$getLast(NA) %is% "a"
  x$getLast(NA) %is% "c"
  x$getLast(NA) %is% "d"
  x$length() %is% 1
  x$append("f")
  x$append("g")
  x$append("h")
  x$getFirst(NA) %is% "e"
  x$peek(1) %is% "f"
  x$peek(2) %is% "g"
  x$peek(3) %is% "h"
  x$peek(4, NA) %is% NA
  x$getFirst(NA) %is% "f"
  x$getFirst(NA) %is% "g"
  x$getFirst(NA) %is% "h"

  x <- deque(len=4)
  x$append("a")
  x$prepend("z")
  x$peek(1) %is% "z"
  x$peek(2) %is% "a"
  x$append("b")
  x$length() %is% 3
  x$prepend("y")
  x$peek(2) %is% "z"
  x$peek(3) %is% "a"
  x$length() %is% 4
  x$append("c")
  x$length() %is% 5
  x$getFirst() %is% "y"
  x$getFirst() %is% "z"
  x$getFirst() %is% "a"
  x$getFirst() %is% "b"
  x$getFirst() %is% "c"
  x$getFirst(NULL) %is% NULL

  x <- deque(len=4)
  for (i in 1:100) {
    for (j in 1:i)
      x$append(j)
    for (j in 1:i)
      assert(x$getFirst(NULL) == j)
    assert(is.null(x$getFirst(NULL)))
    #
    for (j in i:1)
      x$prepend(j)
    for (j in i:1)
      assert(x$getLast(NULL) == j)
    assert(is.null(x$getFirst(NULL)))
    #
    for (j in 1:i)
      x$append(j)
    for (j in i:1)
      assert(x$getLast(NULL) == j)
    assert(is.null(x$getLast(NULL)))
    #
    for (j in i:1)
      x$prepend(j)
    for (j in i:1)
      assert(x$getLast(NULL) == j)
    assert(is.null(x$getLast(NULL)))
  }
  x$length() %is% 0

})
