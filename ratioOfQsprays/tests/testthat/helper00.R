ROQ1 <- function() {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  (x^2 + 5*y + z - 1) / (x + 1)
}

ROQ2 <- function() {
  x <- qlone(1)
  y <- qlone(2)
  (x^2 + 5*y - 5) / (x^2 + y^2)
}

ROQ3 <- function() {
  x <- qlone(1)
  z <- qlone(3)
  (4*x^2 + 9*z + 13) / (x + z)
}

ROQ4 <- function() {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  w <- qlone(4)
  new(
    "ratioOfQsprays",
    numerator = x + 1,
    denominator = x + 1
  )
  #(x^4 + 5*y + z - 1) / (x + 1 + w)
}

Print <- function(roq) {
  capture.output(roq)
}
