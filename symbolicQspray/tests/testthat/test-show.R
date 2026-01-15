test_that("show - default", {
  set.seed(421)
  Q <- rSymbolicQspray()
  expect_identical(
    Print(Q),
    "{ [ -a1^2.a3^3 - 5/2*a1^2 - 5/2*a3 ] %//% [ a1^4.a3^3 - 3/2*a2^4 ] } * X^3.Y  +  { [ 5/3*a1^4.a3^4 - 1/3*a1^2.a2.a3^3 ] %//% [ a1^2 - a2^2.a3^3 ] } * Y^3 "
  )
  expect_identical(
    Print(Q + Qlone(4)),
    "{ [ -a1^2.a3^3 - 5/2*a1^2 - 5/2*a3 ] %//% [ a1^4.a3^3 - 3/2*a2^4 ] } * X1^3.X2  +  { [ 5/3*a1^4.a3^4 - 1/3*a1^2.a2.a3^3 ] %//% [ a1^2 - a2^2.a3^3 ] } * X2^3  +  { [ 1 ] } * X4 "
  )
})

test_that("show options", {
  set.seed(421)
  Q <- rSymbolicQspray()
  showSymbolicQsprayOption(Q, "a") <- "x"
  expect_identical(
    Print(Q),
    "{ [ -x1^2.x3^3 - 5/2*x1^2 - 5/2*x3 ] %//% [ x1^4.x3^3 - 3/2*x2^4 ] } * X^3.Y  +  { [ 5/3*x1^4.x3^4 - 1/3*x1^2.x2.x3^3 ] %//% [ x1^2 - x2^2.x3^3 ] } * Y^3 "
  )
  showSymbolicQsprayOption(Q, "X") <- "A"
  expect_identical(
    Print(Q),
    "{ [ -x1^2.x3^3 - 5/2*x1^2 - 5/2*x3 ] %//% [ x1^4.x3^3 - 3/2*x2^4 ] } * A1^3.A2  +  { [ 5/3*x1^4.x3^4 - 1/3*x1^2.x2.x3^3 ] %//% [ x1^2 - x2^2.x3^3 ] } * A2^3 "
  )
  showSymbolicQsprayOption(Q, "showMonomial") <-
    showMonomialXYZ(c("U","V","W"), collapse = "%")
  expect_identical(
    Print(Q),
    "{ [ -x1^2.x3^3 - 5/2*x1^2 - 5/2*x3 ] %//% [ x1^4.x3^3 - 3/2*x2^4 ] } * U^3%V  +  { [ 5/3*x1^4.x3^4 - 1/3*x1^2.x2.x3^3 ] %//% [ x1^2 - x2^2.x3^3 ] } * V^3 "
  )
  expect_identical(
    Print(Q * Qlone(4)),
    "{ [ -x1^2.x3^3 - 5/2*x1^2 - 5/2*x3 ] %//% [ x1^4.x3^3 - 3/2*x2^4 ] } * U1^3%U2%U4  +  { [ 5/3*x1^4.x3^4 - 1/3*x1^2.x2.x3^3 ] %//% [ x1^2 - x2^2.x3^3 ] } * U2^3%U4 "
  )
  showSymbolicQsprayOption(Q, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ(c("a","b","c"), lbracket = "@ ", rbracket = " @")
  expect_identical(
    Print(Q),
    "{ @ -a^2.c^3 - 5/2*a^2 - 5/2*c @  %//%  @ a^4.c^3 - 3/2*b^4 @ } * U^3%V  +  { @ 5/3*a^4.c^4 - 1/3*a^2.b.c^3 @  %//%  @ a^2 - b^2.c^3 @ } * V^3 "
  )
  expect_identical(
    Print(Q * qlone(4)),
    "{ @ -a1^2.a3^3.a4 - 5/2*a1^2.a4 - 5/2*a3.a4 @  %//%  @ a1^4.a3^3 - 3/2*a2^4 @ } * U^3%V  +  { @ 5/3*a1^4.a3^4.a4 - 1/3*a1^2.a2.a3^3.a4 @  %//%  @ a1^2 - a2^2.a3^3 @ } * V^3 "
  )
  expect_identical(
    Print(qlone(4) * Q),
    "{ @ -a1^2.a3^3.a4 - 5/2*a1^2.a4 - 5/2*a3.a4 @  %//%  @ a1^4.a3^3 - 3/2*a2^4 @ } * U^3%V  +  { @ 5/3*a1^4.a3^4.a4 - 1/3*a1^2.a2.a3^3.a4 @  %//%  @ a1^2 - a2^2.a3^3 @ } * V^3 "
  )
  showSymbolicQsprayOption(Q, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ(c("p","q","r"))
  expect_identical(
    Print(Q),
    "{ [ -p^2.r^3 - 5/2*p^2 - 5/2*r ]  %//%  [ p^4.r^3 - 3/2*q^4 ] } * U^3%V  +  { [ 5/3*p^4.r^4 - 1/3*p^2.q.r^3 ]  %//%  [ p^2 - q^2.r^3 ] } * V^3 "
  )
  expect_identical(
    Print(qlone(4) * Q),
    "{ [ -p1^2.p3^3.p4 - 5/2*p1^2.p4 - 5/2*p3.p4 ]  %//%  [ p1^4.p3^3 - 3/2*p2^4 ] } * U^3%V  +  { [ 5/3*p1^4.p3^4.p4 - 1/3*p1^2.p2.p3^3.p4 ]  %//%  [ p1^2 - p2^2.p3^3 ] } * V^3 "
  )
  expect_identical(
    Print(getCoefficient(Q, c(3, 1))),
    "[ -p^2.r^3 - 5/2*p^2 - 5/2*r ]  %//%  [ p^4.r^3 - 3/2*q^4 ] "
  )
})

test_that("show - univariate", {
  Q <- (qlone(1) / (1+qlone(1))) * Qlone(1)
  expect_identical(
    Print(Q),
    "{ [ a ] %//% [ a + 1 ] } * X "
  )
  showSymbolicQsprayOption(Q, "a") <- "w"
  expect_identical(
    Print(Q),
    "{ [ w ] %//% [ w + 1 ] } * X "
  )
  showSymbolicQsprayOption(Q, "X") <- "A"
  expect_identical(
    Print(Q),
    "{ [ w ] %//% [ w + 1 ] } * A "
  )
  Q <- (qlone(1) / (qlone(1)+qlone(2))) * Qlone(1)
  expect_identical(
    Print(Q),
    "{ [ a1 ] %//% [ a1 + a2 ] } * X "
  )
  Q <- Qlone(1) * (qlone(1) / (qlone(1)+qlone(2)))
  expect_identical(
    Print(Q),
    "{ [ a1 ] %//% [ a1 + a2 ] } * X "
  )
  R <- ((qlone(1)+qlone(2))) * Qlone(1)
  expect_identical(
    Print(Q*R),
    "{ [ a ] } * X^2 "
  )
  showSymbolicQsprayOption(Q, "showRatioOfQsprays") <-
    showRatioOfQspraysX1X2X3("a")
  expect_identical(
    Print(Q*R),
    "{ [ a1 ] } * X^2 "
  )
})

test_that("showRatioOfQspraysXYZ is OK", {
  a <- qlone(1)
  b <- qlone(2)
  X <- Qlone(1)
  Q <- a*X + (a+b)*X^2
  showSymbolicQsprayOption(Q, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ("u")
  expect_identical(
    Print(Q),
    "{ [ u1 + u2 ] } * X^2  +  { [ u1 ] } * X "
  )
  showSymbolicQsprayOption(Q, "showSymbolicQspray") <-
    showSymbolicQsprayABCXYZ("b", "Z")
  expect_identical(
    Print(Q),
    "{ [ b1 + b2 ] } * Z^2  +  { [ b1 ] } * Z "
  )
})
