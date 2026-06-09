context("internals")

expect_error(get_formula_terms("string"), "formula must be of class 'formula'")
expect_error(get_formula_response("string"), "formula must be of class 'formula'")

set.seed(10)
formula = z ~ x + y + t + u - 1
protected = z ~ y + u + v

data = data.frame(z = rnorm(10),
                  x = rnorm(10),
                  y = 1:10,
                  t = rnorm(10),
                  u = runif(10))

subs_all = subformula(formula, protected = protected)
subs_protected = subformula(formula)

expect_equal(get_formula_response(formula), "z")
expect_equal(get_formula_terms(formula), c("x", "y", "t", "u"))

expect_equal(dim(terms_matrix(z ~ x + y + t + u, protected = protected)),
             c(4, 4))
expect_equal(dim(terms_matrix(z ~ x + y + t + I(u^2) + u,
                              protected = c("t","u","w"))), c(8, 5))
expect_equal(dim(terms_matrix(z ~ x + y + t + u)),
             c(16, 4))
