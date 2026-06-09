context("subformula")

expect_error(subformula(1))


set.seed(10)
env = new.env()
formula = as.formula("z ~ x + y + t + u - 1", env = env)
protected = z ~ y + u + v
subs_all = subformula(formula, protected = protected)
subs_protected = subformula(formula)

expect_identical(attr(subs_all[[1]], ".Environment"), env)
expect_equal(class(subs_all[[1]]), "formula")
expect_equal(length(subformula(formula, protected = protected)), 4)
expect_equal(length(subformula(formula)), 16 - 1)

formula = z ~ x + y + t + u
protected = z ~ y + u + v
expect_equal(length(subformula(formula, protected = protected)), 4)
expect_equal(length(subformula(formula)), 16)
