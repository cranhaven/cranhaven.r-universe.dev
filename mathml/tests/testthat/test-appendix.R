library(testthat)

rolog::consult(system.file(file.path("pl", "lm.pl"), package="mathml"))
test_that("linearmodel",
          {
            q <- mathml(quote(lm(EOT ~ T0 + Therapy, data=d, na.action=na.fail)))
            expect_equal(q, "<math><mrow><mi>EOT</mi><mo>=</mo><mrow><mrow><msub><mi>b</mi><mn>0</mn></msub><mo>+</mo><mrow><mrow><msub><mi>b</mi><mi>T0</mi></msub><mo>&#x2062;</mo><mi>T0</mi></mrow><mo>+</mo><mrow><msub><mi>b</mi><mi>Therapy</mi></msub><mo>&#x2062;</mo><mi>Therapy</mi></mrow></mrow></mrow><mo>+</mo><mi>&epsilon;</mi></mrow></mrow></math>")
          })


rolog::consult(system.file(file.path("pl", "nthroot.pl"), package="mathml"))
test_that("nthroot",
          {
            q <- mathml(quote(nthroot(a * (b + c), 3L)^2L))
            expect_equal(q, "<math><msup><mrow><mo>[</mo><mroot><mrow><mi>a</mi><mo>&sdot;</mo><mrow><mo>(</mo><mrow><mi>b</mi><mo>+</mo><mi>c</mi></mrow><mo>)</mo></mrow></mrow><mn>3</mn></mroot><mo>]</mo></mrow><mn>2</mn></msup></math>")
          })


rolog::consult(system.file(file.path("pl", "pval.pl"), package="mathml"))
test_that("pval1",
          {
            q <- mathml(quote(pval(0.539, P)))
            expect_equal(q,"<math><mrow><mi>P</mi><mo>=</mo><mn>0.54</mn></mrow></math>")
          })

test_that("pval2",
          {
            q <- mathml(quote(pval(0.0137, p)))
            expect_equal(q,"<math><mrow><mi>p</mi><mo>=</mo><mn>0.014</mn></mrow></math>")
          })

test_that("pval3",
          {
            q <- mathml(quote(pval(0.0003, P)))
            expect_equal(q, "<math><mrow><mi>P</mi><mo>&lt;</mo><mn>0.001</mn></mrow></math>")
          })      
