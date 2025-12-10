
x = data.frame(id=c(1,2,3,4,5),  name=c('a','b','c', 'd','e'))

y=data.frame(id=c(1,2,3,4,7,6,5), tens=c(10,29,37,46,58, 34, 44),
                 name=c('a','b','c','d','e', 'f','g'))
y2=data.frame(id1=c(1,2,3,4,7,6,5), tens1=c(10,29,37,46,58, 34, 44),
             name1=c('a','b','c','d','e', 'f','g'))
mode <- 'bet' ; mode2 <- 'best'

test_that("Both x and y are datasets", code = { expect_error(getdiff(x= x$id, y= y))})

test_that("All column names are different", code = { expect_error(getdiff(x= x, y= y2))})

test_that("Expects a dataframe",
          code = {
            expect_s3_class(getdiff(x= x, y= y), 'data.frame')

            #extract rows when all columns are considered
            expect_type(getdiff(x=x, y= y, full = TRUE), 'double')
            })

test_that(desc = "Same datasets-error",
          code = {
            x1 <- data.frame(a=c(1,2,4,5,8), b= c(7,8,9,9.8,4))

            x2 <- data.frame(a=c(1,2,4,5,8), b= c(7,8,9,9.8,4))
            expect_error(getdiff(x1, x2))
          })



test_that("Returns error if the choices are not in x",
          code= {expect_error(match.argc(x= mode, choices = c('best','abs')))})

test_that("Returns error if the choices are not in x",
          code= {expect_error(match.argc(x= mode, choices = c('best','abs')))})

# test_that("Suggested package return an error if the name is wrong",
#           code = {expect_null((suppressWarnings(suggested.packages(listpkgs = 'dgdhl', reason = 'checksa')))$dgdhl)
#            })

