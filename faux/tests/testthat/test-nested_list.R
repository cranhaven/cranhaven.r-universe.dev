ac <- function(x) {
  class(x) <- c("nested_list", "character")
  x
}

# empty ----
test_that("empty", {
  empty <- "{empty}" %>% ac()
  expect_equal(nested_list(c()), empty)
  expect_equal(nested_list(list()), empty)
  expect_equal(nested_list(NULL), empty)
  expect_equal(nested_list(""), ac(""))
  
  x <- list(list(list()))
  comp <- "1.     1. {empty}" %>% ac()
  expect_equal(nested_list(x), comp)
})

# unnamed vectors ----
test_that("unnamed vectors", {
  x <- "A"
  comp <- "A" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- 1:3
  comp <- "1. 1\n2. 2\n3. 3" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- 1:3
  comp <- "1. `1`\n2. `2`\n3. `3`" %>% ac()
  expect_equal(nested_list(x, quote = "`"), comp)
})

# named vectors ----
test_that("unnamed vectors", {
  x <- c(a = "A", b = "B")
  comp <- "* a: A\n* b: B" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- c(a = "A", b = "B")
  comp <- ">* a: A\n>* b: B" %>% ac()
  expect_equal(nested_list(x, pre = ">"), comp)
})


# unnamed lists ----
test_that("unnamed lists", {
  x <- list(list())
  comp <- "1. {empty}" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list(c())
  comp <- "1. {empty}" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list("A", "B")
  comp <- "1. A\n2. B" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list("A", "B")
  comp <- "    1. A\n    2. B" %>% ac()
  expect_equal(nested_list(x, pre = "    "), comp)
})
  
## named lists ----
test_that("named lists", {
  x <- list(a = list())
  comp <- "* a: {empty}" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list(a = c())
  comp <- "* a: {empty}" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list(a = 1)
  comp <- "* a: 1" %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list(a = 1)
  comp <- "    * a: 1" %>% ac()
  expect_equal(nested_list(x, pre = "    "), comp)
  
  x <- list(a = 1, 2)
  comp <- "* a: 1\n* {2}: 2" %>% ac()
  expect_equal(nested_list(x), comp)
})

## nested lists ----
test_that("nested lists", {
  x <- list(list("A", "B"), list("C", "D"))
  comp <- paste0("1. \n    1. A\n    2. B\n",
                 "2. \n    1. C\n    2. D") %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list(x1 = list("A", "B"), x2 = list("C", "D"))
  comp <- paste0("* x1: \n    1. A\n    2. B\n",
                 "* x2: \n    1. C\n    2. D") %>% ac()
  expect_equal(nested_list(x), comp)
  
  x <- list(x1 = list(a = "A", b = "B"), x2 = list())
  comp <- paste0("* x1: \n    * a: A\n    * b: B\n",
                 "* x2: {empty}") %>% ac()
  expect_equal(nested_list(x), comp)
})



## code ----
test_that("code", {
  f1 <- function(a = 1) {
    a + 10
  }
  x <- list(
    f1 = f1,
    f2 = function() { "hi"}
  )
  
  comp <- "* f1: 
    ```r
    function (a = 1) 
    {
        a + 10
    }
    ```
* f2: 
    ```r
    function () 
    {
        \"hi\"
    }
    ```" %>% ac()
  
  expect_equal(nested_list(x), comp)
  
  comp2 <- strsplit(comp, "\n")[[1]] %>%
    paste0(">", .) %>% 
    paste(collapse = "\n") %>%
    ac()
  
  expect_equal(nested_list(x, pre = ">"), comp2)
})

## example ----
test_that("example", {
  x <- list(
    a = list(a1 = "Named", a2 = "List"),
    b = list("Unnamed", "List"),
    c = c(c1 = "Named", c2 = "Vector"),
    d = c("Unnamed", "Vector"),
    e = list(e1 = list("A", "B", "C"),
             e2 = list(a = "A", b = "B"),
             e3 = c("A", "B", "C"),
             e4 = 100),
    f = "not a list or vector"
  )
  
  comp <- "* a: 
    * a1: Named
    * a2: List
* b: 
    1. Unnamed
    2. List
* c: 
    * c1: Named
    * c2: Vector
* d: 
    1. Unnamed
    2. Vector
* e: 
    * e1: 
        1. A
        2. B
        3. C
    * e2: 
        * a: A
        * b: B
    * e3: 
        1. A
        2. B
        3. C
    * e4: 100
* f: not a list or vector" %>% ac()
  
  expect_equal(nested_list(x), comp)
})


