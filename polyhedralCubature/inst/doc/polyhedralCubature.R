## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
f <- function(x, y, z) x*(x+1) - y*z^2
integrate(Vectorize(function(x) { 
  integrate(Vectorize(function(y) { 
    integrate(function(z) { 
      f(x,y,z) 
    }, -10, 6 - x - y)$value
   }), -5, 3 - x)$value 
}), -5, 4) 

## ----Ab-----------------------------------------------------------------------
A <- rbind(
  c(-1, 0, 0), # -x
  c( 1, 0, 0), # x
  c( 0,-1, 0), # -y
  c( 1, 1, 0), # x+y
  c( 0, 0,-1), # -z
  c( 1, 1, 1)  # x+y+z
)
b <- c(5, 4, 5, 3, 10, 6)

## ----integrate_function-------------------------------------------------------
library(polyhedralCubature)
f <- function(x, y, z) {
  x*(x+1) - y*z^2
}
integrateOverPolyhedron(f, A, b)

## ----getAb--------------------------------------------------------------------
library(ompr)
model <- MIPModel() %>%
  add_variable(x) %>% add_variable(y) %>% add_variable(z) %>%
  add_constraint(-5 <= x) %>% add_constraint(x <= 4) %>%
  add_constraint(-5 <= y) %>% add_constraint(y <= 3 - x) %>%
  add_constraint(-10 <= z) %>% add_constraint(z <= 6 - x - y)
getAb(model)

## ----integrate_spray, message=FALSE-------------------------------------------
library(spray)
x <- lone(1, 3); y <- lone(2, 3); z <- lone(3, 3)
p <- f(x, y, z)
integrateOverPolyhedron(p, A, b)

## ----character_mode-----------------------------------------------------------
storage.mode(A) <- "character"
storage.mode(b) <- "character"

## ----as_character_fraction----------------------------------------------------
as.character(1/3)

## ----integrate_qspray---------------------------------------------------------
library(qspray)
x <- qlone(1); y <- qlone(2); z <- qlone(3)
q <- f(x, y, z)
integrateOverPolyhedron(q, A, b)

