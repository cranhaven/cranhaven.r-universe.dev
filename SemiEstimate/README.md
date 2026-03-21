# SemiEstimate

author: JinhuaSu

## breif introduction

Semi-parametric estimation problem can be solved by two-step Newton-Raphson iteration. The implicit Profiling method(our arXiv paper with the title of "Implicit Profiling Estimation for Semiparametric Models with Bundled Parameters" is available at https://arxiv.org/abs/2108.07928.) is an improved method of two-step NR iteration especially for the implicit-bundled type of the parametric part and non-parametric part. This package provides a function semislv() supporting the above two methods and numeric derivative approximation for unprovided Jacobian matrix.

## designer

- S3 usage

- functional object

- there is a global varible s3 -> final result

- there are different small function to modify it

- function factory + s3

## develop log

### 2021.8.16

- check the description done
- add liuyang code for the vignettes part translate the rmd or comment some done
- add basic test for validation pick
- write a paragraph

### 2021.8.5

- add the pdf doc pick done
- debug for the basic experiments done
- fix the BBoptim done
- to solve alpha problem for the simple case done
- give a simple outline for liuyang to do the after writing doing
- add some check and automatic test: add basic test

### 2021.8.4

- Nice chance to find the simulation results as liu yang pick
- add the oxygen doc and the pdf doc

### 2021.7.26

- read R package and run a simple package pick
- remove the unessary part of my package and run it
- add more nessary part for meta test
- change the case into the my package wrapper
- debugging
  - use do.call to replace the all the ellipsis
  - think about a tiny structure to refract the current the code

https://stackoverflow.com/questions/30283389/packing-and-unpacking-elements-from-list-in-r

finding a useful method for realizing the two part: (1) mget (2) list2env (3) ellipsis(The ellipsis is a powerful tool for extending functions. Unfortunately this power comes at a cost: misspelled arguments will be silently ignored. The ellipsis package provides a collection of functions to catch problems and alert the user.)

## usage

a function:

build jac_list: check name is correct

```
new_Date <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}

new_Date(c(-1, 0, 1))
#> [1] "1969-12-31" "1970-01-01" "1970-01-02"
```

semislv <- function(theta0, lambda0, Phi_fn, Psi_fn, jac = list(), ...,
method = c("iterative", "implicit"), jacobian=FALSE, control=list())

all build function should be the constructor:

https://adv-r.hadley.nz/s3.html

## S3

---

eqfns(class):

$Phi_fn

$Psi_fn

---

jac(class):

$Phi_der_theta_fn

$Phi_der_lambda_fn

$Psi_der_theta_fn

$Psi_der_lambda_fn

constructor: new_jac -> function()
validator: check the expression name if there is (iter2)

---

quasijac(class):

$Phi_der_theta_fn

$Phi_der_lambda_fn

$Psi_der_theta_fn

$Psi_der_lambda_fn

constructor: new_jac -> function()
validator: check the expression name if there is (iter2)

---

semijac(class):

$Phi_der_theta_fn

$Phi_der_lambda_fn

$Psi_der_theta_fn

$Psi_der_lambda_fn

constructor: new_jac -> function()
validator: check the expression name if there is (iter2)

---

diyjac(class):

$ordered_fn

$itermedials(class)

$return_fn

constructor: new_jac -> function()
validator: check the expression name if there is (iter2)

---

iterspace(class): -> {"ITAT","IPAT","ITHM","IPHM"}

$initials(base list)

$eqfns

$jac_like

$iter_step

$update_delta

$parameters(base list): copy from initial at the step 1

---

resspace(list)

iterspace -> respace

## fn

generic:

update(iterspace) -> (iterspace, iter_over_flag)

update.ITAT

update.IPAT

update.ITHM

update.IPHM

savestats(resspace, iterspace)
