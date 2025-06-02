# flifo: Don't get stuck with stacks in R

[![Travis-CI Build Status](https://travis-ci.org/paulponcet/flifo.svg?branch=master)](https://travis-ci.org/paulponcet/flifo) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/flifo)](https://cran.r-project.org/package=flifo) [![](https://cranlogs.r-pkg.org/badges/flifo)](https://cran.r-project.org/package=flifo)

The flifo package provides a few functions to create and manipulate FIFO (First In First Out), LIFO (Last In First Out), and NINO (Not In or Never Out) stacks in R.


## Installation

You can install flifo from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("paulponcet/flifo")
```

## Usage

Functions `fifo`, `lifo`, and `nino` are provided to 
create empty stacks. For instance: 

```R
library(flifo)

# Create an empty LIFO
s <- lifo()
print(s)

is.empty(s) # TRUE
is.fifo(s)  # FALSE
is.lifo(s)  # TRUE
```

Then `push` and `pop` enable one to add elements to and retrieve 
elements from the stack, respectively. 

```R
# Add values to 's'
push(s, 0.3)
push(s, data.frame(x=1:2, y=2:3))
print(s)
size(s)# in bytes

pop(s) # get the last element inserted
size(s)
```

A maximum number of elements can be specified at the creation of the stack (no limit in the number of elements is the default). 

```R
s <- fifo(max_size = 3)
max_size(s)

# max_size can be changed
max_size(s) <- 2
push(s, 1)
push(s, 2)
push(s, 3) # generates an error (the stack is full)
```

If an object exists in the current environment `e` and is pushed into the stack, it disappears from `e`: 

```R
s <- lifo()
x <- 3.14
exists("x") # TRUE
push(s, x)
exists("x") # FALSE
```

The `nino` function creates a stack from which we cannot not retrieve anything: 


```R
s <- nino()
push(s, "foo")
print(s)
pop(s) # generates an error
```
