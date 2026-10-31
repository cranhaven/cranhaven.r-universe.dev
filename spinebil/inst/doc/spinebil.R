## ----example------------------------------------------------------------------
library(spinebil)
## sample from the spiral distribution
d <- spiral_data(100, 4)
## the first two parameters are noise
## parameters 3 and 4 contain a spiral
## we write a list with the nuisance and structured plane
m <- list(basis_matrix(1,2,4), basis_matrix(3,4,4))
## the index functions to be evaluated should also be passed in a list
index_list <- list(tourr::holes(), tourr::cmass())
index_labels <- c("holes", "cmass")
## we can now compute the index traces and plot them
trace <- get_trace(d, m, index_list, index_labels)
plot_trace(trace)

