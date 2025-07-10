rand_tensor <- function (modes = c(3, 4, 5), drop = FALSE)
{
  as.tensor(array(rnorm(prod(modes)), dim = modes), drop = drop)
}