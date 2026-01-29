# library(memoise)
# # library(gmp)
# library(Rmpfr)

# noncentral_generalised_factorial_coefficient <- memoise::memoise(function(n, k, s, r) {
#   # print(paste('n =', n, 'k =', k))
#   stopifnot(n >= 0, k >= 0)
#   if (k == 0) {
#     if (n == 0) {
#       1
#     } else {
#       Rmpfr::pochMpfr(r, n)
#     }
#   }
#   else {
#     if (k > n) {
#       0
#     } else {
#       (s * k + r - n + 1) * noncentral_generalised_factorial_coefficient(n - 1, k, s, r) + s * noncentral_generalised_factorial_coefficient(n - 1, k - 1, s, r)
#     }
#   }
# })




# noncentral_generalised_factorial_coefficient(0,0,1,2)
# noncentral_generalised_factorial_coefficient(3,0,1,2)
# noncentral_generalised_factorial_coefficient(3,4,1,2)
# noncentral_generalised_factorial_coefficient(4,3,1,2)
# noncentral_generalised_factorial_coefficient(20,3,1,2)

# noncentral_generalised_factorial_coefficient(1,1,0.5,0)
# noncentral_generalised_factorial_coefficient(10, 4, 0.5, 0)
# noncentral_generalised_factorial_coefficient(100, 1, 0.5, 0)
# noncentral_generalised_factorial_coefficient(100, 4, 0.5, 0)
# noncentral_generalised_factorial_coefficient(100, 50, 0.5, 0)
# library(parallel)
# res = mclapply(1:100, FUN = function(x) noncentral_generalised_factorial_coefficient(100, x, 0.5, 0))
#
# noncentral_generalised_factorial_coefficient(6, 5, 0.4, 0)
# noncentral_generalised_factorial_coefficient(6, 5, 0.5, 0)
# noncentral_generalised_factorial_coefficient(6, 5, 0.9, 0)
# noncentral_generalised_factorial_coefficient(100, 1, 0.4, 0)
