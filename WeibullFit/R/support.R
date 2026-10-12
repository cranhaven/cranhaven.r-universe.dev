
add.col <- function(dataf, vec, namevec) {
  if (nrow(dataf) < length(vec) ){ dataf <-  # pads rows if needed
        rbind(dataf, matrix(NA, length(vec)-nrow(dataf), ncol(dataf),
              dimnames=list( NULL, names(dataf) ) ) )
	}
      length(vec) <- nrow(dataf) # pads with NA's
      dataf[, namevec] <- vec; # names new col properly
      return(dataf)
}

defineNroConsultas <- function(total) {
  if (total > 500) {
    resto = 1
    n = 2
    while (resto !=0) {
      n = n + 1
      resto = total %% n;
    }
    return (n)
  } else {
    return (1)
  }
}

# defineClasses <- function (limiteMin, limiteMax, nroClasses) {
#   classes = list()
#   i = 1
#   inicial = round(limiteMin)
#   while (inicial<round(limiteMax)) {
#     classes[[i]] = c(nroClasses,c(inicial,((inicial+(inicial+3))/2), inicial+3))
#     inicial = inicial + 3
#     i = i+1
#     nroClasses = nroClasses - 1
#   }
#   return (classes)
# }

clc <- function() {cat("\f")}
