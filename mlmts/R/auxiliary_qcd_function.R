

library(quantspec)

auxiliary_qcd_function <- function(X, levels = c(0.1, 0.5, 0.9), freq = NULL, type = 'clipped',...) {

  if (is.null(freq)) {

  qSPG <- quantspec::smoothedPG(X, levels.1 = levels, type = type,...)
  freq <- quantspec::getFrequencies(qSPG) # Fourier frequencies
  qSPGv <- quantspec::getValues(qSPG, frequencies = freq)

  matrix <- matrix(qSPGv, ncol = 1)
  quantities <- c(Re(matrix), Im(matrix))
  quantities

  } else {

    qSPG <- quantspec::smoothedPG(X, levels.1 = levels, type = type, frequencies = freq,...)
    freq <- quantspec::getFrequencies(qSPG) # Fourier frequencies
    qSPGv <- quantspec::getValues(qSPG, frequencies = freq)

    matrix <- matrix(qSPGv, ncol = 1)
    quantities <- c(Re(matrix), Im(matrix))
    quantities

  }

}
