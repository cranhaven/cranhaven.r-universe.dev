#The following functions are needed to perform Step 0 of SCALPEL

#Helper function for imageFastSmooth below
#m is nrow(Y), n is ncol(Y)
imageFastSmoothHelper = function(m, n) {
  M = 2 * m
  N = 2 * n
  xi = -(m - 1):m
  yi = -(n - 1):n
  dd = sqrt((matrix(xi, M, N)^2 + matrix(yi, M, N, byrow = TRUE)^2))
  out = matrix(0.5 * exp(-abs(dd)), nrow = M, ncol = N)
  out2 = matrix(0, M, N)
  out2[m, n] = 1
  W = stats::fft(out)/stats::fft(out2)
  W = W/(M * N)

  temp2 = matrix(0, nrow = M, ncol = N)
  temp2[1:m, 1:n] = 1
  temp2 = Re(stats::fft(stats::fft(temp2) * W, inverse = TRUE))[1:m, 1:n]
  return(list(temp2=temp2, W=W))
}

#Function to perform spatial and temporal smoothing
#a simplified version of fields::image.smooth taking into account that we don't have irregularly spaced data, etc.
#takes in Y and prepped, which is the result from calling imageFastSmoothHelper
#returns the same result as fields::image.smooth(Y)$z
imageFastSmooth = function(Y, prepped) {
  m = nrow(Y)
  n = ncol(Y)

  temp = matrix(0, nrow = 2*m, ncol = 2*n)
  temp[1:m, 1:n] = Y
  temp = Re(stats::fft(stats::fft(temp) * prepped$W, inverse = TRUE))[1:m, 1:n]
  temp = temp/prepped$temp2
  return(temp)
}
