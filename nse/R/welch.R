f.welch = function(y,
                   blocksize = NULL,
                   overlap = 0.5,
                   type,
                   steep) {
  if (is.null(blocksize)) {
    blocksize = round(length(y) / 4)
  }
  overlap = blocksize * overlap
  start = seq(from = 1, to = length(y), by = overlap)
  start = start[1:(length(start) - 1)]
  end = start + blocksize
  end[length(end)] = length(y)
  value = NULL
  for (i in 1:length(start)) {
    m = f.optimal_h(y[start[i]:end[i]], type = type)
    kern = f.kernel_addon(type = type,
                                m,
                                steep = steep,
                                y = y[start[i]:end[i]])
    value[i] = spectrum(y[start[i]:end[i]] ,
                        kernel = kern,
                        taper = 0.5,
                        plot = FALSE)[[2]][1]
  }
  return(mean(value))
}