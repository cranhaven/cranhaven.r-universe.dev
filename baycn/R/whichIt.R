whichIt <- function (burnIn,
                     iterations,
                     thinTo) {

  # Determine which iterations will be kept.
  if (burnIn == 0) {

    return (ceiling(seq(from = 1,
                        to = iterations,
                        length.out = thinTo)))

  } else {

    return (ceiling(seq(from = burnIn * iterations,
                        to = iterations,
                        length.out = thinTo)))

  }

}
