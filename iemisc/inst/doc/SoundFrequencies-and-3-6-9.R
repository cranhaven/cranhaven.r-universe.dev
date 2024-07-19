## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library(iemisc)

reduce_single_digit("174 Hz")

reduce_single_digit("285 Hz")

reduce_single_digit("396 Hz")

reduce_single_digit("417 Hz")

reduce_single_digit("528 Hz")

reduce_single_digit("639 Hz")

reduce_single_digit("741 Hz")

reduce_single_digit("852 Hz")

reduce_single_digit("963 Hz")


## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

reduce_single_digit("432 Hz") # A = 432 Hertz

reduce_single_digit("440 Hz") # A = 440 Hertz

reduce_single_digit("444 Hz") # A = 444 Hertz

reduce_single_digit("128 Hz") # C = 128 Hertz

reduce_single_digit("256 Hz") # C = 256 Hertz

reduce_single_digit("512 Hz") # C = 512 Hertz

reduce_single_digit("528 Hz") # C = 528 Hertz


## ----echo = FALSE-------------------------------------------------------------

loadNamespace("printr")

## ----tidy = TRUE, comment = "", results = "asis"------------------------------

help(reduce_single_digit, package = "iemisc")


