twobit_seqstats <- function(filepath)
{
    filepath <- normarg_filepath(filepath)
    .Call("C_get_twobit_seqstats", filepath, PACKAGE="Rtwobitlib")
}

twobit_seqlengths <- function(filepath)
{
    filepath <- normarg_filepath(filepath)
    .Call("C_get_twobit_seqlengths", filepath, PACKAGE="Rtwobitlib")
}

