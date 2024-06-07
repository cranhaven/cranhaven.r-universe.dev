# Compute the determinant of a matrix, which can be 1x1 (scalar)
gdet <- function(M)
{
	ifelse(is.matrix(M), det(M), M[1])
}
