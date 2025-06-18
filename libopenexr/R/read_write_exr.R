#' Read an OpenEXR image
#'
#' Load an RGBA OpenEXR image into R numeric matrices.
#'
#' @param path Character scalar. Path to an `.exr` file.
#' @param array Default `FALSE`. Return a 4-layer RGBA array instead of a list.
#' @return A list with elements `r`, `g`, `b`, `a` (numeric matrices), and
#'   the integer dimensions `width`, `height`.
#' @export
#' @examples
#' #Write the included data to an EXR file
#' tmpfile = tempfile(fileext = ".exr")
#' write_exr(tmpfile,
#'           widecolorgamut[,,1],
#'           widecolorgamut[,,2],
#'           widecolorgamut[,,3],
#'           widecolorgamut[,,4])
#' exr_file = read_exr(tmpfile)
#' str(exr_file)
read_exr = function(path, array = FALSE) {
	path = path.expand(path)
	stopifnot(is.character(path), length(path) == 1L)
	exr = .Call("C_read_exr", path, PACKAGE = "libopenexr")
	if (!array) {
		return(exr)
	} else {
		exr_arr = array(data = 0, dim = c(exr$width, exr$height, 4))
		exr_arr[,, 1] = exr$r
		exr_arr[,, 2] = exr$g
		exr_arr[,, 3] = exr$b
		exr_arr[,, 4] = exr$a
		return(exr_arr)
	}
}

#' Write an OpenEXR image
#'
#' Save RGBA numeric matrices to an OpenEXR file (32‑bit float, ZIP compression).
#'
#' @param path Character scalar output file.
#' @param r Numeric matrix, red channel.
#' @param g Numeric matrix, green channel.
#' @param b Numeric matrix, blue channel.
#' @param a Numeric matrix, alpha channel.
#' @return None.
#' @export
#' @examples
#' #Write the included data to an EXR file
#' tmpfile = tempfile(fileext = ".exr")
#' write_exr(tmpfile,
#'           widecolorgamut[,,1],
#'           widecolorgamut[,,2],
#'           widecolorgamut[,,3],
#'           widecolorgamut[,,4])
write_exr = function(
	path,
	r,
	g,
	b,
	a = matrix(1, nrow = nrow(r), ncol = ncol(r))
) {
	path = path.expand(path)
	stopifnot(
		all(dim(r) == dim(g)) &&
			all(dim(r) == dim(b)) &&
			all(dim(r) == dim(a))
	)
	.Call(
		"C_write_exr",
		path,
		r,
		g,
		b,
		a,
		as.integer(ncol(r)),
		as.integer(nrow(r)),
		PACKAGE = "libopenexr"
	)
	invisible(NULL)
}
