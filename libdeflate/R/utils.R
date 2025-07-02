#' pkg-config snapshot from install time
#'
#' Returns a data.frame with columns
#' `module`, `cflags`, `libs`, `static_libs`.
#'
#' @keywords internal
pkgcfg_db = local({
	cache = NULL
	function() {
		if (is.null(cache)) {
			path = system.file(
				"extdata",
				"pkgcfg_db.rds",
				package = "libdeflate"
			)
			cache <<- if (file.exists(path)) readRDS(path) else data.frame()
		}
		cache
	}
})
