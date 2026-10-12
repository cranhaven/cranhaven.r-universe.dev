template_re <- '^\\s*%\\s*\\\\VignetteTemplate\\{([^}]+)\\}\\{([^}]+)\\}\\s*$'

#' @importFrom stats setNames
args_template <- function(lines, fmt) {
	template_lines <- grep(template_re, lines, value = TRUE)
	templates <- setNames(
		sub(template_re, '\\2', template_lines),
		sub(template_re, '\\1', template_lines))
	
	if (fmt %in% names(templates)) {
		c('--template', templates[[fmt]])
	} else character(0L)
}
