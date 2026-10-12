preprocessors_re <- '^\\s*%%\\s*\\\\VignettePreprocessors\\{%s\\}\\{\\s*([^}]+?)\\s*\\}\\s*$'

args_preprocessors <- function(lines, fmt) {
	kind <- switch(
		fmt, html = 'HTML', latex = 'Latex', markdown = 'Markdown',
		pdf = 'PDF', rst = 'RST', script = 'Script', slides = 'Slides')
	
	re <- sprintf(preprocessors_re, fmt)
	
	preprocessors_lines <- grep(re, lines, value = TRUE)
	preprocessors <- unlist(lapply(
		sub(re, '\\1', preprocessors_lines),
		function(pps) strsplit(pps, '\\s*,\\s*')))
	
	if (length(preprocessors) > 0L) {
		sprintf('--%sExporter.preprocessors=["%s"]', kind, paste(preprocessors, collapse = '","'))
	} else character(0L)
}
