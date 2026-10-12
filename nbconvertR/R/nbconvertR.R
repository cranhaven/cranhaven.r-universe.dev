metaext_re <- '[.]ipynbmeta$'

#' Jupyter/IPython Notebook Conversion
#' 
#' An R interface for using Jupyter/IPython system calls to convert .ipynb notebooks unsing meta information.
#' When passed "<filename>.ipynbmeta" it will convert "<filename>.ipynb" to "<filename>.<ext>".
#' 
#' Apart from the standard \code{\%\\VignetteIndexEntry{<name>}} and \code{\%\\VignetteEngine{<namespace>::<name>}} directives,
#' it also understands:
#' \enumerate{
#' \item{\code{\%\\VignetteTemplate{<format>}{<filename>}}}{, which will pass a \code{--template} parameter to \code{nbconvert}}
#' \item{\code{\%\\VignettePreprocessors{<format>}{<module>.<Preproc>[, ...]}}}{, which will pass \code{--<Format>Exporter.preprocessors=["<module>.<Preproc>",...]} to \code{nbconvert}.}
#' }
#' 
#' @param file   A file with a .ipynbmeta extension that contains vignette metadata lines
#' @param fmt    A format supported by \code{nbconvert}. "script" will create an .r file, and "slides" a reveal.js-powered html presentation.
#' @param quiet  Suppress command output if TRUE (the output will always be shown on error)
#' @param ...    Ignored for now
#' @return  The filename of the resulting document, script or presentation
#' 
#' @examples \dontrun{
#' path <- system.file('doc/test-vignette.ipynbmeta', package = 'nbconvertR')
#' nbconvert(path, 'pdf')
#' }
#' 
#' @export
nbconvert <- function(
	file,
	fmt = c('html', 'latex', 'markdown', 'pdf', 'rst', 'script', 'slides'),
	#encoding = 'UTF-8',
	quiet = TRUE,
	...) {
	
	fmt <- match.arg(fmt)
	ext <- switch(
		fmt, html = '.html', latex = '.tex', markdown = '.md',
		pdf = '.pdf', rst = '.rst', script = '.r', slides = '.slides.html')
	
	lines <- readLines(file)
	
	ipynb_file <- sub(metaext_re, '.ipynb', file)
	
	args <- c(
		'nbconvert',
		args_template(lines, fmt),
		args_preprocessors(lines, fmt),
		'--to', fmt,
		ipynb_file)
	
	# allow preprocessors to be imported from the vignette dir and current dir
	pythonpath <- file.path('PYTHONPATH=.', dirname(file), Sys.getenv('PYTHONPATH'), fsep = .Platform$path.sep)
	# if !quiet, the command will directly write to stdout/err
	r <- system3('jupyter', args, capture = quiet, env = pythonpath)
	
	if (r$code != 0) {
		call <- paste(pythonpath, 'jupyter', paste(shQuote(args), collapse = ' '))
		msg <- sprintf('The call %s failed with exit status %s', dQuote(call), r$code)
		if (!is.na(r$msg)) msg <- paste(msg, 'and errmsg', r$msg)
		if (quiet) writeLines(c('Call failed. Output:', r$out), stderr())
		stop(msg)
	}
	
	filename <- sub(metaext_re, ext, basename(file))
	file.path(getwd(), filename)
}

formats <- eval(formals(nbconvert)$fmt)

#' @eval paste0('@aliases ', paste(paste0('nbconvert_', formats), collapse = '\n'))
#' @eval paste0('@usage\n', paste(sprintf('nbconvert_%s(file, quiet = TRUE, ...)', formats), collapse = '\n'))
#' @evalNamespace paste(sprintf('export(nbconvert_%s)', formats), collapse = '\n')
#' @name nbconvert
lapply(formats, function(fmt) assign(
		paste0('nbconvert_', fmt),
		function(file, quiet = TRUE, ...) nbconvert(file, fmt, quiet, ...),
		environment(nbconvert)))


.onLoad = function(libname, pkgname) {
	tools::vignetteEngine(
		'nbconvert',
		weave  = nbconvert_latex,
		tangle = nbconvert_script,
		pattern = metaext_re,
		package = 'nbconvertR')
	
	for (fmt in formats) {
		name <- paste0('nbconvert_', fmt)
		tools::vignetteEngine(
			name,
			weave  = get(name, environment(nbconvert)),
			tangle = nbconvert_script,
			pattern = metaext_re,
			package = 'nbconvertR')
	}
}
