context('vignette building')

check_installed <- function(cmdline) {
	if (!nzchar(Sys.which(cmdline[[1L]])))
		skip(paste('Command', cmdline[[1L]], 'not found.'))
	r <- tryCatch({
		system3(cmdline[[1]], c(cmdline[-1], '--version'), capture = TRUE)
	}, error = function(e) list(code = 128L, out = toString(e)))
	
	if (r$code == 0L) {
		message('Found ', paste(cmdline, collapse = ' '), ' ', r$out)
	} else {
		skip(r$out)
	}
}

expect_creates_file <- function(name, code) {
	if (file.exists(name)) file.remove(name)
	force(code)
	created <- file.exists(name)
	if (created) file.remove(name)
	expect_true(created)
}

test_that('the test vignette can be built', {
	check_installed(c('jupyter', 'nbconvert'))
	check_installed('pdflatex')
	expect_creates_file('test-vignette.pdf', {
		tools::buildVignette('test-vignette.ipynbmeta', weave = TRUE, tangle = FALSE)
	})
})

test_that('the test script can be created', {
	check_installed(c('jupyter', 'nbconvert'))
	expect_creates_file('test-vignette.r', {
		tools::buildVignette('test-vignette.ipynbmeta', weave = FALSE, tangle = TRUE)
		script <- readChar('test-vignette.r', 20L)
		expect_identical(gsub('\r|\n', '', script), "'hi!'")
	})
})
