context('preprocessor args')

test_that('empty preprocessor args work', {
	expect_identical(args_preprocessors(character(0L), 'html'), character(0L))
	expect_identical(args_preprocessors('other stuff', 'html'), character(0L))
	expect_identical(args_preprocessors('%\\VignetteIndexEntry{html}', 'html'), character(0L))
})

test_that('a wrong preprocessor kind is skipped', {
	expect_identical(args_preprocessors('%\\VignettePreprocessors{latex}{foo.Bar}', 'html'), character(0L))
})

test_that('one preprocessor arg works', {
	expect_identical(args_preprocessors('%\\VignettePreprocessors{latex}{foo.Bar}', 'latex'),
									 '--LatexExporter.preprocessors=["foo.Bar"]')
	expect_identical(args_preprocessors('%\\VignettePreprocessors{html}{  foo.Bar\t}', 'html'),
									 '--HTMLExporter.preprocessors=["foo.Bar"]')
})

test_that('comma separated preprocessor args work', {
	expect_identical(args_preprocessors('%\\VignettePreprocessors{latex}{foo.Bar,baz.Bam}', 'latex'),
									 '--LatexExporter.preprocessors=["foo.Bar","baz.Bam"]')
	expect_identical(args_preprocessors('%\\VignettePreprocessors{html}{  foo.Bar\t, baz.Bam\t}', 'html'),
									 '--HTMLExporter.preprocessors=["foo.Bar","baz.Bam"]')
})
