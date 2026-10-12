context('template args')

test_that('empty template args work', {
	expect_identical(args_template(character(0L), 'latex'), character(0L))
	expect_identical(args_template('other stuff', 'latex'), character(0L))
	expect_identical(args_template('%\\VignetteIndexEntry{aname}', 'latex'), character(0L))
})

test_that('wrong format for template args yields nothing', {
	expect_identical(args_template('%\\VignetteTemplate{latex}{foo.tplx}', 'html'), character(0L))
	expect_identical(args_template('%\\VignetteTemplate{html}{bar.tpl}',  'latex'), character(0L))
})

test_that('one template arg works', {
	expect_identical(args_template('%\\VignetteTemplate{latex}{foo.tplx}', 'latex'), c('--template', 'foo.tplx'))
	expect_identical(args_template('%\\VignetteTemplate{html}{bar.tpl}',   'html'),  c('--template', 'bar.tpl'))
})

test_that('mixed template args work', {
	lines <- c(
		'%\\VignetteTemplate{html}{bar.tpl}',
		'%\\VignetteTemplate{latex}{foo.tplx}')
	expect_identical(args_template(lines,	'latex'), c('--template', 'foo.tplx'))
})
