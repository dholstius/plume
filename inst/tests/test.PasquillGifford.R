context('Pasquill-Gifford curves')

x <- 500.0

test_that('Exact solution', {
	sigma <- PasquillGifford.exact('D')
	sg <- lapply(sigma(x), round, digits=1)
	expect_equal(sg$y, 36.1)
	expect_equal(sg$z, 18.3)
})

test_that('Spline interpolation', {
	sigma <- PasquillGifford.spline('D')
	sg <- lapply(sigma(x), round, digits=1)
	expect_equal(sg$y, 36.1)
	expect_equal(sg$z, 18.3)
})