context('Gaussian plume equation')

test_that('500 meter case', {
	f <- GaussianPlume(Q=10.0, H=50.0, u=6.0, sigma=PasquillGifford('D'))
	pred <- f(cbind(x=500, y=0, z=0))
	expect_equal(round(pred * 1e6, digits=1), 19.2)
})