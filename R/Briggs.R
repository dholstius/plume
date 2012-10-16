#' Briggs
#' 
#' Briggs neutral curves for urban areas
#' 
#' @param stability Pasquill stability class
#' @return a function sigma(x) returning a list with y and z components
#' @references Briggs, G.A., 1973. Diffusion estimation for small emissions. ATDL contribution, File No. 79. Air Resources Atmospheric Turbulence and Diffusion Laboratory. NOAA, Oakridge, Tennessee, 59pp.
#' @export
Briggs <- function(stability='D') {
	if (stability == 'D') {
		sigma <- function(x) {
			list(
				y = 0.16 * x * sqrt(1 + 0.0004 * x),
				z = 0.14 * x * sqrt(1 + 0.0003 * x)
			)
		}
	} else {
		stop('Unimplemented')
	}
	return(sigma)
}