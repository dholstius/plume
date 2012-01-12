#' GaussianPlume
#' 
#' Factory method for the basic Gaussian point-source dispersion equation
#' 
#' @param Q source emission rate (g/s)
#' @param H source height (m)
#' @param u wind speed (m/s)
#' @param sigma (optional) function describing plume diffusivity (see Note)
#'
#' @return a function f(x, y, z) parameterized with the above terms
#'
#' @export
GaussianPlume <- function(Q, H, u, sigma=PasquillGifford('D')) {

	plume.FUN <- function(receptors) {

		# Extract coordinates
		if (inherits(receptors, 'Spatial')) {
			coords <- coordinates(receptors)
		} else {
			coords <- receptors
		}
		x <- coords[,1]
		y <- coords[,2]
		if(ncol(coords) == 2) {
			z <- 1.8
			warning("No z values supplied. Defaulting to ", z, " meters.")
		} else if (ncol(coords) == 3) {
			z <- coords[,3]
		} else {
			warning("Coordinates have more than 3 dimensions. Using only the first 3.")
		}
		
		# Compute concentrations
		sg <- sigma(x)
	    f <- dnorm(y, sd = sg$y)
		g1 <- dnorm(z - H, sd = sg$z)
		g2 <- dnorm(z + H, sd = sg$z)
		g <- g1 + g2
		C <- Q / u * f * g
	
		# Return a nicely named vector
		names(C) <- row.names(receptors)
 		return(C)

	}
	
    return(plume.FUN)

} 