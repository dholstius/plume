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
	f <- function(receptors) {
		x <- receptors[,1]
		y <- receptors[,2]
		if(ncol(receptors) == 2) {
			warning('No z values supplied. Defaulting to 1.8 m.')
			z <- 1.8
		} else {
			z <- receptors[,3]
		}
	    sg <- sigma(x)
	    crosswind <- exp(-(y ^ 2) / (2 * sg$y ^ 2)) / (sg$y * sqrt(2*pi))
	    v1 <- -(z - H) ^ 2 / (2 * sg$z ^ 2)
	    v2 <- -(z + H) ^ 2 / (2 * sg$z ^ 2)
	    vertical <- (exp(v1) + exp(v2)) / (sg$z * sqrt(2*pi))
	    concentrations <- Q / u * crosswind * vertical
	    names(concentrations) <- row.names(receptors)
	    return(concentrations)
 	}
    return(f)
} 