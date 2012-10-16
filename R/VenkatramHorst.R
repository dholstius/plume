#' VenkatramHorst
#' 
#' Venkatram-Horst approximation to dispersion from a finite line source
#' 
#' @param Q source emission rate (g/s)
#' @param u wind speed (m/s)
#' @param phi wind angle
#' @param L length of line source (m)
#' @param sigma (optional) function describing plume diffusivity (see Note)
#' @return a function f(x, y) parameterized with the above terms
#'
#' @references Venkatram, A., and Horst, T.W. (2006) Atmospheric Environment 40, 2401–2408.
#'
#' @export
VenkatramHorst <- function(Q, u, phi, sigma=Briggs('D')) {
	C <- function(x, y) {
		stop('Unimplemented')
	}
	return(C)
}