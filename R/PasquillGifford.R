#' PasquillGifford
#'
#' Factory methods for a sigma function
#'
#' @param stability Pasquill stability class (A-F)
#'
#' @return a function sigma(x) returning a list with y and z components
#'
#' @rdname PasquillGifford
#' @export
PasquillGifford.exact <- function(stability='D') {
	if (stability == 'A') {
		sigma <- function(x) {
			x.km <- x / 1e3
			breaks <- c(0, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 100.0)
			i <- as.integer(cut(x.km, breaks, right=TRUE))
            a <- c(122.800, 158.080, 170.220, 179.520, 217.410, 258.890, 346.750, 453.850)[i]
			b <- c(0.94470, 1.05420, 1.09320, 1.12620, 1.26440, 1.40940, 1.72830, 2.11660)[i]
			sgz <- a * x.km ^ b
            sgz <- pmin(5000.0, sgz)
            Tc <- 24.1670
            Td <- 2.5334
	        Theta <- 0.017453293 * (Tc - Td * log(x.km))
	        sgy <- 465.11628 * x.km * tan(Theta)
	        values <- list(y=sgy, z=sgz)
	        return(values)
		}
	} else if (stability == 'B') {
		sigma <- function(x) {
			x.km <- x / 1e3
			breaks <- c(0, 0.20, 0.40, 100.0)
			i <- as.integer(cut(x.km, breaks, right=TRUE))
            a <- c(90.673, 98.483, 109.300)[i]
			b <- c(0.93198, 0.98332, 1.09710)[i]
			sgz <- a * x.km ^ b
            sgz <- pmin(5000.0, sgz)
            Tc <- 18.3330
            Td <- 1.8096
	        Theta <- 0.017453293 * (Tc - Td * log(x.km))
	        sgy <- 465.11628 * x.km * tan(Theta)
	        values <- list(y=sgy, z=sgz)
	        return(values)
		}
	} else if (stability == 'C') {
		sigma <- function(x) {
        	x.km <- x / 1e3
	        a <- 61.141
            b <- 0.91465
            sgz <- a * x.km ^ b
            sgz <- pmin(5000.0, sgz)
            Tc <- 12.5000
            Td <- 1.0857
	        Theta <- 0.017453293 * (Tc - Td * log(x.km))
	        sgy <- 465.11628 * x.km * tan(Theta)
	        values <- list(y=sgy, z=sgz)
	        return(values)
        } 
	} else if (stability == 'D') {
		sigma <- function(x) {
        	x.km <- x / 1e3
			breaks <- c(0, 0.30, 1.00, 3.00, 10.00, 30.00, 100.0)
			i <- as.integer(cut(x.km, breaks, right=TRUE))
			a <- c(34.459, 32.093, 32.093, 33.504, 36.650, 44.053)[i]
			b <- c(0.86974, 0.81066, 0.64403, 0.60486, 0.56589, 0.51179)[i]
			sgz <- a * x.km ^ b
			Tc <- 8.3330
			Td <- 0.72382
			Theta <- 0.017453293 * (Tc - Td * log(x.km))
			sgy <- 465.11628 * x.km * tan(Theta)
			values <- list(y=sgy, z=sgz)
			return(values) 
		}
	} else if (stability == 'E') {
		sigma <- function(x) {
        	x.km <- x / 1e3
			breaks <- c(0, 0.10, 0.30, 1.00, 2.00, 4.00, 10.00, 20.00, 40.00, 100.0)
			i <- as.integer(cut(x.km, breaks, right=TRUE))
			a <- c(24.260, 23.331, 21.628, 21.628, 22.534, 24.703, 26.970, 35.420, 47.618)[i]
			b <- c(0.83660, 0.81956, 0.75660, 0.63077, 0.57154, 0.50527, 0.46713, 0.37615, 0.29592)[i]
			sgz <- a * x.km ^ b
			Tc <- 6.2500
			Td <- 0.54287
			Theta <- 0.017453293 * (Tc - Td * log(x.km))
			sgy <- 465.11628 * x.km * tan(Theta)
			values <- list(y=sgy, z=sgz)
			return(values)
 		} 
	} else if (stability == 'F') {
		sigma <- function(x) {
        	x.km <- x / 1e3
			breaks <- c(0, 0.20, 0.70, 1.00, 2.00, 3.00, 7.00, 15.00, 30.00, 60.00, 100.00)
			i <- as.integer(cut(x.km, breaks, right=TRUE))
			a <- c(15.209, 14.457, 13.953, 13.953, 14.823, 16.187, 17.836, 22.651, 27.074, 34.219)[i]
			b <- c(0.81558, 0.78407, 0.68465, 0.63227, 0.54503, 0.46490, 0.41507, 0.32681, 0.27436, 0.21716)[i]
			sgz <- a * x.km ^ b
			Tc <- 4.1667
			Td <- 0.36191
			Theta <- 0.017453293 * (Tc - Td * log(x.km))
			sgy <- 465.11628 * x.km * tan(Theta)
			values <- list(y=sgy, z=sgz)
			return(values)
 		} 
	} else {
		stop("Unimplemented for stability class ", stability)
    }
    return(sigma)
}

#' PasquillGifford.spline
#'
#' @param exact.x distances at which to evaluate PasquillGifford.exact (m)
#'
#' @rdname PasquillGifford
#' @export
PasquillGifford.spline <- function(stability='D', exact.x=10^seq(0, 5, by=0.1)) {
    require(splines)
    sigma <- PasquillGifford.exact(stability)
    exact.values <- sigma(exact.x)
    sgy.spline <- splinefun(exact.x, exact.values$y)
    sgz.spline <- splinefun(exact.x, exact.values$z)
    sigma <- function(x) {
        sgy <- sgy.spline(x)
        sgz <- sgz.spline(x)
        values <- list(y=sgy, z=sgz)
        return(values)
    }
    return(sigma)
}

#' PasquillGifford.spline
#'
#' @export
PasquillGifford <- PasquillGifford.exact