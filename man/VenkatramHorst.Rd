\name{VenkatramHorst}
\alias{VenkatramHorst}
\title{VenkatramHorst}
\usage{
  VenkatramHorst(Q, u, phi, sigma = Briggs("D"))
}
\arguments{
  \item{Q}{source emission rate (g/s)}

  \item{u}{wind speed (m/s)}

  \item{phi}{wind angle}

  \item{L}{length of line source (m)}

  \item{sigma}{(optional) function describing plume
  diffusivity (see Note)}
}
\value{
  a function f(x, y) parameterized with the above terms
}
\description{
  Venkatram-Horst approximation to dispersion from a finite
  line source
}
\references{
  Venkatram, A., and Horst, T.W. (2006) Atmospheric
  Environment 40, 2401–2408.
}

