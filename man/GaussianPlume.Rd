\name{GaussianPlume}
\alias{GaussianPlume}
\title{GaussianPlume}
\usage{
  GaussianPlume(Q, H, u, sigma = PasquillGifford("D"))
}
\arguments{
  \item{Q}{source emission rate (g/s)}

  \item{H}{source height (m)}

  \item{u}{wind speed (m/s)}

  \item{sigma}{(optional) function describing plume
  diffusivity (see Note)}
}
\value{
  a function f(x, y, z) parameterized with the above terms
}
\description{
  Factory method for the basic Gaussian point-source
  dispersion equation
}

