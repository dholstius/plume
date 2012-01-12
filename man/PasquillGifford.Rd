\name{PasquillGifford.exact}
\alias{PasquillGifford}
\alias{PasquillGifford.exact}
\alias{PasquillGifford.spline}
\title{PasquillGifford}
\usage{
  PasquillGifford.exact(stability = "D")

  PasquillGifford.spline(stability = "D",
    exact.x = 10^seq(0, 5, by = 0.1))

  PasquillGifford(stability = "D")
}
\arguments{
  \item{stability}{Pasquill stability class (A-F)}

  \item{exact.x}{distances at which to evaluate
  PasquillGifford.exact (m)}
}
\value{
  a function sigma(x) that returns a list with y and z
  components
}
\description{
  Factory methods for a sigma function

  PasquillGifford.spline

  PasquillGifford.spline
}

