% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{gramx}
\alias{gramx}
\title{gramx}
\usage{
gramx(x,complexity)
}
\arguments{
\item{x}{data}

\item{complexity}{tuning parameter in Gaussian kernel}
}
\value{
gram matrix Q x KX x Q
}
\description{
gramx
}
\examples{
vec <- matrix(rnorm(4),2,2)
res <- gramx(vec,1)
}
\references{
Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
}
