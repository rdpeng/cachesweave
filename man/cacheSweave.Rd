\name{cacheSweave}
\alias{cacheSweave}
\alias{cacheSweaveDriver}

\title{Cache Sweave Computations}
\description{
  Cache computations when using Sweave
}
\usage{
cacheSweave(expr, prefix = "", envir = parent.frame(), keys = NULL)
cacheSweaveDriver()
}

\arguments{
  \item{expr}{code to be evaluated}
  \item{prefix}{character, to be used for naming the cache file}
  \item{envir}{environment into which cached results should be loaded}
  \item{keys}{character vector of keys to be cached/loaded}
}
\details{
  \code{cacheSweaveDriver} is used directly with \code{Sweave} and is
  passed as the \code{driver} argument.  \code{cacheSweave} can be used
  within the R code chunk.
}
\value{
  Nothing useful is returned.
}

\author{Roger D. Peng \email{rpeng@jhsph.edu}, with much code for
\code{cacheSweaveDriver} copied from Sweave source by Friedrich Leisch
}

\seealso{
  \code{\link{Sweave}}
}

\keyword{utilities}% at least one, from doc/KEYWORDS

