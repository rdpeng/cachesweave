\name{setCacheDir}
\alias{setCacheDir}
\alias{getCacheDir}

\title{Set Cache Directory}
\description{
  Set/Get cache directory where files are stored
}
\usage{
setCacheDir(path)
getCacheDir()
}

\arguments{
  \item{path}{character, directory where cache files will be stored}
}
\details{
  \code{setCacheDir} will create the cache directory if it does not
  already exist.  When the package \code{cacheSweave} is loaded, the
  cache directory is initially set to the directory "cache".
  This function can be called in the very first code chunk
  of the Sweave document (in which case caching should \emph{not} be
  turned on for that code chunk) or before Sweave is called on the
  document.
}
\value{
  \code{getCacheDir} returns the current cache directory.
}
\author{Roger D. Peng \email{rpeng@jhsph.edu}}

\keyword{utilities}% at least one, from doc/KEYWORDS
