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
  cache directory is initially set to the current directory (".").
}
\value{
  \code{getCacheDir} returns the current cache directory.
}
\author{Roger D. Peng \email{rpeng@jhsph.edu}}

\keyword{utilities}% at least one, from doc/KEYWORDS
