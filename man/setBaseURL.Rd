\name{setBaseURL}
\alias{setBaseURL}
\alias{getBaseURL}

\title{Set Cache Directory}
\description{
  Set/Get base URL for embedding links
}
\usage{
setBaseURL(URL)
getBaseURL()
}

\arguments{
  \item{URL}{character, URL for embedding links}
}
\details{
  \code{setBaseURL} will set the base URL for embedding links in a
  document if it does not already exist.  
}
\value{
  \code{getBaseURL} returns the current base URL.
}
\author{Roger D. Peng \email{rpeng@jhsph.edu}}

\keyword{utilities}% at least one, from doc/KEYWORDS
