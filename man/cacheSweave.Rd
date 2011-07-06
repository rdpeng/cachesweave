\name{cacheSweave}
\alias{cacheSweaveDriver}
\alias{cacheTangleDriver}
\alias{cacheSweave}
\alias{cacheSweave-package}

\title{Cache Sweave Computations}
\description{
  Cache computations when using Sweave
}
\usage{
cacheSweaveDriver()
cacheTangleDriver()
}

\details{
  Computations in Sweave documents can be cached by setting the option
  \code{cache=true} in the code check declaration.  When this option is
  set, objects that are created in the code chunk (either through
  assignment or by side effects) are stored in a key-value database in
  the directory specified by \code{setCacheDir}.

  Caching and dependencies is implemented by storing chunks modification 
  time in a metadata database. To use dependencies label your dependencies 
  and refer to them using the syntax \code{dependson=A;B}. See example.
  You can add trace information to your output file using the option
  \code{trace=true}. This will show the modification times of the chunks
  and whether cached or newly evaluated chunks are being used.
  
  The \code{cacheSweaveDriver} function is used directly with
  \code{Sweave} and is passed as the \code{driver} argument.
  For tangling the \code{cacheTangleDriver} needs to be used if you
  have dependencies in your source file.

  \code{cacheSweaveDriver} also creates a "map file" which has the
  extention ".map" which contains metadata for each of the code chunks
  in a document.  This map file can be used in conjunction with the
  \code{RRPM} package for creating reproducible research packages.
}

\note{
  Code chunks that have side effects that do not result in objects being
  created in the global environment (i.e. plotting or system
  interaction) should generally not be cached since the code will not be
  run upon reevaluation.
}

\value{
  Nothing useful is returned.
}

\author{Roger D. Peng \email{rpeng@jhsph.edu}, with much code for
\code{cacheSweaveDriver} copied from Sweave source by Friedrich Leisch
}

\examples{
\dontrun{
<<A,cache=true>>=
x <- rnorm(100)
@
<<B,cache=true>>=
y <- rnorm(100)
@
<<C,cache=true,dependson=A;B>>=
print(sum(x,y))
@
}
}

\seealso{
  \code{\link{Sweave}} and package \code{RRPM}
}

\keyword{utilities}% at least one, from doc/KEYWORDS

